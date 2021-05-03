#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <memory>
#include <string>
#include <utility>
#include <vector>
#include "WAVM/Emscripten/Emscripten.h"
#include "WAVM/IR/FeatureSpec.h"
#include "WAVM/IR/Module.h"
#include "WAVM/IR/Operators.h"
#include "WAVM/IR/Types.h"
#include "WAVM/IR/Validate.h"
#include "WAVM/IR/Value.h"
#include "WAVM/Inline/BasicTypes.h"
#include "WAVM/Inline/CLI.h"
#include "WAVM/Inline/Config.h"
#include "WAVM/Inline/Errors.h"
#include "WAVM/Inline/Hash.h"
#include "WAVM/Inline/HashMap.h"
#include "WAVM/Inline/Timing.h"
#include "WAVM/Inline/Version.h"
#include "WAVM/LLVMJIT/LLVMJIT.h"
#include "WAVM/Logging/Logging.h"
#include "WAVM/ObjectCache/ObjectCache.h"
#include "WAVM/Platform/File.h"
#include "WAVM/Platform/Memory.h"
#include "WAVM/Runtime/Linker.h"
#include "WAVM/Runtime/Runtime.h"
#include "WAVM/VFS/SandboxFS.h"
#include "WAVM/WASI/WASI.h"
#include "WAVM/WASM/WASM.h"
#include "WAVM/WASTParse/WASTParse.h"
#include "wavm.h"

using namespace WAVM;
using namespace WAVM::IR;
using namespace WAVM::Runtime;

// A resolver that generates a stub if an inner resolver does not resolve a name.
struct StubFallbackResolver : Resolver
{
	StubFallbackResolver(Resolver& inInnerResolver, Compartment* inCompartment)
	: innerResolver(inInnerResolver), compartment(inCompartment)
	{
	}

	virtual bool resolve(const std::string& moduleName,
						 const std::string& exportName,
						 IR::ExternType type,
						 Object*& outObject) override
	{
		if(innerResolver.resolve(moduleName, exportName, type, outObject)) { return true; }
		else
		{
			Log::printf(Log::debug,
						"Generating stub for %s.%s : %s\n",
						moduleName.c_str(),
						exportName.c_str(),
						asString(type).c_str());
			return generateStub(
				moduleName, exportName, type, outObject, compartment, StubFunctionBehavior::trap);
		}
	}

private:
	Resolver& innerResolver;
	GCPointer<Compartment> compartment;
};


//此函数，将加载WASM、或WAT文件，并返回Runtime::Module
//若是WASM文件，则调用loadBinaryModule函数
//      此函数会首先WAVM::loadBinaryModule,将WASM文件中的内容序列化到IR::Module中，
//      然后编译此Module，产生objCode，IR::Module+objCode就是Runtime::Module
//      编译产生objCode的过程是可以进行Cache的，WAVM的Cache功能是缓存预编译的objCode而不是我一开始认为IR::Module，
//      而Faasm的确有IR-Module的缓存
//若是WAT文件，则调用parseModule函数
//      此函数的作用是将WAT文件加载到IR::Module中，
//      然后调用compileModule(irModule)生成RuntimeModule，注意compileModule会自动的调用缓存
static bool loadTextOrBinaryModule(const char* filename,
								   std::vector<U8>&& fileBytes,
								   const IR::FeatureSpec& featureSpec,
								   ModuleRef& outModule)
{
	// If the file starts with the WASM binary magic number, load it as a binary module.
	if(fileBytes.size() >= sizeof(WASM::magicNumber)
	   && !memcmp(fileBytes.data(), WASM::magicNumber, sizeof(WASM::magicNumber)))
	{
		WASM::LoadError loadError;
		if(Runtime::loadBinaryModule(
			   fileBytes.data(), fileBytes.size(), outModule, featureSpec, &loadError))
		{ return true; }
		else
		{
			Log::printf(Log::error,
						"Error loading WebAssembly binary file: %s\n",
						loadError.message.c_str());
			return false;
		}
	}
	else
	{
		// Make sure the WAST file is null terminated.
		fileBytes.push_back(0);

		// Parse the module text format to IR.
		std::vector<WAST::Error> parseErrors;
		IR::Module irModule(featureSpec);
		if(!WAST::parseModule(
			   (const char*)fileBytes.data(), fileBytes.size(), irModule, parseErrors))
		{
			Log::printf(Log::error, "Error parsing WebAssembly text file:\n");
			WAST::reportParseErrors(filename, (const char*)fileBytes.data(), parseErrors);
			return false;
		}

		// Compile the IR.
		outModule = Runtime::compileModule(irModule);

		return true;
	}
}

static bool loadPrecompiledModule(std::vector<U8>&& fileBytes,
								  const IR::FeatureSpec& featureSpec,
								  ModuleRef& outModule)
{
	// Module就是WASM的内存存在形式，也就是将WASM文件解析到内存，用一个数据结构存放WASM各个段的内容，
	// 因此这一部分是可以直接从文件中恢复的，在FAASM中也有次module的缓存实现
	IR::Module irModule(featureSpec);

	// Deserialize the module IR from the binary format.
	WASM::LoadError loadError;
	// 根据wasm文件，初始化Module，这一步就是把module的各个段进行赋值
	if(!WASM::loadBinaryModule(fileBytes.data(), fileBytes.size(), irModule, &loadError))
	{
		Log::printf(
			Log::error, "Error loading WebAssembly binary file: %s\n", loadError.message.c_str());
		return false;
	}

	// Check for a precompiled object section.
	// 很据自定义段precompiled_object，来判断是否是预编译过的
	// 预编译就是把解释执行的代码编译成可执行的代码，编译执行-JIT-解释执行
	// 而预编译的代码就存放在自定义段precompiled_object中;
	const CustomSection* precompiledObjectSection = nullptr;
	for(const CustomSection& customSection : irModule.customSections)
	{
		if(customSection.name == "wavm.precompiled_object")
		{
			precompiledObjectSection = &customSection;
			break;
		}
	}
	if(!precompiledObjectSection)
	{
		Log::printf(Log::error, "Input file did not contain 'wavm.precompiled_object' section.\n");
		return false;
	}
	else
	{
		// Load the IR + precompiled object code as a runtime module.
		outModule = Runtime::loadPrecompiledModule(irModule, precompiledObjectSection->data);
		return true;
	}
}

static void reportLinkErrors(const LinkResult& linkResult)
{
	Log::printf(Log::error, "Failed to link module:\n");
	for(auto& missingImport : linkResult.missingImports)
	{
		Log::printf(Log::error,
					"Missing import: module=\"%s\" export=\"%s\" type=\"%s\"\n",
					missingImport.moduleName.c_str(),
					missingImport.exportName.c_str(),
					asString(missingImport.type).c_str());
	}
}

static const char* getABIListHelpText()
{
	return "  none        No ABI: bare virtual metal.\n"
		   "  emscripten  Emscripten ABI, such as it is.\n"
		   "  wasi        WebAssembly System Interface ABI.\n";
}

void showRunHelp(Log::Category outputCategory)
{
	Log::printf(outputCategory,
				"Usage: wavm run [options] <program file> [program arguments]\n"
				"  <program file>        The WebAssembly module (.wast/.wasm) to run\n"
				"  [program arguments]   The arguments to pass to the WebAssembly function\n"
				"\n"
				"Options:\n"
				"  --function=<name>     Specify function name to run in module (default:main)\n"
				"  --precompiled         Use precompiled object code in program file\n"
				"  --nocache             Don't use the WAVM object cache\n"
				"  --enable <feature>    Enable the specified feature. See the list of supported\n"
				"                        features below.\n"
				"  --abi=<abi>           Specifies the ABI used by the WASM module. See the list\n"
				"                        of supported ABIs below. The default is to detect the\n"
				"                        ABI based on the module imports/exports.\n"
				"  --mount-root <dir>    Mounts <dir> as the WASI root directory\n"
				"  --wasi-trace=<level>  Sets the level of WASI tracing:\n"
				"                        - syscalls\n"
				"                        - syscalls-with-callstacks\n"
				"\n"
				"ABIs:\n"
				"%s"
				"\n"
				"Features:\n"
				"%s"
				"\n",
				getABIListHelpText(),
				getFeatureListHelpText().c_str());
}

template<Uptr numPrefixChars>
static bool stringStartsWith(const char* string, const char (&prefix)[numPrefixChars])
{
	return !strncmp(string, prefix, numPrefixChars - 1);
}

enum class ABI
{
	detect,
	bare,
	emscripten,
	wasi
};

struct State
{
	// 用于开启那些特性
	IR::FeatureSpec featureSpec;

	// Command-line options.
	// 可执行wasm的文件路径
	const char* filename = nullptr;
    // 指定入口函数名称
    const char* functionName = nullptr;
	// 挂载的根目录，不知道啥作用
	const char* rootMountPath = nullptr;
	// wasm的运行参数
	std::vector<std::string> runArgs;
	// 指定ABI，可以
	ABI abi = ABI::detect;
	// wasm文件是否是预编译的
	bool precompiled = false;
	//是否允许开启AVM object cache
	bool allowCaching = true;
	// 追踪Syscall
	WASI::SyscallTraceLevel wasiTraceLavel = WASI::SyscallTraceLevel::none;

	// Objects that need to be cleaned up before exiting.
	GCPointer<Compartment> compartment = createCompartment();
	std::shared_ptr<Emscripten::Process> emscriptenProcess;
	std::shared_ptr<WASI::Process> wasiProcess;
	std::shared_ptr<VFS::FileSystem> sandboxFS;

	~State()
	{
		emscriptenProcess.reset();
		wasiProcess.reset();

		WAVM_ERROR_UNLESS(tryCollectCompartment(std::move(compartment)));
	}

	bool parseCommandLineAndEnvironment(char** argv)
	{
		char** nextArg = argv;
		while(*nextArg)
		{
			if(stringStartsWith(*nextArg, "--function="))
			{
				// Specify function name to run in module (default:main) 指定函数的入口地址
				if(functionName)
				{
					Log::printf(Log::error,
								"'--function=' may only occur once on the command line.\n");
					return false;
				}

				functionName = *nextArg + strlen("--function=");
			}
			else if(stringStartsWith(*nextArg, "--abi="))
			{
                /**
                 * 指定实现的接口，如果是wasi，那就可以用系统的接口了，自定义的接口应该也是从这里进行设置和导入的
                 * Specifies the ABI used by the WASM module. See the list
                 *  of supported ABIs below. The default is to detect the
                 * ABI based on the module imports/exports.
                 *
                 *  none        No ABI: bare virtual metal.
                 *  emscripten  Emscripten ABI, such as it is.
                 *  wasi        WebAssembly System Interface ABI.
                 *
                 **/
                 // detect是初值，枚举类型
				if(abi != ABI::detect)
				{
					Log::printf(Log::error, "'--abi=' may only occur once on the command line.\n");
					return false;
				}

				const char* abiString = *nextArg + strlen("--abi=");
				if(!strcmp(abiString, "bare")) { abi = ABI::bare; }
				else if(!strcmp(abiString, "emscripten"))
				{
					abi = ABI::emscripten;
				}
				else if(!strcmp(abiString, "wasi"))
				{
					abi = ABI::wasi;
				}
				else
				{
					Log::printf(Log::error,
								"Unknown ABI '%s'. Supported ABIs:\n"
								"%s"
								"\n",
								abiString,
								getABIListHelpText());
					return false;
				}
			}
			else if(!strcmp(*nextArg, "--enable"))
			{
				/**
				 * Enable the specified feature. See the list of supported features below.
				 *
				 * Features:
				 *   mvp                       WebAssembly MVP
				 *   import-export-mutable-globals   Allows importing and exporting mutable globals
				 *   non-trapping-float-to-int   Non-trapping float-to-int conversion
				 *   sign-extension            Sign-extension
				 *   multivalue                Multiple results and block parameters
				 *   bulk-memory               Bulk memory
				 *   ref-types                 Reference types
				 *   simd                      128-bit SIMD
				 *   atomics                   Shared memories and atomic instructions
				 *   exception-handling        Exception handling
				 *   extended-name-section     Extended name section
				 *   multi-memory              Multiple memories
				 *   memory64                  Memories with 64-bit addresses
				 *   all-proposed              All features proposed for standardization
				 *
				 *   shared-tables           * Shared tables
				 *   legacy-instr-names      * Legacy instruction names
				 *   any-extern-kind-elems   * Elem segments containing non-func externs
				 *   quoted-names            * Quoted names in text format
				 *   wat-custom-sections     * Custom sections in text format
				 *   interleaved-load-store  * Interleaved SIMD load&store instructions
				 *   table64                 * Tables with 64-bit indices
				 *
				 *   all                     * All features supported by WAVM
				 *                           * Indicates a non-standard feature
				 * */
				++nextArg;
				if(!*nextArg)
				{
					Log::printf(Log::error, "Expected feature name following '--enable'.\n");
					return false;
				}

				if(!parseAndSetFeature(*nextArg, featureSpec, true))
				{
					Log::printf(Log::error,
								"Unknown feature '%s'. Supported features:\n"
								"%s"
								"\n",
								*nextArg,
								getFeatureListHelpText().c_str());
					return false;
				}
			}
			else if(!strcmp(*nextArg, "--precompiled"))
			{
				// 是否是预编译的wasm文件，预编译也就是用wavm compile处理过的，预编译之后的文件显然体积更大
				/** Use precompiled object code in program file **/
				precompiled = true;
			}
			else if(!strcmp(*nextArg, "--nocache"))
			{
				//暂时不知道是干嘛的
				/**Don't use the WAVM object cache**/
				allowCaching = false;
			}
			else if(!strcmp(*nextArg, "--mount-root"))
			{
				/**Mounts <dir> as the WASI root directory**/
				if(rootMountPath)
				{
					Log::printf(Log::error,
								"'--mount-root' may only occur once on the command line.\n");
					return false;
				}

				++nextArg;
				if(!*nextArg)
				{
					Log::printf(Log::error, "Expected path following '--mount-root'.\n");
					return false;
				}

				rootMountPath = *nextArg;
			}
			else if(stringStartsWith(*nextArg, "--wasi-trace="))
			{
				if(wasiTraceLavel != WASI::SyscallTraceLevel::none)
				{
					Log::printf(Log::error,
								"--wasi-trace=' may only occur once on the command line.\n");
					return false;
				}

				const char* levelString = *nextArg + strlen("--mount-root=");

				if(!strcmp(levelString, "syscalls"))
				{ wasiTraceLavel = WASI::SyscallTraceLevel::syscalls; }
				else if(!strcmp(levelString, "syscalls-with-callstacks"))
				{
					wasiTraceLavel = WASI::SyscallTraceLevel::syscallsWithCallstacks;
				}
				else
				{
					Log::printf(Log::error, "Invalid WASI trace level: %s\n", levelString);
					return false;
				}
			}
			else if((*nextArg)[0] != '-')
			{
				filename = *nextArg;
				++nextArg;
				break;
			}
			else
			{
				Log::printf(Log::error, "Unknown command-line argument: '%s'\n", *nextArg);
				showRunHelp(Log::error);
				return false;
			}

			++nextArg;
		}

		if(!filename)
		{
			showRunHelp(Log::error);
			return false;
		}
        // 剩余数据当作WASM的运行参数
		while(*nextArg) { runArgs.push_back(*nextArg++); };

		// Check that the requested features are supported by the host CPU.
		// 检查当前的CPU是否支持在--enable中指定的各种特性
		switch(LLVMJIT::validateTarget(LLVMJIT::getHostTargetSpec(), featureSpec))
		{
		case LLVMJIT::TargetValidationResult::valid: break;

		case LLVMJIT::TargetValidationResult::unsupportedArchitecture:
			Log::printf(Log::error, "Host architecture is not supported by WAVM.");
			return false;
		case LLVMJIT::TargetValidationResult::x86CPUDoesNotSupportSSE41:
			Log::printf(Log::error,
						"Host X86 CPU does not support SSE 4.1, which"
						" WAVM requires for WebAssembly SIMD code.\n");
			return false;
		case LLVMJIT::TargetValidationResult::wavmDoesNotSupportSIMDOnArch:
			Log::printf(Log::error, "WAVM does not support SIMD on the host CPU architecture.\n");
			return false;
		case LLVMJIT::TargetValidationResult::memory64Requires64bitTarget:
			Log::printf(Log::error, "Host CPU does not support 64-bit memories.\n");
			return false;
		case LLVMJIT::TargetValidationResult::table64Requires64bitTarget:
			Log::printf(Log::error, "Host CPU does not support 64-bit tables.\n");
			return false;

		case LLVMJIT::TargetValidationResult::invalidTargetSpec:
		default: WAVM_UNREACHABLE();
		};
        // WAVM_SCOPED_DISABLE_SECURE_CRT_WARNINGS是windows相关的，linux下可以认为是直接执行
		// 环境变量WAVM_OBJECT_CACHE_DIR指定了Obj Cache的存放位置
		const char* objectCachePath
			= WAVM_SCOPED_DISABLE_SECURE_CRT_WARNINGS(getenv("WAVM_OBJECT_CACHE_DIR"));
		if(allowCaching && objectCachePath && *objectCachePath)
		{
			Uptr maxBytes = 1024 * 1024 * 1024;
            // WAVM_OBJECT_CACHE_MAX_MB提供了cache的最大内存限制
			const char* maxMegabytesEnv
				= WAVM_SCOPED_DISABLE_SECURE_CRT_WARNINGS(getenv("WAVM_OBJECT_CACHE_MAX_MB"));
			if(maxMegabytesEnv && *maxMegabytesEnv)
			{
				int maxMegabytes = atoi(maxMegabytesEnv);
				if(maxMegabytes <= 0)
				{
					Log::printf(
						Log::error,
						"Invalid object cache size \"%s\". Expected an integer greater than 1.",
						maxMegabytesEnv);
					return false;
				}
				maxBytes = Uptr(maxMegabytes) * 1000000;
			}

			// Calculate a "code key" that identifies the code involved in compiling WebAssembly to
			// object code in the cache. If recompiling the module would produce different object
			// code, the code key should be different, and if recompiling the module would produce
			// the same object code, the code key should be the same.
			/***
			 * 计算一个“代码密钥”，该代码密钥标识将WebAssembly编译为缓存中的目标代码所涉及的代码。
			 * 如果重新编译模块将产生不同的目标代码，则代码密钥应不同；
			 * 如果重新编译模块将产生相同的目标代码，则代码密钥应相同。
			 * 此密钥和clang、WAVM的v版本相关
			 * */
			 // 返回llvm的版本信息
			LLVMJIT::Version llvmjitVersion = LLVMJIT::getVersion();
			U64 codeKey = 0;
			codeKey = Hash<U64>()(llvmjitVersion.llvmMajor, codeKey);
			codeKey = Hash<U64>()(llvmjitVersion.llvmMinor, codeKey);
			codeKey = Hash<U64>()(llvmjitVersion.llvmPatch, codeKey);
			codeKey = Hash<U64>()(llvmjitVersion.llvmjitVersion, codeKey);
			codeKey = Hash<U64>()(WAVM_VERSION_MAJOR, codeKey);
			codeKey = Hash<U64>()(WAVM_VERSION_MINOR, codeKey);
			codeKey = Hash<U64>()(WAVM_VERSION_PATCH, codeKey);

			// Initialize the object cache.
            // object cache缓存了compileModule的执行结果，保存到了MDB数据库，MDB的是微软的一个感觉类似KV的，但是好像有顺序的数据库
			// ObjectCacheInterface是一种接口，可以自定义实现，这里的实现使用了磁盘的存储
			// 缓存的是字节类型数据
			std::shared_ptr<Runtime::ObjectCacheInterface> objectCache;
			ObjectCache::OpenResult openResult
				= ObjectCache::open(objectCachePath, maxBytes, codeKey, objectCache);
			switch(openResult)
			{
			case ObjectCache::OpenResult::doesNotExist:
				Log::printf(
					Log::error, "Object cache directory \"%s\" does not exist.\n", objectCachePath);
				return false;
			case ObjectCache::OpenResult::notDirectory:
				Log::printf(Log::error,
							"Object cache path \"%s\" does not refer to a directory.\n",
							objectCachePath);
				return false;
			case ObjectCache::OpenResult::notAccessible:
				Log::printf(
					Log::error, "Object cache path \"%s\" is not accessible.\n", objectCachePath);
				return false;
			case ObjectCache::OpenResult::invalidDatabase:
				Log::printf(
					Log::error, "Object cache database in \"%s\" is not valid.\n", objectCachePath);
				return false;
			case ObjectCache::OpenResult::tooManyReaders:
				Log::printf(Log::error,
							"Object cache database in \"%s\" has too many concurrent readers.\n",
							objectCachePath);
				return false;

			case ObjectCache::OpenResult::success:
				Runtime::setGlobalObjectCache(std::move(objectCache));
				break;
			default: WAVM_UNREACHABLE();
			};
		}

		return true;
	}

	// 此函数自动检测Module所使用的ABI
	// 如果是拥有emscripten_metadata的自定义段，那么就是emscripten，也就是和Js可以胶合，运行在browser上的
	// 如果导入的函数，全部都是wasi_开头的，那么就是用的wasi的abi
	// 如果没有导入任何函数没，那么就是没有任何的abi，bare类型
	bool detectModuleABI(const IR::Module& irModule)
	{
		// First, check if the module has an Emscripten metadata section, which is an unambiguous
		// signal that it uses the Emscripten ABI.
		for(const IR::CustomSection& customSection : irModule.customSections)
		{
			if(customSection.name == "emscripten_metadata")
			{
				Log::printf(
					Log::debug,
					"Module has an \'emscripten_metadata\' section: using Emscripten ABI.\n");
				abi = ABI::emscripten;
				return true;
			}
		}

		// Otherwise, check whether it has any WASI or non-WASI imports.
		bool hasWASIImports = false;
		bool hasNonWASIImports = false;
		for(const auto& import : irModule.functions.imports)
		{
			if(stringStartsWith(import.moduleName.c_str(), "wasi_")) { hasWASIImports = true; }
			else
			{
				hasNonWASIImports = true;
			}
		}

		if(hasNonWASIImports)
		{
			// If there are any non-WASI imports, it might be an Emscripten module. However, since
			// it didn't have the 'emscripten_metadata' section, WAVM can't use it.
			Log::printf(
				Log::error,
				"Module appears to be an Emscripten module, but does not have an"
				" 'emscripten_metadata' section. WAVM only supports Emscripten modules compiled"
				" with '-s EMIT_EMSCRIPTEN_METADATA=1'.\n"
				"If this is not an Emscripten module, please use '--abi=<ABI>' on the WAVM"
				" command line to specify the correct ABI.\n");
			return false;
		}
		else if(hasWASIImports)
		{
			Log::printf(Log::debug, "Module has only WASI imports: using WASI ABI.\n");
			abi = ABI::wasi;
			return true;
		}
		else
		{
			Log::printf(Log::debug, "Module has no imports: using bare ABI.\n");
			abi = ABI::bare;
			return true;
		}
	}

	bool initABIEnvironment(const IR::Module& irModule)
	{
		// If the user didn't specify an ABI on the command-line, try to detect it from the module.
		// 首先要确定abi的类型，然后才能根据此进行链接
		if(abi == ABI::detect && !detectModuleABI(irModule)) { return false; }

		// If a directory to mount as the root filesystem was passed on the command-line, create a
		// SandboxFS for it.
		if(rootMountPath)
		{
			if(abi != ABI::wasi)
			{
				Log::printf(Log::error, "--mount-root may only be used with the WASI ABI.\n");
				return false;
			}

			std::string absoluteRootMountPath;
			if(rootMountPath[0] == '/' || rootMountPath[0] == '\\' || rootMountPath[0] == '~'
			   || rootMountPath[1] == ':')
			{ absoluteRootMountPath = rootMountPath; }
			else
			{
				absoluteRootMountPath
					= Platform::getCurrentWorkingDirectory() + '/' + rootMountPath;
			}
			sandboxFS = VFS::makeSandboxFS(&Platform::getHostFS(), absoluteRootMountPath);
		}

		if(abi == ABI::emscripten)
		{
			std::vector<std::string> args = runArgs;
			args.insert(args.begin(), "/proc/1/exe");

			// Instantiate the Emscripten environment.
			emscriptenProcess
				= Emscripten::createProcess(compartment,
											std::move(args),
											{},
											Platform::getStdFD(Platform::StdDevice::in),
											Platform::getStdFD(Platform::StdDevice::out),
											Platform::getStdFD(Platform::StdDevice::err));
		}
		else if(abi == ABI::wasi)
		{
			std::vector<std::string> args = runArgs;
			args.insert(args.begin(), "/proc/1/exe");

			// Create the WASI process.
			wasiProcess = WASI::createProcess(compartment,
											  std::move(args),
											  {},
											  sandboxFS.get(),
											  Platform::getStdFD(Platform::StdDevice::in),
											  Platform::getStdFD(Platform::StdDevice::out),
											  Platform::getStdFD(Platform::StdDevice::err));
		}
		else if(abi == ABI::bare)
		{
			// Require that a function name to invoke is specified for bare ABI modules.
			if(!functionName)
			{
				Log::printf(
					Log::error,
					"You must specify the name of the function to invoke when running a bare ABI"
					" module. e.g. wavm run --abi=bare --function=main ...\n");
				return false;
			}
		}

		if(wasiTraceLavel != WASI::SyscallTraceLevel::none)
		{
			if(abi != ABI::wasi)
			{
				Log::printf(Log::error, "--wasi-trace may only be used with the WASI ABI.\n");
				return false;
			}

			WASI::setSyscallTraceLevel(wasiTraceLavel);
		}

		return true;
	}

	I32 execute(const IR::Module& irModule, Instance* instance)
	{
		// Create a WASM execution context.
		Context* context = Runtime::createContext(compartment);

		// Call the module start function, if it has one.
		Function* startFunction = getStartFunction(instance);
		if(startFunction) { invokeFunction(context, startFunction); }

		if(emscriptenProcess)
		{
			// Initialize the Emscripten instance.
			if(!Emscripten::initializeProcess(*emscriptenProcess, context, irModule, instance))
			{ return EXIT_FAILURE; }
		}

		// Look up the function export to call, validate its type, and set up the invoke arguments.
		Function* function = nullptr;
		std::vector<Value> invokeArgs;
		WAVM_ASSERT(abi != ABI::bare || functionName);
		if(functionName)
		{
			function = asFunctionNullable(getInstanceExport(instance, functionName));
			if(!function)
			{
				Log::printf(Log::error, "Module does not export '%s'\n", functionName);
				return EXIT_FAILURE;
			}

			const FunctionType functionType = getFunctionType(function);

			if(functionType.params().size() != runArgs.size())
			{
				Log::printf(Log::error,
							"'%s' expects %" WAVM_PRIuPTR
							" argument(s), but command line had %" WAVM_PRIuPTR ".\n",
							functionName,
							Uptr(functionType.params().size()),
							Uptr(runArgs.size()));
				return EXIT_FAILURE;
			}

			for(Uptr argIndex = 0; argIndex < runArgs.size(); ++argIndex)
			{
				const std::string& argString = runArgs[argIndex];
				Value value;
				switch(functionType.params()[argIndex])
				{
				case ValueType::i32: value = (U32)atoi(argString.c_str()); break;
				case ValueType::i64: value = (U64)atol(argString.c_str()); break;
				case ValueType::f32: value = (F32)atof(argString.c_str()); break;
				case ValueType::f64: value = atof(argString.c_str()); break;
				case ValueType::v128:
				case ValueType::externref:
				case ValueType::funcref:
					Errors::fatalf("Cannot parse command-line argument for %s function parameter",
								   asString(functionType.params()[argIndex]));

				case ValueType::none:
				case ValueType::any:
				default: WAVM_UNREACHABLE();
				}
				invokeArgs.push_back(value);
			}
		}
		else if(abi == ABI::wasi || abi == ABI::emscripten)
		{
			// WASI/Emscripten just calls a _start function with the signature ()->().
			function = getTypedInstanceExport(instance, "_start", FunctionType());
			if(!function)
			{
				Log::printf(Log::error, "WASM module doesn't export WASI _start function.\n");
				return EXIT_FAILURE;
			}
		}
		WAVM_ASSERT(function);

		// Split the tagged argument values into their types and untagged values.
		std::vector<ValueType> invokeArgTypes;
		std::vector<UntaggedValue> untaggedInvokeArgs;
		for(const Value& arg : invokeArgs)
		{
			invokeArgTypes.push_back(arg.type);
			untaggedInvokeArgs.push_back(arg);
		}

		// Infer the expected type of the function from the number and type of the invoke's
		// arguments and the function's actual result types.
		const FunctionType invokeSig(getFunctionType(function).results(),
									 TypeTuple(invokeArgTypes));

		// Allocate an array to receive the invoke results.
		std::vector<UntaggedValue> untaggedInvokeResults;
		untaggedInvokeResults.resize(invokeSig.results().size());

		// Invoke the function.
		invokeFunction(
			context, function, invokeSig, untaggedInvokeArgs.data(), untaggedInvokeResults.data());

		if(untaggedInvokeResults.size() == 1 && invokeSig.results()[0] == ValueType::i32)
		{ return untaggedInvokeResults[0].i32; }
		else
		{
			// Convert the untagged result values to tagged values.
			std::vector<Value> invokeResults;
			invokeResults.resize(invokeSig.results().size());
			for(Uptr resultIndex = 0; resultIndex < untaggedInvokeResults.size(); ++resultIndex)
			{
				const ValueType resultType = invokeSig.results()[resultIndex];
				const UntaggedValue& untaggedResult = untaggedInvokeResults[resultIndex];
				invokeResults[resultIndex] = Value(resultType, untaggedResult);
			}

			Log::printf(Log::debug,
						"%s returned: %s\n",
						functionName ? functionName : "Module entry point",
						asString(invokeResults).c_str());

			return EXIT_SUCCESS;
		}
	}

	int run(char** argv)
	{
		// Parse the command line.
		// 主要的工作包括：
		// 根据命令行的参数给State的字段赋值
		// 检查开启的特性是否在本CPU上支持
		// 初始化Cache(主要是建立MDB数据库，如果没有的话，数据的添加当然是具体执行WASM的时候)
		if(!parseCommandLineAndEnvironment(argv)) { return EXIT_FAILURE; }

		// Read the specified file into a byte array.
		std::vector<U8> fileBytes;
		if(!loadFile(filename, fileBytes)) { return EXIT_FAILURE; }

		// Load the module from the byte array
		Runtime::ModuleRef module = nullptr;
		// 预编译的WASM已经将objCode写到了自己的自定义段中，因此可以直接获取，
		// 否则调用loadTextOrBinaryModule，此函数同样的可以通过缓存机制快速获取
		if(precompiled)
		{
			if(!loadPrecompiledModule(std::move(fileBytes), featureSpec, module))
			{ return EXIT_FAILURE; }
		}
		else if(!loadTextOrBinaryModule(filename, std::move(fileBytes), featureSpec, module))
		{
			return EXIT_FAILURE;
		}
		//RuntimeModule=IRModule+objCode
		const IR::Module& irModule = Runtime::getModuleIR(module);

		// Initialize the ABI-specific environment.
		// 确定abi的类型
		// 如果是wasi，那就要设置rootMountPath和配置wasiTraceLevel
		// 此外,wasi和emscripten还要配置wasiProcess
		if(!initABIEnvironment(irModule)) { return EXIT_FAILURE; }

		// Link the module with the intrinsic modules.
		// 将模块与内部模块链接。
		LinkResult linkResult;
		if(abi == ABI::emscripten)
		{
			StubFallbackResolver stubFallbackResolver(
				Emscripten::getInstanceResolver(*emscriptenProcess), compartment);
			linkResult = linkModule(irModule, stubFallbackResolver);
		}
		else if(abi == ABI::wasi)
		{
			linkResult = linkModule(irModule, WASI::getProcessResolver(*wasiProcess));
		}
		else if(abi == ABI::bare)
		{
			NullResolver nullResolver;
			linkResult = linkModule(irModule, nullResolver);
		}
		else
		{
			WAVM_UNREACHABLE();
		}

		if(!linkResult.success)
		{
			reportLinkErrors(linkResult);
			return EXIT_FAILURE;
		}

		// Instantiate the module.
		// 模块实例化
		Instance* instance = instantiateModule(
			compartment, module, std::move(linkResult.resolvedImports), filename);
		if(!instance) { return EXIT_FAILURE; }

		// Take the module's memory as the WASI process memory.
		// 将模块的内存用作WASI进程内存。
		if(abi == ABI::wasi)
		{
			Memory* memory = asMemoryNullable(getInstanceExport(instance, "memory"));
			if(!memory)
			{
				Log::printf(Log::error, "WASM module doesn't export WASI memory.\n");
				return EXIT_FAILURE;
			}
			WASI::setProcessMemory(*wasiProcess, memory);
		}

		// Execute the program.
		Timing::Timer executionTimer;
		auto executeThunk = [&] { return execute(irModule, instance); };
		int result;
		if(emscriptenProcess) { result = Emscripten::catchExit(std::move(executeThunk)); }
		else if(wasiProcess)
		{
			result = WASI::catchExit(std::move(executeThunk));
		}
		else
		{
			result = executeThunk();
		}
		Timing::logTimer("Executed program", executionTimer);

		// Log the peak memory usage.
		Uptr peakMemoryUsage = Platform::getPeakMemoryUsageBytes();
		Log::printf(
			Log::metrics, "Peak memory usage: %" WAVM_PRIuPTR "KiB\n", peakMemoryUsage / 1024);

		return result;
	}

	int runAndCatchRuntimeExceptions(char** argv)
	{
		int result = EXIT_FAILURE;
		// catchRuntimeExceptions传递两个匿名函数为参数，第一个为run用于执行，第二个用于返回异常
		// 此函数的主要作用应该是在执行出错时打印调用栈等调试信息，其主体还是运行run函数
		// 我实际测试了一下，基本上没有卵用，无法准确的定位
		Runtime::catchRuntimeExceptions([&result, argv, this]() { result = run(argv); },
										[](Runtime::Exception* exception) {
											// Treat any unhandled exception as a fatal error.
											Errors::fatalf("Runtime exception: %s",
														   describeException(exception).c_str());
										});
		return result;
	}
};

int execRunCommand(int argc, char** argv)
{
	State state;
	return state.runAndCatchRuntimeExceptions(argv);
}
