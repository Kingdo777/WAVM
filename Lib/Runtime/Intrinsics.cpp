#include "WAVM/Runtime/Intrinsics.h"
#include <initializer_list>
#include <string>
#include <utility>
#include <vector>
#include "RuntimePrivate.h"
#include "WAVM/IR/FeatureSpec.h"
#include "WAVM/IR/Module.h"
#include "WAVM/IR/Operators.h"
#include "WAVM/IR/Types.h"
#include "WAVM/IR/Validate.h"
#include "WAVM/Inline/Errors.h"
#include "WAVM/Inline/Hash.h"
#include "WAVM/Inline/HashMap.h"
#include "WAVM/Inline/Serialization.h"
#include "WAVM/Inline/Timing.h"
#include "WAVM/Platform/RWMutex.h"
#include "WAVM/Runtime/Runtime.h"

namespace WAVM { namespace Intrinsics {
	struct ModuleImpl
	{
		HashMap<std::string, Intrinsics::Function*> functionMap;
		HashMap<std::string, Intrinsics::Table*> tableMap;
		HashMap<std::string, Intrinsics::Memory*> memoryMap;
		HashMap<std::string, Intrinsics::Global*> globalMap;
	};
}}

using namespace WAVM;
using namespace WAVM::Runtime;
using namespace WAVM::IR;

Intrinsics::Module::~Module()
{
	if(impl) { delete impl; }
}

static void initializeModule(Intrinsics::Module* moduleRef)
{
	if(!moduleRef->impl) { moduleRef->impl = new Intrinsics::ModuleImpl; }
}

Intrinsics::Function::Function(Intrinsics::Module* moduleRef,
							   const char* inName,
							   void* inNativeFunction,
							   FunctionType inType)
: name(inName), type(inType), nativeFunction(inNativeFunction)
{
	initializeModule(moduleRef);

	if(moduleRef->impl->functionMap.contains(name))
	{ Errors::fatalf("Intrinsic function already registered: %s", name); }
	moduleRef->impl->functionMap.set(name, this);
}

Intrinsics::Global::Global(Intrinsics::Module* moduleRef,
						   const char* inName,
						   ValueType inType,
						   Value inValue)
: name(inName), type(inType), value(inValue)
{
	initializeModule(moduleRef);

	if(moduleRef->impl->globalMap.contains(name))
	{ Errors::fatalf("Intrinsic global already registered: %s", name); }
	moduleRef->impl->globalMap.set(name, this);
}

Intrinsics::Table::Table(Intrinsics::Module* moduleRef, const char* inName, const TableType& inType)
: name(inName), type(inType)
{
	initializeModule(moduleRef);

	if(moduleRef->impl->tableMap.contains(name))
	{ Errors::fatalf("Intrinsic table already registered: %s", name); }
	moduleRef->impl->tableMap.set(name, this);
}

Intrinsics::Memory::Memory(Intrinsics::Module* moduleRef,
						   const char* inName,
						   const MemoryType& inType)
: name(inName), type(inType)
{
	initializeModule(moduleRef);

	if(moduleRef->impl->memoryMap.contains(name))
	{ Errors::fatalf("Intrinsic memory already registered: %s", name); }
	moduleRef->impl->memoryMap.set(name, this);
}

Instance* Intrinsics::instantiateModule(
	Compartment* compartment,
	const std::initializer_list<const Intrinsics::Module*>& moduleRefs,
	std::string&& debugName)
{
	Timing::Timer timer;

	IR::Module irModule(FeatureLevel::wavm);
	irModule.featureSpec.nonWASMFunctionTypes = true;
	DisassemblyNames names;

    // 将自定义的函数写到了新建的IRModule的types,functions.imports和import中，函数的地址指针写入functionImportBindings
	std::vector<FunctionImportBinding> functionImportBindings;
	for(const Intrinsics::Module* moduleRef : moduleRefs)
	{
		if(moduleRef->impl)
		{
			// 这里是将自定义的函数写入了IrModule的types,functions.imports和import
			// 真正的函数的实现的指针放到了functionImportBindings中
			// 这里写入的是import,但是其他三个都是写到了export
			// 为什么是写到了functions.imports呢,因此写到这里只需要给出包名和函数名以及类型,但是要写到functions.defs的话
			// 这是WASM的函数,需要给出字节码的,但是我们这里其实是外部的本地函数,因此要写到functions.imports中,并且用了functionImportBindings
			// 来本地函数的存放指针,但是为什么没用同时放到export呢?不放到export怎么被其他人引用呢
			for(const auto& pair : moduleRef->impl->functionMap)
			{
				functionImportBindings.push_back({pair.value->getNativeFunction()});
				const Uptr typeIndex = irModule.types.size();
				const Uptr functionIndex = irModule.functions.size();
				irModule.types.push_back(pair.value->getType());
				//注意这里的包名是空的
				irModule.functions.imports.push_back({{typeIndex}, "", pair.value->getName()});
				irModule.imports.push_back({ExternKind::function, functionIndex});
				names.functions.push_back({pair.value->getName(), {}, {}});
			}

			for(const auto& pair : moduleRef->impl->tableMap)
			{
				const Uptr tableIndex = irModule.tables.size();
				irModule.tables.defs.push_back({pair.value->getType()});
				names.tables.push_back(pair.value->getName());
				irModule.exports.push_back({pair.value->getName(), ExternKind::table, tableIndex});
			}

			for(const auto& pair : moduleRef->impl->memoryMap)
			{
				const Uptr memoryIndex = irModule.memories.size();
				irModule.memories.defs.push_back({pair.value->getType()});
				names.memories.push_back(pair.value->getName());
				irModule.exports.push_back(
					{pair.value->getName(), ExternKind::memory, memoryIndex});
			}

			for(const auto& pair : moduleRef->impl->globalMap)
			{
				InitializerExpression initializerExpression;
				switch(pair.value->getType())
				{
				case ValueType::i32:
					initializerExpression.type = InitializerExpression::Type::i32_const;
					initializerExpression.i32 = pair.value->getValue().i32;
					break;
				case ValueType::i64:
					initializerExpression.type = InitializerExpression::Type::i64_const;
					initializerExpression.i64 = pair.value->getValue().i64;
					break;
				case ValueType::f32:
					initializerExpression.type = InitializerExpression::Type::f32_const;
					initializerExpression.f32 = pair.value->getValue().f32;
					break;
				case ValueType::f64:
					initializerExpression.type = InitializerExpression::Type::f64_const;
					initializerExpression.f64 = pair.value->getValue().f64;
					break;
				case ValueType::v128:
					initializerExpression.type = InitializerExpression::Type::v128_const;
					initializerExpression.v128 = pair.value->getValue().v128;
					break;

				case ValueType::externref:
				case ValueType::funcref:
					Errors::fatal("Intrinsic reference-typed globals are not supported");

				case ValueType::none:
				case ValueType::any:
				default: WAVM_UNREACHABLE();
				};

				const Uptr globalIndex = irModule.globals.size();
				irModule.globals.defs.push_back(
					{GlobalType(pair.value->getType(), false), initializerExpression});
				names.globals.push_back(pair.value->getName());
				irModule.exports.push_back(
					{pair.value->getName(), ExternKind::global, globalIndex});
			}
		}
	}

	// Generate thunks for the intrinsic functions.
    // 生成内部函数的thunks
	for(Uptr functionImportIndex = 0; functionImportIndex < irModule.functions.imports.size();
		++functionImportIndex)
	{
		const FunctionImport& functionImport = irModule.functions.imports[functionImportIndex];
		const FunctionType intrinsicFunctionType = irModule.types[functionImport.type.index];
		const FunctionType wasmFunctionType(intrinsicFunctionType.results(),
											intrinsicFunctionType.params(),
											CallingConvention::wasm);

		const Uptr wasmFunctionTypeIndex = irModule.types.size();
		irModule.types.push_back(wasmFunctionType);

		// 下面操作是将加载参数和调用函数,封装到codeStream,可以认为是WASM的调用函数命令
		// WASM的函数调用,在调用函数的时候会先把参数放到操作数栈
		// call指令会先将参数加载到自己的局部变量表中,然后调用local_get将其放到操作数栈进行操作
		// 在这里,我们先执行了local_get,然后再进行call,然后将其放入codeStream中
		Serialization::ArrayOutputStream codeStream;
		OperatorEncoderStream opEncoder(codeStream);
		for(Uptr paramIndex = 0; paramIndex < intrinsicFunctionType.params().size(); ++paramIndex)
		{ opEncoder.local_get({paramIndex}); }
		opEncoder.call({functionImportIndex});
		opEncoder.end();
        // 将自定义的函数再写入functions.def和export中，其实函数的字节码为codeStream
		// 从这里可以看到,我们在WASM中用内部函数封装了一层外部的本地函数,内部函数所用就是执行call指令
		// 而前面的local_get就是为了将参数放到操作数栈
		// 但是为什么要封装这一层呢??
		// 哈哈,是因为这写本地的函数是没有字节码的,他们直接对应于一个函数指针,因此他的函数定位也是import
		// 而export是不能把自己import的内容export的,因为本地是没有实体的,必须是本module存在字节码实体的才会被导出
		const Uptr wasmFunctionIndex = irModule.functions.size();
		irModule.functions.defs.push_back({{wasmFunctionTypeIndex}, {}, codeStream.getBytes(), {}});
		names.functions.push_back({"thunk:" + functionImport.exportName, {}, {}});
		irModule.exports.push_back(
			{functionImport.exportName, ExternKind::function, wasmFunctionIndex});
	}

	setDisassemblyNames(irModule, names);
    // 内容检查
	if(WAVM_ENABLE_ASSERTS)
	{
		try
		{
			std::shared_ptr<IR::ModuleValidationState> moduleValidationState
				= IR::createModuleValidationState(irModule);
			validatePreCodeSections(*moduleValidationState);
			validateCodeSection(*moduleValidationState);
			validatePostCodeSections(*moduleValidationState);
		}
		catch(ValidationException const& exception)
		{
			Errors::fatalf("Validation exception in intrinsic module: %s",
						   exception.message.c_str());
		}
	}

	ModuleRef module = compileModule(irModule);
	Instance* instance = instantiateModuleInternal(compartment,
												   module,
												   std::move(functionImportBindings),
												   {},
												   {},
												   {},
												   {},
												   std::move(debugName));

	Timing::logTimer("Instantiated intrinsic module", timer);
	return instance;
}

HashMap<std::string, Intrinsics::Function*> Intrinsics::getUninstantiatedFunctions(
	const std::initializer_list<const Intrinsics::Module*>& moduleRefs)
{
	HashMap<std::string, Intrinsics::Function*> result;

	for(const Intrinsics::Module* moduleRef : moduleRefs)
	{
		if(moduleRef->impl)
		{
			for(const auto& pair : moduleRef->impl->functionMap)
			{ result.addOrFail(pair.key, pair.value); }
		}
	}

	return result;
}
