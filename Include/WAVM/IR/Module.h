#pragma once

#include <stdint.h>
#include <cstddef>
#include <memory>
#include <string>
#include <vector>
#include "WAVM/IR/FeatureSpec.h"
#include "WAVM/IR/IR.h"
#include "WAVM/IR/Types.h"
#include "WAVM/Inline/Assert.h"
#include "WAVM/Inline/BasicTypes.h"
#include "WAVM/Inline/Errors.h"

namespace WAVM { namespace IR {
	enum class Opcode : U16;

	// An initializer expression: serialized like any other code, but only supports a few specific
	// instructions.
	template<typename Ref> struct InitializerExpressionBase
	{
		enum class Type : U16
		{
			i32_const = 0x0041,
			i64_const = 0x0042,
			f32_const = 0x0043,
			f64_const = 0x0044,
			v128_const = 0xfd02,
			global_get = 0x0023,
			ref_null = 0x00d0,
			ref_func = 0x00d2,
			invalid = 0xffff
		};
		union
		{
			Type type;
			Opcode typeOpcode;
		};
		union
		{
			I32 i32;
			I64 i64;
			F32 f32;
			F64 f64;
			V128 v128;
			Ref ref;
			ReferenceType nullReferenceType;
		};
		InitializerExpressionBase() : type(Type::invalid) {}
		InitializerExpressionBase(I32 inI32) : type(Type::i32_const), i32(inI32) {}
		InitializerExpressionBase(I64 inI64) : type(Type::i64_const), i64(inI64) {}
		InitializerExpressionBase(F32 inF32) : type(Type::f32_const), f32(inF32) {}
		InitializerExpressionBase(F64 inF64) : type(Type::f64_const), f64(inF64) {}
		InitializerExpressionBase(V128 inV128) : type(Type::v128_const), v128(inV128) {}
		InitializerExpressionBase(Type inType, Ref inRef) : type(inType), ref(inRef)
		{
			WAVM_ASSERT(type == Type::global_get || type == Type::ref_func);
		}
		InitializerExpressionBase(ReferenceType inNullReferenceType)
		: type(Type::ref_null), nullReferenceType(inNullReferenceType)
		{
		}

		friend bool operator==(const InitializerExpressionBase& a,
							   const InitializerExpressionBase& b)
		{
			if(a.type != b.type) { return false; }
			switch(a.type)
			{
			case Type::i32_const: return a.i32 == b.i32;
			case Type::i64_const: return a.i64 == b.i64;
			// For FP constants, use integer comparison to test for bitwise equality. Using FP
			// comparison can't distinguish when NaNs are identical.
			case Type::f32_const: return a.i32 == b.i32;
			case Type::f64_const: return a.i64 == b.i64;
			case Type::v128_const:
				return a.v128.u64x2[0] == b.v128.u64x2[0] && a.v128.u64x2[1] == b.v128.u64x2[1];
			case Type::global_get: return a.ref == b.ref;
			case Type::ref_null: return true;
			case Type::ref_func: return a.ref == b.ref;
			case Type::invalid: return true;
			default: WAVM_UNREACHABLE();
			};
		}

		friend bool operator!=(const InitializerExpressionBase& a,
							   const InitializerExpressionBase& b)
		{
			return !(a == b);
		}
	};

	typedef InitializerExpressionBase<Uptr> InitializerExpression;

	// A function definition
	// 函数段记录了函数的类型的索引、代码段记录了函数的局部变量信息和字节码
	// 因此代码段和函数段组成了WASM的所有函数
	// 在这里把函数段和代码段的内容合并到了一个数据结构中，统称函数段
	// 下面就是函数段中一个函数的定义
	struct FunctionDef
	{
		// 函数类型的索引，继承自原WASM的函数段
		IndexedFunctionType type;
		// 函数局部变量的信息，也就是每个局部变量的值类型，继承自原WASM的代码段
		std::vector<ValueType> nonParameterLocalTypes;
		// 函数的字节码，继承自原WASM的代码段
		std::vector<U8> code;
		std::vector<std::vector<Uptr>> branchTables;
	};

	// A table definition
	// 表段中存放的就是表类型，因此直接定义即可
	struct TableDef
	{
		TableType type;
	};

	// A memory definition
	// 内存段中存放的就是内存类型，因此直接定义即可
	struct MemoryDef
	{
		MemoryType type;
	};

	// A global definition
	// 全局段存放的是全局变量，全局变量包括：
	// 变量类型和初始化表达式
	struct GlobalDef
	{
		GlobalType type;
		InitializerExpression initializer;
	};

	// A tagged tuple type definition
	// 这应该是最新的标准中的异常段
	struct ExceptionTypeDef
	{
		ExceptionType type;
	};

	// Describes an object imported into a module or a specific type
	// 这里定义了一个模板数据结构，用于描述导入段的导入类型
	// 导入导出只能操作4个对象（不考虑exception）也就是函数、内存、全局变量和表，其中函数指的是函数段的内容，即函数类型索引
	// 导入的条目首先要确定自己的导入类型，在WASM中有一个自己的tag来标志，在这里的实现上是给每一种导入类型定义了一个统一的模板类
	// 导入条目的主要内容是所导入的模块名和项目名，这都是字符串，又称作symbol，最终将在链接阶段把导入导出的内容链接起来
	template<typename Type> struct Import
	{
		Type type;
		std::string moduleName;
		std::string exportName;
	};

	typedef Import<IndexedFunctionType> FunctionImport;
	typedef Import<TableType> TableImport;
	typedef Import<MemoryType> MemoryImport;
	typedef Import<GlobalType> GlobalImport;
	typedef Import<ExceptionType> ExceptionTypeImport;

	// Describes an export from a module.
	// 对于导出条目，主要是
	// ·
	// 条目的名字，此处同Import中的exportName是指一个，只有被导出的才能被其他的module导入，所以导出时
	//   定义的名字，要和导入时保持一直
	// · 导出的类型，ExternKind是枚举类型
	// · 导出的索引，module中所有的导出项都是有索引的，通过具体的索引找到具体的项目
	struct Export
	{
		std::string name;

		ExternKind kind;

		// An index into the module's kind-specific IndexSpace.
		Uptr index;
	};

	// Identifies an element of a kind-specific IndexSpace in a module.
	// WAVM实现的导入段的表项，只有类型和索引
	// 其实上面实现的Import才是完整的导入段的定义
	// WAVM在实现导入段时候直接将导入的部分添加到了各自的实现的段上去了
	// 自己仅仅留下了对于各个段的索引
	// 比如有一个导入的全局变量 env.PORT
	// 原本导入段应该包括了此导入项的类型(global)，moduleName(env)，ImportName(PORT)
	// 但是在实现上我们是把env.PORT扔到了全局段中了，因为在下面定义的IndexSpace中是把导入的部分和自己实现的部分柔和到一起了
	// 这样我们只需要在导入段中保留索引，在IndexSpace<GlobalDef,GlobalType>中就可以找到了
	struct KindAndIndex
	{
		ExternKind kind;
		Uptr index;

		friend bool operator==(const KindAndIndex& left, const KindAndIndex& right)
		{
			return left.kind == right.kind && left.index == right.index;
		}
	};

	// A data segment: a literal sequence of bytes that is copied into a Runtime::Memory when
	// instantiating a module
	// 数据段：实例化模块时复制到Runtime :: Memory中的字节的文字序列
	//	准去地说，这里的DataSegment是数据段的一个条目，包括上面的Import、Export，下面的ElemSegment、CustomSection
	//	都分别是导入段、导出段、元素段和自定义段的条目
	//	数据段条目的主要内容包括：
	//	内存索引，及要初始化那一块内存
	//	偏移量，即从哪里开始初始化
	//	字节数组，即初始化具体内容
	struct DataSegment
	{
		bool isActive;
		Uptr memoryIndex;
		InitializerExpression baseOffset;
		std::shared_ptr<std::vector<U8>> data;
	};

	// An element expression: a literal reference used to initialize a table element.
	struct ElemExpr
	{
		enum class Type
		{
			invalid = 0,

			// These must match the corresponding Opcode members.
			ref_null = 0xd0,
			ref_func = 0xd2
		};
		union
		{
			Type type;
			Opcode typeOpcode;
		};
		union
		{
			Uptr index;
			ReferenceType nullReferenceType;
		};

		ElemExpr() : type(Type::invalid) {}

		ElemExpr(ReferenceType inNullReferenceType)
		: type(Type::ref_null), nullReferenceType(inNullReferenceType)
		{
		}

		ElemExpr(Type inType, Uptr inIndex = UINTPTR_MAX) : type(inType), index(inIndex) {}

		friend bool operator==(const ElemExpr& a, const ElemExpr& b)
		{
			if(a.type != b.type) { return false; }
			switch(a.type)
			{
			case ElemExpr::Type::ref_func: return a.index == b.index;
			case ElemExpr::Type::ref_null: return true;

			case ElemExpr::Type::invalid:
			default: WAVM_UNREACHABLE();
			}
		}

		friend bool operator!=(const ElemExpr& a, const ElemExpr& b)
		{
			if(a.type != b.type) { return true; }
			switch(a.type)
			{
			case ElemExpr::Type::ref_func: return a.index != b.index;
			case ElemExpr::Type::ref_null: return false;

			case ElemExpr::Type::invalid:
			default: WAVM_UNREACHABLE();
			}
		}
	};

	// An elem segment: a literal sequence of table elements.
	// 元素段是对表的初始化，一开始的标准，表内容只能是函数索引
	// 猜测对应于Encoding==index Type==active
	struct ElemSegment
	{
		enum class Encoding
		{
			index,
			expr,
		};

		enum class Type
		{
			active,
			passive,
			declared
		};
		Type type;

		// Only valid if type == active.
		Uptr tableIndex;
		InitializerExpression baseOffset;

		struct Contents
		{
			Encoding encoding;

			// Only valid if encoding == expr.
			ReferenceType elemType;
			std::vector<ElemExpr> elemExprs;

			// Only valid if encoding == index.
			ExternKind externKind;
			std::vector<Uptr> elemIndices;
		};
		std::shared_ptr<Contents> contents;
	};

	// Identifies sections in the binary format of a module in the order they are required to occur.
	// 每个段都有自己的ID分别是：
	// 类型段（1）、导入（2）导出段（7）、函数（3）代码段（10）、表（4）元素段（9）、内存（5）数据段（11）、全局段（6）、起始段（8）
	// 以及0号的自定义段
	enum class OrderedSectionID : U8
	{
		moduleBeginning,

		type,
		import,
		function,
		table,
		memory,
		global,
		exceptionType,
		export_,
		start,
		elem,
		dataCount,
		code,
		data,
	};

	WAVM_API const char* asString(OrderedSectionID id);

	// A custom module section as an array of bytes
	// 自定义段是一个字节数组，WASM可以拥有多个自定义段，每个自定义端都拥有一个唯一的name字符串标志
	struct CustomSection
	{
		OrderedSectionID afterSection{OrderedSectionID::moduleBeginning};
		std::string name;
		std::vector<U8> data;

		CustomSection() {}
		CustomSection(OrderedSectionID inAfterSection,
					  std::string&& inName,
					  std::vector<U8>&& inData)
		: afterSection(inAfterSection), name(std::move(inName)), data(std::move(inData))
		{
		}
	};

	// An index-space for imports and definitions of a specific kind.
	// 此数据结构用于描述可导入的四个（其实是5个，但是还是先不考虑exception）
	// 之所以如此是因为这个四个段的条目分为两部分，一部分是从外部导入的，一部分是自己定义的
	// 对于自己定义的，他们都有自己的专门的数据结构：FunctionDef、MemoryDef、TableDef、GlobalDef
	// 对于导入的也有自己的专门的数据结构：Import<Type>，这是模板类，包括了：
    //	typedef Import<IndexedFunctionType> FunctionImport;
    //	typedef Import<TableType> TableImport;
    //	typedef Import<MemoryType> MemoryImport;
    //	typedef Import<GlobalType> GlobalImport;
	// 其实他们的区别真的很小，如果不考虑Global，各自的类型就是各自段中的条目，因为Function、Memory、Table的
	// 实际内容都在code、data、element中定义
	// 但是在实现上，WAVM把函数段和代码段融合到一起了，GlobalType中是不包括全局变量的具体数值的，数值是在GlobalDef中包含的
	// Import<Type>其实就是在各自Type的基础之上添加了模块名和导出时定义的名字
	// 因此下面这个数据结构将上述两种定义融合到一块，组成了函数段、表段、内存段、全局段
	template<typename Definition, typename Type> struct IndexSpace
	{
		std::vector<Import<Type>> imports;
		std::vector<Definition> defs;

		Uptr size() const { return imports.size() + defs.size(); }
		const Type& getType(Uptr index) const
		{
			if(index < imports.size()) { return imports[index].type; }
			else
			{
				return defs[index - imports.size()].type;
			}
		}
		bool isImport(Uptr index) const
		{
			WAVM_ASSERT(index < size());
			return index < imports.size();
		}
		bool isDef(Uptr index) const
		{
			WAVM_ASSERT(index < size());
			return index >= imports.size();
		}
		const Definition& getDef(Uptr index) const
		{
			WAVM_ASSERT(isDef(index));
			return defs[index - imports.size()];
		}
	};

	// A WebAssembly module definition
	// 正儿八经的用于描述WASM module的结构，包含内容是：
	// 1、用于描述开启指定特性的字段，featureSpec
	// 2、WASM规定的11个段：
	//      类型段，types
	//      函数段+代码段，functions
	//      表段，tables        元素段，elemSegments
	//      内存段，memories    数据段，dataSegments
	//      全局段，globals
	//      导入段，imports     导出段，exports
	//      起始段，startFunctionIndex
	struct Module
	{
		FeatureSpec featureSpec;
		//类型段，记录所有的函数（导入的、自定义的）的参数以及返回值的个数和类型
		//类型段就是函数类型的集合
		std::vector<FunctionType> types;

		IndexSpace<FunctionDef, IndexedFunctionType> functions;
		IndexSpace<TableDef, TableType> tables;
		IndexSpace<MemoryDef, MemoryType> memories;
		IndexSpace<GlobalDef, GlobalType> globals;
		IndexSpace<ExceptionTypeDef, ExceptionType> exceptionTypes;

		std::vector<KindAndIndex> imports;
		std::vector<Export> exports;
		std::vector<DataSegment> dataSegments;
		std::vector<ElemSegment> elemSegments;
		std::vector<CustomSection> customSections;

		Uptr startFunctionIndex;

		Module(const FeatureSpec& inFeatureSpec = FeatureSpec())
		: featureSpec(inFeatureSpec), startFunctionIndex(UINTPTR_MAX)
		{
		}
	};

	// Finds a named custom section in a module.
	WAVM_API bool findCustomSection(const Module& module,
									const char* customSectionName,
									Uptr& outCustomSectionIndex);

	// Inserts a named custom section in a module, before any custom sections that come after later
	// known sections, but after all custom sections that come after the same known section.
	WAVM_API void insertCustomSection(Module& module, CustomSection&& customSection);

	// Functions that determine whether the binary form of a module will have specific sections.

	inline bool hasTypeSection(const Module& module) { return module.types.size() > 0; }
	inline bool hasImportSection(const Module& module)
	{
		WAVM_ASSERT((module.imports.size() > 0)
					== (module.functions.imports.size() > 0 || module.tables.imports.size() > 0
						|| module.memories.imports.size() > 0 || module.globals.imports.size() > 0
						|| module.exceptionTypes.imports.size() > 0));
		return module.imports.size() > 0;
	}
	inline bool hasFunctionSection(const Module& module)
	{
		return module.functions.defs.size() > 0;
	}
	inline bool hasTableSection(const Module& module) { return module.tables.defs.size() > 0; }
	inline bool hasMemorySection(const Module& module) { return module.memories.defs.size() > 0; }
	inline bool hasGlobalSection(const Module& module) { return module.globals.defs.size() > 0; }
	inline bool hasExceptionTypeSection(const Module& module)
	{
		return module.exceptionTypes.defs.size() > 0;
	}
	inline bool hasExportSection(const Module& module) { return module.exports.size() > 0; }
	inline bool hasStartSection(const Module& module)
	{
		return module.startFunctionIndex != UINTPTR_MAX;
	}
	inline bool hasElemSection(const Module& module) { return module.elemSegments.size() > 0; }
	inline bool hasDataCountSection(const Module& module)
	{
		return module.dataSegments.size() > 0 && module.featureSpec.bulkMemoryOperations;
	}
	inline bool hasCodeSection(const Module& module) { return module.functions.defs.size() > 0; }
	inline bool hasDataSection(const Module& module) { return module.dataSegments.size() > 0; }

	WAVM_API OrderedSectionID getMaxPresentSection(const Module& module,
												   OrderedSectionID maxSection);

	// Resolve an indexed block type to a FunctionType.
	// 将快类型解析为函数类型，要我说就是找麻烦，直接用函数索引不就好了吗
	inline FunctionType resolveBlockType(const Module& module, const IndexedBlockType& indexedType)
	{
		switch(indexedType.format)
		{
		case IndexedBlockType::noParametersOrResult: return FunctionType();
		case IndexedBlockType::oneResult: return FunctionType(TypeTuple(indexedType.resultType));
		case IndexedBlockType::functionType: return module.types[indexedType.index];
		default: WAVM_UNREACHABLE();
		};
	}

	// Maps declarations in a module to names to use in disassembly.
	struct DisassemblyNames
	{
		struct Function
		{
			std::string name;
			std::vector<std::string> locals;
			std::vector<std::string> labels;

			Function(std::string&& inName = std::string(),
					 std::initializer_list<std::string>&& inLocals = {},
					 std::initializer_list<std::string>&& inLabels = {})
			: name(std::move(inName)), locals(inLocals), labels(inLabels)
			{
			}
		};

		std::string moduleName;
		std::vector<std::string> types;
		std::vector<Function> functions;
		std::vector<std::string> tables;
		std::vector<std::string> memories;
		std::vector<std::string> globals;
		std::vector<std::string> elemSegments;
		std::vector<std::string> dataSegments;
		std::vector<std::string> exceptionTypes;
	};
	// Func的名字也就是symbol是记录在自定义段的
	// Looks for a name section in a module. If it exists, deserialize it into outNames.
	// If it doesn't exist, fill outNames with sensible defaults.
	WAVM_API void getDisassemblyNames(const Module& module, DisassemblyNames& outNames);

	// Serializes a DisassemblyNames structure and adds it to the module as a name section.
	WAVM_API void setDisassemblyNames(Module& module, const DisassemblyNames& names);
}}
