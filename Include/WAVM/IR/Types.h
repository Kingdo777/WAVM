#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <initializer_list>
#include <string>
#include <vector>
#include "WAVM/IR/IR.h"
#include "WAVM/Inline/Assert.h"
#include "WAVM/Inline/BasicTypes.h"
#include "WAVM/Inline/Errors.h"
#include "WAVM/Inline/Hash.h"

namespace WAVM { namespace Runtime {
	struct Object;
	struct Function;
}}
/***
 *
 * 第一部分主要是定义了值类型，基本的值类型包括了
 *  1、I32、I64、F32、F64
 *  2、新添加的16字节大小的 V128
 *  3、引用类型，包括函数引用（内部函数）和外部
 *  4、两个特殊的类型：none、any
 * 第二部分就是是上述类型的操作
 *  1、asString（）
 *  2、获取长度等等
 *
 * */
namespace WAVM { namespace IR {
	// The type of a WebAssembly operand
	enum class ValueType : U8
	{
		none,
		any,
		i32,
		i64,
		f32,
		f64,
		v128,
		externref,
		funcref
	};

	static constexpr U8 numValueTypes = U8(ValueType::funcref) + 1;

	// The reference types subset of ValueType.
	enum class ReferenceType : U8
	{
		none = U8(ValueType::none),

		externref = U8(ValueType::externref),
		funcref = U8(ValueType::funcref)
	};

	inline ValueType asValueType(ReferenceType type) { return ValueType(type); }

	inline bool isNumericType(ValueType type)
	{
		return type == ValueType::i32 || type == ValueType::i64 || type == ValueType::f32
			   || type == ValueType::f64 || type == ValueType::v128;
	}

	inline bool isReferenceType(ValueType type)
	{
		return type == ValueType::externref || type == ValueType::funcref;
	}

	inline bool isSubtype(ValueType subtype, ValueType supertype)
	{
		return subtype == supertype || subtype == ValueType::none || supertype == ValueType::any;
	}

	inline bool isSubtype(ReferenceType subtype, ReferenceType supertype)
	{
		return isSubtype(asValueType(subtype), asValueType(supertype));
	}

	inline std::string asString(I32 value) { return std::to_string(value); }
	inline std::string asString(I64 value) { return std::to_string(value); }
	WAVM_API std::string asString(F32 value);
	WAVM_API std::string asString(F64 value);

	inline std::string asString(const V128& v128)
	{
		// buffer needs 50 characters:
		// i32x4 0xHHHHHHHH 0xHHHHHHHH 0xHHHHHHHH 0xHHHHHHHH\0
		char buffer[50];
		snprintf(buffer,
				 sizeof(buffer),
				 "i32x4 0x%.8x 0x%.8x 0x%.8x 0x%.8x",
				 v128.u32x4[0],
				 v128.u32x4[1],
				 v128.u32x4[2],
				 v128.u32x4[3]);
		return std::string(buffer);
	}

	inline U8 getTypeByteWidth(ValueType type)
	{
		switch(type)
		{
		case ValueType::i32: return 4;
		case ValueType::i64: return 8;
		case ValueType::f32: return 4;
		case ValueType::f64: return 8;
		case ValueType::v128: return 16;
		case ValueType::externref:
		case ValueType::funcref: return sizeof(void*);

		case ValueType::none:
		case ValueType::any:
		default: WAVM_UNREACHABLE();
		};
	}

	inline U8 getTypeBitWidth(ValueType type) { return getTypeByteWidth(type) * 8; }

	inline const char* asString(ValueType type)
	{
		switch(type)
		{
		case ValueType::none: return "none";
		case ValueType::any: return "any";
		case ValueType::i32: return "i32";
		case ValueType::i64: return "i64";
		case ValueType::f32: return "f32";
		case ValueType::f64: return "f64";
		case ValueType::v128: return "v128";
		case ValueType::externref: return "externref";
		case ValueType::funcref: return "funcref";
		default: WAVM_UNREACHABLE();
		};
	}

	inline const char* asString(ReferenceType type)
	{
		switch(type)
		{
		case ReferenceType::none: return "none";
		case ReferenceType::externref: return "externref";
		case ReferenceType::funcref: return "funcref";
		default: WAVM_UNREACHABLE();
		};
	}

	// The tuple of value types.
	// 值类型元组，用于描述函数的参数和返回值：(I32,I32)-->(I32)
	struct TypeTuple
	{
		// 构造函数，下面的构造函数都是通过getUniqueImpl实现的
		// 最好用的应该就是通过vector进行转化
		TypeTuple() : impl(getUniqueImpl(0, nullptr)) {}
		WAVM_API explicit TypeTuple(ValueType inElem);
		WAVM_API TypeTuple(const std::initializer_list<ValueType>& inElems);
		WAVM_API explicit TypeTuple(const std::vector<ValueType>& inElems);
		WAVM_API TypeTuple(const ValueType* inElems, Uptr numElems);

		// 下面定义了对于元组的基本操作
		const ValueType* begin() const { return impl->elems; }
		const ValueType* end() const { return impl->elems + impl->numElems; }
		const ValueType* data() const { return impl->elems; }

		ValueType operator[](Uptr index) const
		{
			WAVM_ASSERT(index < impl->numElems);
			return impl->elems[index];
		}

		Uptr getHash() const { return impl->hash; }
		Uptr size() const { return impl->numElems; }

		friend bool operator==(const TypeTuple& left, const TypeTuple& right)
		{
			return left.impl == right.impl;
		}
		friend bool operator!=(const TypeTuple& left, const TypeTuple& right)
		{
			return left.impl != right.impl;
		}
        // 真正存放数据的地方：
		// 用于唯一标志的hash值，
		// 元素的数据个数
		// 存放元素的数据区指针
		// 数据区的大小也就是元素的个数，是在构造函数中就确定的
	private:
		struct Impl
		{
			Uptr hash;
			Uptr numElems;
			ValueType elems[1];
            // 两个个构造参数
			// 第一个就是将inElems地址开始的inNumElems个元素，用memcpy进行初始化，hash需要手动计算
			// 第二个就是无脑复制
			Impl(Uptr inNumElems, const ValueType* inElems);
            Impl(const Impl& inCopy);
            // 返回指定个数的元素所需的地址空间大小，所以说可以认为元组是定长的
			static Uptr calcNumBytes(Uptr numElems)
			{
				return offsetof(Impl, elems) + numElems * sizeof(ValueType);
			}
		};

		const Impl* impl;

		TypeTuple(const Impl* inImpl) : impl(inImpl) {}

		// 为了避免重复的内存占用，代码中定义了一个全局的元组结构，用于存放所有的定义过的元组
		// 通过hash值来区别元组
		WAVM_API static const Impl* getUniqueImpl(Uptr numElems, const ValueType* inElems);
	};
    // 将元素结构进行字符串输出
	inline std::string asString(TypeTuple typeTuple)
	{
		if(typeTuple.size() == 1) { return asString(typeTuple[0]); }
		else
		{
			std::string result = "(";
			for(Uptr elementIndex = 0; elementIndex < typeTuple.size(); ++elementIndex)
			{
				if(elementIndex != 0) { result += ", "; }
				result += asString(typeTuple[elementIndex]);
			}
			result += ")";
			return result;
		}
	}
    // 是否是子类型元组，需要保证元组中的每个类型元素都是父元素的sub
	inline bool isSubtype(TypeTuple subtype, TypeTuple supertype)
	{
		if(subtype == supertype) { return true; }
		else if(subtype.size() != supertype.size())
		{
			return false;
		}
		else
		{
			for(Uptr elementIndex = 0; elementIndex < subtype.size(); ++elementIndex)
			{
				if(!isSubtype(subtype[elementIndex], supertype[elementIndex])) { return false; }
			}
			return true;
		}
	}

	// Infer value and result types from a C type.
    // 通过具体数据类型的值类型
	// 函数索引指向的是Runtime::Function*
	// externref指向的是Runtime::Object*，其实这里的Obj就可以理解为导入的对象
	template<typename> constexpr ValueType inferValueType();
	template<> constexpr ValueType inferValueType<I8>() { return ValueType::i32; }
	template<> constexpr ValueType inferValueType<U8>() { return ValueType::i32; }
	template<> constexpr ValueType inferValueType<I16>() { return ValueType::i32; }
	template<> constexpr ValueType inferValueType<U16>() { return ValueType::i32; }
	template<> constexpr ValueType inferValueType<I32>() { return ValueType::i32; }
	template<> constexpr ValueType inferValueType<U32>() { return ValueType::i32; }
	template<> constexpr ValueType inferValueType<I64>() { return ValueType::i64; }
	template<> constexpr ValueType inferValueType<U64>() { return ValueType::i64; }
	template<> constexpr ValueType inferValueType<F32>() { return ValueType::f32; }
	template<> constexpr ValueType inferValueType<F64>() { return ValueType::f64; }
	template<> constexpr ValueType inferValueType<Runtime::Object*>()
	{
		return ValueType::externref;
	}
	template<> constexpr ValueType inferValueType<Runtime::Function*>()
	{
		return ValueType::funcref;
	}
	template<> constexpr ValueType inferValueType<const Runtime::Object*>()
	{
		return ValueType::externref;
	}
	template<> constexpr ValueType inferValueType<const Runtime::Function*>()
	{
		return ValueType::funcref;
	}
    // 根据值的类型返回一个值类型元组，元组的大小为1,此外还包含了几个特殊情况void、I8、I16、U8、U16等
	template<typename T> inline TypeTuple inferResultType()
	{
		return TypeTuple(inferValueType<T>());
	}
	template<> inline TypeTuple inferResultType<void>() { return TypeTuple(); }

	// Don't allow quietly promoting I8/I16 return types to an I32 WebAssembly type: the C function
	// may not zero the extra bits in the I32 register before returning, and the WebAssembly
	// function will see that junk in the returned I32.
    // 不允许悄悄地将I8 / I16返回类型提升为I32 WebAssembly类型。
	// 因为C函数可能不会在返回之前将I32寄存器中的额外位清零，这将使得并且WebAssembly函数会在返回的I32中看到垃圾。
	template<> inline TypeTuple inferResultType<I8>();
	template<> inline TypeTuple inferResultType<U8>();
	template<> inline TypeTuple inferResultType<I16>();
	template<> inline TypeTuple inferResultType<U16>();

	// The calling convention for a function.
	// 调用函数的约定，不知道是个啥意思
	// 哦哦，可能是针对外部func而言的，比如调用C语言的函数，那么应该怎么传参数，怎么拿返回值等等
	enum class CallingConvention
	{
		wasm,
		intrinsic,// 内嵌函数
		intrinsicWithContextSwitch,
		c,
		cAPICallback,
	};

	inline std::string asString(CallingConvention callingConvention)
	{
		switch(callingConvention)
		{
		case CallingConvention::wasm: return "wasm";
		case CallingConvention::intrinsic: return "intrinsic";
		case CallingConvention::intrinsicWithContextSwitch: return "intrinsic_with_context_switch";
		case CallingConvention::c: return "c";
		case CallingConvention::cAPICallback: return "c_api_callback";

		default: WAVM_UNREACHABLE();
		};
	}

	// The type of a WebAssembly function
	// 函数类型，也就是函数段的内容
	// 函数类型用于描述函数的参数与返回值的数目和类型
	//
	// funcType {
	//     TypeTuple param; 用于描述参数类型
	//     TypeTuple result;用于描述返回值的类型
	// }
	struct FunctionType
	{
		// Used to represent a function type as an abstract pointer-sized value in the runtime.
		// 用于在运行时将函数类型表示为抽象指针大小的值。也就是将const Impl* impl转为成Uptr类型的数值
		struct Encoding
		{
			Uptr impl;
		};
        // 构造函数主要还是给自己的灵魂字段impl赋值
		// 在这里同样使用了getUniqueImpl，这个全局的存储，用于存放相同的函数类型
		// 这个函数也是之在构造函数中可用过
		FunctionType(TypeTuple inResults = TypeTuple(),
					 TypeTuple inParams = TypeTuple(),
					 CallingConvention inCallingConvention = CallingConvention::wasm)
		: impl(getUniqueImpl(inResults, inParams, inCallingConvention))
		{
		}

		FunctionType(Encoding encoding) : impl(reinterpret_cast<const Impl*>(encoding.impl)) {}

		TypeTuple results() const { return impl->results; }
		TypeTuple params() const { return impl->params; }
		CallingConvention callingConvention() const { return impl->callingConvention; }
		Uptr getHash() const { return impl->hash; }
		Encoding getEncoding() const { return Encoding{reinterpret_cast<Uptr>(impl)}; }

		friend bool operator==(const FunctionType& left, const FunctionType& right)
		{
			return left.impl == right.impl;
		}

		friend bool operator!=(const FunctionType& left, const FunctionType& right)
		{
			return left.impl != right.impl;
		}

	private:
		struct Impl
		{
			Uptr hash;
			TypeTuple results;
			TypeTuple params;
			CallingConvention callingConvention;
			// 就一个构造函数,而且此构造函数，仅仅在getUniqueImpl中被调用，这样的话，所有的Impl就会保证是唯一的
			Impl(TypeTuple inResults, TypeTuple inParams, CallingConvention inCallingConvention);
		};

		const Impl* impl;

		FunctionType(const Impl* inImpl) : impl(inImpl) {}

		WAVM_API static const Impl* getUniqueImpl(TypeTuple results,
												  TypeTuple params,
												  CallingConvention callingConvention);
	};
    // 函数类型的索引，有索引的就是在段内用vector存放的
	struct IndexedFunctionType
	{
		Uptr index;
	};
    // 块类型，可以理解为是一种内置函数，一开始的标准中，块类型没有参数，只有一个或0个返回值，在最新的标准中
	// 块类型可以和函数一样拥有相同的函数类型，但是两者在理论的定义上是非常不一样的：

	// WASM是基于栈进行操作的，所有的操作包括函数调用或者是简单的加法操作都是在操作 “操作数栈”
	// 首先对于每个函数而言，都有自己的操作操作数栈，不同函数之间的操作数栈是不共享的，毕竟程序的开始就是从主函数开始的
	// 对于函数而言，其本质依然是从操作数栈中取走操作数作为自己的参数，然后返回返回值到操作数栈
	// 需要注意的是，函数和普通的指令相比，他有自己的参数和局部变量，参数和局部变量是区别有操作数的
	// 可以认为每个函数都拥有自己的参数、局部变量表（参数可以认为是只读的局部变量）
	// 可以用指令通过操作数栈来设置局部变量，也可以将局部变量的值放到操作数栈
	// 上面都是理论上的，但是从实现的角度，其实所有的函数是可以共享一个操作数栈的，此外局部变量表也可以在此操作数栈中实现，
	// 具体就是：
	// A调用B ，A将参数放到操作数栈，进入call指令的处理，直接在操作数栈上开辟局部变量的空间，这样从初始参数到整个局部变量空间就是上面的局部变量表了，
	// B的操作数栈则紧跟在局部表的后面，当B运行结束，需要将栈中B相关的所有元素出栈，然后结果进栈，从而实现了前面说的：对于函数而言，其本质依然是从操作数栈中取走操作数作为自己的参数，然后返回返回值到操作数栈
	// 此实现其实是充分的借鉴了汇编语言函数栈的设计

	// 块类型和函数类型在理论上就不同，因为快操作是没有自己的局部变量表这个概念的，他是直接操作当前的函数栈
	// 因此块最佳的操作还是不要有参数，因为即便有参数，但是不能用类似于函数中取参数的操作，所以功能上是受限的
	struct IndexedBlockType
	{
		// 最开始的标准中块操作就是没有参数，最多只允许一个返回值的
		enum Format
		{
			noParametersOrResult, //没有参数和返回值
			oneResult,            //只有一个参数
			functionType          //和函数一样
		};
		Format format;
		//如果是noParametersOrResult或者oneResult，那么可以直接用ValueType resultType直接返回其类型，就不需要索引了
		//如果是functionType，那就返回一个索引index，和IndexedFunctionType一样可以在类型段中查询具体的类型
		union
		{
			ValueType resultType;
			Uptr index;
		};
	};

	inline std::string asString(const FunctionType& functionType)
	{
		std::string result
			= asString(functionType.params()) + "->" + asString(functionType.results());
		if(functionType.callingConvention() != CallingConvention::wasm)
		{ result += "(calling_conv " + asString(functionType.callingConvention()) + ')'; }
		return result;
	}

	inline bool isSubtype(FunctionType subtype, FunctionType supertype)
	{
		if(subtype == supertype) { return true; }
		else
		{
			return isSubtype(supertype.params(), subtype.params())
				   && isSubtype(subtype.results(), supertype.results())
				   && supertype.callingConvention() == subtype.callingConvention();
		}
	}

	// The index type for a memory or table.
	// 这里看不懂，这个定义的作用，内存和表目前只能是一个，他们的索引只能是0,索引类型是什么？？
	// 可能标准已经放开了限制,但是索引一般不会太大，于是默认索引都是I32,但是为了将来考虑，还是允许使用I64的索引？？
	enum class IndexType : U8
	{
		i32 = U8(ValueType::i32),
		i64 = U8(ValueType::i64),
	};

	inline ValueType asValueType(IndexType indexType)
	{
		switch(indexType)
		{
		case IndexType::i32: return ValueType::i32;
		case IndexType::i64: return ValueType::i64;
		default: WAVM_UNREACHABLE();
		};
	}

	// A size constraint: a range of expected sizes for some size-constrained type.
	// If max==UINT64_MAX, the maximum size is unbounded.
	// 这里定义的是大小限制，主要针对表数据和内存数据
	// 限制类型，包括了最小值和最大值
	// 最小值是必须的，但是最大值可以没有在这里如果max==UINT64_MAX，那么就表示没有限制
	struct SizeConstraints
	{
		U64 min;
		U64 max;

		friend bool operator==(const SizeConstraints& left, const SizeConstraints& right)
		{
			return left.min == right.min && left.max == right.max;
		}
		friend bool operator!=(const SizeConstraints& left, const SizeConstraints& right)
		{
			return left.min != right.min || left.max != right.max;
		}
		friend bool isSubset(const SizeConstraints& sub, const SizeConstraints& super)
		{
			return sub.min >= super.min && sub.max <= super.max;
		}
	};

	inline std::string asString(const SizeConstraints& sizeConstraints)
	{
		return std::to_string(sizeConstraints.min)
			   + (sizeConstraints.max == UINT64_MAX ? ".."
													: ".." + std::to_string(sizeConstraints.max));
	}

	// The type of a table
	// 表类型中包含了元素的类型（目前只能是函数索引）、元素的数目限制等
	// 这里还多了isShared和IndexType
	struct TableType
	{
		// 表的元素类型
		ReferenceType elementType;
		bool isShared;
		IndexType indexType;
		SizeConstraints size;

		TableType()
		: elementType(ReferenceType::none), isShared(false), indexType(IndexType::i32), size()
		{
		}
		TableType(ReferenceType inElementType,
				  bool inIsShared,
				  IndexType inIndexType,
				  SizeConstraints inSize)
		: elementType(inElementType), isShared(inIsShared), indexType(inIndexType), size(inSize)
		{
		}

		friend bool operator==(const TableType& left, const TableType& right)
		{
			return left.elementType == right.elementType && left.isShared == right.isShared
				   && left.indexType == right.indexType && left.size == right.size;
		}
		friend bool operator!=(const TableType& left, const TableType& right)
		{
			return left.elementType != right.elementType || left.isShared != right.isShared
				   || left.indexType != right.indexType || left.size != right.size;
		}
		friend bool isSubtype(const TableType& sub, const TableType& super)
		{
			return super.elementType == sub.elementType && super.isShared == sub.isShared
				   && super.indexType == sub.indexType && isSubset(sub.size, super.size);
		}
	};

	inline std::string asString(const TableType& tableType)
	{
		const char* indexString;
		switch(tableType.indexType)
		{
		case IndexType::i32: indexString = ""; break;
		case IndexType::i64: indexString = "i64 "; break;
		default: WAVM_UNREACHABLE();
		};

		return std::string(indexString) + asString(tableType.size)
			   + (tableType.isShared ? " shared funcref" : " funcref");
	}

	// The type of a memory
	// 内存类型主要就是描述内存的页数限制
	struct MemoryType
	{
		bool isShared;
		IndexType indexType;
		SizeConstraints size;

		MemoryType() : isShared(false), indexType(IndexType::i32), size({0, UINT64_MAX}) {}
		MemoryType(bool inIsShared, IndexType inIndexType, const SizeConstraints& inSize)
		: isShared(inIsShared), indexType(inIndexType), size(inSize)
		{
		}

		friend bool operator==(const MemoryType& left, const MemoryType& right)
		{
			return left.isShared == right.isShared && left.indexType == right.indexType
				   && left.size == right.size;
		}
		friend bool operator!=(const MemoryType& left, const MemoryType& right)
		{
			return left.isShared != right.isShared || left.indexType != right.indexType
				   || left.size != right.size;
		}
		friend bool isSubtype(const MemoryType& sub, const MemoryType& super)
		{
			return super.isShared == sub.isShared && super.indexType == sub.indexType
				   && isSubset(sub.size, super.size);
		}
	};

	inline std::string asString(const MemoryType& memoryType)
	{
		const char* indexString;
		switch(memoryType.indexType)
		{
		case IndexType::i32: indexString = ""; break;
		case IndexType::i64: indexString = "i64 "; break;
		default: WAVM_UNREACHABLE();
		};

		return std::string(indexString) + asString(memoryType.size)
			   + (memoryType.isShared ? " shared" : "");
	}

	// The type of a global
	// 全局变量类型主要描述全局变量的值类型和可变性
	struct GlobalType
	{
		ValueType valueType;
		bool isMutable;

		GlobalType() : valueType(ValueType::any), isMutable(false) {}
		GlobalType(ValueType inValueType, bool inIsMutable)
		: valueType(inValueType), isMutable(inIsMutable)
		{
		}

		friend bool operator==(const GlobalType& left, const GlobalType& right)
		{
			return left.valueType == right.valueType && left.isMutable == right.isMutable;
		}
		friend bool operator!=(const GlobalType& left, const GlobalType& right)
		{
			return left.valueType != right.valueType || left.isMutable != right.isMutable;
		}
		friend bool isSubtype(const GlobalType& sub, const GlobalType& super)
		{
			if(super.isMutable != sub.isMutable) { return false; }
			else if(super.isMutable)
			{
				return super.valueType == sub.valueType;
			}
			else
			{
				return isSubtype(sub.valueType, super.valueType);
			}
		}
	};

	inline std::string asString(const GlobalType& globalType)
	{
		if(globalType.isMutable) { return std::string("global ") + asString(globalType.valueType); }
		else
		{
			return std::string("immutable ") + asString(globalType.valueType);
		}
	}

	struct ExceptionType
	{
		TypeTuple params;

		friend bool operator==(const ExceptionType& left, const ExceptionType& right)
		{
			return left.params == right.params;
		}
		friend bool operator!=(const ExceptionType& left, const ExceptionType& right)
		{
			return left.params != right.params;
		}
	};

	inline std::string asString(const ExceptionType& exceptionType)
	{
		return asString(exceptionType.params);
	}

	// The type of an external object: something that can be imported or exported from a module.
	enum class ExternKind : U8
	{
		invalid,

		// Standard object kinds that may be imported/exported from WebAssembly modules.
		function,
		table,
		memory,
		global,
		exceptionType,
	};
	struct ExternType
	{
		const ExternKind kind;

		ExternType() : kind(ExternKind::invalid) {}
		ExternType(FunctionType inFunction) : kind(ExternKind::function), function(inFunction) {}
		ExternType(TableType inTable) : kind(ExternKind::table), table(inTable) {}
		ExternType(MemoryType inMemory) : kind(ExternKind::memory), memory(inMemory) {}
		ExternType(GlobalType inGlobal) : kind(ExternKind::global), global(inGlobal) {}
		ExternType(ExceptionType inExceptionType)
		: kind(ExternKind::exceptionType), exceptionType(inExceptionType)
		{
		}
		ExternType(ExternKind inKind) : kind(inKind) {}

		friend FunctionType asFunctionType(const ExternType& objectType)
		{
			WAVM_ASSERT(objectType.kind == ExternKind::function);
			return objectType.function;
		}
		friend TableType asTableType(const ExternType& objectType)
		{
			WAVM_ASSERT(objectType.kind == ExternKind::table);
			return objectType.table;
		}
		friend MemoryType asMemoryType(const ExternType& objectType)
		{
			WAVM_ASSERT(objectType.kind == ExternKind::memory);
			return objectType.memory;
		}
		friend GlobalType asGlobalType(const ExternType& objectType)
		{
			WAVM_ASSERT(objectType.kind == ExternKind::global);
			return objectType.global;
		}
		friend ExceptionType asExceptionType(const ExternType& objectType)
		{
			WAVM_ASSERT(objectType.kind == ExternKind::exceptionType);
			return objectType.exceptionType;
		}

	private:
		union
		{
			FunctionType function;
			TableType table;
			MemoryType memory;
			GlobalType global;
			ExceptionType exceptionType;
		};
	};

	inline std::string asString(const ExternType& objectType)
	{
		switch(objectType.kind)
		{
		case ExternKind::function: return "func " + asString(asFunctionType(objectType));
		case ExternKind::table: return "table " + asString(asTableType(objectType));
		case ExternKind::memory: return "memory " + asString(asMemoryType(objectType));
		case ExternKind::global: return asString(asGlobalType(objectType));
		case ExternKind::exceptionType:
			return "exception_type " + asString(asExceptionType(objectType));

		case ExternKind::invalid:
		default: WAVM_UNREACHABLE();
		};
	}

	inline ReferenceType asReferenceType(const ExternKind kind)
	{
		switch(kind)
		{
		case ExternKind::function: return ReferenceType::funcref;

		case ExternKind::table:
		case ExternKind::memory:
		case ExternKind::global:
		case ExternKind::exceptionType:
		case ExternKind::invalid:
		default: return ReferenceType::externref;
		}
	}

	inline ReferenceType asReferenceType(const ExternType& type)
	{
		return asReferenceType(type.kind);
	}
}}

// These specializations need to be declared within a WAVM namespace scope to work around a GCC bug.
// It should be ok to write "template<> struct WAVM::Hash...", but old versions of GCC will
// erroneously reject that. See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=56480
namespace WAVM {
	template<> struct Hash<IR::TypeTuple>
	{
		Uptr operator()(IR::TypeTuple typeTuple, Uptr seed = 0) const
		{
			return Hash<Uptr>()(typeTuple.getHash(), seed);
		}
	};

	template<> struct Hash<IR::FunctionType>
	{
		Uptr operator()(IR::FunctionType functionType, Uptr seed = 0) const
		{
			return Hash<Uptr>()(functionType.getHash(), seed);
		}
	};
}
