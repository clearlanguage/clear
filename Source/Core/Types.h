#pragma once

#include "Parsing/Tokens.h"
#include "API/LLVM/LLVMInclude.h"

#include <variant>

namespace clear {

	enum class VariableType //TODO: remove typed pointers and replace with just a single pointer type
	{
		None = 0, Int8, Int16, Int32, Int64,
		Uint8, Uint16, Uint32, Uint64, Bool,
		Float32, Float64, String, UserDefinedType,
		Array, Int8Pointer, Int16Pointer, Int32Pointer, 
		Int64Pointer, Uint8Pointer, Uint16Pointer, Uint32Pointer, 
		Uint64Pointer, BoolPointer, Float32Pointer, Float64Pointer, 
		StringPointer, UserDefinedTypePointer, ArrayPointer
	};

	enum class TypeKind
	{
		None = 0, RValue, Variable, VariableReference, Value
	};


	enum class BinaryExpressionType
	{
		None = 0, Add, Sub, Mul,
		Div, Mod, Less, LessEq,
		Greater, GreaterEq, Eq, Assignment
	};
	
	struct StructMetaData;

	extern BinaryExpressionType GetBinaryExpressionTypeFromTokenType(TokenType type);
	extern VariableType	GetVariableTypeFromTokenType(TokenType tokenType);
	extern VariableType GetVariablePointerTypeFromTokenType(TokenType tokenType);
	extern llvm::Type*	GetLLVMVariableType(VariableType type);

	class AbstractType 
	{
	public:
		using MemberType = std::pair<std::string, AbstractType>;

	public:
		AbstractType() = default;
		AbstractType(const Token& token);
		AbstractType(const Token& token, TypeKind kind, bool isPointer = false);
		AbstractType(VariableType type, TypeKind kind = TypeKind::RValue, const std::string& userDefinedType = "");
		AbstractType(const std::string_view& value); //auto generate type from a value

		static llvm::Type* GetPtrType(const AbstractType& type);
		static AbstractType TypeToPtrType(const AbstractType& type);

		static llvm::StructType* GetStructType(const std::string& name);
		static StructMetaData&   GetStructInfo(const std::string& name);
		static StructMetaData&   GetStructMetaDataFromAllocInst(llvm::AllocaInst* alloc);


		static void CreateStructType(const std::string& name, const std::vector<MemberType>& members);

		inline const VariableType Get()				   const { return m_Type; };
		inline const TypeKind     GetKind()			   const { return m_Kind; }
		inline const std::string& GetUserDefinedType() const { return m_UserDefinedType; }

		inline llvm::Type* GetLLVMType() const { return m_LLVMType; }

		const bool IsFloatingPoint() const;
		const bool IsIntegral()		 const;
		const bool IsSigned()		 const;
		const bool IsPointer()		 const;

		inline operator VariableType() const { return m_Type; }
		inline operator TypeKind() const { return m_Kind; }
		inline operator bool() const { return Get() != VariableType::None; }

		inline const bool operator==(const AbstractType& other) const;
		inline const bool operator!=(const AbstractType& other) const;

		inline const bool operator==(VariableType other) const;
		inline const bool operator!=(VariableType other) const;


	private:
		VariableType m_Type = VariableType::None;
		TypeKind m_Kind = TypeKind::None;
		llvm::Type*  m_LLVMType = nullptr;
		std::string  m_UserDefinedType = "";
	};

	struct StructMetaData
	{
		llvm::StructType* Struct = nullptr;
		std::map<std::string, uint32_t> Indices;
		std::vector<AbstractType> Types;
	};
}