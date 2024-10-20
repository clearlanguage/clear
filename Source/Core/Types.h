#pragma once

#include "Tokens.h"
#include "API/LLVM/LLVMInclude.h"

#include <variant>

namespace clear {

	enum class VariableType 
	{
		None = 0, Int8, Int16, Int32, Int64,
		Uint8, Uint16, Uint32, Uint64, Bool,
		Float32, Float64, String, UserDefinedType,
		Array
	};

	enum class TypeKind
	{
		None = 0, RValue, Variable
	};


	enum class BinaryExpressionType
	{
		None = 0, Add, Sub, Mul,
		Div, Mod, Less, LessEq,
		Greater, GreaterEq, Eq, Assignment
	};
	
	struct ObjectReferenceInfo
	{
		llvm::StructType* Struct = nullptr;
		std::map<std::string, uint32_t> Indices;
	};

	extern BinaryExpressionType GetBinaryExpressionTypeFromTokenType(TokenType type);
	extern VariableType	GetVariableTypeFromTokenType(TokenType tokenType);
	extern llvm::Type*	GetLLVMVariableType(VariableType type);
	extern llvm::Value*	GetLLVMConstant(VariableType type, const std::string& data);
	extern bool IsTypeIntegral(VariableType type);

	class AbstractType 
	{
	public:
		AbstractType() = default;
		AbstractType(const Token& token);
		AbstractType(VariableType type, TypeKind kind = TypeKind::RValue, const std::string& userDefinedType = "");
		AbstractType(const std::string_view& value); //auto generate type from a value

		inline const VariableType Get() const { return m_Type; };
		inline const TypeKind GetKind() const { return m_Kind; }
		inline const std::string& GetUserDefinedType() const { return m_UserDefinedType; }
		inline const llvm::Type*  GetLLVMType() const { return m_LLVMType; }
		inline llvm::Type* GetLLVMType() { return m_LLVMType; }

		const bool IsFloatingPoint() const;
		const bool IsIntegral()		 const;
		const bool IsSigned()		 const;
		const bool IsPointer()		 const;

		inline operator VariableType() const { return m_Type; }
		inline operator TypeKind() const { return m_Kind; }

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
}