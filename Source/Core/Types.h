#pragma once

#include "Tokens.h"
#include "API/LLVM/LLVMInclude.h"

#include <variant>

namespace clear {

	enum class VariableType
	{
		None = 0, Int8, Int16, Int32, Int64,
		Uint8, Uint16, Uint32, Uint64, Bool,
		Float32, Float64, UserDefinedType, Array
	};

	enum class VariableKind
	{
		None = 0, Paramter, Local
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
		AbstractType(VariableType type, const std::string& userDefinedType = "");
		AbstractType(const std::string_view& value); //auto generate type from a value

		inline const VariableType Get() const { return m_Type; };
		inline const std::string& GetUserDefinedType() const { return m_UserDefinedType; }
		inline const llvm::Type*  GetLLVMType() const { return m_LLVMType; }

		inline operator VariableType() const { return m_Type; }

	private:
		VariableType m_Type = VariableType::None;
		llvm::Type*  m_LLVMType = nullptr;
		std::string  m_UserDefinedType = "";
	};
}