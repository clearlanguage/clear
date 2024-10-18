#pragma once

#include "Tokens.h"
#include "API/LLVM/LLVMInclude.h"

#include <variant>

namespace clear {

	enum class VariableType
	{
		None = 0, Int8, Int16, Int32, Int64,
		Uint8, Uint16, Uint32, Uint64, Bool,
		Float32, Float64, Struct, Object, 
		ConstantString
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

	using Field = std::variant<std::string, VariableType>;
	
	struct ObjectReferenceInfo
	{
		llvm::StructType* Struct = nullptr;
		std::map<std::string, uint32_t> Indices;
	};

	extern BinaryExpressionType GetBinaryExpressionTypeFromTokenType(TokenType type);
	extern VariableType	GetVariableTypeFromTokenType(TokenType tokenType);
	extern llvm::Type*	GetLLVMVariableType(VariableType type);
	extern llvm::Value*	GetLLVMConstant(VariableType type, const std::string& data);

	class AbstractType
	{
	public:
		AbstractType() = default;
		AbstractType(const Token& token);
		AbstractType(VariableType type);
		AbstractType(const std::string_view& value); //auto generate type from a value

		inline const VariableType Get() const { return m_Type; };
		inline const llvm::Type*  GetLLVMType() const { return m_LLVMType; }


	private:
		VariableType m_Type = VariableType::None;
		llvm::Type*  m_LLVMType = nullptr;
	};

	class AbstractValue
	{
	public:
	private:
		AbstractType m_Type;
		llvm::Value* m_Value;
	};
}