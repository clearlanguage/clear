#pragma once

#include "Tokens.h"
#include "API/LLVM/LLVMInclude.h"

#include <variant>

namespace clear {

	enum class VariableType
	{
		None = 0, Int8, Int16, Int32, Int64,
		Uint8, Uint16, Uint32, Uint64, Bool,
		Float32, Float64, Struct, Object 
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
	extern VariableType GetVariableTypeFromTokenType(TokenType tokenType);
	extern llvm::Type*  GetLLVMVariableType(VariableType type);
	extern llvm::Value* GetLLVMConstant(VariableType type, const std::string& data);
}