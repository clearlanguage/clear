#include "Types.h"

#include "API/LLVM/LLVMBackend.h"
#include "Utils.h"

namespace clear {

	llvm::Type* GetLLVMVariableType(VariableType type)
	{
		auto& context = *LLVM::Backend::GetContext();

		switch (type)
		{
			case VariableType::Int8:	return llvm::Type::getInt8Ty(context);
			case VariableType::Int16:	return llvm::Type::getInt16Ty(context);
			case VariableType::Int32:	return llvm::Type::getInt32Ty(context);
			case VariableType::Int64:	return llvm::Type::getInt64Ty(context);
			case VariableType::Uint8:	return llvm::Type::getInt8Ty(context);
			case VariableType::Uint16:	return llvm::Type::getInt16Ty(context);
			case VariableType::Uint32:	return llvm::Type::getInt32Ty(context);
			case VariableType::Uint64:	return llvm::Type::getInt64Ty(context);
			case VariableType::Bool:    return llvm::Type::getInt1Ty(context);
			case VariableType::Float32: return llvm::Type::getFloatTy(context);
			case VariableType::Float64:	return llvm::Type::getDoubleTy(context);
				//TODO:
			case VariableType::Struct:
			case VariableType::Object:
			case VariableType::None:
			default:
				return llvm::Type::getVoidTy(context);
		}
	}

	llvm::Value* GetLLVMConstant(VariableType type, const std::string& data)
	{
		auto& context = *LLVM::Backend::GetContext();

		//TODO: add strings

		switch (type)
		{
			case VariableType::Int8:    return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),  (int8_t)std::stoi(data),     true);
			case VariableType::Int16:   return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), (int16_t)std::stoi(data),    true);
			case VariableType::Int32:   return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), (int32_t)std::stoi(data),    true);
			case VariableType::Int64:   return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), (int64_t)std::stoll(data),   true);
			case VariableType::Uint8:   return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),  (uint8_t)std::stoull(data),  false);
			case VariableType::Uint16:  return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), (uint16_t)std::stoull(data), false);
			case VariableType::Uint32:  return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), (uint32_t)std::stoull(data), false);
			case VariableType::Uint64:  return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), (uint64_t)std::stoull(data), false);
			case VariableType::Float32: return llvm::ConstantFP::get(llvm::Type::getFloatTy(context),  (float)std::stod(data));
			case VariableType::Float64: return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), (double)std::stod(data));
			case VariableType::None:
			default:
				return nullptr;
		}
	}

	VariableType GetVariableTypeFromTokenType(TokenType tokenType)
	{
		switch (tokenType)
		{
			case TokenType::Int8Type:		return VariableType::Int8;
			case TokenType::Int16Type:		return VariableType::Int16;
			case TokenType::Int32Type:		return VariableType::Int32;
			case TokenType::Int64Type:		return VariableType::Int64;
			case TokenType::UInt8Type:		return VariableType::Uint8;
			case TokenType::UInt16Type:		return VariableType::Uint16;
			case TokenType::UInt32Type:		return VariableType::Uint32;
			case TokenType::UInt64Type:		return VariableType::Uint64;
			case TokenType::Float32Type:	return VariableType::Float32;
			case TokenType::Float64Type:	return VariableType::Float64;
			case TokenType::None:
			default:
				break;
		}

		return VariableType::None;
	}

	BinaryExpressionType GetBinaryExpressionTypeFromTokenType(TokenType type)
	{
		switch (type)
		{
			case TokenType::Assignment:			return BinaryExpressionType::Assignment;
			case TokenType::MulOp:				return BinaryExpressionType::Mul;
			case TokenType::AddOp:				return BinaryExpressionType::Add;
			case TokenType::DivOp:				return BinaryExpressionType::Div;
			case TokenType::SubOp:				return BinaryExpressionType::Sub;
			case TokenType::ModOp:				return BinaryExpressionType::Mod;
			case TokenType::IsEqual:			return BinaryExpressionType::Eq;
			case TokenType::GreaterThan:		return BinaryExpressionType::Greater;
			case TokenType::LessThan:			return BinaryExpressionType::Less;
			case TokenType::LessThanEqual:		return BinaryExpressionType::LessEq;
			case TokenType::GreaterThanEqual:	return BinaryExpressionType::Greater;

				//TODO:
			case TokenType::Not:
			case TokenType::NotEqual:
			case TokenType::BinaryShiftLeft:
			default:
				break;
		}

		return BinaryExpressionType::None;
	}

	AbstractType::AbstractType(const Token& token)
		: m_Type(GetVariableTypeFromTokenType(token.TokenType)), m_LLVMType(GetLLVMVariableType(m_Type))
	{
	}

	AbstractType::AbstractType(VariableType type)
		: m_Type(type), m_LLVMType(GetLLVMVariableType(type))
	{
	}

	AbstractType::AbstractType(const std::string_view& value)
	{
		if (IsValidNumber(value))
		{

		}

		
	}

}