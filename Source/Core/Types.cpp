#include "Types.h"

#include "API/LLVM/LLVMBackend.h"
#include "Utils.h"
#include "Log.h"

namespace clear {

	static size_t s_StringCount = 0;

	static llvm::Value* CreateConstantString(const std::string& data)
	{
		auto& module  = *LLVM::Backend::GetModule();
		auto& context = *LLVM::Backend::GetContext();

		llvm::Constant* strConstant = llvm::ConstantDataArray::getString(context, data, true);

		llvm::GlobalVariable* globalStr = new llvm::GlobalVariable(
			module,                                         
			strConstant->getType(),                          
			true,                                            
			llvm::GlobalValue::PrivateLinkage,               
			strConstant,                                     
			".str"                                           
		);

		llvm::Constant* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
		llvm::Constant* indices[] = { zero, zero };
		llvm::Constant* strPtr = llvm::ConstantExpr::getGetElementPtr(
			globalStr->getValueType(),                       
			globalStr,                                     
			indices                                         
		);

		return strPtr;
	}

	llvm::Type* GetLLVMVariableType(VariableType type)
	{
		auto& context = *LLVM::Backend::GetContext();

		switch (type)
		{
			case VariableType::Int8:		    return llvm::Type::getInt8Ty(context);
			case VariableType::Int16:		    return llvm::Type::getInt16Ty(context);
			case VariableType::Int32:		    return llvm::Type::getInt32Ty(context);
			case VariableType::Int64:		    return llvm::Type::getInt64Ty(context);
			case VariableType::Uint8:		    return llvm::Type::getInt8Ty(context);
			case VariableType::Uint16:		    return llvm::Type::getInt16Ty(context);
			case VariableType::Uint32:		    return llvm::Type::getInt32Ty(context);
			case VariableType::Uint64:		    return llvm::Type::getInt64Ty(context);
			case VariableType::Bool:		    return llvm::Type::getInt1Ty(context);
			case VariableType::Float32:		    return llvm::Type::getFloatTy(context);
			case VariableType::Float64:		    return llvm::Type::getDoubleTy(context);
			case VariableType::UserDefinedType:	return llvm::PointerType::get(context , 0);
			case VariableType::None:
			default:
				return llvm::Type::getVoidTy(context);
		}
	}

	llvm::Value* GetLLVMConstant(VariableType type, const std::string& data)
	{
		auto& context = *LLVM::Backend::GetContext();

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
			case VariableType::Bool:	return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context),  data == "true" ? 1 : 0);
			case VariableType::None:
			default:
				return nullptr;
		}
	}

	bool IsTypeIntegral(VariableType type)
	{
		switch (type)
		{
			case VariableType::Int8:
			case VariableType::Int16:
			case VariableType::Int32:
			case VariableType::Int64:
			case VariableType::Uint8:
			case VariableType::Uint16:
			case VariableType::Uint32:
			case VariableType::Uint64:
			case VariableType::Float32:
			case VariableType::Float64:
				return true;
			default:
				break;
		}

		return false;
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
			case TokenType::Bool:			return VariableType::Bool;
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
		if (token.TokenType == TokenType::RValueNumber ||
			token.TokenType == TokenType::RValueChar ||
			token.TokenType == TokenType::RValueString)
		{
			m_Kind = TypeKind::RValue;
		}
		else
		{
			m_Kind = TypeKind::Variable;
		}

	}

	AbstractType::AbstractType(const Token& token, TypeKind kind)
		: m_Type(GetVariableTypeFromTokenType(token.TokenType)), m_LLVMType(GetLLVMVariableType(m_Type)), m_Kind(kind)
	{
		if (token.TokenType == TokenType::RValueNumber ||
			token.TokenType == TokenType::RValueChar ||
			token.TokenType == TokenType::RValueString)
		{
			m_Kind = TypeKind::RValue;
		}
		else
		{
			m_Kind = TypeKind::Variable;
		}

	}

	AbstractType::AbstractType(VariableType type, TypeKind kind, const std::string& userDefinedtype)
		: m_Type(type), m_LLVMType(GetLLVMVariableType(type)), m_UserDefinedType(userDefinedtype), m_Kind(kind)
	{
	}

	AbstractType::AbstractType(const std::string_view& value)
		: m_Kind(TypeKind::RValue)
	{
		//handle the case where the type may be a number
		NumberInfo info = GetNumberInfoFromLiteral(value);
		
		if (info.Valid) 
		{
			if (info.IsSigned && !info.IsFloatingPoint)
			{
				switch (info.BitsNeeded)
				{
					case 8:  m_Type = VariableType::Int8;  break;
					case 16: m_Type = VariableType::Int16; break;
					case 32: m_Type = VariableType::Int32; break;
					case 64: m_Type = VariableType::Int64; break;
					default:
						break;
				}
			}
			else if (!info.IsFloatingPoint)
			{
				switch (info.BitsNeeded)
				{
					case 8:  m_Type = VariableType::Uint8;  break;
					case 16: m_Type = VariableType::Uint16; break;
					case 32: m_Type = VariableType::Uint32; break;
					case 64: m_Type = VariableType::Uint64; break;
					default:
						break;
				}
			}
			else 
			{
				switch (info.BitsNeeded)
				{
					case 32: m_Type = VariableType::Float32; break;
					case 64: m_Type = VariableType::Float64; break;
					default:
						break;
				}
			}
		}
		else if (value == "true" || value == "false")
		{
			m_Type = VariableType::Bool;
		}
		else
		{
			m_Type = VariableType::Array;
		}


		CLEAR_VERIFY(m_Type != VariableType::None, "could not evaluate type of ", value);
		m_LLVMType = GetLLVMVariableType(m_Type);

		return;
	}

	const bool AbstractType::IsSigned() const
	{
		switch (m_Type)
		{
			case VariableType::Int8:
			case VariableType::Int16:
			case VariableType::Int32:
			case VariableType::Int64:
			case VariableType::Float32:
			case VariableType::Float64:
				return true;
			case VariableType::Bool:
			case VariableType::Uint8:
			case VariableType::Uint16:
			case VariableType::Uint32:
			case VariableType::Uint64:
			case VariableType::String:
			case VariableType::UserDefinedType:
			case VariableType::Array:
			case VariableType::None:
			default:
				break;
		}

		return false;
	}

	const bool AbstractType::IsPointer() const
	{
		//TODO:
		return false;
	}

	const bool AbstractType::IsFloatingPoint() const
	{
		switch (m_Type)
		{
			case VariableType::Float32:
			case VariableType::Float64:
				return true;
			case VariableType::Int8:
			case VariableType::Int16:
			case VariableType::Int32:
			case VariableType::Int64:
			case VariableType::Bool:
			case VariableType::Uint8:
			case VariableType::Uint16:
			case VariableType::Uint32:
			case VariableType::Uint64:
			case VariableType::String:
			case VariableType::UserDefinedType:
			case VariableType::Array:
			case VariableType::None:
			default:
				break;
		}

		return false;
	}

	const bool AbstractType::IsIntegral() const
	{
		switch (m_Type)
		{
			case VariableType::Int8:
			case VariableType::Int16:
			case VariableType::Int32:
			case VariableType::Int64:
			case VariableType::Uint8:
			case VariableType::Uint16:
			case VariableType::Uint32:
			case VariableType::Uint64:
				return true;
			case VariableType::Bool:
			case VariableType::Float32:
			case VariableType::Float64:
			case VariableType::String:
			case VariableType::UserDefinedType:
			case VariableType::Array:
			case VariableType::None:
			default:
				break;
		}

		return false;
	}

	const bool AbstractType::operator==(const AbstractType& other) const
	{
		return m_Type == other.m_Type && 
			   m_UserDefinedType == other.m_UserDefinedType;
	}

	const bool AbstractType::operator!=(const AbstractType& other) const
	{
		return !(*this == other);
	}

	inline const bool AbstractType::operator==(VariableType other) const
	{
		return m_Type == other;
	}

	inline const bool AbstractType::operator!=(VariableType other) const
	{
		return !(*this == other);
	}

}