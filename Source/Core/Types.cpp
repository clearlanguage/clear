#include "Types.h"

#include "API/LLVM/LLVMBackend.h"
#include "Utils.h"
#include "Log.h"

namespace clear {

	static std::map<std::string, StructMetaData>   s_StructTypes;

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
			case VariableType::String:			return llvm::PointerType::get(GetLLVMVariableType(VariableType::Int8), 0);
			case VariableType::Int8Pointer:		return llvm::PointerType::get(GetLLVMVariableType(VariableType::Int8), 0);
			case VariableType::Int16Pointer:	return llvm::PointerType::get(GetLLVMVariableType(VariableType::Int16), 0);
			case VariableType::Int32Pointer:	return llvm::PointerType::get(GetLLVMVariableType(VariableType::Int32), 0);
			case VariableType::Int64Pointer:	return llvm::PointerType::get(GetLLVMVariableType(VariableType::Int64), 0);
			case VariableType::Uint8Pointer:	return llvm::PointerType::get(GetLLVMVariableType(VariableType::Uint8), 0);
			case VariableType::Uint16Pointer:	return llvm::PointerType::get(GetLLVMVariableType(VariableType::Uint16), 0);
			case VariableType::Uint32Pointer:	return llvm::PointerType::get(GetLLVMVariableType(VariableType::Uint32), 0);
			case VariableType::Uint64Pointer:	return llvm::PointerType::get(GetLLVMVariableType(VariableType::Uint64), 0);
			case VariableType::BoolPointer:		return llvm::PointerType::get(GetLLVMVariableType(VariableType::Bool), 0);
			case VariableType::Float32Pointer:	return llvm::PointerType::get(GetLLVMVariableType(VariableType::Float32), 0);
			case VariableType::Float64Pointer:	return llvm::PointerType::get(GetLLVMVariableType(VariableType::Float64), 0);
			case VariableType::StringPointer:	return llvm::PointerType::get(GetLLVMVariableType(VariableType::String), 0);
			case VariableType::None:
			default:
				return llvm::Type::getVoidTy(context);
		}
	}

	VariableType GetVariableTypeFromTokenType(TokenType tokenType)
	{
		switch (tokenType)
		{
			case TokenType::CharType:		return VariableType::Int8;
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
			case TokenType::StringType:		return VariableType::String;
			case TokenType::None:
			default:
				break;
		}

		return VariableType::None;
	}

	VariableType GetVariablePointerTypeFromTokenType(TokenType tokenType)
	{
		switch (tokenType)
		{
			case TokenType::Int8Type:		return VariableType::Int8Pointer;
			case TokenType::Int16Type:		return VariableType::Int16Pointer;
			case TokenType::Int32Type:		return VariableType::Int32Pointer;
			case TokenType::Int64Type:		return VariableType::Int64Pointer;
			case TokenType::UInt8Type:		return VariableType::Uint8Pointer;
			case TokenType::UInt16Type:		return VariableType::Uint16Pointer;
			case TokenType::UInt32Type:		return VariableType::Uint32Pointer;
			case TokenType::UInt64Type:		return VariableType::Uint64Pointer;
			case TokenType::Float32Type:	return VariableType::Float32Pointer;
			case TokenType::Float64Type:	return VariableType::Float64Pointer;
			case TokenType::Bool:			return VariableType::BoolPointer;
			case TokenType::StringType:		return VariableType::StringPointer;
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

	AbstractType::AbstractType(const Token& token, TypeKind kind, bool isPointer)
		: m_Kind(kind)
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

		m_Type = isPointer ? GetVariablePointerTypeFromTokenType(token.TokenType) : GetVariableTypeFromTokenType(token.TokenType);
		m_LLVMType = GetLLVMVariableType(m_Type);
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
			m_Type = VariableType::String;
		}

		
		CLEAR_VERIFY(m_Type != VariableType::None, "could not evaluate type of ", value);
		m_LLVMType = GetLLVMVariableType(m_Type);

		return;
	}

	llvm::StructType* AbstractType::GetStructType(const std::string& name)
	{
		return s_StructTypes.contains(name) ? s_StructTypes.at(name).Struct : nullptr;
	}

	static StructMetaData s_NullReference;

	StructMetaData& AbstractType::GetStructInfo(const std::string& name)
	{
		return s_StructTypes.contains(name) ? s_StructTypes.at(name) : s_NullReference;
	}

	StructMetaData& AbstractType::GetStructMetaDataFromAllocInst(llvm::AllocaInst* alloc)
	{
		for (auto& [name, reference] : s_StructTypes)
		{
			if (reference.Struct == alloc->getAllocatedType())
				return reference;
		}

		return s_NullReference;
	}

	void AbstractType::CreateStructType(const std::string& name, const std::vector<MemberType>& members)
	{
		std::vector<llvm::Type*> types;

		StructMetaData info;
		uint32_t k = 0;

		for (auto& [memberName, type] : members)
		{
			if (type == VariableType::UserDefinedType)
			{
				auto& structName = type.GetUserDefinedType();
				types.push_back(s_StructTypes[structName].Struct);
			}
			else
			{
				types.push_back(GetLLVMVariableType(type));
			}

			info.Indices[memberName] = k++;
			info.Types.push_back(type);
		}

		info.Struct = llvm::StructType::create(types);
		s_StructTypes[name] = info;
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
			default:
				break;
		}

		return false;
	}

	const bool AbstractType::IsPointer() const
	{
		switch (m_Type)
		{
			case VariableType::String:
			case VariableType::Array:
			case VariableType::Int8Pointer:
			case VariableType::Int16Pointer:
			case VariableType::Int32Pointer:
			case VariableType::Int64Pointer:
			case VariableType::Uint8Pointer:
			case VariableType::Uint16Pointer:
			case VariableType::Uint32Pointer:
			case VariableType::Uint64Pointer:
			case VariableType::BoolPointer:
			case VariableType::Float32Pointer:
			case VariableType::Float64Pointer:
			case VariableType::StringPointer:
			case VariableType::UserDefinedTypePointer:
			case VariableType::ArrayPointer:
				return true;
			default:
				break;
		}

		return false;
	}

	const bool AbstractType::IsFloatingPoint() const
	{
		switch (m_Type)
		{
			case VariableType::Float32:
			case VariableType::Float64:
				return true;
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