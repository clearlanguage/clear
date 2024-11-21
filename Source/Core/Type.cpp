#include "Type.h"

#include "Parsing/Tokens.h"

#include "API/LLVM/LLVMBackend.h"
#include "Utils.h"
#include "Log.h"

namespace clear {

    //may need to move this to a module class when we want to compile concurrently
    static std::map<std::string, StructMetaData> s_UserDefinedTypesRegistry;
    static std::map<std::string, Ref<Type>>      s_VariableTypeRegistry;

    static TypeID GetTypeIDFromToken(TokenType tokenType)
	{
		switch (tokenType)
		{
			case TokenType::CharType:		return TypeID::Int8;
			case TokenType::Int8Type:		return TypeID::Int8;
			case TokenType::Int16Type:		return TypeID::Int16;
			case TokenType::Int32Type:		return TypeID::Int32;
			case TokenType::Int64Type:		return TypeID::Int64;
			case TokenType::UInt8Type:		return TypeID::Uint8;
			case TokenType::UInt16Type:		return TypeID::Uint16;
			case TokenType::UInt32Type:		return TypeID::Uint32;
			case TokenType::UInt64Type:		return TypeID::Uint64;
			case TokenType::Float32Type:	return TypeID::Float32;
			case TokenType::Float64Type:	return TypeID::Float64;
			case TokenType::Bool:			return TypeID::Bool;
			case TokenType::StringType:		return TypeID::String;
            case TokenType::RValueNumber:   
			case TokenType::None:
			default:
				break;
		}

		return TypeID::None;
	}

    static llvm::Type* GetType(TypeID type)
	{
		auto& context = *LLVM::Backend::GetContext();

		switch (type)
		{
			case TypeID::Int8:		    return llvm::Type::getInt8Ty(context);
			case TypeID::Int16:		    return llvm::Type::getInt16Ty(context);
			case TypeID::Int32:		    return llvm::Type::getInt32Ty(context);
			case TypeID::Int64:		    return llvm::Type::getInt64Ty(context);
			case TypeID::Uint8:		    return llvm::Type::getInt8Ty(context);
			case TypeID::Uint16:		return llvm::Type::getInt16Ty(context);
			case TypeID::Uint32:		return llvm::Type::getInt32Ty(context);
			case TypeID::Uint64:		return llvm::Type::getInt64Ty(context);
			case TypeID::Bool:		    return llvm::Type::getInt1Ty(context);
			case TypeID::Float32:		return llvm::Type::getFloatTy(context);
			case TypeID::Float64:		return llvm::Type::getDoubleTy(context);
			case TypeID::String:			
			case TypeID::Pointer:	    return llvm::PointerType::get(context , 0);
			case TypeID::None:
			default:
				return llvm::Type::getVoidTy(context);
		}
	}

    static TypeID GetIntegerTypeFromString(const std::string& str)
    {
        TypeID id = TypeID::None;

        NumberInfo info = GetNumberInfoFromLiteral(str);

        if (info.Valid) 
		{
			if (info.IsSigned && !info.IsFloatingPoint)
			{
				switch (info.BitsNeeded)
				{
					case 8:  id = TypeID::Int8;  break;
					case 16: id = TypeID::Int16; break;
					case 32: id = TypeID::Int32; break;
					case 64: id = TypeID::Int64; break;
					default:
						break;
				}
			}
			else if (!info.IsFloatingPoint)
			{
				switch (info.BitsNeeded)
				{
					case 8:  id = TypeID::Uint8;  break;
					case 16: id = TypeID::Uint16; break;
					case 32: id = TypeID::Uint32; break;
					case 64: id = TypeID::Uint64; break;
					default:
						break;
				}
			}
			else 
			{
				switch (info.BitsNeeded)
				{
					case 32: id = TypeID::Float32; break;
					case 64: id = TypeID::Float64; break;
					default:
						break;
				}
			}
		}

        return id;
    }

    Type::Type(TypeID type, TypeKindID typeKind, TypeID underlying)
    {
        m_ID = type;
        m_LLVMType = GetType(type);
        m_TypeKindID = typeKind;

        if(underlying != TypeID::None)
            m_Underlying = Ref<Type>::Create(underlying, typeKind);
    }

    Type::Type(const Token &token, bool isPointer)
    {
        if(isPointer)
        {
            CLEAR_VERIFY(token.TokenType != TokenType::RValueChar && token.TokenType != TokenType::RValueNumber && token.TokenType != TokenType::RValueString, "");

            m_ID = TypeID::Pointer;
            m_TypeKindID = TypeKindID::Variable;
            m_LLVMType = GetType(m_ID);
            m_Underlying = Ref<Type>::Create(token, false);

            return;
        }


        if(token.TokenType == TokenType::RValueString)
        {
            m_ID = TypeID::String;
            m_TypeKindID = TypeKindID::Constant;
            m_LLVMType = GetType(m_ID);
            m_Underlying = Ref<Type>::Create(TypeID::Int8, TypeKindID::Constant);

            return;
        }

        if(token.TokenType == TokenType::BooleanData)
        {
            m_ID = TypeID::Bool;
            m_TypeKindID = TypeKindID::Constant;
            m_LLVMType = GetType(m_ID);

            return;
        }

        if(token.TokenType == TokenType::RValueNumber)
        {
            m_ID = GetIntegerTypeFromString(token.Data);
            m_TypeKindID = TypeKindID::Constant;
            m_LLVMType = GetType(m_ID);

            return;
        }

        if(token.TokenType == TokenType::Null)
        {
            m_ID = TypeID::Pointer;
            m_TypeKindID = TypeKindID::Constant;
            m_LLVMType = GetType(m_ID);

            return;
        }

        if(token.TokenType == TokenType::TypeIdentifier)
        {
            m_ID = TypeID::UserDefinedType;
            m_TypeKindID = TypeKindID::Variable;

            auto it = s_UserDefinedTypesRegistry.find(token.Data);
            if(it != s_UserDefinedTypesRegistry.end())
			{
                m_LLVMType = it->second.Struct;
				m_UserDefinedTypeIdentifier = token.Data;	
			}
            else 
			{
                CLEAR_UNREACHABLE("invalid type identifer");
			}

            return;
        }

        if(token.TokenType == TokenType::VariableReference || token.TokenType == TokenType::VariableName)
        {
            auto it = s_VariableTypeRegistry.find(token.Data);
            
            if(it == s_VariableTypeRegistry.end())
                return;

            auto& type = it->second;

            m_ID = type->GetID();
            m_LLVMType = type->Get();
            m_Underlying = type->GetUnderlying();
            m_TypeKindID = TypeKindID::Reference;
            return;
        }

        m_ID = GetTypeIDFromToken(token.TokenType);

        if(m_ID == TypeID::String)
        {
            m_Underlying = Ref<Type>::Create(TypeID::Int8);
        }

        if(m_ID == TypeID::None)
            return;

        m_TypeKindID = TypeKindID::Variable;
        m_LLVMType = GetType(m_ID);
    }

    Type::Type(const std::string& userDefinedTypeName, const std::vector<MemberType>& members)
        : m_UserDefinedTypeIdentifier(userDefinedTypeName), m_ID(TypeID::UserDefinedType), m_TypeKindID(TypeKindID::Variable)
    {
        if(members.empty())
        {
            auto it = s_UserDefinedTypesRegistry.find(userDefinedTypeName);
            
            CLEAR_VERIFY(it != s_UserDefinedTypesRegistry.end(), "");
            m_LLVMType = it->second.Struct;
            return;
        }

        std::vector<llvm::Type*> types;

		StructMetaData info;
		uint32_t k = 0;

		for (auto& [type, memberName] : members)
		{
			info.Indices[memberName] = k++;
			info.Types.push_back(type);
            types.push_back(type->Get());
		}

		info.Struct = llvm::StructType::create(types, userDefinedTypeName);

        CLEAR_VERIFY(!s_UserDefinedTypesRegistry.contains(userDefinedTypeName), "");
		s_UserDefinedTypesRegistry[userDefinedTypeName] = info;

        m_LLVMType = info.Struct;
    }

    Type::Type(const Ref<Type>& elementType, size_t count)
        : m_ID(TypeID::Array), m_LLVMType(llvm::ArrayType::get(elementType->Get(), count)), m_TypeKindID(TypeKindID::Variable), m_Underlying(elementType)
    {
        CLEAR_VERIFY(elementType, "type cannot be null");
    }

    Type::Type(const Ref<Type>& pointTo)
        : m_ID(TypeID::Pointer), m_LLVMType(GetType(TypeID::Pointer)), m_TypeKindID(TypeKindID::Variable), m_Underlying(pointTo)
    {
        CLEAR_VERIFY(pointTo, "type cannot be null");
    }

    bool Type::IsFloatingPoint() const
    {
        switch (m_ID)
		{
			case TypeID::Float32:
			case TypeID::Float64:
				return true;
			default:
				break;
		}

		return false;
    }

    bool Type::IsIntegral() const
    {
        switch (m_ID)
		{
			case TypeID::Int8:
			case TypeID::Int16:
			case TypeID::Int32:
			case TypeID::Int64:
			case TypeID::Uint8:
			case TypeID::Uint16:
			case TypeID::Uint32:
			case TypeID::Uint64:
				return true;
			default:
				break;
		}

		return false;
    }
    bool Type::IsSigned() const
    {
        switch (m_ID)
		{
			case TypeID::Int8:
			case TypeID::Int16:
			case TypeID::Int32:
			case TypeID::Int64:
			case TypeID::Float32:
			case TypeID::Float64:
				return true;
			default:
				break;
		}

		return false;
    }

    bool Type::IsPointer() const
    {
        switch (m_ID)
		{
			case TypeID::Array:
			case TypeID::String:
			case TypeID::Pointer:
				return true;
			default:
				break;
		}

		return false;
    }

    StructMetaData& Type::GetStructMetaData(const std::string& name)
    {
        static StructMetaData s_NullStructMetaData;
        auto it = s_UserDefinedTypesRegistry.find(name);

        if(it == s_UserDefinedTypesRegistry.end())
            return s_NullStructMetaData;

        return it->second;
    }

    void Type::RegisterVariableType(const std::string& name, const Ref<Type>& type)
    {
        CLEAR_PARSER_VERIFY(!s_VariableTypeRegistry.contains(name), "already registered");
        s_VariableTypeRegistry[name] = type;
    }

    void Type::RemoveVariableType(const std::string &name)
    {
        auto it = s_UserDefinedTypesRegistry.find(name);

        if(it == s_UserDefinedTypesRegistry.end())
            return;

        s_UserDefinedTypesRegistry.erase(it);
    }

    Ref<Type> Type::GetVariableTypeFromName(const std::string& name)
    {
        auto it = s_VariableTypeRegistry.find(name);

        CLEAR_VERIFY(it != s_VariableTypeRegistry.end(), "could not find type");
        return it->second;
    }

    BinaryExpressionType Type::GetBinaryExpressionTypeFromToken(TokenType type)
    {
        switch (type)
	    {
			case TokenType::Assignment:			return BinaryExpressionType::Assignment;
			case TokenType::MultiplyAssign:
			case TokenType::MulOp:				return BinaryExpressionType::Mul;
			case TokenType::PlusAssign:
			case TokenType::AddOp:				return BinaryExpressionType::Add;
			case TokenType::DivideAssign:
			case TokenType::DivOp:				return BinaryExpressionType::Div;
			case TokenType::MinusAssign:
			case TokenType::SubOp:				return BinaryExpressionType::Sub;
			case TokenType::ModuloAssign:
			case TokenType::ModOp:				return BinaryExpressionType::Mod;
			case TokenType::IsEqual:			return BinaryExpressionType::Eq;
			case TokenType::NotEqual:			return BinaryExpressionType::NotEq;
			case TokenType::GreaterThan:		return BinaryExpressionType::Greater;
			case TokenType::LessThan:			return BinaryExpressionType::Less;
			case TokenType::LessThanEqual:		return BinaryExpressionType::LessEq;
			case TokenType::GreaterThanEqual:	return BinaryExpressionType::GreaterEq;
			case TokenType::BitwiseNot:			return BinaryExpressionType::BitwiseNot;
			case TokenType::LeftShift:			return BinaryExpressionType::BitwiseLeftShift;
			case TokenType::RightShift:			return BinaryExpressionType::BitwiseRightShift;
			case TokenType::BitwiseOr:			return BinaryExpressionType::BitwiseOr;
			case TokenType::BitwiseXor:			return BinaryExpressionType::BitwiseXor;
			case TokenType::BitwiseAnd:			return BinaryExpressionType::BitwiseAnd;
			case TokenType::IndexOperator:		return BinaryExpressionType::Index;	

			default:
				break;
		}

		return BinaryExpressionType::None;
    }

    UnaryExpressionType Type::GetUnaryExpressionTypeFromToken(TokenType type)
    {
        switch (type)
        {
			case TokenType::Increment:      return UnaryExpressionType::Increment;
			case TokenType::Decrement:      return UnaryExpressionType::Decrement;
			case TokenType::BitwiseNot:     return UnaryExpressionType::BitwiseNot;
			case TokenType::AddressOp:	    return UnaryExpressionType::Reference;
			case TokenType::DereferenceOp:	return UnaryExpressionType::Dereference;
			case TokenType::Negation:       return UnaryExpressionType::Negation; 

			default:
				break;
		}

		return UnaryExpressionType::None;
    }

    UnaryExpressionType Type::GetPostUnaryExpressionTypeFromToken(TokenType type)
    {
        switch (type)
		{
			case TokenType::Increment:  return UnaryExpressionType::PostIncrement;
			case TokenType::Decrement:  return UnaryExpressionType::PostDecrement;
			default:
				break;
		}

		return UnaryExpressionType::None;
    }
}