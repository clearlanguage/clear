#include "Type.h"

#include "Parsing/Tokens.h"

#include "API/LLVM/LLVMBackend.h"
#include "Utils.h"
#include "Log.h"

namespace clear {

    static std::map<std::string, StructMetaData> s_UserDefinedTypesRegistry;

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

    Type::Type(const std::string &rvalue)
        : m_TypeKindID(TypeKindID::Constant)
    {
        NumberInfo info = GetNumberInfoFromLiteral(rvalue);

        if (info.Valid) 
		{
			if (info.IsSigned && !info.IsFloatingPoint)
			{
				switch (info.BitsNeeded)
				{
					case 8:  m_ID = TypeID::Int8;  break;
					case 16: m_ID = TypeID::Int16; break;
					case 32: m_ID = TypeID::Int32; break;
					case 64: m_ID = TypeID::Int64; break;
					default:
						break;
				}
			}
			else if (!info.IsFloatingPoint)
			{
				switch (info.BitsNeeded)
				{
					case 8:  m_ID = TypeID::Uint8;  break;
					case 16: m_ID = TypeID::Uint16; break;
					case 32: m_ID = TypeID::Uint32; break;
					case 64: m_ID = TypeID::Uint64; break;
					default:
						break;
				}
			}
			else 
			{
				switch (info.BitsNeeded)
				{
					case 32: m_ID = TypeID::Float32; break;
					case 64: m_ID = TypeID::Float64; break;
					default:
						break;
				}
			}
		}
		else if (rvalue == "null")
		{
			m_ID = TypeID::Pointer;
		}
		else 
		{
			m_ID = TypeID::String;
		}


        m_LLVMType = GetType(m_ID);
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

    
}