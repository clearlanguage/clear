#include "TypeRegistry.h"

#include "API/LLVM/LLVMInclude.h"
#include "Core/Log.h"
#include "Utils.h"

namespace clear 
{
    TypeRegistry::TypeRegistry(std::shared_ptr<llvm::LLVMContext> context)
        : m_Context(context)
    {
    }

    void TypeRegistry::RegisterBuiltinTypes()
    {
        TypeFlagSet signedIntegerFlags;
        signedIntegerFlags.set((size_t)TypeFlags::Integral);
        signedIntegerFlags.set((size_t)TypeFlags::Signed);

        m_Types["int8"]  = std::make_shared<PrimitiveType>(llvm::Type::getInt8Ty(*m_Context), signedIntegerFlags, "int8");
        m_Types["int16"] = std::make_shared<PrimitiveType>(llvm::Type::getInt16Ty(*m_Context), signedIntegerFlags, "int16");
        m_Types["int32"] = std::make_shared<PrimitiveType>(llvm::Type::getInt32Ty(*m_Context), signedIntegerFlags, "int32");
        m_Types["int64"] = std::make_shared<PrimitiveType>(llvm::Type::getInt64Ty(*m_Context), signedIntegerFlags, "int64");

        TypeFlagSet integerFlags;
        integerFlags.set((size_t)TypeFlags::Integral);

        m_Types["uint8"]  = std::make_shared<PrimitiveType>(llvm::Type::getInt8Ty(*m_Context),  integerFlags, "uint8");
        m_Types["uint16"] = std::make_shared<PrimitiveType>(llvm::Type::getInt16Ty(*m_Context), integerFlags, "uint16");
        m_Types["uint32"] = std::make_shared<PrimitiveType>(llvm::Type::getInt32Ty(*m_Context), integerFlags, "uint32");
        m_Types["uint64"] = std::make_shared<PrimitiveType>(llvm::Type::getInt64Ty(*m_Context), integerFlags, "uint64");
        m_Types["bool"]   = std::make_shared<PrimitiveType>(llvm::Type::getInt1Ty(*m_Context), integerFlags, "bool");

        m_Types["int"]  = m_Types["int64"];
        m_Types["uint"] = m_Types["uint64"];


        TypeFlagSet floatingFlags;
        floatingFlags.set((size_t)TypeFlags::Floating);
        floatingFlags.set((size_t)TypeFlags::Signed);

        m_Types["float32"] = std::make_shared<PrimitiveType>(llvm::Type::getFloatTy(*m_Context),  floatingFlags, "float32");
        m_Types["float64"] = std::make_shared<PrimitiveType>(llvm::Type::getDoubleTy(*m_Context), floatingFlags, "float64");
        
        m_Types["float"] = m_Types["float64"];


        m_Types["void"] = std::make_shared<PrimitiveType>(*m_Context);
    }

    void TypeRegistry::RegisterType(const std::string& name, std::shared_ptr<Type> type)
    {
        CLEAR_VERIFY(!m_Types.contains(name), "conflicting type name ", name);
        m_Types[name] = type;

        //if(type->IsArray() || type->IsPointer()) 
        //{
        //    CascadeType(type);
        //}
    }

    std::shared_ptr<Type> TypeRegistry::GetType(const std::string& name) const
    {
        if(m_Types.contains(name)) 
            return m_Types.at(name);
        
        return nullptr;
    }

    std::shared_ptr<Type> TypeRegistry::GetPointerTo(std::shared_ptr<Type> base)
    {
        if(!base) 
            return GetPointerTo(m_Types["void"]);

        std::string hash = base->GetHash();
        hash += "*";

        if(m_Types.contains(hash)) 
            return m_Types.at(hash);

        std::shared_ptr<PointerType> ptr = std::make_shared<PointerType>(base, *m_Context);
        m_Types[hash] = ptr;

        return ptr;
    }

    std::shared_ptr<Type> TypeRegistry::GetArrayFrom(std::shared_ptr<Type> base, size_t count)
    {
        CLEAR_VERIFY(base, "invalid base");

        std::string hash = base->GetHash();
        hash += "[" + std::to_string(count) + "]";

        if(m_Types.contains(hash)) 
            return m_Types.at(hash);

        std::shared_ptr<ArrayType> ptr = std::make_shared<ArrayType>(base, count);
        m_Types[hash] = ptr;

        return ptr;
    }

    std::shared_ptr<Type> TypeRegistry::GetSignedType(std::shared_ptr<Type> type)
    {
        CLEAR_VERIFY(type->IsIntegral(), "only works on integral types!");

        std::string hash = type->GetHash();

        if(hash[0] == 'u') 
            return GetType(hash.substr(1, hash.size()));

        return type;
    }

    std::shared_ptr<Type> TypeRegistry::GetTypeFromToken(const Token& token)
    {
        if(token.TokenType == TokenType::RValueString || token.TokenType == TokenType::StringType)
        {
            return GetPointerTo(GetType("int8"));
        }

        if(token.TokenType == TokenType::BooleanData)
        {
            return GetType("bool");
        }

        if(token.TokenType == TokenType::RValueNumber)
        {
            return GetType(GuessTypeNameFromNumber(token.Data));
        }

        if(token.TokenType == TokenType::TypeIdentifier)
        {
            return GetType(token.Data);
        }

        return GetType(GetTypeNameFromTokenType(token.TokenType));
    }

    std::shared_ptr<Type> TypeRegistry::CreateStruct(const std::string& name, const std::vector<std::pair<std::string, std::shared_ptr<Type>>>& members)
    {
        std::shared_ptr<StructType> structType = std::make_shared<StructType>(name, members);
        m_Types[name] = structType;
        return structType;
    }

    std::string TypeRegistry::GetTypeNameFromTokenType(TokenType type)
    {
        switch (type)
		{
			case TokenType::CharType:		return "int8";
			case TokenType::Int8Type:		return "int8";
			case TokenType::Int16Type:		return "int16";
			case TokenType::Int32Type:		return "int32";
			case TokenType::Int64Type:		return "int64";
			case TokenType::UInt8Type:		return "uint8";
			case TokenType::UInt16Type:		return "uint16";
			case TokenType::UInt32Type:		return "uint32";
			case TokenType::UInt64Type:		return "uint64";
			case TokenType::Float32Type:	return "float32";
			case TokenType::Float64Type:	return "float64";
			case TokenType::Bool:			return "bool";
			case TokenType::StringType:		return "int8*";
        	case TokenType::Void:           return "void";
            case TokenType::RValueNumber:   
			case TokenType::None:
			default:
				break;
		}

		return "";
    }



    std::string TypeRegistry::GuessTypeNameFromNumber(const std::string& number)
    {
        NumberInfo info = GetNumberInfoFromLiteral(number);

        if (!info.Valid)
        {
            CLEAR_LOG_ERROR("invalid number ", number);
            return "";
        } 

        if(info.IsFloatingPoint)
        {
            switch (info.BitsNeeded)
			{
				case 32: return "float32"; break;
				case 64: return "float64"; break;
				default:
					break;
			}
        }
		else if (info.IsSigned)
		{
			switch (info.BitsNeeded)
			{
				case 8:  return "int8";
				case 16: return "int16";
				case 32: return "int32";
				case 64: return "int64";
				default:
					break;
			}
		}
		else
		{
			switch (info.BitsNeeded)
			{
				case 8:  return "uint8";
				case 16: return "uint16";
				case 32: return "uint32";
				case 64: return "uint64";
				default:
					break;
			}
		}
        
        CLEAR_LOG_ERROR("unable to guess type for ", number);
        return "";
    }

    void TypeRegistry::CascadeType(std::shared_ptr<Type> type)
    {
        if(auto ty = std::dynamic_pointer_cast<PointerType>(type))
        {
            std::string hash = ty->GetBaseType()->GetHash();

            if(!m_Types.contains(hash))
            {
                m_Types[hash] = ty->GetBaseType();
                CascadeType(ty);
                return;
            }

        }
    }
}
