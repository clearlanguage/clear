#include "TypeRegistry.h"

#include "API/LLVM/LLVMInclude.h"
#include "AST/ASTNode.h"
#include "Core/Log.h"
#include "Core/Utils.h"
#include "Symbols/Type.h"
#include <emmintrin.h>
#include <optional>
#include <string>

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
        m_Types["bool"]   = std::make_shared<PrimitiveType>(llvm::Type::getInt1Ty(*m_Context),  integerFlags, "bool");

        m_Types["int"]  = m_Types["int32"];
        m_Types["uint"] = m_Types["uint32"];
        m_Types["string"] = GetPointerTo(m_Types["int8"]); //TODO: gonna be a class soon

        TypeFlagSet floatingFlags;
        floatingFlags.set((size_t)TypeFlags::Floating);
        floatingFlags.set((size_t)TypeFlags::Signed);

        m_Types["float32"] = std::make_shared<PrimitiveType>(llvm::Type::getFloatTy(*m_Context),  floatingFlags, "float32");
        m_Types["float64"] = std::make_shared<PrimitiveType>(llvm::Type::getDoubleTy(*m_Context), floatingFlags, "float64");
        
        m_Types["float"] = m_Types["float32"];

        m_Types["opaque_ptr"] = std::make_shared<PointerType>(nullptr, *m_Context);

        m_Types["void"] = std::make_shared<PrimitiveType>(*m_Context);
    }

    void TypeRegistry::RegisterType(const std::string& name, std::shared_ptr<Type> type)
    {
        CLEAR_VERIFY(!m_Types.contains(name), "conflicting type name ", name);
        m_Types[name] = type;
    }

    void TypeRegistry::RemoveType(const std::string& name)
    {
        m_Types.erase(name);
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
            return nullptr;

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

    std::shared_ptr<Type> TypeRegistry::GetConstFrom(std::shared_ptr<Type> base)
    {
        CLEAR_VERIFY(base, "invalid base");

        std::string hash = "const" + base->GetHash();

        if(m_Types.contains(hash)) 
            return m_Types.at(hash);

        std::shared_ptr<ConstantType> ptr = std::make_shared<ConstantType>(base);
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
        if(token.GetData() == "null") return m_Types["opaque_ptr"];

        if(token.IsType(TokenType::String))
        {
            return GetPointerTo(GetType("int8"));
        }

        if(token.IsType(TokenType::Char))
        {
            return GetType("int8");
        }

        if(token.IsType(TokenType::Keyword) && (token.GetData() == "true" || token.GetData() == "false") )
        {
            return GetType("bool");
        }

        if(token.IsType(TokenType::Number))
        {
            return GetType(GuessTypeNameFromNumber(token.GetData()));
        }

        if(token.IsType(TokenType::Identifier))
        {
            return GetType(token.GetData());
        }

        return GetType(token.GetData());
    }

    void TypeRegistry::CreateClassTemplate(std::string_view name, std::shared_ptr<ASTNodeBase> classNode, llvm::ArrayRef<std::string> generics)
    {
        m_ClassTemplates[std::string(name)] = ClassTemplate { classNode, llvm::SmallVector<std::string>(generics) };
    }

    std::optional<ClassTemplate> TypeRegistry::GetClassTemplate(std::string_view name)
    {
        if(m_ClassTemplates.contains(std::string(name)))
            return m_ClassTemplates.at(std::string(name));

        return std::nullopt;
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
}
