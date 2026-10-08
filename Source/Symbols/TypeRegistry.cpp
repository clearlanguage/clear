#include "TypeRegistry.h"

#include "API/LLVM/LLVMInclude.h"
#include "AST/ASTNode.h"
#include "Core/Log.h"
#include "Core/Utils.h"
#include "Symbols/Symbol.h"
#include "Symbols/Type.h"
// #include <emmintrin.h>
#include <optional>
#include <map>
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
        m_Types["str"] = std::make_shared<StrType>(m_Types["int8"], *m_Context);
        m_Types["string"] = m_Types["str"];

        TypeFlagSet floatingFlags;
        floatingFlags.set((size_t)TypeFlags::Floating);
        floatingFlags.set((size_t)TypeFlags::Signed);

        m_Types["float32"] = std::make_shared<PrimitiveType>(llvm::Type::getFloatTy(*m_Context),  floatingFlags, "float32");
        m_Types["float64"] = std::make_shared<PrimitiveType>(llvm::Type::getDoubleTy(*m_Context), floatingFlags, "float64");
        
        m_Types["float"] = m_Types["float64"]; // like Python, float is double precision

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

    // Pointer, array and const types are shared by every module: `*Rect` made while compiling one file
    // is the very same object as `*Rect` made in another, so types can be compared by identity.
    // The key is the base type's identity (not its name, two modules may both define a class `Node`).
    static std::shared_ptr<Type>& DerivedTypeSlot(const std::shared_ptr<Type>& base, size_t kind)
    {
        static std::map<std::pair<Type*, size_t>, std::shared_ptr<Type>> s_DerivedTypes;
        return s_DerivedTypes[{ base.get(), kind }];
    }

    static constexpr size_t s_PointerKind = (size_t)-1;
    static constexpr size_t s_ConstKind   = (size_t)-2;

    std::shared_ptr<Type> TypeRegistry::GetPointerTo(std::shared_ptr<Type> base)
    {
        if(!base) 
            return nullptr;

        auto& slot = DerivedTypeSlot(base, s_PointerKind);

        if (!slot)
            slot = std::make_shared<PointerType>(base, *m_Context);

        m_Types.try_emplace(slot->GetHash(), slot);
        return slot;
    }

    std::shared_ptr<Type> TypeRegistry::GetArrayFrom(std::shared_ptr<Type> base, size_t count)
    {
        CLEAR_VERIFY(base, "invalid base");

        auto& slot = DerivedTypeSlot(base, count);

        if (!slot)
            slot = std::make_shared<ArrayType>(base, count);

        m_Types.try_emplace(slot->GetHash(), slot);
        return slot;
    }

    std::shared_ptr<Type> TypeRegistry::GetConstFrom(std::shared_ptr<Type> base)
    {
        CLEAR_VERIFY(base, "invalid base");

        auto& slot = DerivedTypeSlot(base, s_ConstKind);

        if (!slot)
            slot = std::make_shared<ConstantType>(base);

        m_Types.try_emplace(slot->GetHash(), slot);
        return slot;
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
            return GetType("str");
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

    std::string TypeRegistry::GuessTypeNameFromNumber(const std::string& number)
    {
        NumberInfo info = GetNumberInfoFromLiteral(number);

        if (!info.Valid)
        {
            CLEAR_LOG_ERROR("invalid number ", number);
            return "";
        } 

        // like Python, a float literal is double precision unless it is cast
        if(info.IsFloatingPoint)
            return "float64";
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
