#include "Type.h"

#include "API/LLVM/LLVMInclude.h"
#include "Core/Log.h"

#include <functional>

namespace clear
{
    bool Type::IsSigned()
    {
        return GetFlags().test((size_t)TypeFlags::Signed);
    }
    bool Type::IsFloatingPoint()
    {
        return GetFlags().test((size_t)TypeFlags::Floating);
    }
    bool Type::IsPointer()
    {
        return GetFlags().test((size_t)TypeFlags::Pointer);
    }
    bool Type::IsIntegral()
    {
        return GetFlags().test((size_t)TypeFlags::Integral);
    }
    bool Type::IsArray()
    {
        return GetFlags().test((size_t)TypeFlags::Array);
    }
    bool Type::IsCompound()
    {
        return GetFlags().test((size_t)TypeFlags::Compound);
    }

    bool Type::IsVariadic()
    {
        return GetFlags().test((size_t)TypeFlags::Variadic);
    }

    bool Type::ContainsAll(TypeFlagSet set)
    {
        return (GetFlags() & set) == set;
    }

    bool Type::ContainsAny(TypeFlagSet set)
    {
        return (GetFlags() & set).any();
    }

    TypeFlagSet Type::MaskWith(TypeFlagSet mask)
    {
        return GetFlags() & mask;
    }

    PrimitiveType::PrimitiveType(llvm::LLVMContext& context)
    {
        m_LLVMType = llvm::Type::getVoidTy(context);
        m_Flags.set((size_t)TypeFlags::Void);
        m_Size = 0;
    }

    PrimitiveType::PrimitiveType(llvm::Type* type, TypeFlagSet flags, const std::string& name)
        : m_LLVMType(type), m_Flags(flags), m_Size(type->getScalarSizeInBits()), m_Name(name)
    {
        CLEAR_VERIFY(m_LLVMType, "null type not allowed");
    }

    size_t PrimitiveType::GetID() const
    {
        return std::hash<std::string>()(m_Name);
    }

    PointerType::PointerType(std::shared_ptr<Type> baseType, llvm::LLVMContext& context)
        : m_BaseType(baseType)
    {
        m_LLVMType = llvm::PointerType::get(context , 0);
        m_Flags.set((size_t)TypeFlags::Pointer);
    }

    void PointerType::SetBaseType(std::shared_ptr<Type> type)
    {
        m_BaseType = type;
    }

    size_t PointerType::GetID() const 
    {
        return std::hash<std::string>()(GetHash());
    }

    ArrayType::ArrayType(std::shared_ptr<Type> baseType, size_t count)
        : m_LLVMType(llvm::ArrayType::get(baseType->Get(), count)), m_BaseType(baseType), 
          m_Count(count)
    {
        m_Flags.set((size_t)TypeFlags::Array);
    }

    std::string ArrayType::GetHash() const
    {
        return m_BaseType->GetHash() + "[" + std::to_string(m_Count) + "]";
    }

    std::string ArrayType::GetShortHash() const
    {
        return m_BaseType->GetShortHash() + "A" + std::to_string(m_Count);
    }

    size_t ArrayType::GetID() const 
    {
        return std::hash<std::string>()(GetHash());
    }

    void ArrayType::SetBaseType(std::shared_ptr<Type> type)
    {
        m_BaseType = type;
    }

    StructType::StructType(const std::string& name, const std::vector<std::pair<std::string, std::shared_ptr<Type>>>& members)
        : m_Name(name)
    {
        std::vector<llvm::Type*> types;

		uint32_t k = 0;

		for (auto& [memberName, type] : members)
		{
			m_MemberIndices[memberName] = k++;
            m_MemberTypes[memberName] = type;
			types.push_back(type->Get());
		}

		m_LLVMType = llvm::StructType::create(types, name);

        m_Flags.set((size_t)TypeFlags::Compound);
    }

    size_t StructType::GetMemberIndex(const std::string& member)
    {
        return m_MemberIndices.at(member);
    }

    std::shared_ptr<Type> StructType::GetMemberType(const std::string& member)
    {
        return m_MemberTypes.at(member);
    }

    std::shared_ptr<Type> StructType::GetMemberAtIndex(uint64_t index)
    {
        for(const auto& [member, mIndex] : m_MemberIndices)
        {
            if(index == mIndex) return m_MemberTypes[member];
        }

        CLEAR_UNREACHABLE("unable to find index", index);
        return nullptr;
    }

    size_t StructType::GetID() const
    {
        if (m_CachedID.has_value())
            return m_CachedID.value();

       size_t hash = std::hash<std::string>()(GetHash());

       for (const auto& [name, type] : m_MemberTypes)
       {
           hash ^= type->GetID() + 0x9e3779b9 + (hash << 6) + (hash >> 2);
       }

       m_CachedID = hash;
       return m_CachedID.value();
    }

    VariadicArgumentsHolder::VariadicArgumentsHolder()
    {
        m_Flags.set((size_t)TypeFlags::Variadic);
    }
}
 