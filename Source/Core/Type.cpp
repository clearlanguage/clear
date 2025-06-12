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

    bool Type::IsConstant()
    {
        return GetFlags().test((size_t)TypeFlags::Constant);
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

    void ArrayType::SetBaseType(std::shared_ptr<Type> type)
    {
        m_BaseType = type;
    }

    StructType::StructType(const std::string& name, llvm::LLVMContext& context)
        : m_Name(name), m_LLVMType(llvm::StructType::create(context, name))
    {
        m_Flags.set((size_t)TypeFlags::Compound);
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

    void StructType::SetBody(const std::vector<std::pair<std::string, std::shared_ptr<Type>>>& members)
    {
        std::vector<llvm::Type*> types;

		uint32_t k = 0;

		for (auto& [memberName, type] : members)
		{
			m_MemberIndices[memberName] = k++;
            m_MemberTypes[memberName] = type;
			types.push_back(type->Get());
		}
        
        m_LLVMType->setBody(types);
    }   

    size_t StructType::GetMemberIndex(const std::string& member)
    {
        return m_MemberIndices.at(member);
    }

    std::shared_ptr<Type> StructType::GetMemberType(const std::string& member)
    {
        return m_MemberTypes.at(member);
    }

    void StructType::SetMember(const std::string& member, std::shared_ptr<Type> type)
    {
        CLEAR_VERIFY(m_MemberTypes.contains(member), "invalid member");
        CLEAR_VERIFY(m_MemberTypes[member]->IsPointer() && type->IsPointer(), "cannot redefine a member that is not a pointer");

        m_MemberTypes[member] = type;
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

    VariadicArgumentsHolder::VariadicArgumentsHolder()
    {
        m_Flags.set((size_t)TypeFlags::Variadic);
    }
}
 