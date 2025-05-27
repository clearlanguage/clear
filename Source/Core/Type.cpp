#include "Type.h"

#include "API/LLVM/LLVMBackend.h"
#include "Core/Log.h"

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

    PrimitiveType::PrimitiveType()
    {
        auto& context = *LLVM::Backend::GetContext();

        m_LLVMType = llvm::Type::getVoidTy(context);
        m_Flags.set((size_t)TypeFlags::Void);
        m_Size = 0;
    }

    PrimitiveType::PrimitiveType(llvm::Type* type, TypeFlagSet flags, const std::string& name)
        : m_LLVMType(type), m_Flags(flags), m_Size(type->getScalarSizeInBits()), m_Name(name)
    {
        CLEAR_VERIFY(m_LLVMType, "null type not allowed");
    }

    PointerType::PointerType(std::shared_ptr<Type> baseType)
        : m_BaseType(baseType)
    {
        auto& context = *LLVM::Backend::GetContext();
        m_LLVMType = llvm::PointerType::get(context , 0);
        m_Flags.set((size_t)TypeFlags::Pointer);
    }

    size_t PointerType::GetSize() const
    {
        auto& module = *LLVM::Backend::GetModule();
        const llvm::DataLayout& layout = module.getDataLayout();

        return (size_t)layout.getPointerSizeInBits(m_LLVMType->getPointerAddressSpace());
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
}
 