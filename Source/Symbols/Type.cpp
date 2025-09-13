#include "Type.h"

#include "API/LLVM/LLVMInclude.h"
#include "Core/Log.h"
#include "Symbols/Symbol.h"

#include <functional>
#include <llvm/CodeGen/MachineOperand.h>
#include <llvm/IR/LLVMContext.h>
#include <memory>

namespace clear
{
    bool Type::IsSigned()
    {
        return m_Flags.test((size_t)TypeFlags::Signed);
    }
    bool Type::IsFloatingPoint()
    {
        return m_Flags.test((size_t)TypeFlags::Floating);
    }
    bool Type::IsPointer()
    {
        return m_Flags.test((size_t)TypeFlags::Pointer);
    }
    bool Type::IsIntegral()
    {
        return m_Flags.test((size_t)TypeFlags::Integral);
    }
    bool Type::IsArray()
    {
        return m_Flags.test((size_t)TypeFlags::Array);
    }
    bool Type::IsCompound()
    {
        return m_Flags.test((size_t)TypeFlags::Compound);
    }

    bool Type::IsClass()
    {
        return m_Flags.test((size_t)TypeFlags::Class);
    }

    bool Type::IsConst()
    {
        return m_Flags.test((size_t)TypeFlags::Constant);
    }

    bool Type::IsGeneric()
    {
        return m_Flags.test((size_t)TypeFlags::Generic);
    }

    void Type::Toggle(TypeFlags flag)
    {
        m_Flags.flip((size_t)flag);
    }

    void Type::Toggle(TypeFlagSet set)
    {
        m_Flags ^= set;
    }

    PrimitiveType::PrimitiveType(llvm::LLVMContext& context)
    {
        m_LLVMType = llvm::Type::getVoidTy(context);
        Toggle(TypeFlags::Void);
    }

    PrimitiveType::PrimitiveType(llvm::Type* type, TypeFlagSet flags, const std::string& name)
        : m_LLVMType(type), m_Name(name)
    {
        CLEAR_VERIFY(m_LLVMType, "null type not allowed");
        Toggle(flags);
    }

    PointerType::PointerType(std::shared_ptr<Type> baseType, llvm::LLVMContext& context)
        : m_BaseType(baseType)
    {
        m_LLVMType = llvm::PointerType::get(context , 0);
        Toggle(TypeFlags::Pointer);
    }

    void PointerType::SetBaseType(std::shared_ptr<Type> type)
    {
        m_BaseType = type;
    }

    ArrayType::ArrayType(std::shared_ptr<Type> baseType, size_t count)
        : m_LLVMType(llvm::ArrayType::get(baseType->Get(), count)), m_BaseType(baseType), 
          m_Count(count)
    {
        Toggle(TypeFlags::Array);
    }

    std::string ArrayType::GetHash() const
    {
        return m_BaseType->GetHash() + "[" + std::to_string(m_Count) + "]";
    }

    void ArrayType::SetBaseType(std::shared_ptr<Type> type)
    {
        m_BaseType = type;
    }

	ClassType::ClassType(llvm::StringRef name, llvm::LLVMContext& context)
		: m_Name(name), m_LLVMType(llvm::StructType::create(context, name))
	{
		Toggle(TypeFlags::Compound);
		Toggle(TypeFlags::Class);
	}

	void ClassType::SetBody(llvm::ArrayRef<std::pair<std::string, std::shared_ptr<Symbol>>> members)
	{
		llvm::SmallVector<llvm::Type*> types;
		
		for (const auto& [memberName, member] : members)
		{
			if (member->Kind == SymbolKind::Type)
			{
				types.push_back(member->GetType()->Get());
				m_MemberValues[memberName] = member->GetType();
			}
			else 
			{
				m_MemberFunctions[memberName] = member;
			}
		}

		m_LLVMType->setBody(types);
	}

	std::optional<std::shared_ptr<Symbol>> ClassType::GetMember(llvm::StringRef name)
	{
		std::string strName = std::string(name);
		
		{
			auto it = m_MemberFunctions.find(strName);

			if (it != m_MemberFunctions.end())
				return it->second;
		}

		{
			auto it = m_MemberValues.find(strName);

			if (it != m_MemberValues.end())
			{
				std::shared_ptr<Symbol> symbol = std::make_shared<Symbol>(Symbol::CreateType(it->second));
				return symbol;
			}
		}

		return std::nullopt;
	}

	std::optional<std::shared_ptr<Symbol>> ClassType::GetMemberValueByIndex(size_t index)
	{
		if (index >= m_MemberValues.size())
			return std::nullopt;
		
		auto it = m_MemberValues.begin() + index;
		return std::make_shared<Symbol>(Symbol::CreateType(it->second));
	}

	std::optional<size_t> ClassType::GetMemberValueIndex(llvm::StringRef name)
	{
		auto it = m_MemberValues.find(std::string(name));
		if (it == m_MemberValues.end())
			return std::nullopt;

		return std::distance(m_MemberValues.begin(), it);
	}

    ConstantType::ConstantType(std::shared_ptr<Type> base)
        : m_Base(base)
    {
        Toggle(TypeFlags::Constant);
        Toggle(base->GetFlags());
    }

    GenericType::GenericType(llvm::StringRef name)
        : m_Name(name)
    {
        Toggle(TypeFlags::Generic);
    }
}
 
