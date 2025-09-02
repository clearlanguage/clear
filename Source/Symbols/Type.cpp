#include "Type.h"

#include "API/LLVM/LLVMInclude.h"
#include "Core/Log.h"
#include "SymbolTable.h"
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

    bool Type::IsVariadic()
    {
        return m_Flags.test((size_t)TypeFlags::Variadic);
    }

    bool Type::IsConst()
    {
        return m_Flags.test((size_t)TypeFlags::Constant);
    }

    bool Type::IsTrait()
    {
        return m_Flags.test((size_t)TypeFlags::Trait);
    }

    bool Type::IsEnum()
    {
        return m_Flags.test((size_t)TypeFlags::Enum);
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

        if(baseType && baseType->IsTrait())
            Toggle(TypeFlags::Trait);
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

        if(baseType->IsTrait())
            Toggle(TypeFlags::Trait);
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

    VariadicArgumentsHolder::VariadicArgumentsHolder()
    {
        Toggle(TypeFlags::Variadic);
    }

    ConstantType::ConstantType(std::shared_ptr<Type> base)
        : m_Base(base)
    {
        Toggle(TypeFlags::Constant);
        Toggle(base->GetFlags());
    }

    TraitType::TraitType(const std::vector<std::string>& functions, const std::vector<std::pair<std::string, std::shared_ptr<Type>>>& members, const std::string& name)
        : m_Functions(functions), m_Name(name), m_Members(members)
    {
        Toggle(TypeFlags::Trait);
    }

    bool TraitType::DoesClassImplementTrait(std::shared_ptr<ClassType> classTy)
    {  
        // const auto& members = classTy->GetMemberTypes();
        //
        // for(const auto& [name, type] : m_Members)
        // {
        //     if(!members.contains(name))
        //         return false;
        //
        //     if(type->Get() != members.at(name)->Get())
        //         return false;
        // }
        //
        // std::string className = classTy->GetHash();
        // const auto& classFunctions = classTy->GetFunctions();
        //
        // for(auto& function : m_Functions)
        // {
        //     size_t argBegin = function.find_first_of('$');
        //
        //     std::string name = function.substr(4, argBegin - 4);
        //     std::string rest = function.substr(argBegin + 1);
        //
        //     std::string classFunctionName = std::format("_CLR{}.{}${}P{}", className, name, className, rest);
        //     if(!classFunctions.contains(classFunctionName))
        //         return false;
        // }
        //
        // return true;
	
		CLEAR_UNREACHABLE("TODO");
		return false;
    }

    EnumType::EnumType(std::shared_ptr<Type> integerType, const std::string& name)
        : m_Type(integerType), m_Name(name)
    {
        Toggle(TypeFlags::Enum);
        Toggle(integerType->GetFlags());
    }

    void EnumType::InsertEnumValue(const std::string& name, int64_t value)
    {
        m_EnumValues[name] = value;
    }

    int64_t EnumType::GetEnumValue(const std::string& name)
    {
        CLEAR_VERIFY(m_EnumValues.contains(name), "invalid enum value ", "name");
        return m_EnumValues.at(name);
    }

    GenericType::GenericType(llvm::StringRef name)
        : m_Name(name)
    {
        Toggle(TypeFlags::Generic);
    }
}
 
