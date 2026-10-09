#include "Type.h"

#include "API/LLVM/LLVMInclude.h"
#include "Core/Log.h"
#include "Symbols/Symbol.h"

#include <functional>
#include <llvm/CodeGen/MachineOperand.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DataLayout.h>
#include <memory>
#include <format>

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

    bool Type::IsEnum()
    {
        return m_Flags.test((size_t)TypeFlags::Enum);
    }

    bool Type::IsTuple()
    {
        return m_Flags.test((size_t)TypeFlags::Tuple);
    }

    bool Type::IsFunction()
    {
        return m_Flags.test((size_t)TypeFlags::Function);
    }

    FunctionPointerType::FunctionPointerType(llvm::ArrayRef<std::shared_ptr<Type>> parameters, std::shared_ptr<Type> returnType, llvm::LLVMContext& context)
        : m_Parameters(parameters.begin(), parameters.end()), m_ReturnType(returnType)
    {
        llvm::SmallVector<llvm::Type*> types;

        for (auto& parameter : m_Parameters)
            types.push_back(parameter->Get());

        m_FunctionType = llvm::FunctionType::get(returnType ? returnType->Get() : llvm::Type::getVoidTy(context), types, false);
        m_LLVMType = llvm::PointerType::get(context, 0);

        Toggle(TypeFlags::Function);
    }

    std::string FunctionPointerType::GetHash() const
    {
        std::string hash = "function(";

        for (size_t i = 0; i < m_Parameters.size(); i++)
            hash += (i ? ", " : "") + m_Parameters[i]->GetHash();

        hash += ")";

        if (m_ReturnType)
            hash += " -> " + m_ReturnType->GetHash();

        return hash;
    }

    TupleType::TupleType(llvm::ArrayRef<std::shared_ptr<Type>> elements, llvm::LLVMContext& context)
        : m_Elements(elements.begin(), elements.end())
    {
        llvm::SmallVector<llvm::Type*> types;

        for (auto& element : m_Elements)
            types.push_back(element->Get());

        m_LLVMType = llvm::StructType::get(context, types);

        Toggle(TypeFlags::Compound);
        Toggle(TypeFlags::Tuple);
    }

    std::string TupleType::GetHash() const
    {
        std::string hash = "(";

        for (size_t i = 0; i < m_Elements.size(); i++)
            hash += (i ? ", " : "") + m_Elements[i]->GetHash();

        return hash + ")";
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
				MemberFunctions[memberName] = member;
			}
		}

		m_LLVMType->setBody(types);
	}

	static uint64_t StorageSizeOf(llvm::Type* type)
	{
		// semantic analysis runs before the target is known, the generic 64 bit layout gives the same sizes
		static llvm::DataLayout layout("e-m:e-i64:64-f80:128-n8:16:32:64-S128");
		return type->isSized() ? layout.getTypeAllocSize(type).getFixedValue() : 0;
	}

	bool ClassType::DerivesFrom(const std::shared_ptr<ClassType>& other) const
	{
		for (auto base = Base; base; base = base->Base)
		{
			if (base == other)
				return true;
		}

		return false;
	}

	bool ClassType::Satisfies(const std::shared_ptr<ClassType>& trait) const
	{
		for (auto& own : Traits)
		{
			if (own == trait)
				return true;
		}

		return Base && Base->Satisfies(trait);
	}

	std::optional<size_t> ClassType::FindCase(llvm::StringRef name) const
	{
		for (size_t i = 0; i < Cases.size(); i++)
		{
			if (Cases[i].Name == name)
				return i;
		}

		return std::nullopt;
	}

	void ClassType::SetVariantBody(llvm::ArrayRef<VariantCase> cases, llvm::ArrayRef<std::pair<std::string, std::shared_ptr<Symbol>>> methods)
	{
		IsVariant = true;
		Cases.assign(cases.begin(), cases.end());

		uint64_t largest = 0;
		llvm::LLVMContext& context = m_LLVMType->getContext();

		for (auto& variantCase : Cases)
		{
			llvm::SmallVector<llvm::Type*> fields;

			for (auto& [name, type] : variantCase.Fields)
				fields.push_back(type->Get());

			variantCase.Payload = llvm::StructType::get(context, fields);
			largest = std::max(largest, StorageSizeOf(variantCase.Payload));
		}

		auto int32 = llvm::Type::getInt32Ty(context);
		auto storage = llvm::ArrayType::get(llvm::Type::getInt64Ty(context), (largest + 7) / 8);
		m_LLVMType->setBody({ int32, storage });

		for (const auto& [name, method] : methods)
			MemberFunctions[name] = method;
	}

	void ClassType::SetUnionBody(llvm::ArrayRef<std::pair<std::string, std::shared_ptr<Symbol>>> members)
	{
		IsUnion = true;
		uint64_t largest = 0;

		for (const auto& [memberName, member] : members)
		{
			if (member->Kind == SymbolKind::Type)
			{
				m_MemberValues[memberName] = member->GetType();
				largest = std::max(largest, StorageSizeOf(member->GetType()->Get()));
			}
			else
			{
				MemberFunctions[memberName] = member;
			}
		}

		llvm::LLVMContext& context = m_LLVMType->getContext();
		m_LLVMType->setBody({ llvm::ArrayType::get(llvm::Type::getInt64Ty(context), (largest + 7) / 8) });
	}

	std::optional<std::shared_ptr<Symbol>> ClassType::GetMember(llvm::StringRef name)
	{
		std::string strName = std::string(name);
		
		{
			auto it = MemberFunctions.find(strName);

			if (it != MemberFunctions.end())
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

    EnumType::EnumType(llvm::StringRef name, std::shared_ptr<Type> underlying)
        : m_Name(name), m_Underlying(underlying)
    {
        Toggle(underlying->GetFlags());
        Toggle(TypeFlags::Enum);
    }

    bool EnumType::AddValue(const std::string& name, int64_t value)
    {
        return m_Values.insert({ name, value }).second;
    }

    std::optional<int64_t> EnumType::GetValue(llvm::StringRef name) const
    {
        auto it = m_Values.find(name.str());

        if (it == m_Values.end())
            return std::nullopt;

        return it->second;
    }

    bool IsOwning(const std::shared_ptr<Type>& type)
    {
        if (!type)
            return false;

        if (auto array = std::dynamic_pointer_cast<ArrayType>(type))
            return IsOwning(array->GetBaseType());

        // a generator or task owns its frame
        if (std::dynamic_pointer_cast<CoroutineType>(type))
            return true;

        auto classType = std::dynamic_pointer_cast<ClassType>(type);

        if (!classType || classType->IsUnion || classType->IsTrait)
            return false;

        if (classType->IsVariant)
        {
            for (auto& variantCase : classType->Cases)
                for (auto& [name, fieldType] : variantCase.Fields)
                    if (IsOwning(fieldType))
                        return true;

            return false;
        }

        if (classType->MemberFunctions.contains("__destruct__"))
            return true;

        for (const auto& [name, fieldType] : classType->GetMemberValues())
            if (IsOwning(fieldType))
                return true;

        return false;
    }

    bool IsCopyable(const std::shared_ptr<Type>& type)
    {
        if (!IsOwning(type))
            return true;

        if (auto array = std::dynamic_pointer_cast<ArrayType>(type))
            return IsCopyable(array->GetBaseType());

        // a generator or task is one running computation: it can be moved, not duplicated
        if (std::dynamic_pointer_cast<CoroutineType>(type))
            return false;

        auto classType = std::dynamic_pointer_cast<ClassType>(type);

        if (classType->IsVariant)
        {
            for (auto& variantCase : classType->Cases)
                for (auto& [name, fieldType] : variantCase.Fields)
                    if (!IsCopyable(fieldType))
                        return false;

            return true;
        }

        // operator copy (a generic container copies its items too: List[Connection] is not copyable)
        if (classType->MemberFunctions.contains("__copy__"))
        {
            for (auto& argument : classType->GenericArguments)
                if (argument && !IsCopyable(argument))
                    return false;

            return true;
        }

        // it cleans up something itself (a file, a connection): only it knows how to copy that
        if (classType->MemberFunctions.contains("__destruct__"))
            return false;

        for (const auto& [name, fieldType] : classType->GetMemberValues())
            if (!IsCopyable(fieldType))
                return false;

        return true;
    }

    std::string CoroutineType::GetHash() const
    {
        return std::format("{}[{}]", m_Kind == Kind::Generator ? "Generator" : "Task", m_Value ? m_Value->GetHash() : "none");
    }

    std::string GetDisplayName(const std::shared_ptr<Type>& type)
    {
        if (!type)
            return "void";

        if (auto coroutine = std::dynamic_pointer_cast<CoroutineType>(type))
            return std::format("{}[{}]", coroutine->GetKind() == CoroutineType::Kind::Generator ? "Generator" : "Task", 
                               coroutine->GetValueType() ? GetDisplayName(coroutine->GetValueType()) : "none");

        if (type->GetHash() == "str")
            return "str";

        if (auto pointer = std::dynamic_pointer_cast<PointerType>(type))
            return pointer->GetBaseType() ? "*" + GetDisplayName(pointer->GetBaseType()) : "null";

        if (auto array = std::dynamic_pointer_cast<ArrayType>(type))
            return std::format("[{}; {}]", array->GetArraySize(), GetDisplayName(array->GetBaseType()));

        if (auto function = std::dynamic_pointer_cast<FunctionPointerType>(type))
        {
            std::string name = "function(";
            for (size_t i = 0; i < function->GetParameters().size(); i++)
                name += (i ? ", " : "") + GetDisplayName(function->GetParameters()[i]);
            name += ")";
            return function->GetReturnType() ? name + " -> " + GetDisplayName(function->GetReturnType()) : name;
        }

        if (auto tuple = std::dynamic_pointer_cast<TupleType>(type))
        {
            std::string name = "(";
            for (size_t i = 0; i < tuple->GetElements().size(); i++)
                name += (i ? ", " : "") + GetDisplayName(tuple->GetElements()[i]);
            return name + ")";
        }

        return type->GetHash();
    }

    GenericType::GenericType(llvm::StringRef name)
        : m_Name(name)
    {
        Toggle(TypeFlags::Generic);
    }
}
 
