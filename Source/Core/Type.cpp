#include "Type.h"

#include "API/LLVM/LLVMInclude.h"
#include "Core/Log.h"
#include "AST/SymbolTable.h"

#include <functional>

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

        m_Size = 0;
    }

    PrimitiveType::PrimitiveType(llvm::Type* type, TypeFlagSet flags, const std::string& name)
        : m_LLVMType(type), m_Size(type->getScalarSizeInBits()), m_Name(name)
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
        Toggle(TypeFlags::Compound);
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
        Toggle(TypeFlags::Compound);
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
        CLEAR_VERIFY(m_MemberIndices.contains(member), member, " is not a member of ", m_Name);
        return m_MemberIndices.at(member);
    }

    std::shared_ptr<Type> StructType::GetMemberType(const std::string& member)
    {
        CLEAR_VERIFY(m_MemberTypes.contains(member), member, " is not a member of ", m_Name);
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

    std::string ClassType::ConvertFunctionToClassFunction(const std::string& name)
    {
        //_CLRfunction_name$args%rt -> _CLRClassName.function_name$ClassNamePargs%rt
    
        CLEAR_VERIFY(name.starts_with("_CLR"), "Not a valid mangled name");
    
        size_t startArgs = name.find_first_of('$');
        size_t endArgs   = name.find_last_of('%');
    
        CLEAR_VERIFY(startArgs != std::string::npos && endArgs != std::string::npos && startArgs < endArgs,
                     "Invalid mangled name structure");
        
        std::string functionName = name.substr(4, startArgs - 4);
        std::string args         = name.substr(startArgs + 1, endArgs - startArgs - 1);
        std::string returnType   = name.substr(endArgs);
        
        std::string classPrefix    = std::format("_CLR{}.{}", m_StructType->GetHash(), functionName);
        std::string classArgsAndRt = std::format("${}P{}{}", m_StructType->GetHash(), args, returnType);
        
        return classPrefix + classArgsAndRt;
    }

    void ClassType::PushFunction(const std::string &name)
    {
        m_Functions.insert(name);
    }

    bool ClassType::HasFunctionMangled(const std::string& name)
    {
        CLEAR_VERIFY(name.starts_with("_CLR"), "not a valid mangled name");

        //_CLRfunction_name$args%rt
        std::string classPrefix = std::format("_CLR{}.", m_StructType->GetHash());
        std::string classFunction = name;
        classFunction.replace(0, 4, classPrefix);

        return m_Functions.contains(classFunction);
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

    ClassType::ClassType(std::shared_ptr<StructType> structTy)
        : m_StructType(structTy)
    {
        Toggle(TypeFlags::Compound);
        Toggle(TypeFlags::Class);
    }

    size_t ClassType::GetMemberIndex(const std::string& member)
    {
        return m_StructType->GetMemberIndex(member);
    }

    std::shared_ptr<Type> ClassType::GetMemberType(const std::string& member)
    {
        return m_StructType->GetMemberType(member);
    }

    void ClassType::SetMember(const std::string& member, std::shared_ptr<Type> type)
    {
        m_StructType->SetMember(member, type);
    }


    TraitType::TraitType(const std::vector<std::string>& functions, const std::vector<std::pair<std::string, std::shared_ptr<Type>>>& members, const std::string& name)
        : m_Functions(functions), m_Name(name), m_Members(members)
    {
        Toggle(TypeFlags::Trait);
    }

    bool TraitType::DoesClassImplementTrait(std::shared_ptr<ClassType> classTy)
    {  
        const auto& members = classTy->GetMemberTypes();

        for(const auto& [name, type] : m_Members)
        {
            if(!members.contains(name))
                return false;

            if(type->Get() != members.at(name)->Get())
                return false;
        }

        std::string className = classTy->GetHash();
        const auto& classFunctions = classTy->GetFunctions();
        
        for(auto& function : m_Functions)
        {
            size_t argBegin = function.find_first_of('$');

            std::string name = function.substr(4, argBegin - 4);
            std::string rest = function.substr(argBegin+1);

            std::string classFunctionName = std::format("_CLR{}.{}${}P{}", className, name, className, rest);

            if(!classFunctions.contains(classFunctionName))
                return false;
        }

        return true;
    }
}
 