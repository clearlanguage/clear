#pragma once

#include "Lexing/Tokens.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

#include <bitset>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace clear 
{
    enum class BinaryExpressionType : uint8_t
	{
		None = 0, Add, Sub, Mul, Div, Pow, Mod, Less, LessEq,
		Greater, GreaterEq, Eq, NotEq, PositivePointerArithmetic,
		NegatedPointerArithmetic, Assignment, BitwiseLeftShift,
		BitwiseRightShift, BitwiseNot, BitwiseAnd, BitwiseOr,
		BitwiseXor, Index, And, Or, Not, MemberAccess
	};

	enum class UnaryExpressionType : uint8_t
	{
		None = 0, BitwiseNot, Negation, 
        Dereference, Reference, Cast, PostIncrement, 
        PostDecrement, PreIncrement, PreDecrement, Not, Unpack
	};

    enum class TypeFlags
    {
        None = 0, Floating, Integral, 
        Pointer, Signed, Array, Compound, 
        Void, Variadic, Constant, Class,
        Trait, Enum, Count
    };
    
    using TypeFlagSet = std::bitset<(size_t)TypeFlags::Count>;

    class Type 
    {
    public:
        Type() = default;
        virtual ~Type() = default;

        virtual llvm::Type* Get()      const = 0;
        virtual size_t      GetSize()  const { return 0; };
        virtual std::string GetHash()  const = 0;
        virtual std::string GetShortHash() const = 0;

        bool IsSigned();
        bool IsFloatingPoint();
        bool IsPointer();
        bool IsIntegral();
        bool IsArray();
        bool IsCompound();
        bool IsClass();
        bool IsVariadic();
        bool IsConst();
        bool IsTrait();
        bool IsEnum();

        TypeFlagSet GetFlags() const { return m_Flags; }

    protected:
        void Toggle(TypeFlags flag);
        void Toggle(TypeFlagSet set);


    private:
        TypeFlagSet m_Flags;
    };

    class PrimitiveType : public Type 
    {
    public:
        PrimitiveType(llvm::LLVMContext& context);
        PrimitiveType(llvm::Type* type, TypeFlagSet flags, const std::string& name);
        
        virtual ~PrimitiveType() = default;

        virtual llvm::Type* Get() const override { return m_LLVMType; }
        virtual size_t GetSize() const override { return m_Size; }
        virtual std::string GetHash() const override { return m_Name; }
        virtual std::string GetShortHash() const override { return m_Name[0] + std::to_string(GetSize()); }

    private:
        llvm::Type* m_LLVMType;
        size_t m_Size;
        std::string m_Name = "void";
    };

    class PointerType : public Type 
    {
    public:
        PointerType(std::shared_ptr<Type> baseType, llvm::LLVMContext& context);

        virtual ~PointerType() = default;

        virtual llvm::Type* Get() const override  { return m_LLVMType; }
        virtual std::string GetHash() const override { return m_BaseType->GetHash() + "*"; }
        virtual std::string GetShortHash() const override { return m_BaseType->GetShortHash() + "P"; }

        std::shared_ptr<Type> GetBaseType() const { return m_BaseType; }
        void SetBaseType(std::shared_ptr<Type> type);

    private:
        std::shared_ptr<Type> m_BaseType;
        llvm::PointerType* m_LLVMType;
    };

    class ArrayType : public Type 
    {
    public:
        ArrayType(std::shared_ptr<Type> baseType, size_t count);
        
        virtual ~ArrayType() = default;

        virtual llvm::Type* Get() const override  { return m_LLVMType; }
        virtual std::string GetHash() const override;
        virtual std::string GetShortHash() const override;

        std::shared_ptr<Type> GetBaseType() const { return m_BaseType; }
        void SetBaseType(std::shared_ptr<Type> type);
        size_t GetArraySize() const { return m_Count; }


    private:
        std::shared_ptr<Type> m_BaseType;
        llvm::ArrayType* m_LLVMType;
        size_t m_Count;
    };

    class StructType : public Type 
    {
    public:
        StructType(const std::string& name, llvm::LLVMContext& context);
        StructType(const std::string& name, const std::vector<std::pair<std::string, std::shared_ptr<Type>>>& members);

        void SetBody(const std::vector<std::pair<std::string, std::shared_ptr<Type>>>& members);

        virtual ~StructType() = default;
            
        virtual llvm::Type* Get() const override { return m_LLVMType; }
        virtual std::string GetHash() const override { return m_Name; };
        virtual std::string GetShortHash() const override { return m_Name; };

        size_t GetMemberIndex(const std::string& member);
        std::shared_ptr<Type> GetMemberType(const std::string& member);
        void SetMember(const std::string& member, std::shared_ptr<Type> type);
        std::shared_ptr<Type> GetMemberAtIndex(uint64_t index);

        void AddDefaultValue(const std::string& member, llvm::Value* value); // assumes value is of correct type already
        llvm::Value* GetDefaultValue(const std::string& member);

        const auto& GetMemberTypes()   const { return m_MemberTypes; }
        const auto& GetMemberIndices() const {return m_MemberIndices; }

    private:
        llvm::StructType* m_LLVMType;
        std::unordered_map<std::string, std::shared_ptr<Type>> m_MemberTypes;
        std::unordered_map<std::string, size_t> m_MemberIndices;
        std::unordered_map<std::string, llvm::Value*> m_DefaultValues;

        std::string m_Name;
    };

    class ClassType : public Type
    {
    public:
        ClassType(std::shared_ptr<StructType> structTy);

        virtual llvm::Type* Get() const override { return m_StructType->Get(); }
        virtual std::string GetHash() const override { return m_StructType->GetHash(); };
        virtual std::string GetShortHash() const override { return m_StructType->GetShortHash(); };

        size_t GetMemberIndex(const std::string& member);
        std::shared_ptr<Type> GetMemberType(const std::string& member);
        void SetMember(const std::string& member, std::shared_ptr<Type> type);
        std::shared_ptr<Type> GetMemberAtIndex(uint64_t index);

        void AddDefaultValue(const std::string& member, llvm::Value* value);
        llvm::Value* GetDefaultValue(const std::string& member);

        std::shared_ptr<StructType> GetBaseType() { return m_StructType; }

        std::string ConvertFunctionToClassFunction(const std::string& name);

        void PushFunction(const std::string& name);
        bool HasFunctionMangled(const std::string& name);

        const auto& GetMemberTypes()   const { return m_StructType->GetMemberTypes(); }
        const auto& GetMemberIndices() const {return  m_StructType->GetMemberIndices(); }
        const auto& GetFunctions()     const {return  m_Functions; }

    private:
        std::shared_ptr<StructType> m_StructType;
        std::set<std::string> m_Functions;
    };
    
    class VariadicArgumentsHolder : public Type 
    {
    public:
        VariadicArgumentsHolder();
        virtual ~VariadicArgumentsHolder() = default;

        virtual llvm::Type* Get() const override { return m_LLVMType; }
        virtual std::string GetHash() const override { return "variadic"; };
        virtual std::string GetShortHash() const override { return "va"; };

    private:
        llvm::Type* m_LLVMType = nullptr;
    };

    class ConstantType : public Type
    {
    public:
        ConstantType(std::shared_ptr<Type> base);
        virtual ~ConstantType() = default;

        virtual llvm::Type* Get() const override { return m_Base->Get(); }
        virtual std::string GetHash() const override { return m_Base->GetHash() + "const"; };
        virtual std::string GetShortHash() const override { return m_Base->GetShortHash() + "c"; };

        std::shared_ptr<Type> GetBaseType() { return m_Base; }

    private:
        std::shared_ptr<Type> m_Base;
    };

    class TraitType : public Type
    {
    public:
        TraitType(const std::vector<std::string>& functions,
                  const std::vector<std::pair<std::string, std::shared_ptr<Type>>>& members, 
                  const std::string& name);
        
        virtual ~TraitType() = default;

        virtual llvm::Type* Get() const override { return nullptr; }
        virtual std::string GetHash() const override { return m_Name; };
        virtual std::string GetShortHash() const override { return m_Name; };

        bool DoesClassImplementTrait(std::shared_ptr<ClassType> classTy);

    private:
        std::vector<std::string> m_Functions;
        std::vector<std::pair<std::string, std::shared_ptr<Type>>> m_Members;
        std::string m_Name;
    };

    class EnumType : public Type 
    {
    public:
        EnumType(std::shared_ptr<Type> integerType, const std::string& name);
        virtual ~EnumType() = default;

        void InsertEnumValue(const std::string& name, int64_t value);

        int64_t GetEnumValue(const std::string& name);

        virtual size_t GetSize() const override { return m_Type->GetSize(); }
        virtual llvm::Type* Get() const override { return m_Type->Get(); }
        virtual std::string GetHash() const override { return m_Type->GetHash(); };
        virtual std::string GetShortHash() const override { return m_Type->GetHash(); };
        
    private:
        std::string m_Name;
        std::shared_ptr<Type> m_Type;
        std::unordered_map<std::string, int64_t> m_EnumValues;
    };

    template <typename To, typename From>
    std::shared_ptr<To> dyn_cast(std::shared_ptr<From> val) requires std::is_base_of_v<Type, From>
    {
        if(auto constTy = std::dynamic_pointer_cast<ConstantType>(val))
        {
            if constexpr (std::is_same_v<To, ConstantType>)
                return constTy;

            return std::dynamic_pointer_cast<To>(constTy->GetBaseType());
        }

        return std::dynamic_pointer_cast<To>(val);
    }
}

