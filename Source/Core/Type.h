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
        PostDecrement, PreIncrement, PreDecrement, Not
	};

    enum class TypeFlags
    {
        None = 0, Floating, Integral, 
        Pointer, Signed, Array, Compound, 
        Void, Variadic, Constant, Class, Count
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
        bool IsVariadic();
        bool IsConst();

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

        const auto& GetMemberTypes()   const { return m_MemberTypes; }
        const auto& GetMemberIndices() const {return m_MemberIndices; }

    private:
        llvm::StructType* m_LLVMType;
        std::unordered_map<std::string, std::shared_ptr<Type>> m_MemberTypes;
        std::unordered_map<std::string, size_t> m_MemberIndices;

        std::string m_Name;
    };

    class ClassType : public Type
    {
    public:
        ClassType(std::shared_ptr<StructType> structTy, const std::vector<std::string>& functions);

        virtual llvm::Type* Get() const override { return m_StructType->Get(); }
        virtual std::string GetHash() const override { return m_StructType->GetHash(); };
        virtual std::string GetShortHash() const override { return m_StructType->GetShortHash(); };

        size_t GetMemberIndex(const std::string& member);
        std::shared_ptr<Type> GetMemberType(const std::string& member);
        void SetMember(const std::string& member, std::shared_ptr<Type> type);
        std::shared_ptr<Type> GetMemberAtIndex(uint64_t index);

        const auto& GetMemberTypes()   const { return m_StructType->GetMemberTypes(); }
        const auto& GetMemberIndices() const {return  m_StructType->GetMemberIndices(); }
        const auto& GetFunctions()     const {return  m_Functions; }

    private:
        std::shared_ptr<StructType> m_StructType;
        std::vector<std::string> m_Functions;
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
        virtual std::string GetHash() const override { return "const" + m_Base->GetHash(); };
        virtual std::string GetShortHash() const override { return "c" + m_Base->GetShortHash(); };

    private:
        std::shared_ptr<Type> m_Base;
    };
}

