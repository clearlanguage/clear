#pragma once

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
		BitwiseXor, Index
	};

	enum class UnaryExpressionType : uint8_t
	{
		None = 0, BitwiseNot, Negation, 
        Dereference, Reference, Cast, PostIncrement, 
        PostDecrement, PreIncrement, PreDecrement
	};

    enum class TypeFlags
    {
        None = 0, Floating, Integral, 
        Pointer, Signed, Array, Compound, 
        Void, Count
    };
    
    using TypeFlagSet = std::bitset<(size_t)TypeFlags::Count>;

    class Type 
    {
    public:
        Type() = default;
        virtual ~Type() = default;

        virtual llvm::Type* Get()      const = 0;
        virtual TypeFlagSet GetFlags() const = 0;
        virtual size_t      GetSize()  const { return 0; };
        virtual std::string GetHash()  const = 0;
        virtual std::string GetShortHash() const = 0;
            
        bool IsSigned();
        bool IsFloatingPoint();
        bool IsPointer();
        bool IsIntegral();
        bool IsArray();
        bool IsCompound();

        bool ContainsAll(TypeFlagSet set);
        bool ContainsAny(TypeFlagSet set);

        TypeFlagSet MaskWith(TypeFlagSet mask);

    };

    class PrimitiveType : public Type 
    {
    public:
        PrimitiveType(llvm::LLVMContext& context);
        PrimitiveType(llvm::Type* type, TypeFlagSet flags, const std::string& name);
        
        virtual ~PrimitiveType() = default;

        virtual llvm::Type* Get() const override { return m_LLVMType; }
        virtual TypeFlagSet GetFlags() const override { return m_Flags; };
        virtual size_t GetSize() const override { return m_Size; }
        virtual std::string GetHash() const override { return m_Name; }
        virtual std::string GetShortHash() const override { return m_Name[0] + std::to_string(GetSize()); }

    private:
        llvm::Type* m_LLVMType;
        TypeFlagSet m_Flags;
        size_t m_Size;
        std::string m_Name = "void";
    };

    class PointerType : public Type 
    {
    public:
        PointerType(std::shared_ptr<Type> baseType, llvm::LLVMContext& context);

        virtual ~PointerType() = default;

        virtual llvm::Type* Get() const override  { return m_LLVMType; }
        virtual TypeFlagSet GetFlags() const override { return m_Flags; };
        virtual std::string GetHash() const override { return m_BaseType->GetHash() + "*"; }
        virtual std::string GetShortHash() const override { return m_BaseType->GetShortHash() + "*"; }


        std::shared_ptr<Type> GetBaseType() const { return m_BaseType; }
        void SetBaseType(std::shared_ptr<Type> type);

    private:
        std::shared_ptr<Type> m_BaseType;
        llvm::PointerType* m_LLVMType;
        TypeFlagSet m_Flags;
    };

    class ArrayType : public Type 
    {
    public:
        ArrayType(std::shared_ptr<Type> baseType, size_t count);
        
        virtual ~ArrayType() = default;

        virtual llvm::Type* Get() const override  { return m_LLVMType; }
        virtual TypeFlagSet GetFlags() const override { return m_Flags; };
        virtual std::string GetHash() const override;
        virtual std::string GetShortHash() const override;

        std::shared_ptr<Type> GetBaseType() const { return m_BaseType; }
        void SetBaseType(std::shared_ptr<Type> type);
        size_t GetArraySize() const { return m_Count; }


    private:
        std::shared_ptr<Type> m_BaseType;
        llvm::ArrayType* m_LLVMType;
        TypeFlagSet m_Flags;
        size_t m_Count;
    };

    class StructType : public Type 
    {
    public:
        StructType(const std::string& name, const std::vector<std::pair<std::string, std::shared_ptr<Type>>>& members);

        virtual ~StructType() = default;
            
        virtual llvm::Type* Get() const override { return m_LLVMType; }
        virtual TypeFlagSet GetFlags() const override { return m_Flags; };
        virtual std::string GetHash() const override { return m_Name; };
        virtual std::string GetShortHash() const override { return m_Name; };

        size_t GetMemberIndex(const std::string& member);
        std::shared_ptr<Type> GetMemberType(const std::string& member);

        const auto& GetMemberTypes()   const { return m_MemberTypes; }
        const auto& GetMemberIndices() const {return m_MemberIndices; }

    private:
        llvm::StructType* m_LLVMType;
        TypeFlagSet m_Flags;
        std::unordered_map<std::string, std::shared_ptr<Type>> m_MemberTypes;
        std::unordered_map<std::string, size_t> m_MemberIndices;

        std::string m_Name;
    };

}

