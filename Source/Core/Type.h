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
		BitwiseXor, Index, AccessOp
	};

	enum class UnaryExpressionType : uint8_t
	{
		None = 0, BitwiseNot,	Increment, 
		Decrement, Negation, PostIncrement, 
		PostDecrement, Reference, Dereference, 
		Cast
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
        virtual size_t      GetSize()  const = 0;
        virtual std::string GetHash()  const = 0;
            
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
        PrimitiveType();
        PrimitiveType(llvm::Type* type, TypeFlagSet flags, const std::string& name);
        
        virtual ~PrimitiveType() = default;

        virtual llvm::Type* Get() const override { return m_LLVMType; }
        virtual TypeFlagSet GetFlags() const override { return m_Flags; };
        virtual size_t GetSize() const override { return m_Size; }
        virtual std::string GetHash() const override {return m_Name;}

    private:
        llvm::Type* m_LLVMType;
        TypeFlagSet m_Flags;
        size_t m_Size;
        std::string m_Name = "null_type";
    };

    class PointerType : public Type 
    {
    public:
        PointerType(std::shared_ptr<Type> baseType);

        virtual ~PointerType() = default;

        virtual llvm::Type* Get() const override  { return m_LLVMType; }
        virtual TypeFlagSet GetFlags() const override { return m_Flags; };
        virtual size_t GetSize() const override;
        virtual std::string GetHash() const override { return m_BaseType->GetHash() + "*"; }

        std::shared_ptr<Type> GetBaseType() const { return m_BaseType; }

    private:
        std::shared_ptr<Type> m_BaseType;
        llvm::PointerType* m_LLVMType;
        TypeFlagSet m_Flags;
    };

    //class StructType : public Type 
    //{
    //public:
    //    StructType(const std::string& name, llvm::StructType* llvmType,
    //               const std::vector<std::pair<std::string, std::shared_ptr<Type>>>& members)
    //        : name(name), llvmType(llvmType), members(members) {}
//
    //    llvm::Type* Get() const override { return llvmType; }
//
    //    const auto& GetMembers() const { return members; }
//
    //private:
    //    std::string name;
    //    llvm::StructType* llvmType;
    //    std::vector<std::pair<std::string, std::shared_ptr<Type>>> members;
    //};

}

