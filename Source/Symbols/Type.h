#pragma once

#include "Core/Log.h"
#include "Lexing/Token.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/DerivedTypes.h"

#include <bitset>
#include <llvm/ADT/MapVector.h>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace llvm
{
	template <>
	struct DenseMapInfo<std::string>
	{
		static inline std::string getEmptyKey()
		{
			return std::string("\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01", 16);
		}

		static inline std::string getTombstoneKey()
		{
			return std::string("\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02", 16);
		}

		static unsigned getHashValue(const std::string &Val)
		{
			return static_cast<unsigned>(std::hash<std::string>{}(Val));
		}

		static bool isEqual(const std::string &LHS, const std::string &RHS)
		{
			return LHS == RHS;
		}
	};

}

namespace clear 
{
    enum class TypeFlags
    {
        None = 0, Floating, Integral, 
        Pointer, Signed, Array, Compound, 
        Void, Variadic, Constant, Class,
        Generic, Enum, Tuple, Count
    };
    
    using TypeFlagSet = std::bitset<(size_t)TypeFlags::Count>;

    class Type;
    class ConstantType;
    class ClassType;

    template <typename To, typename From>
    std::shared_ptr<To> dyn_cast(std::shared_ptr<From> val) requires std::is_base_of_v<Type, From>
    {
        if(auto constTy = std::dynamic_pointer_cast<ConstantType>(val))
        {
            if constexpr (std::is_same_v<To, ConstantType>)
                return constTy;

            return dyn_cast<To>(constTy->GetBaseType());
        }

        return std::dynamic_pointer_cast<To>(val);
    }

    class Type : public std::enable_shared_from_this<Type>
    {
    public:
        Type() = default;
        virtual ~Type() = default;

        virtual llvm::Type* Get()      const = 0;
        virtual std::string GetHash()  const = 0;

        size_t GetSizeInBytes(llvm::Module& module_) const { return module_.getDataLayout().getTypeAllocSize(Get()); };

        bool IsSigned();
        bool IsFloatingPoint();
        bool IsPointer();
        bool IsIntegral();
        bool IsArray();
        bool IsCompound();
        bool IsClass();
        bool IsConst();
        bool IsGeneric();
        bool IsEnum();
        bool IsTuple();

        TypeFlagSet GetFlags() const { return m_Flags; }

        template<typename T>
        std::shared_ptr<T> As() 
        {
            auto ty = dyn_cast<T>(shared_from_this());
            CLEAR_VERIFY(ty, "failed to cast");

            return ty;
        }

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
        virtual std::string GetHash() const override { return m_Name; }

    private:
        llvm::Type* m_LLVMType;
        std::string m_Name = "void";
    };

    class PointerType : public Type 
    {
    public:
        PointerType(std::shared_ptr<Type> baseType, llvm::LLVMContext& context);

        virtual ~PointerType() = default;

        virtual llvm::Type* Get() const override  { return m_LLVMType; }
        virtual std::string GetHash() const override { return m_BaseType ? m_BaseType->GetHash() + "*" : "null"; }
       
        std::shared_ptr<Type> GetBaseType() const { return m_BaseType; }
        void SetBaseType(std::shared_ptr<Type> type);

    private:
        std::shared_ptr<Type> m_BaseType;
        llvm::PointerType* m_LLVMType;
    };

    // the type of string literals: a pointer to zero-terminated bytes whose ==, < and friends compare
    // contents rather than addresses; it converts freely to and from *int8 for C functions
    class StrType : public PointerType
    {
    public:
        using PointerType::PointerType;
        virtual std::string GetHash() const override { return "str"; }
    };

    class ArrayType : public Type 
    {
    public:
        ArrayType(std::shared_ptr<Type> baseType, size_t count);
        virtual ~ArrayType() = default;

        virtual llvm::Type* Get() const override  { return m_LLVMType; }
        virtual std::string GetHash() const override;

        std::shared_ptr<Type> GetBaseType() const { return m_BaseType; }
        void SetBaseType(std::shared_ptr<Type> type);
        size_t GetArraySize() const { return m_Count; }


    private:
        std::shared_ptr<Type> m_BaseType;
        llvm::ArrayType* m_LLVMType;
        size_t m_Count;
    };
	
    struct Symbol;
    class ASTNodeBase;
	
    class ClassType : public Type
    {
    public:
        ClassType(llvm::StringRef name, llvm::LLVMContext& context);
		void SetBody(llvm::ArrayRef<std::pair<std::string, std::shared_ptr<Symbol>>> members);

        virtual llvm::Type* Get() const override { return m_LLVMType; }
        virtual std::string GetHash() const override { return m_Name; };
	
		std::optional<std::shared_ptr<Symbol>> GetMember(llvm::StringRef name);
		std::optional<std::shared_ptr<Symbol>> GetMemberValueByIndex(size_t index);
		std::optional<size_t> GetMemberValueIndex(llvm::StringRef name);
		const auto& GetMemberValues() const { return m_MemberValues; }
		llvm::DenseMap<std::string,  std::shared_ptr<Symbol>> MemberFunctions;
		std::vector<std::shared_ptr<ASTNodeBase>> MemberDefaults; // per field, null when the field has no default

		// for instances of generic classes: List[int32] remembers "List" and [int32]
		std::string GenericOrigin;
		std::vector<std::shared_ptr<Type>> GenericArguments;

    private:
		llvm::StructType* m_LLVMType = nullptr;
		llvm::MapVector<std::string, std::shared_ptr<Type>> m_MemberValues;
		std::string m_Name;
    };
    
    class ConstantType : public Type
    {
    public:
        ConstantType(std::shared_ptr<Type> base);
        virtual ~ConstantType() = default;

        virtual llvm::Type* Get() const override { return m_Base->Get(); }
        virtual std::string GetHash() const override { return m_Base->GetHash() + "const"; };

        std::shared_ptr<Type> GetBaseType() { return m_Base; }

    private:
        std::shared_ptr<Type> m_Base;
    };

    // a named set of integer constants: `enum Color: Red, Green, Blue`
    class EnumType : public Type
    {
    public:
        EnumType(llvm::StringRef name, std::shared_ptr<Type> underlying);
        virtual ~EnumType() = default;

        virtual llvm::Type* Get() const override { return m_Underlying->Get(); }
        virtual std::string GetHash() const override { return m_Name; }

        std::shared_ptr<Type> GetUnderlyingType() const { return m_Underlying; }

        bool AddValue(const std::string& name, int64_t value);
        std::optional<int64_t> GetValue(llvm::StringRef name) const;
        const auto& GetValues() const { return m_Values; }

    private:
        std::string m_Name;
        std::shared_ptr<Type> m_Underlying;
        llvm::MapVector<std::string, int64_t> m_Values;
    };

    // (int, float64): a fixed group of values, laid out like a struct with unnamed fields
    class TupleType : public Type
    {
    public:
        TupleType(llvm::ArrayRef<std::shared_ptr<Type>> elements, llvm::LLVMContext& context);
        virtual ~TupleType() = default;

        virtual llvm::Type* Get() const override { return m_LLVMType; }
        virtual std::string GetHash() const override;

        const auto& GetElements() const { return m_Elements; }

    private:
        std::vector<std::shared_ptr<Type>> m_Elements;
        llvm::StructType* m_LLVMType;
    };

    // a type as it is written in Clear source (*int8, [4; float64]), for diagnostics
    std::string GetDisplayName(const std::shared_ptr<Type>& type);

    class GenericType : public Type 
    {
    public:
        GenericType(llvm::StringRef name);
        ~GenericType() = default;

        virtual llvm::Type* Get() const override { return nullptr; }
        virtual std::string GetHash() const override { return m_Name; };
        
    private:
        std::string m_Name;
    };
}

