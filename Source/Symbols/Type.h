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
        Generic, Count
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
        virtual std::string GetHash() const override { return m_BaseType->GetHash() + "*"; }
       
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

        std::shared_ptr<Type> GetBaseType() const { return m_BaseType; }
        void SetBaseType(std::shared_ptr<Type> type);
        size_t GetArraySize() const { return m_Count; }


    private:
        std::shared_ptr<Type> m_BaseType;
        llvm::ArrayType* m_LLVMType;
        size_t m_Count;
    };
	
    struct Symbol;
	
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

    private:
		llvm::StructType* m_LLVMType = nullptr;
		llvm::MapVector<std::string, std::shared_ptr<Type>> m_MemberValues;
		llvm::DenseMap<std::string,  std::shared_ptr<Symbol>> m_MemberFunctions;
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

