#pragma once

#include "Ref.h"

#include "API/LLVM/LLVMInclude.h"

#include <map>
#include <vector>
#include <concepts>

namespace clear {

    enum class TypeID : uint8_t
	{
		None = 0, Int8, Int16, Int32, Int64,
		Uint8, Uint16, Uint32, Uint64, Bool,
		Float32, Float64, String, UserDefinedType,
		Array, Pointer
	};

    // Variable meaning it is currently a variable
    // loaded meaning it has just been loaded from the variable
    // constant meaning its just an llvm constant (RValues)

    enum class TypeKindID : uint8_t
    {
        None = 0, Variable, Loaded, Constant
    };


    class Type;

    struct StructMetaData
	{
		llvm::StructType* Struct = nullptr;
		std::map<std::string, uint32_t> Indices;
		std::vector<Ref<Type>> Types;
	};

    struct MemberType
    {
        Ref<Type> Type;
        std::string Name;
    };

    class Type 
    {
    public: 
        Type() = default;
        ~Type() = default;

        Type(const std::string& userDefinedTypeName, const std::vector<MemberType>& members); //leave members empty if referencing an existing struct
        //Type(const Ref<Type>& elementType, size_t count);
        Type(const std::string& rvalue); 
        
        Ref<Type> CreatePointerToThis();

        inline TypeID      GetID() const {return m_ID;}
        inline TypeKindID  GetTypeKindID() const {return m_TypeKindID;}
        
        inline llvm::Type* Get() const { return m_LLVMType; }
        inline Ref<Type>   GetUnderlying() const {return m_Underlying; }

        bool IsFloatingPoint() const;
		bool IsIntegral()	   const;
		bool IsSigned()		   const;
		bool IsPointer()	   const;

    private:
        TypeID       m_ID = TypeID::None;
        TypeKindID   m_TypeKindID = TypeKindID::None;
        llvm::Type*  m_LLVMType = nullptr;

        Ref<Type> m_Underlying;
        std::string m_UserDefinedTypeIdentifier;
    };

    template<typename T>
    concept IsTypeRef = requires(T t)
    {
        { *t } -> std::same_as<Type&>;
    };

    bool operator==(const Type& first, const Type& second) 
    {
        return first.Get() == second.Get();
    }

    bool operator!=(const Type& first, const Type& second) 
    {
        return !(first == second);
    }

    template <typename T1, typename T2>
    bool operator==(const T1& first, const T2& second) requires IsTypeRef<T1> && IsTypeRef<T2>
    {
        return (*first).Get() == (*second).Get();
    }

    template <typename T1, typename T2>
    bool operator!=(const T1& first, const T2& second) requires IsTypeRef<T1> && IsTypeRef<T2>
    {
        return !(first == second);
    }

}
