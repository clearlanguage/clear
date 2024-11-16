#pragma once

#include "Ref.h"

#include "API/LLVM/LLVMInclude.h"
#include "Parsing/Tokens.h"

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

    enum class BinaryExpressionType 
	{
		None = 0, Add, Sub, Mul, Div, Pow, Mod, Less, LessEq,
		Greater, GreaterEq, Eq, NotEq, PositivePointerArithmetic,
		NegatedPointerArithmetic, Assignment, BitwiseLeftShift,
		BitwiseRightShift, BitwiseNot, BitwiseAnd, BitwiseOr,
		BitwiseXor, Index
	};

	enum class UnaryExpressionType 
	{
		None = 0, BitwiseNot,	Increment, 
		Decrement, Negation, PostIncrement, 
		PostDecrement, Reference, Dereference, 
		Cast
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

        Type(const Token& token, bool isPointer = false);

        Type(const std::string& userDefinedTypeName, const std::vector<MemberType>& members); //leave members empty if referencing an existing struct
        Type(const std::string& rvalue); 

        Type(const Ref<Type>& elementType, size_t count); //arrays
        Type(const Ref<Type>& pointTo); //pointers
        
        inline TypeID      GetID() const {return m_ID;}
        inline TypeKindID  GetTypeKindID() const {return m_TypeKindID;}
        
        inline llvm::Type* Get() const { return m_LLVMType; }
        inline Ref<Type>   GetUnderlying() const {return m_Underlying; }

        bool IsFloatingPoint() const;
		bool IsIntegral()	   const;
		bool IsSigned()		   const;
		bool IsPointer()	   const;

        static StructMetaData& GetStructMetaData(const std::string& name);

        static void RegisterVariableType(const std::string& name, const Ref<Type>& type);
		static void RemoveVariableType(const std::string& name);

		static Ref<Type> GetVariableTypeFromName(const std::string& name);

        static BinaryExpressionType GetBinaryExpressionTypeFromToken(TokenType type);
        static UnaryExpressionType  GetUnaryExpressionTypeFromToken(TokenType type);
        static UnaryExpressionType  GetPostUnaryExpressionTypeFromToken(TokenType type);

    private:
        TypeID       m_ID = TypeID::None;
        TypeKindID   m_TypeKindID = TypeKindID::None;
        llvm::Type*  m_LLVMType = nullptr;

        Ref<Type> m_Underlying;
        std::string m_UserDefinedTypeIdentifier;
    };
}
