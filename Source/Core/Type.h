#pragma once


#include "API/LLVM/LLVMInclude.h"
#include "Lexing/Lexer.h"

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

    enum class TypeKindID : uint8_t
    {
        None = 0, Variable, Loaded, Constant, Reference
    };

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


    class Type;

    struct StructMetaData
	{
		llvm::StructType* Struct = nullptr;
		std::map<std::string, uint32_t> Indices;
		std::vector<std::shared_ptr<Type>> Types;
	};

    struct MemberType
    {
        std::shared_ptr<Type> Type;
        std::string Name;
    };

    class Type 
    {
    public: 
        Type() = default;
        ~Type() = default;

        Type(TypeID type, TypeKindID typeKind = TypeKindID::Variable, TypeID underlying = TypeID::None);
        Type(const Token& token, bool isPointer = false);
        Type(const std::string& userDefinedTypeName, const std::vector<MemberType>& members); //leave members empty if referencing an existing struct
        Type(const std::shared_ptr<Type>& elementType, size_t count); //arrays
        Type(const std::shared_ptr<Type>& pointTo); //pointers

        inline TypeID      GetID() const {return m_ID;}
        inline TypeKindID  GetTypeKindID() const {return m_TypeKindID;}

        inline const std::string& GetUserDefinedTypeIdentifer() const {return m_UserDefinedTypeIdentifier;}

        inline llvm::Type* Get() const { return m_LLVMType; }
        inline std::shared_ptr<Type>   GetUnderlying() const {return m_Underlying; }

        bool IsFloatingPoint() const;
		bool IsIntegral()	   const;
		bool IsSigned()		   const;
		bool IsPointer()	   const;

        static StructMetaData& GetStructMetaData(const std::string& name);

        static void RegisterVariableType(const std::string& name, const std::shared_ptr<Type>& type);
		static void RemoveVariableType(const std::string& name);

		static std::shared_ptr<Type> GetVariableTypeFromName(const std::string& name);

        static BinaryExpressionType GetBinaryExpressionTypeFromToken(TokenType type);
        static UnaryExpressionType  GetUnaryExpressionTypeFromToken(TokenType type);
        static UnaryExpressionType  GetPostUnaryExpressionTypeFromToken(TokenType type);

    private:
        TypeID       m_ID = TypeID::None;
        TypeKindID   m_TypeKindID = TypeKindID::None;
        llvm::Type*  m_LLVMType = nullptr;

        std::shared_ptr<Type> m_Underlying;
        std::string m_UserDefinedTypeIdentifier;
    };
}
