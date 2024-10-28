#pragma once

#include "Parsing/Tokens.h"
#include "API/LLVM/LLVMInclude.h"

#include <variant>

namespace clear {

	enum class VariableType 
	{
		None = 0, Int8, Int16, Int32, Int64,
		Uint8, Uint16, Uint32, Uint64, Bool,
		Float32, Float64, String, UserDefinedType,
		Array, Pointer
	};

	enum class TypeKind
	{
		None = 0, RValue, Variable, VariableReference, Value
	};


	enum class BinaryExpressionType
	{
		None = 0, Add, Sub, Mul,
		Div, Mod, Less, LessEq,
		Greater, GreaterEq, Eq, Assignment
	};
	
	struct StructMetaData;

	extern BinaryExpressionType GetBinaryExpressionTypeFromTokenType(TokenType type);
	extern VariableType	GetVariableTypeFromTokenType(TokenType tokenType);
	extern VariableType GetVariablePointerTypeFromTokenType(TokenType tokenType);
	extern llvm::Type*	GetLLVMVariableType(VariableType type);

	class AbstractType 
	{
	public:
		using MemberType = std::pair<std::string, AbstractType>;

	public:
		AbstractType() = default;
		AbstractType(const Token& token);
		AbstractType(const Token& token, TypeKind kind, bool isPointer = false);
		AbstractType(VariableType type, TypeKind kind = TypeKind::RValue, const std::string& userDefinedType = "");
		AbstractType(VariableType type, TypeKind kind, VariableType underlying, const std::string& userDefinedType = "");
		AbstractType(const std::string_view& value); //auto generate type from a value

		static llvm::StructType* GetStructType(const std::string& name);
		static StructMetaData&   GetStructInfo(const std::string& name);
		static StructMetaData&   GetStructMetaDataFromAllocInst(llvm::AllocaInst* alloc);

		static void CreateStructType(const std::string& name, const std::vector<MemberType>& members);
		static void RemoveStructType(const std::string& name);

		static void RegisterVariableType(const std::string& name, const AbstractType& type);
		static void RemoveVariableType(const std::string& name);

		static AbstractType& GetVariableTypeFromName(const std::string& name);

		inline  VariableType Get()			     const { return m_Type; };
		inline  VariableType GetUnderlying()     const { return m_Type; }
		inline  TypeKind     GetKind()		     const { return m_Kind; }
		inline  llvm::Type*  GetLLVMType()       const { return m_LLVMType; }
		inline  llvm::Type*  GetLLVMUnderlying() const { return m_LLVMUnderlyingType; }

		void SetLLVMUnderlyingType(llvm::Type* type);

		inline  const std::string& GetUserDefinedType() const { return m_UserDefinedType; }

		const bool IsFloatingPoint() const;
		const bool IsIntegral()		 const;
		const bool IsSigned()		 const;
		const bool IsPointer()		 const;

		inline operator VariableType() const { return m_Type; }
		inline operator TypeKind() const { return m_Kind; }
		inline operator bool() const { return Get() != VariableType::None; }

		inline bool operator==(const AbstractType& other) const;
		inline bool operator!=(const AbstractType& other) const;

		inline bool operator==(VariableType other) const;
		inline bool operator!=(VariableType other) const;

	private:
		VariableType m_Type = VariableType::None;
		VariableType m_UnderlyingType = VariableType::None;
		TypeKind     m_Kind = TypeKind::None;
		llvm::Type*  m_LLVMType = nullptr;
		llvm::Type*  m_LLVMUnderlyingType = nullptr;
		std::string  m_UserDefinedType;
	};

	struct StructMetaData
	{
		llvm::StructType* Struct = nullptr;
		std::map<std::string, uint32_t> Indices;
		std::vector<AbstractType> Types;
	};
}