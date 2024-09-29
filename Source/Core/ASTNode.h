#pragma once 

#include "Tokens.h"

#include <memory>
#include <vector>

#include <llvm/IR/Value.h>

namespace clear {

	enum class VariableType
	{
		None = 0, Int8, Int16, Int32, Int64,
		Uint8, Uint16, Uint32, Uint64, Bool,
		Float32, Float64, Struct, Object //(struct and object will need implementing later)
	};

	extern llvm::Type* GetVariableType(VariableType type);
	extern VariableType GetVariableTypeFromTokenType(TokenType tokenType);


	enum class ASTNodeType
	{
		Base = 0, Literal, BinaryExpression, 
		VariableExpression, VariableDecleration, 
		FunctionDecleration, ReturnStatement
	};

	//
	// ---------------------- BASE -----------------------
	//
	class ASTNodeBase
	{
	public:
		ASTNodeBase() = default;
		virtual ~ASTNodeBase() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::Base; }
		virtual llvm::Value* Codegen();

		void PushChild(const std::shared_ptr<ASTNodeBase>& child);
		void RemoveChild(const std::shared_ptr<ASTNodeBase>& child);

		void SetParent(const std::shared_ptr<ASTNodeBase>& parent);
		void RemoveParent();

		const auto  GetParent()   const { return m_Parent; }
		const auto& GetChildren() const { return m_Children; }

	private:
		std::shared_ptr<ASTNodeBase> m_Parent = nullptr;
		std::vector<std::shared_ptr<ASTNodeBase>> m_Children;
	};
	//
	// -------------------------------------------------------
	//

	//
	// ---------------------- LITERAL -----------------------
	//
	class ASTNodeLiteral : public ASTNodeBase
	{
	public:
		ASTNodeLiteral(const std::string& data);
		virtual ~ASTNodeLiteral() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::Literal; }
		virtual llvm::Value* Codegen() override;

	private:
		VariableType m_Type;
		std::string m_Data;
	};

	//
	// -----------------------------------------------------------
	//

	enum class BinaryExpressionType
	{
		None = 0, Add, Sub, Mul, 
		Div, Mod, Less, LessEq, 
		Greater, GreaterEq, Eq, Assignment
	};

	//
	// ---------------------- BINARY EXPRESSION -----------------------
	//
	class ASTBinaryExpression : public ASTNodeBase
	{
	public:
		ASTBinaryExpression(BinaryExpressionType type);
		virtual ~ASTBinaryExpression() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::BinaryExpression; }
		virtual llvm::Value* Codegen() override;

		inline const BinaryExpressionType GetExpression() const { return m_Expression; }
		
	private:
		const bool _IsMathExpression() const;
		const bool _IsCmpExpression() const;

		llvm::Value* _CreateMathExpression(llvm::Value* LHS, llvm::Value* RHS);
		llvm::Value* _CreateCmpExpression(llvm::Value* LHS, llvm::Value* RHS);
		llvm::Value* _CreateLoadStoreExpression(llvm::Value* LHS, llvm::Value* RHS);

	private:
		BinaryExpressionType m_Expression;
	};
	//
	// ------------------------------------------------------------------
	//


	struct Argument
	{
		std::string Name;
		VariableType Type;
	};

	//
	// ---------------------- FUNCTION DECELRATION -----------------------
	//
	class ASTFunctionDecleration : public ASTNodeBase
	{
	public:
		ASTFunctionDecleration(const std::string& name, VariableType returnType, const std::vector<Argument>& arugments);
		virtual ~ASTFunctionDecleration() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::FunctionDecleration; }
		virtual llvm::Value* Codegen() override;

		inline const auto  GetArg(size_t idx) const { return m_FunctionArgs[idx]; };
		inline const auto& GetArgs() const { return m_FunctionArgs; }

	private:

	private:
		std::string m_Name;
		VariableType m_ReturnType;
		std::vector<Argument> m_Arguments;
		std::vector<llvm::Value*> m_FunctionArgs;
	};
	//
	// -------------------------------------------------------------------
	//


	//
	// ---------------------- VARIABLE DECELRATION -----------------------
	//
	class ASTVariableDecleration : public ASTNodeBase
	{
	public:
		ASTVariableDecleration(const std::string& name, VariableType type);
		virtual ~ASTVariableDecleration() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::VariableDecleration; }
		virtual llvm::Value* Codegen() override;

		inline const std::string& GetName() const { return m_Name; }

	private:
		std::string m_Name;
		VariableType m_Type;
	};

	enum class VariableKind
	{
		None = 0, Argument, Local
	};

	//
	// ---------------------- VARIABLE EXPRESSION -----------------------
	//
	class ASTVariableExpression : public ASTNodeBase
	{
	public:
		ASTVariableExpression(const std::string& name);
		virtual ~ASTVariableExpression() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::VariableExpression; }
		virtual llvm::Value* Codegen() override;

		inline const std::string& GetName() const { return m_Name; }

	private:
		std::string m_Name;
	};

	//
	// ------------------------------------------------------------------
	//

	class ASTReturnStatement : public ASTNodeBase
	{
	public:
		ASTReturnStatement() = default;
		virtual ~ASTReturnStatement() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::ReturnStatement; }
		virtual llvm::Value* Codegen() override;

	};
}