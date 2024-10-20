#pragma once

#include "Parsing/Tokens.h"
#include "Core/Types.h"

#include <memory>
#include <vector>
#include <string>


namespace clear {

	enum class ASTNodeType
	{
		Base = 0, Literal, BinaryExpression, 
		VariableExpression, VariableDecleration, 
		FunctionDecleration, ReturnStatement, 
		Expression, Struct, FunctionCall
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
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Literal; }
		virtual llvm::Value* Codegen() override;

	private:
		AbstractType m_Type;
		std::string m_Data;
	};

	//
	// -----------------------------------------------------------
	//

	//
	// ---------------------- BINARY EXPRESSION -----------------------
	//
	class ASTBinaryExpression : public ASTNodeBase
	{
	public:
		ASTBinaryExpression(BinaryExpressionType type, AbstractType expectedType = VariableType::None);
		virtual ~ASTBinaryExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::BinaryExpression; }
		virtual llvm::Value* Codegen() override;

		inline const BinaryExpressionType GetExpression() const { return m_Expression; }

	private:
		const bool _IsMathExpression() const;
		const bool _IsCmpExpression() const;

		llvm::Value* _Cast(llvm::Value* casting, AbstractType to);

		llvm::Value* _CreateExpression(llvm::Value* LHS, llvm::Value* RHS, llvm::Value* LHSRawValue, llvm::Value* RHSRawValue);
		llvm::Value* _CreateMathExpression(llvm::Value* LHS, llvm::Value* RHS);
		llvm::Value* _CreateCmpExpression(llvm::Value* LHS, llvm::Value* RHS);
		llvm::Value* _CreateLoadStoreExpression(llvm::Value* LHS, llvm::Value* RHS);

	private:
		BinaryExpressionType m_Expression;
		AbstractType m_ExpectedType;
	};
	//
	// ------------------------------------------------------------------
	//


	struct Paramter
	{
		std::string Name;
		VariableType Type; //TODO: change thhis to field
	};

	//
	// ---------------------- FUNCTION DECELRATION -----------------------
	//
	class ASTFunctionDecleration : public ASTNodeBase
	{
	public:
		ASTFunctionDecleration(const std::string& name, VariableType returnType, const std::vector<Paramter>& arugments);
		virtual ~ASTFunctionDecleration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionDecleration; }
		virtual llvm::Value* Codegen() override;

		inline const std::string& GetName() const { return m_Name; }

	private:
		std::string m_Name;
		VariableType m_ReturnType;
		std::vector<Paramter> m_Paramters;
	};
	//
	// -------------------------------------------------------------------
	//


	struct Argument
	{
		AbstractType Field;
		std::string Data;
	};

	//
	// ---------------------- FUNCTION CALL -----------------------
	//
	class ASTFunctionCall : public ASTNodeBase
	{
	public:
		ASTFunctionCall(const std::string& name, const std::vector<Argument>& arugments);
		virtual ~ASTFunctionCall() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionCall; }
		virtual llvm::Value* Codegen() override;


	private:
		std::string m_Name;
		std::vector<Argument> m_Arguments;
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
		ASTVariableDecleration(const std::string& name, AbstractType type);
		virtual ~ASTVariableDecleration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::VariableDecleration; }
		virtual llvm::Value* Codegen() override;

		inline const std::string& GetName() const { return m_Name; }

	private:
		std::string m_Name;
		AbstractType m_Type;
	};


	//
	// ---------------------- VARIABLE EXPRESSION -----------------------
	//
	class ASTVariableExpression : public ASTNodeBase
	{
	public:
		ASTVariableExpression(const std::string& name);
		virtual ~ASTVariableExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::VariableExpression; }
		virtual llvm::Value* Codegen() override;

		inline const std::string& GetName() const { return m_Name; }

	private:
		std::string m_Name;
	};

	//
	// ------------------------------------------------------------------
	//

	//
	// ---------------------- RETURN STATEMENT -----------------------
	//
	class ASTReturnStatement : public ASTNodeBase
	{
	public:
		ASTReturnStatement() = default;
		virtual ~ASTReturnStatement() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::ReturnStatement; }
		virtual llvm::Value* Codegen() override;
	};

	//
	// ------------------------------------------------------------------
	//

	//
	// ---------------------- EXPRESSION -----------------------
	//

	class ASTExpression : public ASTNodeBase
	{
	public:
		ASTExpression() = default;
		virtual ~ASTExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Expression; }
		virtual llvm::Value* Codegen() override;

	};

	//
	// ------------------------------------------------------------------
	//

	//
	// ---------------------- STRUCT -----------------------
	//

	struct Member
	{
		AbstractType Field;
		std::string Name;
	};

	class ASTStruct : public ASTNodeBase
	{
	public:
		ASTStruct(const std::string& name, const std::vector<Member>& fields);
		virtual ~ASTStruct() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Struct; }
		virtual llvm::Value* Codegen() override;

	private:
		std::vector<Member> m_Members;
		std::string m_Name;
	};

	//
	// ------------------------------------------------------------------
	//
}