#pragma once

#include "Parsing/Tokens.h"
#include "Core/Types.h"
#include "Core/Ref.h"
#include "Core/Value.h"

#include <vector>
#include <string>



namespace clear {

	enum class ASTNodeType
	{
		Base = 0, Literal, BinaryExpression, 
		VariableExpression, VariableDecleration, 
		FunctionDecleration, ReturnStatement, 
		Expression, Struct, FunctionCall, IfExpression
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

		void SetName(const std::string& name);

		void PushChild(const Ref<ASTNodeBase>& child);
		void RemoveChild(const Ref<ASTNodeBase>& child);

		void SetParent(const Ref<ASTNodeBase>& parent);
		void RemoveParent();

		const auto  GetParent()   const { return m_Parent; }
		const auto& GetChildren() const { return m_Children; }

		inline const std::string& GetName() const { return m_Name; }

	private:
		Ref<ASTNodeBase> m_Parent;
		std::vector<Ref<ASTNodeBase>> m_Children;
		std::string m_Name;
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
		Value m_Constant;
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
		const bool _IsMathExpression()    const;
		const bool _IsCmpExpression()     const;
		const bool _IsBitwiseExpression() const;

		llvm::Value* _CreateExpression(llvm::Value* LHS, llvm::Value* RHS, llvm::Value* LHSRawValue, llvm::Value* RHSRawValue, bool signedInteger);
		llvm::Value* _CreateMathExpression(llvm::Value* LHS, llvm::Value* RHS);
		llvm::Value* _CreateCmpExpression(llvm::Value* LHS, llvm::Value* RHS);
		llvm::Value* _CreateLoadStoreExpression(llvm::Value* LHS, llvm::Value* RHS);
		llvm::Value* _CreateBitwiseExpression(llvm::Value* LHS, llvm::Value* RHS, bool signedInteger);

	private:
		BinaryExpressionType m_Expression;
		AbstractType m_ExpectedType;
	};
	//
	// ------------------------------------------------------------------
	//


	struct Paramater
	{
		std::string Name;
		AbstractType Type; 
	};

	//
	// ---------------------- FUNCTION DECELRATION -----------------------
	//
	class ASTFunctionDecleration : public ASTNodeBase
	{
	public:
		ASTFunctionDecleration(const std::string& name, VariableType returnType, const std::vector<Paramater>& arugments);
		virtual ~ASTFunctionDecleration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionDecleration; }
		virtual llvm::Value* Codegen() override;

	private:
		VariableType m_ReturnType;
		std::vector<Paramater> m_Paramaters;
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
		Value m_Value;
	};


	//
	// ---------------------- VARIABLE EXPRESSION -----------------------
	//
	class ASTVariableExpression : public ASTNodeBase
	{
	public:
		ASTVariableExpression(const std::list<std::string>& chain, bool isPointer = false, bool dereference = false);
		virtual ~ASTVariableExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::VariableExpression; }
		virtual llvm::Value* Codegen() override;

		inline const std::string& GetName() const { return m_Name; }

	private:
		std::string m_Name;
		std::list<std::string> m_Chain;
		bool m_PointerFlag = false;
		bool m_Dereference = false;
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

	class ASTStruct : public ASTNodeBase
	{
	public:
		ASTStruct(const std::string& name, const std::vector<AbstractType::MemberType>& fields);
		virtual ~ASTStruct() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Struct; }
		virtual llvm::Value* Codegen() override;

	private:
		std::vector<AbstractType::MemberType> m_Members;
		std::string m_Name;
	};

	//
	// ------------------------------------------------------------------
	//

	//
	// ---------------------- IF -----------------------
	//

	class ASTIfExpression : public ASTNodeBase
	{
	public:
		ASTIfExpression() = default;
		virtual ~ASTIfExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::IfExpression; }
		virtual llvm::Value* Codegen() override;

	};

	//
	// ------------------------------------------------------------------
	//
}