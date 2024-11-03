#pragma once

#include "Parsing/Tokens.h"
#include "Core/Types.h"
#include "Core/Ref.h"
#include "Core/Value.h"

#include <vector>
#include <string>



namespace clear {

	struct Paramater
	{
		std::string Name;
		AbstractType Type;
		bool IsVariadic = false;
	};

	inline std::map<std::string, std::vector<Paramater>> g_FunctionToExpectedTypes;


	enum class ASTNodeType
	{
		Base = 0, Literal, BinaryExpression,
		VariableExpression, VariableDecleration,
		FunctionDefinition, FunctionDecleration,
		ReturnStatement, Expression, Struct,
		FunctionCall, IfExpression, WhileLoop,
		UnaryExpression, Break, Continue
	};


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


	class ASTNodeLiteral : public ASTNodeBase
	{
	public:
		ASTNodeLiteral(const std::string& data);
		virtual ~ASTNodeLiteral() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Literal; }
		virtual llvm::Value* Codegen() override;

		inline AbstractType& GetGeneratedType() { return m_Constant.GetType(); }

	private:
		Value m_Constant;
	};


	class ASTBinaryExpression : public ASTNodeBase
	{
	public:
		ASTBinaryExpression(BinaryExpressionType type, AbstractType expectedType = VariableType::None);
		virtual ~ASTBinaryExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::BinaryExpression; }
		virtual llvm::Value* Codegen() override;

		inline const BinaryExpressionType GetExpression() const { return m_Expression; }
		inline AbstractType& GetExpectedType() { return m_ExpectedType; }

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

	class ASTUnaryExpression : public ASTNodeBase
	{
	public:
		ASTUnaryExpression(UnaryExpressionType type);
		virtual ~ASTUnaryExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::UnaryExpression; }
		virtual llvm::Value* Codegen() override;

	private:
		UnaryExpressionType m_Type;
	};

	class ASTFunctionDefinition : public ASTNodeBase
	{
	public:
		ASTFunctionDefinition(const std::string& name, const AbstractType& returnType, const std::vector<Paramater>& arugments);
		virtual ~ASTFunctionDefinition() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionDefinition; }
		virtual llvm::Value* Codegen() override;

	private:
		AbstractType m_ReturnType;
		std::vector<Paramater> m_Paramaters;
	};


	class ASTFunctionDecleration : public ASTNodeBase
	{
	public:
		ASTFunctionDecleration(const std::string& name, const AbstractType& expectedReturnType, const std::vector<Paramater>& types);
		virtual ~ASTFunctionDecleration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionDecleration; }
		virtual llvm::Value* Codegen() override;

	private:
		std::string m_Name;
		AbstractType m_ExpectedReturnType;
		std::vector<Paramater> m_ExpectedTypes;
	};


	struct Argument
	{
		AbstractType Field;
		std::string Data;
	};

	class ASTFunctionCall : public ASTNodeBase
	{
	public:
		ASTFunctionCall(const std::string& name);
		virtual ~ASTFunctionCall() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionCall; }
		virtual llvm::Value* Codegen() override;


	private:
		std::string m_Name;
	};

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


	class ASTVariableExpression : public ASTNodeBase
	{
	public:
		ASTVariableExpression(const std::list<std::string>& chain, bool isPointer = false, bool dereference = false);
		virtual ~ASTVariableExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::VariableExpression; }
		virtual llvm::Value* Codegen() override;

		inline const std::string& GetName() const { return m_Name; }
		inline AbstractType& GetGeneratedType() { return m_GeneratedType; }

	private:
		std::string m_Name;
		std::list<std::string> m_Chain;
		bool m_PointerFlag = false;
		bool m_Dereference = false;
		AbstractType m_GeneratedType;
	};

	class ASTReturnStatement : public ASTNodeBase
	{
	public:
		ASTReturnStatement(const AbstractType& expectedReturnType, bool createReturn = true);
		virtual ~ASTReturnStatement() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::ReturnStatement; }
		virtual llvm::Value* Codegen() override;

	private:
		AbstractType m_ExpectedReturnType;
		bool m_CreateReturn;
	};


	class ASTExpression : public ASTNodeBase
	{
	public:
		ASTExpression() = default;
		virtual ~ASTExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Expression; }
		virtual llvm::Value* Codegen() override;

	};

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

	class ASTIfExpression : public ASTNodeBase
	{
	public:
		ASTIfExpression() = default;
		virtual ~ASTIfExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::IfExpression; }
		virtual llvm::Value* Codegen() override;

	};

	class ASTWhileLoop : public ASTNodeBase
	{
	public:
		ASTWhileLoop() = default;
		virtual ~ASTWhileLoop() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::WhileLoop; }
		virtual llvm::Value* Codegen() override;
	};

	class ASTBreak : public ASTNodeBase
	{
	public:
		ASTBreak() = default;
		virtual ~ASTBreak() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Break; }
		virtual llvm::Value* Codegen() override;
	};

	class ASTContinue : public ASTNodeBase
	{
	public:
		ASTContinue() = default;
		virtual ~ASTContinue() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Continue; }
		virtual llvm::Value* Codegen() override;
	};

}