#include "Core/Type.h"
#include "Core/Value.h"

#include "Lexing/Tokens.h"

#include <memory>

namespace clear 
{
    enum class ASTNodeType
	{
		Base = 0, Literal, BinaryExpression,
		VariableExpression, VariableDecleration,
		FunctionDefinition, FunctionDecleration,
		ReturnStatement, Expression, Struct,
		FunctionCall, IfExpression, WhileLoop,
		UnaryExpression, Break, Continue, 
		ArrayInitializer, MemberAccess
	};

	struct CodegenResult
	{
		llvm::Value* CodegenValue = nullptr;
		std::shared_ptr<Type> CodegenType;
	};

    class ASTNodeBase
	{
	public:
		ASTNodeBase() = default;
		virtual ~ASTNodeBase() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::Base; }
		virtual CodegenResult Codegen();

		void Push(const std::shared_ptr<ASTNodeBase>& child);
		void Remove(const std::shared_ptr<ASTNodeBase>& child);

		void SetParent(const std::shared_ptr<ASTNodeBase>& parent);
		void RemoveParent();

		const auto  GetParent()   const { return m_Parent; }
		const auto& GetChildren() const { return m_Children; }

	private:
		std::shared_ptr<ASTNodeBase> m_Parent;
		std::vector<std::shared_ptr<ASTNodeBase>> m_Children;
	};

	class ASTNodeLiteral : public ASTNodeBase
	{
	public:
		ASTNodeLiteral(const Token& data);
		virtual ~ASTNodeLiteral() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Literal; }
		virtual CodegenResult Codegen() override;

	private:
		Value m_Constant;
	};

	class ASTBinaryExpression : public ASTNodeBase
	{
	public:
		ASTBinaryExpression(BinaryExpressionType type);
		virtual ~ASTBinaryExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::BinaryExpression; }
		virtual CodegenResult Codegen() override;

		inline const BinaryExpressionType GetExpression() const { return m_Expression; }

	private:
		void HandleTypePromotion(CodegenResult& lhs, CodegenResult& rhs);

		bool IsMathExpression()    const;
		bool IsCmpExpression()     const;
		bool IsBitwiseExpression() const;

		CodegenResult HandleMathExpression(CodegenResult& lhs, CodegenResult& rhs);
		CodegenResult HandleMathExpressionF(CodegenResult& lhs, CodegenResult& rhs);
		CodegenResult HandleMathExpressionSI(CodegenResult& lhs, CodegenResult& rhs);
		CodegenResult HandleMathExpressionUI(CodegenResult& lhs, CodegenResult& rhs);

		CodegenResult HandleCmpExpression(CodegenResult& lhs, CodegenResult& rhs);
		CodegenResult HandleCmpExpressionF(CodegenResult& lhs, CodegenResult& rhs);
		CodegenResult HandleCmpExpressionSI(CodegenResult& lhs, CodegenResult& rhs);
		CodegenResult HandleCmpExpressionUI(CodegenResult& lhs, CodegenResult& rhs);


		CodegenResult HandleBitwiseExpression(CodegenResult& lhs, CodegenResult& rhs);
		CodegenResult HandlePointerArithmetic(CodegenResult& lhs, CodegenResult& rhs);

	private:
		BinaryExpressionType m_Expression;
	};
}