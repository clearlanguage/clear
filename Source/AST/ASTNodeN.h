#include "Core/Type.h"
#include "Core/Value.h"

#include "Lexing/Tokens.h"
#include "SymbolTable.h"

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
		ArrayInitializer, MemberAccess, AssignmentOperator, 
		VariableReference
	};

	struct CodegenResult
	{
		llvm::Value* CodegenValue = nullptr;
		std::shared_ptr<Type> CodegenType;
	};

    class ASTNodeBase
	{
	public:
		ASTNodeBase();
		virtual ~ASTNodeBase() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::Base; }
		virtual CodegenResult Codegen();

		void Push(const std::shared_ptr<ASTNodeBase>& child);
		void Remove(const std::shared_ptr<ASTNodeBase>& child);

		void PropagateSymbolTableToChildren();

		void CreateSymbolTable();

		std::shared_ptr<SymbolTable> GetSymbolTable() { return m_SymbolTable; }

		const auto& GetChildren() const { return m_Children; }
	
	private:
		void PropagateSymbolTable(const std::shared_ptr<SymbolTable>& registry);

	private:
		std::vector<std::shared_ptr<ASTNodeBase>> m_Children;
		std::shared_ptr<SymbolTable> m_SymbolTable;
 
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

	class ASTVariableDeclaration : public ASTNodeBase
	{
	public:
		ASTVariableDeclaration(const std::string& name, std::shared_ptr<Type> type);
		virtual ~ASTVariableDeclaration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::VariableDecleration; }
		virtual CodegenResult Codegen() override;

	private:
		std::string m_Name;
		std::shared_ptr<Type> m_Type;
	};

	class ASTVariableReference : public ASTNodeBase
	{
	public:
		ASTVariableReference(const std::string& name);
		virtual ~ASTVariableReference() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::VariableReference; }
		virtual CodegenResult Codegen() override;

	private:
		std::string m_Name;
	};

	class ASTVariableExpression : public ASTNodeBase
	{
	public:
		ASTVariableExpression(const std::string& name);
		virtual ~ASTVariableExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::VariableExpression; }
		virtual CodegenResult Codegen() override;

	private:
		std::string m_Name;
	};

	enum class AssignmentOperatorType 
	{
		Normal = 0, Mul, Div, Add, Sub, Mod
	};

	class ASTAssignmentOperator : public ASTNodeBase
	{
	public:
		ASTAssignmentOperator(AssignmentOperatorType type);
		virtual ~ASTAssignmentOperator() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::AssignmentOperator; }
		virtual CodegenResult Codegen();

	private:
		void HandleDifferentTypes(CodegenResult& storage, CodegenResult& data);

	private:
		AssignmentOperatorType m_Type;
	};
}