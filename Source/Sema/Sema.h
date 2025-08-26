#pragma once 

#include "ConstEval.h"
#include "Core/Value.h"
#include "Diagnostics/DiagnosticCode.h"
#include "Diagnostics/DiagnosticsBuilder.h"
#include "Sema/Infer.h"
#include "AST/ASTNode.h"
#include "Sema/NameMangling.h"
#include "Sema/SymbolTable.h"

#include <memory>

namespace clear 
{
	enum class ValueRequired : uint8_t
	{
		Any = 0, LValue, RValue
	};
	
	struct SemaContext
	{
		ValueRequired ValueReq = ValueRequired::Any;
	};

    class Sema
    {
	public:
		Sema(std::shared_ptr<Module> clearModule, DiagnosticsBuilder& builder);
		~Sema() = default;

		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTBlock> ast, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTType> type, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTTypeSpecifier> typeSpec, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTVariableDeclaration> decl, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTExpression> expr, SemaContext context);	
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTNodeBase> ast, SemaContext context = {});
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTFunctionDefinition> func, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTFunctionCall> funcCall, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTReturn> returnStatement, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTBinaryExpression> binaryExpression, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTNodeLiteral> literal, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTVariable> variable, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTAssignmentOperator> assignment, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTUnaryExpression> unaryExpr, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTFunctionDeclaration> decl, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTClass> classExpr, SemaContext context);	
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTIfExpression> ifExpr, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTWhileExpression> whileExpr, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTStructExpr> structExpr, SemaContext context);

	private:
		void Report(DiagnosticCode code, Token token);
		std::optional<Symbol> ConstructType(std::shared_ptr<ASTType> type);	
		
		void VisitBinaryExprArithmetic(std::shared_ptr<ASTBinaryExpression> binaryExpr, SemaContext context);	
		std::shared_ptr<ASTNodeBase> VisitBinaryExprMemberAccess(std::shared_ptr<ASTBinaryExpression> binaryExpr, SemaContext context);	
		

    private:
		std::vector<SemaSymbolTable> m_ScopeStack;
		std::shared_ptr<Module> m_Module;
		DiagnosticsBuilder& m_DiagBuilder;
		ConstEval m_ConstantEvaluator;
		Infer m_TypeInferEngine;
		NameMangler m_NameMangler;
		
    };
}
