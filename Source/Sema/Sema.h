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

		void Visit(std::shared_ptr<ASTBlock> ast);
		void Visit(std::shared_ptr<ASTType> type);
		void Visit(std::shared_ptr<ASTTypeSpecifier> typeSpec);
		void Visit(std::shared_ptr<ASTVariableDeclaration> decl);
		void Visit(std::shared_ptr<ASTExpression> expr);	
		void Visit(std::shared_ptr<ASTVariable> variable);
		void Visit(std::shared_ptr<ASTNodeBase> ast);
		void Visit(std::shared_ptr<ASTFunctionDefinition> func);
		void Visit(std::shared_ptr<ASTFunctionCall> funcCall);
		void Visit(std::shared_ptr<ASTReturn> returnStatement);
		void Visit(std::shared_ptr<ASTBinaryExpression> binaryExpression);
		void Visit(std::shared_ptr<ASTNodeLiteral> literal);
	
	private:
		void Report(DiagnosticCode code, Token token);
		std::optional<Symbol> ConstructType(std::shared_ptr<ASTType> type);	

    private:
		std::vector<SemaSymbolTable> m_ScopeStack;
		llvm::SmallVector<SemaContext> m_ContextStack;
		std::shared_ptr<Module> m_Module;
		DiagnosticsBuilder& m_DiagBuilder;
		ConstEval m_ConstantEvaluator;
		Infer m_TypeInferEngine;
		NameMangler m_NameMangler;
		
    };
}
