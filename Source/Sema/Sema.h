#pragma once 

#include "ConstEval.h"
#include "Diagnostics/DiagnosticCode.h"
#include "Diagnostics/DiagnosticsBuilder.h"
#include "Symbols/SymbolTable.h"
#include "AST/ASTNode.h"

#include <memory>

namespace clear 
{
    class Sema
    {
	public:
		Sema(std::shared_ptr<Module> clearModule, DiagnosticsBuilder& builder);
		~Sema() = default;

		void Visit(std::shared_ptr<ASTBlock> ast);
		void Visit(std::shared_ptr<ASTType> type);
		void Visit(std::shared_ptr<ASTVariableDeclaration> decl);
		void Visit(std::shared_ptr<ASTVariable> variable);
		void Visit(std::shared_ptr<ASTNodeBase> ast);
	
	private:
		void Report(DiagnosticCode code, Token token);
		std::optional<Symbol> ConstructType(std::shared_ptr<ASTType> type);	

    private:
		std::vector<std::shared_ptr<SymbolTable>> m_ScopeStack;
		std::shared_ptr<Module> m_Module;
		DiagnosticsBuilder& m_DiagBuilder;
		ConstEval m_ConstantEvaluator;
    };
}
