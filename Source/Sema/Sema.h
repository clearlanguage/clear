#pragma once 

#include "Symbols/SymbolTable.h"
#include "AST/ASTNode.h"

namespace clear 
{
    class Sema
    {
	public:
		Sema();
		~Sema() = default;

		void Visit(std::shared_ptr<ASTBlock> ast);
		void Visit(std::shared_ptr<ASTVariable> variable);
		void Visit(std::shared_ptr<ASTNodeBase> ast);
		
    private:
		std::vector<std::shared_ptr<SymbolTable>> m_ScopeStack;
    };
}
