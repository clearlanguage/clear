#include "Sema.h"
#include "Core/Log.h"

namespace clear
{
    Sema::Sema()
	{
    }

	void Sema::Visit(std::shared_ptr<ASTBlock> ast)
	{
		m_ScopeStack.push_back(std::make_shared<SymbolTable>());
		
		for(auto node : ast->Children)
			node->Accept(*this);

		m_ScopeStack.pop_back();
	}
}
