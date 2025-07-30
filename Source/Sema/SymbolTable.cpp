#include "SymbolTable.h"
#include <llvm/ADT/StringRef.h>
#include <memory>

namespace clear  
{
	std::optional<std::shared_ptr<Symbol>> SemaSymbolTable::InsertEmpty(llvm::StringRef name)
	{
		auto [it, success] = m_Symbols.try_emplace(std::string(name), std::make_shared<Symbol>());
		
		if (!success)
			return std::nullopt;

		return it->second;
	}

	std::optional<std::shared_ptr<Symbol>> SemaSymbolTable::Get(llvm::StringRef name)
	{
		auto it = m_Symbols.find(std::string(name));
		
		if (it == m_Symbols.end())
			return std::nullopt;
		
		return it->second;
	}
}
