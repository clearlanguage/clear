#include "SymbolTable.h"
#include <llvm/ADT/StringRef.h>
#include <memory>

namespace clear  
{
	std::optional<std::shared_ptr<Symbol>> SemaSymbolTable::InsertEmpty(llvm::StringRef name, SymbolEntryType type)
	{
		auto [it, success] = m_Symbols.try_emplace(std::string(name), SymbolEntry { type, std::make_shared<Symbol>() });
		
		if (!success)
			return std::nullopt;

		return it->second.Symbol;
	}

	std::optional<SymbolEntry> SemaSymbolTable::Get(llvm::StringRef name)
	{
		auto it = m_Symbols.find(std::string(name));
		
		if (it == m_Symbols.end())
			return std::nullopt;
		
		return it->second;
	}
}
