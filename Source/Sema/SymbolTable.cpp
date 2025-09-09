#include "SymbolTable.h"
#include <llvm/ADT/StringRef.h>
#include <memory>

namespace clear  
{
	bool SymbolTable::Insert(llvm::StringRef name, SymbolEntryType type, std::shared_ptr<Symbol> symbol)
	{
		auto [it, success] = m_Symbols.try_emplace(std::string(name), SymbolEntry { type, symbol });
		return success;	
	}

	std::optional<std::shared_ptr<Symbol>> SymbolTable::InsertEmpty(llvm::StringRef name, SymbolEntryType type)
	{
		auto [it, success] = m_Symbols.try_emplace(std::string(name), SymbolEntry { type, std::make_shared<Symbol>() });
		
		if (!success)
			return std::nullopt;

		return it->second.Symbol;
	}

	std::optional<SymbolEntry> SymbolTable::Get(llvm::StringRef name)
	{
		auto it = m_Symbols.find(std::string(name));
		
		if (it == m_Symbols.end())
			return std::nullopt;
		
		return it->second;
	}
}
