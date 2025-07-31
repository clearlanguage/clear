#pragma once 

#include "Symbols/Symbol.h"

#include <llvm/ADT/StringRef.h>

#include <memory>
#include <unordered_map>

namespace clear
{
	enum class SymbolEntryType
	{
		None = 0, Variable, Function, GenericFunction,
		FunctionDeclaration
	};

	struct SymbolEntry
	{
		SymbolEntryType Type;
		std::shared_ptr<Symbol> Symbol; 
	};

	class SemaSymbolTable 
	{
	public:
		SemaSymbolTable() = default;
		~SemaSymbolTable() = default;

		std::optional<std::shared_ptr<Symbol>> InsertEmpty(llvm::StringRef name, SymbolEntryType type);
		std::optional<SymbolEntry> Get(llvm::StringRef name);

	private:
		std::unordered_map<std::string, SymbolEntry> m_Symbols;
	};
}
