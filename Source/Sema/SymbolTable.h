#pragma once 

#include "Symbols/Symbol.h"

#include <llvm/ADT/StringRef.h>

#include <memory>
#include <unordered_map>

namespace clear
{
	class SemaSymbolTable 
	{
	public:
		SemaSymbolTable() = default;
		~SemaSymbolTable() = default;

		std::optional<std::shared_ptr<Symbol>> InsertEmpty(llvm::StringRef name);
		std::optional<std::shared_ptr<Symbol>> Get(llvm::StringRef name);

	private:
		std::unordered_map<std::string, std::shared_ptr<Symbol>> m_Symbols;
	};
}
