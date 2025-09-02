#pragma once 

#include "AST/ASTNode.h"
#include "SymbolTable.h"

namespace clear {
	
	struct Cloner 
	{
		Cloner() = default;
		~Cloner() = default;
			
		std::shared_ptr<ASTNodeBase> Clone(std::shared_ptr<ASTNodeBase> node);

		std::shared_ptr<ASTClass> CloneClass(std::shared_ptr<ASTClass> node);
		std::shared_ptr<ASTFunctionDefinition> CloneFunction(std::shared_ptr<ASTFunctionDefinition> node);
		std::shared_ptr<ASTVariableDeclaration> CloneVariableDecl(std::shared_ptr<ASTVariableDeclaration> node);
		std::shared_ptr<ASTVariable> CloneVariable(std::shared_ptr<ASTVariable> node);
		std::shared_ptr<ASTType> CloneType(std::shared_ptr<ASTType> node);
		std::shared_ptr<ASTTypeSpecifier> CloneTypeSpec(std::shared_ptr<ASTTypeSpecifier> node);

		std::shared_ptr<Module> DestinationModule;
		std::unordered_map<std::string, Symbol> SubstitutionMap;
		std::vector<SemaSymbolTable> SymbolTable;
	};
}
