#pragma once 

#include "Symbols/Module.h"
#include <llvm/ADT/ArrayRef.h>
#include <memory>

namespace clear 
{
	
	class ASTFunctionDefinition;

	class NameMangler
	{
	public:
		NameMangler(std::shared_ptr<Module> clearModule);
	
		std::string MangleFunction(llvm::StringRef name, llvm::ArrayRef<std::shared_ptr<Type>> args, std::shared_ptr<Type> returnType);
		std::string MangleClass(llvm::StringRef name, llvm::ArrayRef<std::shared_ptr<Type>> genericArguments);
		// std::string MangleVariant(), MangleEnum() etc...
		
		std::string MangleFunctionFromNode(std::shared_ptr<ASTFunctionDefinition> definition);
		
		std::string MangleGeneric(llvm::StringRef name, llvm::ArrayRef<std::shared_ptr<Type>> args);

	private:
		std::string FormatArgs(llvm::ArrayRef<std::shared_ptr<Type>> args);	

	private:
		std::shared_ptr<Module> m_Module;
	};
}
