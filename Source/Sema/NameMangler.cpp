#include "NameMangling.h"
#include "AST/ASTNode.h"

#include <format>
#include <llvm/ADT/ArrayRef.h>

namespace clear 
{
	NameMangler::NameMangler(std::shared_ptr<Module> clearModule)
		: m_Module(clearModule)
	{
	}
	
	std::string NameMangler::MangleFunction(llvm::StringRef name, llvm::ArrayRef<std::shared_ptr<Type>> args, std::shared_ptr<Type> returnType)
	{
		std::string returnTypeHash = returnType ? returnType->GetHash() : "";
		std::string nameStr = std::string(name);

		return std::format("{}.{}.A{}.R{}", m_Module->GetName(), nameStr, FormatArgs(args), returnTypeHash);
	}

	std::string NameMangler::MangleClass(llvm::StringRef name, llvm::ArrayRef<std::shared_ptr<Type>> genericArgs)
	{
		std::string nameStr = std::string(name);
		return std::format("{}.{}.A{}", m_Module->GetName(), nameStr, FormatArgs(genericArgs));
	}

	std::string NameMangler::MangleFunctionFromNode(std::shared_ptr<ASTFunctionDefinition> func)
	{
		llvm::SmallVector<std::shared_ptr<Type>> funcArgs;
		std::transform(func->Arguments.begin(), func->Arguments.end(), std::back_inserter(funcArgs), [](std::shared_ptr<ASTVariableDeclaration> typeSpec)
				{
					return typeSpec->TypeResolver->ConstructedType.GetType();
				});
		
		return MangleFunction(func->GetName(), funcArgs, func->ReturnType ? func->ReturnType->ConstructedType.GetType() : nullptr);
	}


	std::string NameMangler::MangleGeneric(llvm::StringRef name, llvm::ArrayRef<std::shared_ptr<Type>> args)
	{
		return std::format("{}[{}]", std::string(name), FormatArgs(args));
	}

	std::string NameMangler::FormatArgs(llvm::ArrayRef<std::shared_ptr<Type>> args)
	{
		std::stringstream argStream;
	
		for (size_t i = 0; i < args.size(); i++)
		{
			argStream << args[i]->GetHash();
			
			if (i + 1 == args.size())
				continue;

			argStream << ",";	
		}

		return argStream.str();
	}

}
