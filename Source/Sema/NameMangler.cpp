#include "Core/Log.h"
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
					return typeSpec->ResolvedType;
				});
		
		return MangleFunction(func->GetName(), funcArgs, func->ReturnTypeVal ? func->ReturnTypeVal : nullptr);
	}


	std::string NameMangler::MangleGeneric(llvm::StringRef name, llvm::ArrayRef<Symbol> args)
	{
		std::stringstream argStream;
		
		for (size_t i = 0; i < args.size(); i++)
		{
			if (args[i].Kind == SymbolKind::Type)
			{
				argStream << args[i].GetType()->GetHash();
			}
			else if (args[i].Kind == SymbolKind::Value)
			{
				std::string valStr;
				llvm::raw_string_ostream rso(valStr);
				args[i].GetLLVMValue()->print(rso);  
				rso.flush();
				argStream << valStr;  
			}
			
			if (i + 1 < args.size())
				argStream << ",";
		}

		return std::format("{}[{}]", std::string(name), argStream.str());
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
