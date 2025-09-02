#include "Cloner.h"
#include "Core/Log.h"

namespace clear {
		
	std::shared_ptr<ASTNodeBase> Cloner::Clone(std::shared_ptr<ASTNodeBase> node)
	{
		switch (node->GetType()) 
		{
			case ASTNodeType::Class:					return CloneClass(std::dynamic_pointer_cast<ASTClass>(node));
			case ASTNodeType::FunctionDefinition:		return CloneFunction(std::dynamic_pointer_cast<ASTFunctionDefinition>(node));
			case ASTNodeType::VariableDecleration:		return CloneVariableDecl(std::dynamic_pointer_cast<ASTVariableDeclaration>(node));
			case ASTNodeType::Variable:					return CloneVariable(std::dynamic_pointer_cast<ASTVariable>(node));
			case ASTNodeType::TypeResolver:				return CloneType(std::dynamic_pointer_cast<ASTType>(node));
			case ASTNodeType::TypeSpecifier:			return CloneTypeSpec(std::dynamic_pointer_cast<ASTTypeSpecifier>(node));
			default:
				break;
		}	

		CLEAR_UNREACHABLE("unimplemented");
		return nullptr;
	}

	std::shared_ptr<ASTClass> Cloner::CloneClass(std::shared_ptr<ASTClass> node)
	{
		return node;
	}
	
	std::shared_ptr<ASTFunctionDefinition> Cloner::CloneFunction(std::shared_ptr<ASTFunctionDefinition> node)
	{
		return node;
	}
	
	std::shared_ptr<ASTVariableDeclaration> Cloner::CloneVariableDecl(std::shared_ptr<ASTVariableDeclaration> node)
	{
		return node;
	}

	std::shared_ptr<ASTVariable> Cloner::CloneVariable(std::shared_ptr<ASTVariable> node)
	{
		return node;
	}
	
	std::shared_ptr<ASTType> Cloner::CloneType(std::shared_ptr<ASTType> node)
	{
		return node;
	}

	std::shared_ptr<ASTTypeSpecifier> Cloner::CloneTypeSpec(std::shared_ptr<ASTTypeSpecifier> node)
	{
		return node;
	}
}
