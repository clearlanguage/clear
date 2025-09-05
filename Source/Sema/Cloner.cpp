#include "Cloner.h" 
#include "Core/Operator.h"
#include <memory>

namespace clear {

	std::shared_ptr<ASTNodeBase> Cloner::Clone(std::shared_ptr<ASTNodeBase> node)
	{
		if (!node)
			return nullptr;

		switch (node->GetType()) 
		{
			case ASTNodeType::Class:					return CloneClass(std::dynamic_pointer_cast<ASTClass>(node));
			case ASTNodeType::FunctionDefinition:		return CloneFunction(std::dynamic_pointer_cast<ASTFunctionDefinition>(node));
			case ASTNodeType::VariableDecleration:		return CloneVariableDecl(std::dynamic_pointer_cast<ASTVariableDeclaration>(node));
			case ASTNodeType::Variable:					return CloneVariable(std::dynamic_pointer_cast<ASTVariable>(node));
			case ASTNodeType::TypeResolver:				return CloneType(std::dynamic_pointer_cast<ASTType>(node));
			case ASTNodeType::TypeSpecifier:			return CloneTypeSpec(std::dynamic_pointer_cast<ASTTypeSpecifier>(node));
			case ASTNodeType::Expression:				return CloneExpr(std::dynamic_pointer_cast<ASTExpression>(node));
			case ASTNodeType::BinaryExpression:			return CloneBinaryExpr(std::dynamic_pointer_cast<ASTBinaryExpression>(node));
			case ASTNodeType::UnaryExpression:			return CloneUnaryExpr(std::dynamic_pointer_cast<ASTUnaryExpression>(node));
			case ASTNodeType::Literal:					return CloneLiteral(std::dynamic_pointer_cast<ASTNodeLiteral>(node));
			case ASTNodeType::Block:					return CloneBlock(std::dynamic_pointer_cast<ASTBlock>(node));
			case ASTNodeType::FunctionCall:				return CloneFunctionCall(std::dynamic_pointer_cast<ASTFunctionCall>(node));
			case ASTNodeType::ReturnStatement:			return CloneReturn(std::dynamic_pointer_cast<ASTReturn>(node));
			default: break;
	}	

		CLEAR_UNREACHABLE("unimplemented");
		return nullptr;
	}

	std::shared_ptr<ASTClass> Cloner::CloneClass(std::shared_ptr<ASTClass> node)
	{
		std::shared_ptr<ASTClass> newClass = std::make_shared<ASTClass>(node->GetName());

		for (auto member : node->Members)
			newClass->Members.push_back(CloneTypeSpec(member));
	
		for (auto func : node->MemberFunctions)
			newClass->MemberFunctions.push_back(CloneFunction(func));
		
		for (auto value : node->DefaultValues)
			newClass->DefaultValues.push_back(Clone(value));

		return newClass;
	}
	
	std::shared_ptr<ASTFunctionDefinition> Cloner::CloneFunction(std::shared_ptr<ASTFunctionDefinition> node)
	{
		std::shared_ptr<ASTFunctionDefinition> newNode = std::make_shared<ASTFunctionDefinition>(node->GetName());

		newNode->CodeBlock = CloneBlock(node->CodeBlock);		
		
		if (node->ReturnType)
			newNode->ReturnType = CloneType(node->ReturnType);
		
		for (auto arg : node->Arguments)
			newNode->Arguments.push_back(CloneVariableDecl(arg));

		newNode->SourceModule = DestinationModule;
		newNode->Linkage = node->Linkage;

		return newNode;
	}
	
	std::shared_ptr<ASTVariableDeclaration> Cloner::CloneVariableDecl(std::shared_ptr<ASTVariableDeclaration> node)
	{
		std::shared_ptr<ASTVariableDeclaration> newNode = std::make_shared<ASTVariableDeclaration>(node->GetName());
		
		if (node->TypeResolver)
			newNode->TypeResolver = CloneType(node->TypeResolver);
		
		if (node->Initializer)
			newNode->Initializer = Clone(newNode->Initializer);

		return newNode;
	}

	std::shared_ptr<ASTVariable> Cloner::CloneVariable(std::shared_ptr<ASTVariable> node)
	{
		std::string name = SubstitutionMap.contains(node->GetName().GetData()) ? SubstitutionMap[node->GetName().GetData()] : node->GetName().GetData();
		Token tok = node->GetName();
		tok.SetData(name);

		std::shared_ptr<ASTVariable> newNode = std::make_shared<ASTVariable>(tok);
		return newNode;
	}
	
	std::shared_ptr<ASTType> Cloner::CloneType(std::shared_ptr<ASTType> node)
	{
		std::shared_ptr<ASTType> newNode = std::make_shared<ASTType>();

		for (const auto& token : node->GetTokens())
		{
			if (!token.IsType(TokenType::Identifier))
			{
				newNode->PushToken(token);
				continue;
			}

			auto it = SubstitutionMap.find(token.GetData());

			if (it == SubstitutionMap.end())
				newNode->PushToken(token);
			else 
				newNode->PushToken(Token(token.GetType(), it->second, token.GetSourceFile(), token.LineNumber, token.ColumnNumber));
		}

		for (auto child : node->Children)
			newNode->Children.push_back(Clone(child));

		return newNode;
	}

	std::shared_ptr<ASTTypeSpecifier> Cloner::CloneTypeSpec(std::shared_ptr<ASTTypeSpecifier> node)
	{
		std::shared_ptr<ASTTypeSpecifier> newNode = std::make_shared<ASTTypeSpecifier>(node->GetName());
		newNode->TypeResolver = CloneType(node->TypeResolver);

		return newNode;
	}

	std::shared_ptr<ASTExpression> Cloner::CloneExpr(std::shared_ptr<ASTExpression> node)
	{
		std::shared_ptr<ASTExpression> newNode = std::make_shared<ASTExpression>();
		newNode->RootExpr = Clone(node->RootExpr);
		return newNode;
	}

	std::shared_ptr<ASTBinaryExpression> Cloner::CloneBinaryExpr(std::shared_ptr<ASTBinaryExpression> node)
	{
		std::shared_ptr<ASTBinaryExpression> newNode = std::make_shared<ASTBinaryExpression>(node->GetExpression());
		newNode->LeftSide = Clone(node->LeftSide);
		newNode->RightSide = Clone(node->RightSide);
		
		return newNode;
	}

	std::shared_ptr<ASTUnaryExpression> Cloner::CloneUnaryExpr(std::shared_ptr<ASTUnaryExpression> node)	
	{
		std::shared_ptr<ASTUnaryExpression> newNode = std::make_shared<ASTUnaryExpression>(node->GetOperatorType());
		newNode->Operand = Clone(node->Operand);
		return newNode;
	}

	std::shared_ptr<ASTNodeLiteral> Cloner::CloneLiteral(std::shared_ptr<ASTNodeLiteral> node)
	{
		std::shared_ptr<ASTNodeLiteral> newNode = std::make_shared<ASTNodeLiteral>(node->GetData());
		return newNode;
	}

	std::shared_ptr<ASTBlock> Cloner::CloneBlock(std::shared_ptr<ASTBlock> node)
	{
		std::shared_ptr<ASTBlock> newNode = std::make_shared<ASTBlock>();
		for (auto child : node->Children)
			newNode->Children.push_back(Clone(child));
		
		return newNode;
	}

	std::shared_ptr<ASTFunctionCall> Cloner::CloneFunctionCall(std::shared_ptr<ASTFunctionCall> node)
	{
		std::shared_ptr<ASTFunctionCall> funcCall = std::make_shared<ASTFunctionCall>();
		funcCall->Callee = Clone(node->Callee);
		
		for (auto arg : node->Arguments)
			funcCall->Arguments.push_back(Clone(arg));
		
		return funcCall;
	}

	std::shared_ptr<ASTReturn> Cloner::CloneReturn(std::shared_ptr<ASTReturn> node)
	{
		std::shared_ptr<ASTReturn> returnStatement = std::make_shared<ASTReturn>();
		
		if (node->ReturnValue)
			returnStatement->ReturnValue = Clone(node->ReturnValue);

		return returnStatement;
	}
}
