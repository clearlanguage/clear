#include "Cloner.h" 
#include "AST/ASTNode.h"
#include "Core/Log.h"
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
			case ASTNodeType::TypeSpecifier:			return CloneTypeSpec(std::dynamic_pointer_cast<ASTTypeSpecifier>(node));
			case ASTNodeType::BinaryExpression:			return CloneBinaryExpr(std::dynamic_pointer_cast<ASTBinaryExpression>(node));
			case ASTNodeType::UnaryExpression:			return CloneUnaryExpr(std::dynamic_pointer_cast<ASTUnaryExpression>(node));
			case ASTNodeType::Literal:					return CloneLiteral(std::dynamic_pointer_cast<ASTNodeLiteral>(node));
			case ASTNodeType::Block:					return CloneBlock(std::dynamic_pointer_cast<ASTBlock>(node));
			case ASTNodeType::FunctionCall:				return CloneFunctionCall(std::dynamic_pointer_cast<ASTFunctionCall>(node));
			case ASTNodeType::ReturnStatement:			return CloneReturn(std::dynamic_pointer_cast<ASTReturn>(node));
			case ASTNodeType::StructExpr:				return CloneStructExpr(std::dynamic_pointer_cast<ASTStructExpr>(node));
			case ASTNodeType::Subscript:				return CloneSubscript(std::dynamic_pointer_cast<ASTSubscript>(node));
			case ASTNodeType::CastExpr:					return CloneCastExpr(std::dynamic_pointer_cast<ASTCastExpr>(node));		
			case ASTNodeType::SizeofExpr:				return CloneSizeofExpr(std::dynamic_pointer_cast<ASTSizeofExpr>(node));		
			case ASTNodeType::IsExpr:					return CloneIsExpr(std::dynamic_pointer_cast<ASTIsExpr>(node));		
			case ASTNodeType::AssignmentOperator:		return CloneAssignmentOperator(std::dynamic_pointer_cast<ASTAssignmentOperator>(node));
			case ASTNodeType::ArrayType:				return CloneArrayType(std::dynamic_pointer_cast<ASTArrayType>(node));
			case ASTNodeType::IfExpression:				return CloneIfExpr(std::dynamic_pointer_cast<ASTIfExpression>(node));
			case ASTNodeType::WhileLoop:				return CloneWhileLoop(std::dynamic_pointer_cast<ASTWhileExpression>(node));
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
			newNode->ReturnType = Clone(node->ReturnType);
		
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
			newNode->TypeResolver = Clone(node->TypeResolver);
		
		if (node->Initializer)
			newNode->Initializer = Clone(node->Initializer);

		return newNode;
	}

	std::shared_ptr<ASTVariable> Cloner::CloneVariable(std::shared_ptr<ASTVariable> node)
	{
		std::shared_ptr<ASTVariable> newNode = std::make_shared<ASTVariable>(node->GetName());

		auto it = SubstitutionMap.find(node->GetName().GetData());
		if (it != SubstitutionMap.end())
			newNode->Variable = std::make_shared<Symbol>(it->second);
		
		return newNode;
	}
	
	std::shared_ptr<ASTTypeSpecifier> Cloner::CloneTypeSpec(std::shared_ptr<ASTTypeSpecifier> node)
	{
		std::shared_ptr<ASTTypeSpecifier> newNode = std::make_shared<ASTTypeSpecifier>(node->GetName());
		newNode->TypeResolver = Clone(node->TypeResolver);

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
		CLEAR_VERIFY(newNode->Operand, "failed to clone");

		return newNode;
	}

	std::shared_ptr<ASTNodeLiteral> Cloner::CloneLiteral(std::shared_ptr<ASTNodeLiteral> node)
	{
		std::shared_ptr<ASTNodeLiteral> newNode = std::make_shared<ASTNodeLiteral>(node->GetData());
		return newNode;
	}

	std::shared_ptr<ASTBlock> Cloner::CloneBlock(std::shared_ptr<ASTBlock> node)
	{	
		if (!node) return nullptr;
		
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

	std::shared_ptr<ASTStructExpr> Cloner::CloneStructExpr(std::shared_ptr<ASTStructExpr> node)
	{
		std::shared_ptr<ASTStructExpr> structExpr = std::make_shared<ASTStructExpr>();
		structExpr->TargetType = Clone(node->TargetType);
		
		for (auto arg : node->Values)
			structExpr->Values.push_back(Clone(arg));

		return structExpr;
	}

	std::shared_ptr<ASTSubscript> Cloner::CloneSubscript(std::shared_ptr<ASTSubscript> node)
	{
		std::shared_ptr<ASTSubscript> subscript = std::make_shared<ASTSubscript>();
		subscript->Meaning = node->Meaning;
		subscript->Target = Clone(node->Target);
		
		for (auto arg : node->SubscriptArgs)
			subscript->SubscriptArgs.push_back(Clone(arg));

		return subscript;
	}

	std::shared_ptr<ASTCastExpr> Cloner::CloneCastExpr(std::shared_ptr<ASTCastExpr> node)
	{
		std::shared_ptr<ASTCastExpr> castExpr = std::make_shared<ASTCastExpr>();
		castExpr->Object = Clone(node->Object);
		castExpr->TypeNode = Clone(node->TypeNode);
	
		return castExpr;
	}

	std::shared_ptr<ASTSizeofExpr> Cloner::CloneSizeofExpr(std::shared_ptr<ASTSizeofExpr> node)
	{
		std::shared_ptr<ASTSizeofExpr> sizeofExpr = std::make_shared<ASTSizeofExpr>();
		sizeofExpr->Object = Clone(node->Object);
		
		return sizeofExpr;
	}

	std::shared_ptr<ASTIsExpr> Cloner::CloneIsExpr(std::shared_ptr<ASTIsExpr> node)
	{
		std::shared_ptr<ASTIsExpr> isExpr = std::make_shared<ASTIsExpr>();
		isExpr->Object = Clone(node->Object);
		isExpr->TypeNode = Clone(node->TypeNode);
		
		return isExpr;
	}

	std::shared_ptr<ASTAssignmentOperator> Cloner::CloneAssignmentOperator(std::shared_ptr<ASTAssignmentOperator> node)
	{
		std::shared_ptr<ASTAssignmentOperator> operator_ = std::make_shared<ASTAssignmentOperator>(node->GetAssignType());

		operator_->Storage = Clone(node->Storage);
		operator_->Value = Clone(node->Value);

		return operator_;
	}

	std::shared_ptr<ASTArrayType> Cloner::CloneArrayType(std::shared_ptr<ASTArrayType> node)
	{
		std::shared_ptr<ASTArrayType> arrayType = std::make_shared<ASTArrayType>();
		arrayType->TypeNode = Clone(node->TypeNode);
		arrayType->SizeNode = Clone(node->SizeNode);

		return arrayType;
	}

	std::shared_ptr<ASTIfExpression> Cloner::CloneIfExpr(std::shared_ptr<ASTIfExpression> node)
	{
		std::shared_ptr<ASTIfExpression> ifExpr = std::make_shared<ASTIfExpression>();

		for (const auto& block : node->ConditionalBlocks)
			ifExpr->ConditionalBlocks.push_back({ .Condition = Clone(block.Condition), .CodeBlock = CloneBlock(block.CodeBlock) });
		
		ifExpr->ElseBlock = CloneBlock(node->ElseBlock);
		return ifExpr;
	}

	std::shared_ptr<ASTWhileExpression> Cloner::CloneWhileLoop(std::shared_ptr<ASTWhileExpression> node) {
		std::shared_ptr<ASTWhileExpression> whileLoop = std::make_shared<ASTWhileExpression>();


		ConditionalBlock block = { .Condition = Clone(node->WhileBlock.Condition), .CodeBlock = CloneBlock(node->WhileBlock.CodeBlock) };

		whileLoop->WhileBlock = block;

		return whileLoop;
	}


}
