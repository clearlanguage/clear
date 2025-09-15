#pragma once 

#include "AST/ASTNode.h"

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
		std::shared_ptr<ASTTypeSpecifier> CloneTypeSpec(std::shared_ptr<ASTTypeSpecifier> node);
		std::shared_ptr<ASTBinaryExpression> CloneBinaryExpr(std::shared_ptr<ASTBinaryExpression> node);
		std::shared_ptr<ASTUnaryExpression> CloneUnaryExpr(std::shared_ptr<ASTUnaryExpression> node);	
		std::shared_ptr<ASTNodeLiteral> CloneLiteral(std::shared_ptr<ASTNodeLiteral> node);
		std::shared_ptr<ASTBlock> CloneBlock(std::shared_ptr<ASTBlock> node);
		std::shared_ptr<ASTFunctionCall> CloneFunctionCall(std::shared_ptr<ASTFunctionCall> node);
		std::shared_ptr<ASTReturn> CloneReturn(std::shared_ptr<ASTReturn> node);
		std::shared_ptr<ASTStructExpr> CloneStructExpr(std::shared_ptr<ASTStructExpr> node);
		std::shared_ptr<ASTSubscript> CloneSubscript(std::shared_ptr<ASTSubscript> node);
		std::shared_ptr<ASTCastExpr> CloneCastExpr(std::shared_ptr<ASTCastExpr> node);
		std::shared_ptr<ASTSizeofExpr> CloneSizeofExpr(std::shared_ptr<ASTSizeofExpr> node);
		std::shared_ptr<ASTIsExpr> CloneIsExpr(std::shared_ptr<ASTIsExpr> node);
		std::shared_ptr<ASTAssignmentOperator> CloneAssignmentOperator(std::shared_ptr<ASTAssignmentOperator> node);
		std::shared_ptr<ASTArrayType> CloneArrayType(std::shared_ptr<ASTArrayType> node);
		std::shared_ptr<ASTIfExpression> CloneIfExpr(std::shared_ptr<ASTIfExpression> node);
		std::shared_ptr<ASTWhileExpression> CloneWhileLoop(std::shared_ptr<ASTWhileExpression> node);


		std::shared_ptr<Module> DestinationModule;
		std::unordered_map<std::string, Symbol> SubstitutionMap;
	};
}
