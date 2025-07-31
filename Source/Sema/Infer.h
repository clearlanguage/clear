#pragma once 

#include "AST/ASTNode.h"
#include "Symbols/Module.h"

#include <memory>

namespace clear 
{
	class Infer
	{
	public:
		Infer(std::shared_ptr<Module> clearModule);	
	
		std::shared_ptr<Type> InferTypeFromNode(std::shared_ptr<ASTNodeBase> node);
		std::shared_ptr<Type> InferTypeFromExpr(std::shared_ptr<ASTExpression> expr);
		std::shared_ptr<Type> InferTypeFromBinExpr(std::shared_ptr<ASTBinaryExpression> binExpr);
		std::shared_ptr<Type> InferTypeFromFunctionCall(std::shared_ptr<ASTFunctionCall> funcCall);

		std::shared_ptr<Type> GetCommonType(std::shared_ptr<Type> type1, std::shared_ptr<Type> type2);	

		std::shared_ptr<Type> GetResult() { return m_InferredType; }

	private:
		std::shared_ptr<Type> m_InferredType;
		std::shared_ptr<Module> m_Module;
	};
}
