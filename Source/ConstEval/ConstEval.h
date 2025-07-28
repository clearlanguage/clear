#pragma once 

#include "AST/ASTNode.h"

namespace clear 
{
	struct ConstEval 
	{
		llvm::Constant* CurrentValue = nullptr;
		
		ConstEval() = default;
		~ConstEval() = default;
		
		void Evaluate(std::shared_ptr<ASTExpression> expression);
		void Add(llvm::Constant* other);
		void Sub(llvm::Constant* other);
		void Mul(llvm::Constant* other);
		void Div(llvm::Constant* other);
	}; 
}
