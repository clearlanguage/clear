#pragma once 

#include "AST/ASTNode.h"

#include <type_traits>

namespace clear 
{
	class ConstEval 
	{
	public:
		ConstEval(std::shared_ptr<Module> module_);
		~ConstEval() = default;
		
		void Evaluate(std::shared_ptr<ASTNodeBase> expression);
	
		template<typename T>
		T GetValue() requires std::is_arithmetic_v<T>
		{
			CLEAR_VERIFY(CurrentValue, "cannot access null");
		
			if (auto currentValue = llvm::dyn_cast<llvm::ConstantInt>(CurrentValue))
			{
				if constexpr (std::is_signed_v<T>)
					return (T)currentValue->getSExtValue();

				return (T)currentValue->getZExtValue();
			}
			
			if (auto currentValue = llvm::dyn_cast<llvm::ConstantFP>(CurrentValue))
			{
				return (T)currentValue->getValueAPF().convertToDouble();
			}
		
			
			CLEAR_UNREACHABLE("unimplemented");
			return T {};
		}

	public:
		llvm::Constant* CurrentValue = nullptr;

	private:
		llvm::Constant* EvaluateBinaryExpr(std::shared_ptr<ASTBinaryExpression> binExpr);
		llvm::Constant* EvaluateUnaryExpr(std::shared_ptr<ASTUnaryExpression> unaryExpr);
		//llvm::Constant* EvaluateTernaryExpr(std::shared_ptr<ASTTernaryExpression> ternaryExpr);
		llvm::Constant* EvaluateExpr(std::shared_ptr<ASTExpression> expr);
		
		llvm::Constant* EvaluateGeneric(std::shared_ptr<ASTNodeBase> node);

	private:
		std::shared_ptr<Module> m_ClearModule;
	}; 
}
