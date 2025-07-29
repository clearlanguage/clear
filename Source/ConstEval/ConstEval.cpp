#include "ConstEval.h"

#include "Core/Log.h"
#include "Core/Value.h"
#include "Symbols/Module.h"

#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/Support/Casting.h>
#include <memory>


namespace clear 
{
	ConstEval::ConstEval(std::shared_ptr<Module> module_)
		: m_ClearModule(module_)
	{
	}

	void ConstEval::Evaluate(std::shared_ptr<ASTExpression> expression)
	{
		CurrentValue = EvaluateExpr(expression);
	}
	
	llvm::Constant* ConstEval::EvaluateBinaryExpr(std::shared_ptr<ASTBinaryExpression> binaryExpr)
	{
		llvm::Constant* lhs = EvaluateGeneric(binaryExpr->LeftSide);
		llvm::Constant* rhs = EvaluateGeneric(binaryExpr->RightSide);

		if (!lhs || !rhs)
		{
			return nullptr;
		}

		switch (binaryExpr->GetExpression())
		{
			case OperatorType::Add:
			{
				return llvm::ConstantExpr::getAdd(lhs, rhs);
			}
			case OperatorType::Sub:
			{
				return llvm::ConstantExpr::getSub(lhs, rhs);
			}
			case OperatorType::Mul:
			{
				return llvm::ConstantExpr::getMul(lhs, rhs);
			}
			case OperatorType::Div:
			{
				if (lhs->getType()->isFloatingPointTy())
				{
					return llvm::ConstantExpr::get(llvm::Instruction::FDiv, lhs, rhs);
				}
				else
				{
					return llvm::ConstantExpr::get(llvm::Instruction::SDiv, lhs, rhs);
				}
			}
			case OperatorType::Mod:
			{
				if (lhs->getType()->isFloatingPointTy())
				{
					return llvm::ConstantExpr::get(llvm::Instruction::FRem, lhs, rhs);
				}
				else
				{
					return llvm::ConstantExpr::get(llvm::Instruction::SRem, lhs, rhs);
				}
			}
			case OperatorType::BitwiseAnd:
			{
				return llvm::ConstantExpr::get(llvm::Instruction::And, lhs, rhs);
			}
			case OperatorType::BitwiseOr:
			{
				return llvm::ConstantExpr::get(llvm::Instruction::Or, lhs, rhs);
			}
			case OperatorType::BitwiseXor:
			{
				return llvm::ConstantExpr::getXor(lhs, rhs);
			}
			case OperatorType::LeftShift:
			{
				return llvm::ConstantExpr::get(llvm::Instruction::Shl, lhs, rhs);
			}
			case OperatorType::RightShift:
			{
				return llvm::ConstantExpr::get(llvm::Instruction::AShr, lhs, rhs); // Or getLShr if unsigned
			}
			default:
			{
				CLEAR_UNREACHABLE("Unimplemented binary operator in constant evaluation");
			}
		}

		return nullptr;
	}

	llvm::Constant* ConstEval::EvaluateGeneric(std::shared_ptr<ASTNodeBase> node)
	{
		switch (node->GetType()) 
		{
 			case ASTNodeType::Literal:
			{
				std::shared_ptr<ASTNodeLiteral> literal = std::dynamic_pointer_cast<ASTNodeLiteral>(node);
				Value value(literal->GetData(), m_ClearModule->GetTypeFromToken(literal->GetData()), *m_ClearModule->GetContext(), *m_ClearModule->GetModule());

				return llvm::dyn_cast<llvm::Constant>(value.Get());
			}
 			case ASTNodeType::BinaryExpression:
			{
				return EvaluateBinaryExpr(std::dynamic_pointer_cast<ASTBinaryExpression>(node));
			}
 			case ASTNodeType::Expression:
			{
				return EvaluateExpr(std::dynamic_pointer_cast<ASTExpression>(node));
			}
 			case ASTNodeType::UnaryExpression:
			{
				return EvaluateUnaryExpr(std::dynamic_pointer_cast<ASTUnaryExpression>(node));
			}
			default:
			{
				CLEAR_UNREACHABLE("unimplemented");
			}
		}

		return nullptr;
	}

	llvm::Constant* ConstEval::EvaluateExpr(std::shared_ptr<ASTExpression> expr)
	{
		return EvaluateGeneric(expr->RootExpr);
	}

	llvm::Constant* ConstEval::EvaluateUnaryExpr(std::shared_ptr<ASTUnaryExpression> unaryExpr)
	{
		CLEAR_UNREACHABLE("unimplemented");
		return nullptr;
	}
}
