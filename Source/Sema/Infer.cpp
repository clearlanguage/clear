#include "Infer.h"
#include "AST/ASTNode.h"
#include "Core/Log.h"
#include "Symbols/Module.h"
#include <llvm/Support/CommandLine.h>
#include <memory>

namespace clear 
{
	Infer::Infer(std::shared_ptr<Module> clearModule)
		: m_Module(clearModule)
	{
	}

	std::shared_ptr<Type> Infer::InferTypeFromNode(std::shared_ptr<ASTNodeBase> node)
	{
		switch (node->GetType()) 
		{
			case ASTNodeType::Literal:
			{
				std::shared_ptr<ASTNodeLiteral> literal = std::dynamic_pointer_cast<ASTNodeLiteral>(node);
				return m_Module->GetTypeFromToken(literal->GetData());
			}
			case ASTNodeType::Variable:
			{
				std::shared_ptr<ASTVariable> variable = std::dynamic_pointer_cast<ASTVariable>(node);
				return variable->Variable->GetType();
			}
			case ASTNodeType::BinaryExpression:
			{
				return InferTypeFromBinExpr(std::dynamic_pointer_cast<ASTBinaryExpression>(node));
			}
			case ASTNodeType::Expression:
			{
				return InferTypeFromExpr(std::dynamic_pointer_cast<ASTExpression>(node));
			}
			case ASTNodeType::FunctionCall:
			{
				return InferTypeFromFunctionCall(std::dynamic_pointer_cast<ASTFunctionCall>(node));
			}
			case ASTNodeType::UnaryExpression:
			{
				return InferTypeFromUnaryExpr(std::dynamic_pointer_cast<ASTUnaryExpression>(node));
			}
			case ASTNodeType::StructExpr:
			{
				auto structExpr = std::dynamic_pointer_cast<ASTStructExpr>(node);
				
				switch (structExpr->TargetType->GetType())
				{
					case ASTNodeType::TypeResolver:
					{
						std::shared_ptr<ASTType> type = std::dynamic_pointer_cast<ASTType>(structExpr->TargetType);
						return type->ConstructedType.GetType();
					}
					case ASTNodeType::Variable:
					{
						std::shared_ptr<ASTVariable> variable = std::dynamic_pointer_cast<ASTVariable>(structExpr->TargetType);
						return variable->Variable->GetType();
					}
					default:
					{
						CLEAR_UNREACHABLE("unimplemented");
					}
				}

				return nullptr;
			}
			case ASTNodeType::Load:
			{
				auto load = std::dynamic_pointer_cast<ASTLoad>(node);
				return InferTypeFromNode(load->Operand);
			}
			case ASTNodeType::Subscript:
			{
				std::shared_ptr<ASTSubscript> subscript = std::dynamic_pointer_cast<ASTSubscript>(node);

				if (subscript->Meaning == SubscriptSemantic::ArrayIndex)
				{
					std::shared_ptr<Type> type = InferTypeFromNode(subscript->Target);
					
					for (int64_t i = subscript->SubscriptArgs.size(); i > 0; i--)
						type = type->IsArray() ? type->As<ArrayType>()->GetBaseType() : type->As<PointerType>()->GetBaseType();
 
					return type; 		
				}
			}
			default:
			{
				CLEAR_UNREACHABLE("unimplemented");
			}
		}

		return nullptr;
	}

	std::shared_ptr<Type> Infer::InferTypeFromUnaryExpr(std::shared_ptr<ASTUnaryExpression> unaryExpr)
	{
		std::shared_ptr<Type> base = InferTypeFromNode(unaryExpr->Operand);
	
		if(unaryExpr->GetOperatorType() == OperatorType::Address)
			return m_Module->GetTypeRegistry()->GetPointerTo(base);
		
		if(unaryExpr->GetOperatorType() == OperatorType::Dereference)
			return base->As<PointerType>()->GetBaseType();

		return base;
	}

	std::shared_ptr<Type> Infer::InferTypeFromBinExpr(std::shared_ptr<ASTBinaryExpression> binExpr)
	{
		if (binExpr->ResultantType)
			return binExpr->ResultantType;

		std::shared_ptr<Type> lhsType = InferTypeFromNode(binExpr->LeftSide);
		std::shared_ptr<Type> rhsType = InferTypeFromNode(binExpr->RightSide);
	
		binExpr->ResultantType = GetCommonType(lhsType, rhsType);
		return binExpr->ResultantType;
	}

	std::shared_ptr<Type> Infer::InferTypeFromFunctionCall(std::shared_ptr<ASTFunctionCall> funcCall)
	{
		return InferTypeFromNode(funcCall->Callee);
	}

	std::shared_ptr<Type> Infer::InferTypeFromExpr(std::shared_ptr<ASTExpression> expr)
	{
		return InferTypeFromNode(expr->RootExpr);
	}
	
	std::shared_ptr<Type> Infer::GetCommonType(std::shared_ptr<Type> type1, std::shared_ptr<Type> type2)
	{
		if (type1 == type2)
			return type1;

		if (type1->IsFloatingPoint() && type2->IsFloatingPoint())
		{
			return type1->GetSizeInBytes(*m_Module->GetModule()) > type2->GetSizeInBytes(*m_Module->GetModule()) ? type1 : type2;
		}

		if (type1->IsIntegral() && type2->IsIntegral())
		{
			return type1->GetSizeInBytes(*m_Module->GetModule()) > type2->GetSizeInBytes(*m_Module->GetModule()) ? type1 : type2;
		}

		if (type1->IsIntegral() && type2->IsFloatingPoint())
		{
			return type2;
		}

		if (type1->IsFloatingPoint() && type2->IsIntegral())
		{
			return type1;
		}
		
		return nullptr;
	}
}
