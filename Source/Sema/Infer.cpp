#include "Infer.h"
#include "Core/Log.h"
#include "Symbols/Module.h"

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
			case ASTNodeType::BinaryExpression:
			{
				return InferTypeFromBinExpr(std::dynamic_pointer_cast<ASTBinaryExpression>(node));
			}
			default:
			{
				CLEAR_UNREACHABLE("unimplemented");
			}
		}

		return nullptr;
	}

	std::shared_ptr<Type> Infer::InferTypeFromBinExpr(std::shared_ptr<ASTBinaryExpression> binExpr)
	{
		std::shared_ptr<Type> lhsType = InferTypeFromNode(binExpr->LeftSide);
		std::shared_ptr<Type> rhsType = InferTypeFromNode(binExpr->RightSide);

		return GetCommonType(lhsType, rhsType);
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
