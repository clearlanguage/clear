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
		if (!node)
			return nullptr;

		switch (node->GetType()) 
		{
			case ASTNodeType::Zero:			return std::dynamic_pointer_cast<ASTZero>(node)->ValueType;
			case ASTNodeType::Contains:		return m_Module->Lookup("bool").value()->GetType();
			case ASTNodeType::FunctionRef:	return std::dynamic_pointer_cast<ASTFunctionRef>(node)->FunctionTy;
			case ASTNodeType::VariantConstruct: return std::dynamic_pointer_cast<ASTVariantConstruct>(node)->VariantTy;
			case ASTNodeType::VariantTag:	return std::dynamic_pointer_cast<ASTVariantTag>(node)->TagType;
			case ASTNodeType::UnionConstruct: return std::dynamic_pointer_cast<ASTUnionConstruct>(node)->UnionTy;
			case ASTNodeType::VariantField:
			{
				auto field = std::dynamic_pointer_cast<ASTVariantField>(node);
				return field->VariantTy->As<ClassType>()->Cases[field->CaseIndex].Fields[field->FieldIndex].second;
			}
			case ASTNodeType::OptionalUnwrap:
			case ASTNodeType::OptionalValueOr:
			{
				auto optional = node->GetType() == ASTNodeType::OptionalUnwrap ? std::dynamic_pointer_cast<ASTOptionalUnwrap>(node)->OptionalTy 
																			   : std::dynamic_pointer_cast<ASTOptionalValueOr>(node)->OptionalTy;
				auto classType = optional->As<ClassType>();
				return classType->Cases[classType->FindCase("some").value()].Fields[0].second;
			}
			case ASTNodeType::Lambda:		return nullptr; // not analysed yet
			case ASTNodeType::TupleExpr:	return std::dynamic_pointer_cast<ASTTupleExpr>(node)->TupleTy;
			case ASTNodeType::TupleGet:
			{
				auto get = std::dynamic_pointer_cast<ASTTupleGet>(node);
				return get->TupleTy->As<TupleType>()->GetElements()[get->Index];
			}
			case ASTNodeType::Intrinsic:	return std::dynamic_pointer_cast<ASTIntrinsic>(node)->ResultType;
			case ASTNodeType::Slot:			return std::dynamic_pointer_cast<ASTSlot>(node)->ValueType;
			case ASTNodeType::Construct:	return std::dynamic_pointer_cast<ASTConstruct>(node)->ClassTy;
			case ASTNodeType::Temporary:
			{
				auto temporary = std::dynamic_pointer_cast<ASTTemporary>(node);
				return m_Module->GetTypeRegistry()->GetPointerTo(temporary->ValueType);
			}
			case ASTNodeType::ConstantValue:
			{
				return std::dynamic_pointer_cast<ASTConstantValue>(node)->ValueType;
			}
			case ASTNodeType::SizeofExpr:
			{
				return m_Module->Lookup("uint64").value()->GetType();
			}
			case ASTNodeType::IsExpr:
			{
				return m_Module->Lookup("bool").value()->GetType();
			}
			case ASTNodeType::CastExpr:
			{
				std::shared_ptr<ASTCastExpr> castExpr = std::dynamic_pointer_cast<ASTCastExpr>(node);
				return castExpr->TargetType;
			}
			case ASTNodeType::Literal:
			{
				std::shared_ptr<ASTNodeLiteral> literal = std::dynamic_pointer_cast<ASTNodeLiteral>(node);
				return m_Module->GetTypeFromToken(literal->GetData());
			}
			case ASTNodeType::Variable:
			{
				std::shared_ptr<ASTVariable> variable = std::dynamic_pointer_cast<ASTVariable>(node);
				auto& symbol = variable->Variable;

				if (!symbol)
					return nullptr;

				// a variable from a file that was already compiled holds its storage (a pointer to the value)
				if (symbol->Kind == SymbolKind::Value && symbol->GetLLVMValue() && symbol->GetType()->IsPointer())
					return symbol->GetType()->As<PointerType>()->GetBaseType();

				return symbol->GetType();
			}
			case ASTNodeType::BinaryExpression:
			{
				return InferTypeFromBinExpr(std::dynamic_pointer_cast<ASTBinaryExpression>(node));
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
					case ASTNodeType::Variable:
					{
						std::shared_ptr<ASTVariable> variable = std::dynamic_pointer_cast<ASTVariable>(structExpr->TargetType);
						return variable->Variable->GetType();
					}
					case ASTNodeType::Subscript:
					{
						std::shared_ptr<ASTSubscript> subscript = std::dynamic_pointer_cast<ASTSubscript>(structExpr->TargetType);
						return subscript->GeneratedType->GetType();
					}
					default:
					{
						return InferTypeFromNode(structExpr->TargetType);
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

					if (!type)
						return nullptr;

					if (type->IsClass())
					{
						auto clsType = std::dynamic_pointer_cast<ClassType>(type);
						CLEAR_VERIFY(clsType->MemberFunctions.contains("__getitem__"), "class has no __getitem__ ", clsType->GetHash());
						auto f = clsType->MemberFunctions.at("__getitem__");
						auto returnType =  f->GetFunctionSymbol().FunctionNode->ReturnTypeVal;
						return returnType;

					}
					for (int64_t i = subscript->SubscriptArgs.size(); i > 0; i--)
						type = type->IsArray() ? type->As<ArrayType>()->GetBaseType() : type->As<PointerType>()->GetBaseType();
 
					return type; 		
				}

				return subscript->GeneratedType->GetType();
			}
			case ASTNodeType::ListExpr:
			{
				std::shared_ptr<ASTListExpr> listExpr = std::dynamic_pointer_cast<ASTListExpr>(node);
				return listExpr->ListType;
			}
			case ASTNodeType::TernaryExpression:
			{
				std::shared_ptr<ASTTernaryExpression> ternaryExpr = std::dynamic_pointer_cast<ASTTernaryExpression>(node);
				return InferTypeFromNode(ternaryExpr->Truthy);
			}
			case ASTNodeType::Import:
			{
				return nullptr;
			}

			case ASTNodeType::FunctionDefinition: {
				auto funcDef = std::dynamic_pointer_cast<ASTFunctionDefinition>(node);
				return funcDef->ReturnTypeVal;
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

		if (!base)
			return nullptr;
	
		switch (unaryExpr->GetOperatorType())
		{
			case OperatorType::Address:		return m_Module->GetTypeRegistry()->GetPointerTo(base);
			case OperatorType::Dereference: return base->IsPointer() ? base->As<PointerType>()->GetBaseType() : nullptr;
			case OperatorType::Not:			return m_Module->Lookup("bool").value()->GetType();
			case OperatorType::Negation:
			{
				// -x on an unsigned value produces the signed type of the same width
				if (base->IsIntegral() && !base->IsSigned() && base->GetHash() != "bool")
					return m_Module->Lookup(base->GetHash().substr(1)).value()->GetType();

				return base;
			}
			default:						return base;
		}
	}

	std::shared_ptr<Type> Infer::InferTypeFromBinExpr(std::shared_ptr<ASTBinaryExpression> binExpr)
	{
		if (binExpr->ResultantType)
			return binExpr->ResultantType;

		// member access (TODO need to handle all the cases such as members, artihemtic etc... seperately)
		if (binExpr->GetExpression() == OperatorType::Dot)
		{
			std::shared_ptr<Type> leftType = InferTypeFromNode(binExpr->LeftSide);

			while (leftType && leftType->IsPointer())
				leftType = leftType->As<PointerType>()->GetBaseType();

			if (!leftType || !leftType->IsClass())
				return nullptr;

			std::shared_ptr<ClassType> lhsType = leftType->As<ClassType>();
			std::shared_ptr<ASTVariable> rhs = std::dynamic_pointer_cast<ASTVariable>(binExpr->RightSide);
			
			std::shared_ptr<Symbol> member = lhsType->GetMember(rhs->GetName().GetData()).value_or(nullptr);
			CLEAR_VERIFY(member, "not a valid member");
			
			if (member->Kind == SymbolKind::Type)
			{
				return member->GetType();
			}
			else if (member->Kind == SymbolKind::Function)
			{
				auto& fn = member->GetFunctionSymbol();
				return fn.FunctionNode->ReturnTypeVal;
			}
			else 
			{
				CLEAR_UNREACHABLE("");
			}
		}

		std::shared_ptr<Type> lhsType = InferTypeFromNode(binExpr->LeftSide);
		std::shared_ptr<Type> rhsType = InferTypeFromNode(binExpr->RightSide);

		if (!lhsType || !rhsType)
			return nullptr;

		// shifts keep the type of the value being shifted
		if (binExpr->GetExpression() == OperatorType::LeftShift || binExpr->GetExpression() == OperatorType::RightShift)
		{
			binExpr->ResultantType = lhsType;
			return lhsType;
		}
		
		if (lhsType->IsPointer() && rhsType->IsIntegral())
		{
			binExpr->ResultantType = lhsType;
			return binExpr->ResultantType;
		}

		binExpr->ResultantType = GetCommonType(lhsType, rhsType);
		return binExpr->ResultantType;
	}

	std::shared_ptr<Type> Infer::InferTypeFromFunctionCall(std::shared_ptr<ASTFunctionCall> funcCall)
	{
		if (funcCall->IsBuiltinPrint)
			return m_Module->Lookup("void").value()->GetType();

		if (funcCall->IndirectType)
			return funcCall->IndirectType->As<FunctionPointerType>()->GetReturnType();

		return InferTypeFromNode(funcCall->Callee);
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
