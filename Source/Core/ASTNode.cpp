#include "ASTNode.h"

#include "API/LLVM/LLVMBackend.h"

#include <iostream>

namespace clear {

	void ASTNodeBase::PushChild(const std::shared_ptr<ASTNodeBase>& child)
	{
		m_Children.push_back(child);
	}
	void ASTNodeBase::RemoveChild(const std::shared_ptr<ASTNodeBase>& child)
	{
		auto it = std::find(m_Children.begin(), m_Children.end(), child);
		if (it != m_Children.end())
			m_Children.erase(it);
	}
	void ASTNodeBase::SetParent(const std::shared_ptr<ASTNodeBase>& parent)
	{
		m_Parent = parent;
	}
	void ASTNodeBase::RemoveParent()
	{
		m_Parent.reset();
	}
	ASTNodeLiteral::ASTNodeLiteral(LiteralType type, const std::string& data)
		: m_Type(type), m_Data(data)
	{
	}
	llvm::Value* ASTNodeLiteral::Codegen()
	{
		auto& context = *LLVM::Backend::GetContext();

		//TODO: add strings

		switch (m_Type)
		{
			case LiteralType::Int8:	   return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),   (int8_t)std::stoi(m_Data),     true);
			case LiteralType::Int16:   return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context),  (int16_t)std::stoi(m_Data),    true);
			case LiteralType::Int32:   return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),  (int32_t)std::stoi(m_Data),    true);
			case LiteralType::Int64:   return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context),  (int64_t)std::stoll(m_Data),   true);
			case LiteralType::Uint8:   return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),   (uint8_t)std::stoull(m_Data),  false);
			case LiteralType::Uint16:  return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context),  (uint16_t)std::stoull(m_Data), false);
			case LiteralType::Uint32:  return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),  (uint32_t)std::stoull(m_Data), false);
			case LiteralType::Uint64:  return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context),  (uint64_t)std::stoull(m_Data), false);
			case LiteralType::Float32: return llvm::ConstantFP::get(llvm::Type::getFloatTy(context),   (float)std::stod(m_Data));
			case LiteralType::Float64: return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context),  (double)std::stod(m_Data));
			case LiteralType::None:	
			default:
				return nullptr;
		}
	}
	ASTBinaryExpression::ASTBinaryExpression(BinaryExpressionType type)
		: m_Expression(type)
	{
	}
	llvm::Value* ASTBinaryExpression::Codegen()
	{
		// Assumes the two values in its children are to be added in order
		auto& children = GetChildren();

		if (children.size() != 2)
			return nullptr;

		llvm::Value* LHS = children[0]->Codegen();
		llvm::Value* RHS = children[1]->Codegen();


		if (!LHS || !RHS)
			return nullptr;

		if (_IsMathExpression())
			return _CreateMathExpression(LHS, RHS);
		else
			return _CreateCmpExpression(LHS, RHS);

		return nullptr;
	}

	const bool ASTBinaryExpression::_IsMathExpression() const
	{
		return (int)m_Expression <= (int)BinaryExpressionType::Mod;
	}

	llvm::Value* ASTBinaryExpression::_CreateMathExpression(llvm::Value* LHS, llvm::Value* RHS)
	{
		auto& builder = LLVM::Backend::GetBuilder();
		const bool isFloat = LHS->getType()->isFloatingPointTy();

		switch (m_Expression)
		{
			case BinaryExpressionType::Add:
				return isFloat ? builder->CreateFAdd(LHS, RHS, "faddtmp")
							   : builder->CreateAdd(LHS, RHS, "addtmp");

			case BinaryExpressionType::Sub:
				return isFloat ? builder->CreateFSub(LHS, RHS, "fsubtmp")
							   : builder->CreateSub(LHS, RHS, "subtmp");

			case BinaryExpressionType::Mul:
				return isFloat ? builder->CreateFMul(LHS, RHS, "fmultmp")
							   : builder->CreateMul(LHS, RHS, "multmp");

			case BinaryExpressionType::Div:
				return isFloat ? builder->CreateFDiv(LHS, RHS, "fdivtmp")
							   : builder->CreateSDiv(LHS, RHS, "divtmp");

			case BinaryExpressionType::Mod:
				if (!isFloat)
					return builder->CreateSRem(LHS, RHS, "modtmp");

				break;
			default:
				break;
		}
		
		return nullptr;
	}
	llvm::Value* ASTBinaryExpression::_CreateCmpExpression(llvm::Value* LHS, llvm::Value* RHS)
	{
		auto& builder = LLVM::Backend::GetBuilder();
		const bool isFloat = LHS->getType()->isFloatingPointTy();

		switch (m_Expression)
		{
			case BinaryExpressionType::Less:	
				return isFloat ? builder->CreateFCmpOLT(LHS, RHS)
							   : builder->CreateICmpSLT(LHS, RHS);
			case BinaryExpressionType::LessEq:
				return isFloat ? builder->CreateFCmpOLE(LHS, RHS)
							   : builder->CreateICmpSLE(LHS, RHS);
			case BinaryExpressionType::Greater:
				return isFloat ? builder->CreateFCmpOGT(LHS, RHS)
							   : builder->CreateICmpSGT(LHS, RHS);
			case BinaryExpressionType::GreaterEq:
				return isFloat ? builder->CreateFCmpOGE(LHS, RHS)
							   : builder->CreateICmpSGE(LHS, RHS);
			case BinaryExpressionType::Eq:
				return isFloat ? builder->CreateFCmpOEQ(LHS, RHS)
							   : builder->CreateICmpEQ(LHS, RHS);
			default:
				break;
		}

		return nullptr;
	}
}