#include "ASTNode.h"

#include "API/LLVM/LLVMBackend.h"

#include <iostream>

namespace clear {

	llvm::Value* ASTNodeBase::Codegen()
	{
		for (auto& child : GetChildren())
			child->Codegen();

		return nullptr;
	}

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
							   : builder->CreateAdd(LHS, RHS,  "addtmp");

			case BinaryExpressionType::Sub:
				return isFloat ? builder->CreateFSub(LHS, RHS, "fsubtmp")
							   : builder->CreateSub(LHS, RHS,  "subtmp");

			case BinaryExpressionType::Mul:
				return isFloat ? builder->CreateFMul(LHS, RHS, "fmultmp")
							   : builder->CreateMul(LHS, RHS,  "multmp");

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

	ASTVariableExpression::ASTVariableExpression(const std::string& name)
		: m_Name(name)
	{
	}

	llvm::Value* ASTVariableExpression::Codegen()
	{
		return nullptr;
	}

	ASTVariableDecleration::ASTVariableDecleration(const std::string& name, VariableType type)
		: m_Name(name), m_Type(type)
	{
	}
	llvm::Value* ASTVariableDecleration::Codegen()
	{
		auto& builder = *LLVM::Backend::GetBuilder();
		m_Value = builder.CreateAlloca(GetVariableType(m_Type), nullptr, m_Name);
		return m_Value;
	}

	ASTFunctionDecleration::ASTFunctionDecleration(const std::string& name, VariableType returnType, const std::vector<Argument>& arugments)
		: m_Name(name), m_ReturnType(returnType), m_Arguments(arugments)
	{
	}
	llvm::Value* ASTFunctionDecleration::Codegen()
	{
		auto& module  = *LLVM::Backend::GetModule();
		auto& context = *LLVM::Backend::GetContext();
		auto& builder = *LLVM::Backend::GetBuilder();

		llvm::Type* returnType = GetVariableType(m_ReturnType);
		
		std::vector<llvm::Type*> argumentTypes;
		for (const auto& arugment : m_Arguments)
		{
			argumentTypes.push_back(GetVariableType(arugment.Type));
		}

		//TODO: may want to add variadic arguments in the future
		llvm::FunctionType* functionType = llvm::FunctionType::get(returnType, argumentTypes, false);

		llvm::Function* function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, m_Name, module);
		
		llvm::Function::arg_iterator args = function->arg_begin();

		for (const auto& argument : m_Arguments)
		{
			args->setName(argument.Name);
			args++;
		}

		llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", function);
		builder.SetInsertPoint(entry);

		uint32_t k = 0;
		for (const auto& argument : m_Arguments)
		{
			m_FunctionArgs.push_back(function->getArg(k++));
		}

		//children should contain return statement, any functions that need to be created. If they need
		//function arguments they can get them from the parent GetParent()
		for (const auto& child : GetChildren())
		{
			child->Codegen();
		}

		return function;
	}

	llvm::Type* GetVariableType(VariableType type)
	{
		auto& context = *LLVM::Backend::GetContext();

		switch (type)
		{
			case VariableType::Int8:	return llvm::Type::getInt8Ty(context);
			case VariableType::Int16:	return llvm::Type::getInt16Ty(context);
			case VariableType::Int32:	return llvm::Type::getInt32Ty(context);
			case VariableType::Int64:	return llvm::Type::getInt64Ty(context);
			case VariableType::Uint8:	return llvm::Type::getInt8Ty(context);
			case VariableType::Uint16:	return llvm::Type::getInt16Ty(context);
			case VariableType::Uint32:	return llvm::Type::getInt32Ty(context);
			case VariableType::Uint64:	return llvm::Type::getInt64Ty(context);
			case VariableType::Bool:    return llvm::Type::getInt1Ty(context);
			case VariableType::Float32: return llvm::Type::getFloatTy(context);
			case VariableType::Float64:	return llvm::Type::getDoubleTy(context);
				//TODO:
			case VariableType::Struct:
			case VariableType::Object:
			case VariableType::None:
			default:
				return llvm::Type::getVoidTy(context);
		}
	}
}