#include "ASTNode.h"

#include "API/LLVM/LLVMBackend.h"
#include "Log.h"

#include <iostream>
#include <map>
#include <stack>

namespace clear {

	static std::map<std::string, llvm::AllocaInst*>     s_VariableMap;
	static std::map<std::string, ObjectReferenceInfo>   s_StructTypes;
	static std::stack<llvm::IRBuilderBase::InsertPoint> s_InsertPoints;

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
	ASTNodeLiteral::ASTNodeLiteral(const std::string& data)
		: m_Data(data), m_Type(data)
	{
	}

	llvm::Value* ASTNodeLiteral::Codegen()
	{
		return GetLLVMConstant(m_Type, m_Data);
	}

	ASTBinaryExpression::ASTBinaryExpression(BinaryExpressionType type)
		: m_Expression(type)
	{
	}
	llvm::Value* ASTBinaryExpression::Codegen()
	{
		// Assumes the two values in its children are to be added in order
		auto& builder = *LLVM::Backend::GetBuilder();
		auto& children = GetChildren();

		if (children.size() != 2)
			return nullptr;

		llvm::Value* LHS = children[1]->Codegen();
		llvm::Value* RHS = children[0]->Codegen();

		if (!LHS || !RHS)
			return nullptr;

		llvm::Type* typeLHS = LHS->getType();
		llvm::Type* typeRHS = RHS->getType();

		if (llvm::isa<llvm::AllocaInst>(LHS))
		{
			auto val = llvm::dyn_cast<llvm::AllocaInst>(LHS);
			typeLHS = val->getAllocatedType();
		}

		if (llvm::isa<llvm::AllocaInst>(RHS))
		{
			auto val = llvm::dyn_cast<llvm::AllocaInst>(RHS);
			typeRHS = val->getAllocatedType();
		}

		if (typeLHS != typeRHS)
		{
			if (typeLHS->isFloatingPointTy() && typeRHS->isIntegerTy())
				RHS = builder.CreateSIToFP(RHS, typeLHS);

			else if (typeLHS->isIntegerTy() && typeRHS->isFloatingPointTy())
				LHS = builder.CreateSIToFP(LHS, typeRHS);

			else if (typeLHS->isFloatingPointTy() && typeRHS->isFloatingPointTy())
			{
				auto LHSWidth = typeLHS->getPrimitiveSizeInBits();
				auto RHSWidth = typeRHS->getPrimitiveSizeInBits();

				//if (LHSWidth > RHSWidth)
					//RHS = builder.CreateFPExt(RHS, LHS->getType());
				//else if (LHSWidth < RHSWidth)
					//LHS = builder.CreateFPTrunc(LHS, RHS->getType());
			}
		}

		if (_IsMathExpression())
			return _CreateMathExpression(LHS, RHS);
		else if (_IsCmpExpression())
			return _CreateCmpExpression(LHS, RHS);
		else
			return _CreateLoadStoreExpression(LHS, RHS);

		return nullptr;
	}

	const bool ASTBinaryExpression::_IsMathExpression() const
	{
		return (int)m_Expression <= (int)BinaryExpressionType::Mod;
	}

	const bool ASTBinaryExpression::_IsCmpExpression() const
	{
		return (int)m_Expression <= (int)BinaryExpressionType::Eq && !_IsMathExpression();
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

	llvm::Value* ASTBinaryExpression::_CreateLoadStoreExpression(llvm::Value* LHS, llvm::Value* RHS)
	{
		auto& builder = LLVM::Backend::GetBuilder();

		switch (m_Expression)
		{
			case BinaryExpressionType::Assignment:	return builder->CreateStore(RHS, LHS);
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
		if (!s_VariableMap.contains(m_Name))
		{
			std::cout << "no variable of name " << m_Name << " exists" << std::endl;
			return nullptr;
		}

		llvm::AllocaInst* value = s_VariableMap.at(m_Name);
		CLEAR_VERIFY(value, "value was nullptr");

		return value;
	}

	ASTVariableDecleration::ASTVariableDecleration(const std::string& name, AbstractType type)
		: m_Name(name), m_Type(type)
	{
	}

	llvm::Value* ASTVariableDecleration::Codegen()
	{
		if (s_VariableMap.contains(m_Name))
		{
			std::cout << "variable of the name " << m_Name << " exists" << std::endl;
			return nullptr;
		}

		auto& builder = *LLVM::Backend::GetBuilder();

		if (m_Type.Get() == VariableType::UserDefinedType)
		{
			auto& structType = m_Type.GetUserDefinedType();

			auto value = builder.CreateAlloca(s_StructTypes[structType].Struct, nullptr, m_Name);
			s_VariableMap[m_Name] = value;

			return value;
		}

		auto variableType = m_Type.Get();
		auto value = builder.CreateAlloca(GetLLVMVariableType(variableType), nullptr, m_Name);
		s_VariableMap[m_Name] = value;
		
		return value;
	}

	ASTFunctionDecleration::ASTFunctionDecleration(const std::string& name, VariableType returnType, const std::vector<Paramter>& paramters)
		: m_Name(name), m_ReturnType(returnType), m_Paramters(paramters)
	{
	}
	llvm::Value* ASTFunctionDecleration::Codegen()
	{
		auto& module  = *LLVM::Backend::GetModule();
		auto& context = *LLVM::Backend::GetContext();
		auto& builder = *LLVM::Backend::GetBuilder();

		s_InsertPoints.push(builder.saveIP());


		llvm::Type* returnType = GetLLVMVariableType(m_ReturnType);

		std::vector<llvm::Type*> ParamterTypes;
		for (const auto& Paramter : m_Paramters)
		{
			ParamterTypes.push_back(GetLLVMVariableType(Paramter.Type));
		}

		llvm::FunctionType* functionType = llvm::FunctionType::get(returnType, ParamterTypes, false);

		llvm::Function* function = module.getFunction(m_Name);

		if (function)
		{
			std::cout << "function already defined" << std::endl;
			return nullptr;
		}

		function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, m_Name, module);

		llvm::Function::arg_iterator args = function->arg_begin();

		for (const auto& Paramter : m_Paramters)
		{
			args->setName(Paramter.Name);
			args++;
		}

		llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", function);
		builder.SetInsertPoint(entry);

		uint32_t k = 0;
		for (const auto& Paramter : m_Paramters)
		{
			llvm::AllocaInst* argAlloc = builder.CreateAlloca(GetLLVMVariableType(Paramter.Type), nullptr, Paramter.Name);
			builder.CreateStore(function->getArg(k), argAlloc);
			s_VariableMap[m_Name + "::" + Paramter.Name] = argAlloc;
			k++;
		}

		for (const auto& child : GetChildren())
		{
			child->Codegen();

			if (child->GetType() == ASTNodeType::ReturnStatement)
				break;
		}

		for (const auto& Paramter : m_Paramters)
		{
			s_VariableMap.erase(m_Name + "::" + Paramter.Name);
		}

		if (returnType->isVoidTy() && !builder.GetInsertBlock()->getTerminator())
		{
			builder.CreateRetVoid();
		}

		auto& ip = s_InsertPoints.top();
		builder.restoreIP(ip);
		s_InsertPoints.pop();

		return function;
	}

	llvm::Value* ASTReturnStatement::Codegen()
	{
		auto& builder = *LLVM::Backend::GetBuilder();

		if (GetChildren().size() > 0)
		{
			llvm::Value* returnValue = GetChildren()[0]->Codegen();
			return builder.CreateRet(returnValue);
		}

		return builder.CreateRetVoid();
	}
	llvm::Value* ASTExpression::Codegen()
	{
		auto& builder  = *LLVM::Backend::GetBuilder();
		auto& children = GetChildren();

		std::stack<std::shared_ptr<ASTNodeBase>> stack;

		for (const auto& child : children)
		{
			if (child->GetType() == ASTNodeType::Literal ||
				child->GetType() == ASTNodeType::VariableExpression)
			{
				stack.push(child);
				continue;
			}

			std::shared_ptr<ASTBinaryExpression> binExp = std::dynamic_pointer_cast<ASTBinaryExpression>(child);

			binExp->PushChild(stack.top());
			stack.pop();

			binExp->PushChild(stack.top());
			stack.pop();

			stack.push(binExp);
		}

		return stack.top()->Codegen();
	}

	ASTStruct::ASTStruct(const std::string& name, const std::vector<Member>& fields)
		: m_Name(name), m_Members(fields)
	{
	}

	llvm::Value* ASTStruct::Codegen()
	{
		std::vector<llvm::Type*> types;

		ObjectReferenceInfo info;
		uint32_t k = 0;

		for (auto& member : m_Members)
		{
			if (member.Field.Get() == VariableType::UserDefinedType)
			{
				auto& structName = member.Field.GetUserDefinedType();

				CLEAR_VERIFY(s_StructTypes.contains(structName), "struct hasn't been declared");

				types.push_back(s_StructTypes[structName].Struct);
			}
			else
			{
				auto& variableType = member.Field;
				types.push_back(GetLLVMVariableType(variableType));
			}

			info.Indices[member.Name] = k++;
		}

		info.Struct = llvm::StructType::create(types);
		s_StructTypes[m_Name] = info;

		return nullptr;
	}

	ASTFunctionCall::ASTFunctionCall(const std::string& name, const std::vector<Argument>& arguments)
		:  m_Name(name), m_Arguments(arguments)
	{
	}

	llvm::Value* ASTFunctionCall::Codegen()
	{
		std::vector<llvm::Value*> args;

		auto& builder = *LLVM::Backend::GetBuilder();
		auto& module  = *LLVM::Backend::GetModule();

		//currently only dealing with constants
		for (const auto& argument : m_Arguments)
		{
			if (argument.Field.Get() == VariableType::UserDefinedType)
			{
				CLEAR_HALT(); //(TODO)
			}
			else
			{
				auto& variableType = argument.Field;
				args.push_back(GetLLVMConstant(variableType, argument.Data));
			}
		}

		llvm::Function* callee = module.getFunction(m_Name);
		CLEAR_VERIFY(callee, "not a valid function");

		return builder.CreateCall(callee, args);
	}
}