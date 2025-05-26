/* #include "ASTNode.h"

#include "API/LLVM/LLVMBackend.h"
#include "Core/Log.h"
#include "Core/Type.h"

#include <iostream>
#include <map>
#include <stack>
#include "ASTNodeN.h"

namespace clear {

	struct ReturnValue
	{
		llvm::AllocaInst* Alloca;
		llvm::BasicBlock* Return;
	};

	static std::stack<llvm::IRBuilderBase::InsertPoint>  s_InsertPoints;
	static std::stack<ReturnValue> s_ReturnValues;

	struct ForWhileBlock
	{
		llvm::BasicBlock* End;
		llvm::BasicBlock* Condition;
	};

	static std::stack<ForWhileBlock> s_ForWhileBlocks;

	llvm::Value* ASTNodeBase::Codegen()
	{
		llvm::Value* value = nullptr;

		for (auto& child : GetChildren())
			value = child->Codegen();

		return value;
	}

    void ASTNodeBase::SetName(const std::string &name)
    {
		p_MetaData.Name = name;
	}

	void ASTNodeBase::SetType(const Ref<Type>& type)
	{
		p_MetaData.Type = type;
	}

	void ASTNodeBase::PushChild(const Ref<ASTNodeBase>& child)
	{
		m_Children.push_back(child);
	}
	void ASTNodeBase::RemoveChild(const Ref<ASTNodeBase>& child)
	{
		auto it = std::find(m_Children.begin(), m_Children.end(), child);
		if (it != m_Children.end())
			m_Children.erase(it);
	}
	void ASTNodeBase::SetParent(const Ref<ASTNodeBase>& parent)
	{
		m_Parent = parent;
	}
	void ASTNodeBase::RemoveParent()
	{
		m_Parent.Reset();
	}
	ASTNodeLiteral::ASTNodeLiteral(const Token& data)
		: m_Constant(data)
	{
	}

	llvm::Value* ASTNodeLiteral::Codegen()
	{
		p_MetaData.Type = m_Constant.GetType();
		return m_Constant.Get();
	}

	ASTBinaryExpression::ASTBinaryExpression(BinaryExpressionType type)
		: m_Expression(type)
	{
	}
	
	llvm::Value* ASTBinaryExpression::Codegen() 
	{
		auto& builder = *LLVM::Backend::GetBuilder();
		auto& context = *LLVM::Backend::GetContext();
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() == 2, "incorrect dimensions");

		auto& leftChild  = children[1];
		auto& rightChild = children[0];

		llvm::Value* LHS = leftChild->Codegen();
		llvm::Value* RHS = rightChild->Codegen();

		auto& leftChildType = leftChild->GetMetaData().Type;
		auto& rightChildType = rightChild->GetMetaData().Type;

		llvm::Value* LHSRawValue = LHS;
		llvm::Value* RHSRawValue = RHS;

		CLEAR_VERIFY(LHS, "lhs failed to generate");

		if(m_Expression == BinaryExpressionType::AccessOp)
		{
			std::string& memberName = rightChild->GetMetaData().Name;

			auto& typeIdentifer = leftChild->GetMetaData().Type->GetUserDefinedTypeIdentifer();
			CLEAR_VERIFY(!typeIdentifer.empty(), "invalid type");
			
			auto& structMetaData = Type::GetStructMetaData(typeIdentifer);
			auto& index = structMetaData.Indices.at(memberName);
			
			p_MetaData.Type = structMetaData.Types[index];
			p_MetaData.NeedLoading = true;

			return builder.CreateStructGEP(leftChildType->Get(), LHSRawValue, index, "struct_get_element_ptr");
		}

		CLEAR_VERIFY(RHS, "rhs failed to generate");

		if (m_Expression == BinaryExpressionType::Assignment)
		{
			if (rightChild->GetMetaData().NeedLoading)
			{
				CLEAR_VERIFY(RHSRawValue->getType()->isPointerTy(), "not a valid load type");
				RHSRawValue = builder.CreateLoad(rightChildType->Get(), RHSRawValue, "loaded_value");
			}

			if (RHSRawValue->getType() != LHSRawValue->getType())
				RHSRawValue = Value::CastValue(RHSRawValue, leftChildType, rightChildType);

			return _CreateExpression(LHS, RHS, LHSRawValue, RHSRawValue, leftChildType->IsSigned());
		}

		if (leftChild->GetMetaData().Type && leftChild->GetMetaData().Type->GetID() == TypeID::Array)
		{
			LHSRawValue = builder.CreateInBoundsGEP(leftChildType->Get(), LHSRawValue, {builder.getInt64(0), builder.getInt64(0)}, "array_decay");
			leftChildType = Ref<Type>::Create(leftChildType->GetUnderlying());
		}
		else if (leftChild->GetMetaData().Type && leftChild->GetMetaData().NeedLoading)
		{
			CLEAR_VERIFY(LHSRawValue->getType()->isPointerTy(), "");
			LHSRawValue = builder.CreateLoad(leftChildType->Get(), LHSRawValue, "loaded_value");
		}

		if (rightChild->GetMetaData().Type && rightChild->GetMetaData().Type->GetID() == TypeID::Array)
		{
			RHSRawValue = builder.CreateInBoundsGEP(rightChildType->Get(), RHSRawValue, {builder.getInt64(0), builder.getInt64(0)}, "array_decay");
			rightChildType = Ref<Type>::Create(rightChildType->GetUnderlying());
		}
		else if (rightChild->GetMetaData().Type && rightChild->GetMetaData().NeedLoading)
		{
			CLEAR_VERIFY(RHSRawValue->getType()->isPointerTy(), "");
			RHSRawValue = builder.CreateLoad(rightChildType->Get(), RHSRawValue, "loaded_value");
		}

		if(m_Expression == BinaryExpressionType::Index)
		{
			CLEAR_VERIFY(RHSRawValue->getType()->isIntegerTy(), "");
			CLEAR_VERIFY(LHSRawValue->getType()->isPointerTy(), "");

			p_MetaData.Type = leftChildType->GetUnderlying();
			p_MetaData.NeedLoading = true;
			return builder.CreateInBoundsGEP(leftChildType->GetUnderlying()->Get(), LHSRawValue, RHSRawValue, "array_index");
		}

		if (RHSRawValue->getType() != LHSRawValue->getType())
			RHSRawValue = Value::CastValue(RHSRawValue, leftChildType, rightChildType);

		p_MetaData.Type = leftChildType;

		return _CreateExpression(LHS, RHS, LHSRawValue, RHSRawValue, leftChildType->IsSigned());
	}

	bool ASTBinaryExpression::_IsMathExpression() const
	{
		switch (m_Expression)
		{
			case BinaryExpressionType::Add:
            case BinaryExpressionType::Sub:
			case BinaryExpressionType::Mul:
			case BinaryExpressionType::Div:
			case BinaryExpressionType::Mod:
			case BinaryExpressionType::Pow:
				return true;
			default:
				break;
		}

		return false;
	}

	bool ASTBinaryExpression::_IsCmpExpression() const
	{
		switch (m_Expression)
		{
			case BinaryExpressionType::Less:
			case BinaryExpressionType::LessEq:
			case BinaryExpressionType::Greater:
            case BinaryExpressionType::GreaterEq:
			case BinaryExpressionType::Eq:
			case BinaryExpressionType::NotEq:
				return true;
			default:
				break;
		}

		return false;
	}

	bool ASTBinaryExpression::_IsBitwiseExpression() const
	{
		switch (m_Expression)
		{
			case BinaryExpressionType::BitwiseLeftShift:
			case BinaryExpressionType::BitwiseRightShift:
			case BinaryExpressionType::BitwiseNot:
			case BinaryExpressionType::BitwiseAnd:
			case BinaryExpressionType::BitwiseOr:
			case BinaryExpressionType::BitwiseXor:
				return true;
			default:
				break;
		}

		return false;
	}

	llvm::Value* ASTBinaryExpression::_CreateExpression(llvm::Value* LHS, llvm::Value* RHS, llvm::Value* LHSRawValue, llvm::Value* RHSRawValue, bool signedInteger)
	{
		if (_IsMathExpression())
			return _CreateMathExpression(LHSRawValue, RHSRawValue);
		
		if (_IsCmpExpression())
			return _CreateCmpExpression(LHSRawValue, RHSRawValue);
		
		if (_IsBitwiseExpression())
			return _CreateBitwiseExpression(LHSRawValue, RHSRawValue, signedInteger);

		return _CreateLoadStoreExpression(LHS, RHSRawValue);
	}

	llvm::Value* ASTBinaryExpression::_CreateMathExpression(llvm::Value* LHS, llvm::Value* RHS)
	{
		auto& builder = *LLVM::Backend::GetBuilder();
		auto& module  = *LLVM::Backend::GetModule();

		const bool isFloat = LHS->getType()->isFloatingPointTy();

		switch (m_Expression)
		{
			case BinaryExpressionType::Add:
			{
				return isFloat ? builder.CreateFAdd(LHS, RHS, "faddtmp") : builder.CreateAdd(LHS, RHS, "addtmp");
			}
			case BinaryExpressionType::Sub:
			{
				return isFloat ? builder.CreateFSub(LHS, RHS, "fsubtmp") : builder.CreateSub(LHS, RHS, "subtmp");
			}
			case BinaryExpressionType::Mul:
			{
				return isFloat ? builder.CreateFMul(LHS, RHS, "fmultmp") : builder.CreateMul(LHS, RHS, "multmp");
			}
			case BinaryExpressionType::Div:
			{
				return isFloat ? builder.CreateFDiv(LHS, RHS, "fdivtmp") : builder.CreateSDiv(LHS, RHS, "divtmp");
			}
			case BinaryExpressionType::Mod:
			{
				if (!isFloat)
					return builder.CreateSRem(LHS, RHS, "modtmp");
				
				CLEAR_UNREACHABLE("cannot do mod on floating type");

				break;
			}
			case BinaryExpressionType::Pow:
			{
				llvm::Function* powFunction = llvm::Intrinsic::getDeclaration(&module, llvm::Intrinsic::pow, { builder.getDoubleTy() });
				return builder.CreateCall(powFunction, { LHS, RHS });
			}
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
			case BinaryExpressionType::NotEq:
				return isFloat ? builder->CreateFCmpONE(LHS, RHS)
							   : builder->CreateICmpNE(LHS, RHS);
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

	llvm::Value* ASTBinaryExpression::_CreateBitwiseExpression(llvm::Value* LHS, llvm::Value* RHS, bool signedInteger)
	{
		auto& builder = *LLVM::Backend::GetBuilder();

		switch (m_Expression)
		{ 
			case BinaryExpressionType::BitwiseLeftShift:  return builder.CreateShl(LHS, RHS, "bitwise_shift_left");
			case BinaryExpressionType::BitwiseRightShift: return signedInteger ? builder.CreateAShr(LHS, RHS, "bitwise_a_shift_right") : builder.CreateLShr(LHS, RHS, "bitwise_l_shift_right");
			case BinaryExpressionType::BitwiseXor:		  return builder.CreateXor(LHS, RHS, "bitwise_xor");
			case BinaryExpressionType::BitwiseOr:		  return builder.CreateOr(LHS,  RHS, "bitwise_or");
			case BinaryExpressionType::BitwiseAnd:		  return builder.CreateAnd(LHS, RHS, "bitwise_and");
			case BinaryExpressionType::BitwiseNot:
			{
				CLEAR_UNREACHABLE("");
				break; //TODO: new ast node (astSingleExpression) which only applies an operation onto one operand
			}
			default:
				break;
		}

		CLEAR_UNREACHABLE("");

		return nullptr;
	}

	llvm::Value* ASTBinaryExpression::_CreatePointerArithmeticExpression(llvm::Value* LHS, llvm::Value* RHS, const Ref<Type>& pointerMetaData)
	{
		auto& builder = *LLVM::Backend::GetBuilder();

		if (m_Expression == BinaryExpressionType::NegatedPointerArithmetic)
			RHS = builder.CreateNeg(RHS);

		CLEAR_VERIFY(pointerMetaData->IsPointer(), "");
		p_MetaData.Type = pointerMetaData->GetUnderlying();
		return builder.CreateInBoundsGEP(p_MetaData.Type->Get(), LHS, RHS);
	}

	ASTVariableExpression::ASTVariableExpression(const std::string& variable)
		: m_VariableName(variable)
	{
	}

	llvm::Value* ASTVariableExpression::Codegen()
	{
		auto& builder = *LLVM::Backend::GetBuilder();
		auto& context = *LLVM::Backend::GetContext();

		auto& metaData = Value::GetVariableMetaData(m_VariableName);
		llvm::AllocaInst* value = metaData.Alloca;

		CLEAR_VERIFY(value, "value was nullptr");

		p_MetaData.Type = metaData.Type;
		p_MetaData.NeedLoading = true;

		return value; 
	}

	ASTVariableDeclaration::ASTVariableDeclaration(const std::string& name, Ref<Type> type)
	{
		p_MetaData.Name = name;
		p_MetaData.Type = type;
	}

	llvm::Value* ASTVariableDeclaration::Codegen()
	{		
		auto& builder = *LLVM::Backend::GetBuilder();

		auto ip = builder.saveIP();

		llvm::Function* function = builder.GetInsertBlock()->getParent();

		builder.SetInsertPoint(&function->getEntryBlock());
	
		m_Value = Value(p_MetaData.Type, p_MetaData.Name);

		builder.restoreIP(ip);

		return m_Value.Get();
	}

	ASTFunctionDefinition::ASTFunctionDefinition(const std::string& name, const Ref<Type>& returnType, const std::vector<Paramater>& Paramaters)
		: m_Paramaters(Paramaters)
	{
		g_FunctionMetaData[name] = { m_Paramaters, returnType };
		p_MetaData.Name = name;
		p_MetaData.Type = returnType;
	}
	llvm::Value* ASTFunctionDefinition::Codegen()
	{
		auto& module = *LLVM::Backend::GetModule();
		auto& context = *LLVM::Backend::GetContext();
		auto& builder = *LLVM::Backend::GetBuilder();

		s_InsertPoints.push(builder.saveIP());

		llvm::Type* returnType = p_MetaData.Type->Get();

		std::vector<llvm::Type*> ParamaterTypes;

		for (const auto& Paramater : m_Paramaters)
		{
			ParamaterTypes.push_back(Paramater.Type->Get());
		}

		llvm::FunctionType* functionType = llvm::FunctionType::get(returnType, ParamaterTypes, false);

		llvm::Function* function = module.getFunction(GetName());
		CLEAR_VERIFY(!function, "function already defined");

		function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, GetName(), module);

		llvm::Function::arg_iterator args = function->arg_begin();

		for (const auto& Paramater : m_Paramaters)
		{
			args->setName(GetName() + "::" + Paramater.Name);
			args++;
		}

		llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", function);
		llvm::BasicBlock* body  = llvm::BasicBlock::Create(context, "body");
		builder.SetInsertPoint(entry);

		llvm::BasicBlock* returnBlock = llvm::BasicBlock::Create(context, "return");

		ReturnValue returnStatement{};
		returnStatement.Alloca = !returnType->isVoidTy() ? builder.CreateAlloca(returnType, nullptr, "return_value") : nullptr;
		returnStatement.Return = returnBlock;

		s_ReturnValues.push(returnStatement);

		uint32_t k = 0;

		for (const auto& Paramater : m_Paramaters)
		{
			llvm::AllocaInst* argAlloc = builder.CreateAlloca(Paramater.Type->Get(), nullptr, GetName() + "::" + Paramater.Name);
			builder.CreateStore(function->getArg(k), argAlloc);
			
			Value::RegisterVariable(argAlloc, GetName() + "::" + Paramater.Name, Paramater.Type);

			k++;
		}

		function->insert(function->end(), body);
		builder.SetInsertPoint(body);

		for (const auto& child : GetChildren())
		{
			child->Codegen();
		}

		for (const auto& Paramater : m_Paramaters)
		{
			Value::RemoveVariable(GetName() + "::" + Paramater.Name);
		}

		auto currip = builder.saveIP();

		builder.SetInsertPoint(entry);
		builder.CreateBr(body);

		builder.restoreIP(currip);

		ReturnValue value = s_ReturnValues.top();
		s_ReturnValues.pop();

		if(!builder.GetInsertBlock()->getTerminator())
			builder.CreateBr(returnBlock);

		function->insert(function->end(), returnBlock);
		builder.SetInsertPoint(returnBlock);

		if (returnType->isVoidTy())
		{
			builder.CreateRetVoid();
		}
		else
		{
			llvm::Value* load = builder.CreateLoad(value.Alloca->getAllocatedType(), value.Alloca, "loaded_value");
			builder.CreateRet(load);
		}

		auto& ip = s_InsertPoints.top();
		builder.restoreIP(ip);
		s_InsertPoints.pop();

		return function;
	}

	ASTFunctionDecleration::ASTFunctionDecleration(const std::string& name, const Ref<Type>& expectedReturnType, const std::vector<Paramater>& types)
		: m_ExpectedTypes(types)
	{
		g_FunctionMetaData[name] = { types, expectedReturnType };
		p_MetaData.Name = name;
		p_MetaData.Type = expectedReturnType;
	}

	llvm::Value* ASTFunctionDecleration::Codegen()
	{
		auto& module = *LLVM::Backend::GetModule();

		std::vector<llvm::Type*> types;

		bool isVariadic = false;

		for (auto& type : m_ExpectedTypes)
		{
			if (type.IsVariadic)
			{
				isVariadic = true;
				break;
			}

			types.push_back(type.Type->Get());
		}

		if (!p_MetaData.Type)
			p_MetaData.Type = Ref<Type>::Create(TypeID::None);

		llvm::FunctionType* functionType = llvm::FunctionType::get(p_MetaData.Type->Get(), types, isVariadic);
		module.getOrInsertFunction(p_MetaData.Name, functionType);
		return nullptr;
	}

	ASTReturnStatement::ASTReturnStatement(const Ref<Type>& expectedReturnType, bool createReturn)
		: m_CreateReturn(createReturn)
	{
		p_MetaData.Type = expectedReturnType;
	}

	llvm::Value* ASTReturnStatement::Codegen()
	{
		auto& children = GetChildren();
		auto& builder = *LLVM::Backend::GetBuilder();

		if (children.size() > 0)
		{
			llvm::Value* returnValue = children[0]->Codegen();
			
			if(!returnValue)
			{
				builder.CreateBr(s_ReturnValues.top().Return);
				return nullptr;
			}

			if (children[0]->GetMetaData().NeedLoading)
			{
				returnValue = builder.CreateLoad(children[0]->GetMetaData().Type->Get(), returnValue, "loaded_value");
			}

			if (returnValue->getType() != p_MetaData.Type->Get())
				returnValue = Value::CastValue(returnValue, p_MetaData.Type, {});
			
			CLEAR_VERIFY(returnValue->getType() == p_MetaData.Type->Get(), "unexpected return type");

			llvm::AllocaInst* alloc = s_ReturnValues.top().Alloca;
			
			builder.CreateStore(returnValue, alloc);
		}
		
		builder.CreateBr(s_ReturnValues.top().Return);

		return nullptr;
	}
	
	ASTExpression::ASTExpression(const Ref<Type>& expectedType)
	{
		p_MetaData.Type = expectedType;
	}

	llvm::Value* ASTExpression::Codegen()
	{
		auto& builder  = *LLVM::Backend::GetBuilder();
		auto& children = GetChildren();

		std::stack<Ref<ASTNodeBase>> stack; //ok

		for (const auto& child : children)
		{
			if (child->GetType() == ASTNodeType::Literal ||
				child->GetType() == ASTNodeType::VariableExpression || 
				child->GetType() == ASTNodeType::FunctionCall || 
				child->GetType() == ASTNodeType::MemberAccess)
			{
				stack.push(child);
				continue;
			}

			if (child->GetType() == ASTNodeType::UnaryExpression)
			{
				Ref<ASTUnaryExpression> unaryExpression = DynamicCast<ASTUnaryExpression>(child);
				CLEAR_VERIFY(unaryExpression->GetChildren().size() == 0, "");

				unaryExpression->PushChild(stack.top());
				stack.pop();

				stack.push(unaryExpression);

				continue;
			}

			Ref<ASTBinaryExpression> binExp = DynamicCast<ASTBinaryExpression>(child);

			binExp->PushChild(stack.top());
			stack.pop();

			binExp->PushChild(stack.top());
			stack.pop();

			stack.push(binExp);
		}


		if(stack.size() > 0)
		{
			llvm::Value* value = stack.top()->Codegen();

			p_MetaData.NeedLoading = stack.top()->GetMetaData().NeedLoading;
			p_MetaData.Type = stack.top()->GetMetaData().Type;

			return value;
		}

		return nullptr;
	}

	ASTStruct::ASTStruct(const std::string& name, const std::vector<MemberType>& fields)
		:  m_Members(fields)
	{
		p_MetaData.Name = name;
	}

	llvm::Value* ASTStruct::Codegen()
	{
		//immediatly freed but registers it which is what we want
		Ref<Type> type = Ref<Type>::Create(p_MetaData.Name, m_Members);
		return nullptr;
	}

	ASTFunctionCall::ASTFunctionCall(const std::string& name)
	{
		p_MetaData.Name = name;
	}

	llvm::Value* ASTFunctionCall::Codegen()
	{
		std::vector<llvm::Value*> args;

		auto& builder = *LLVM::Backend::GetBuilder();
		auto& module  = *LLVM::Backend::GetModule();
		auto& context = *LLVM::Backend::GetContext();
		auto& children = GetChildren();

		auto& expected = g_FunctionMetaData.at(p_MetaData.Name);

		uint32_t k = 0;

		for (auto& child : children)
		{
			llvm::Value* gen = child->Codegen();

			if (child->GetMetaData().NeedLoading)
			{
				if (gen->getType()->isArrayTy())
				{
					llvm::ArrayType* type = llvm::dyn_cast<llvm::ArrayType>(gen->getType());

					CLEAR_VERIFY(type, "");
					gen = builder.CreateInBoundsGEP(type, gen, { builder.getInt64(0), builder.getInt64(0) }, "array_decay");
				}

				CLEAR_VERIFY(gen->getType()->isPointerTy(), "");

				gen = builder.CreateLoad(child->GetMetaData().Type->Get(), gen, "loaded_value");
			}
		
			if (k < expected.Parameters.size() && !expected.Parameters[k].IsVariadic && gen->getType() != expected.Parameters[k].Type->Get())
			{
				gen = Value::CastValue(gen, expected.Parameters[k].Type, {});
			}

			k++;
		
			CLEAR_VERIFY(gen, "not a valid argument");
			args.push_back(gen);
		}
		
		
		llvm::Function* callee = module.getFunction(p_MetaData.Name);
		CLEAR_VERIFY(callee, "not a valid function");

		p_MetaData.Type = expected.ReturnType;
		return builder.CreateCall(callee, args);
	}

	llvm::Value* ASTIfExpression::Codegen()
	{
		auto& children = GetChildren();
		auto& context = *LLVM::Backend::GetContext();
		auto& builder = *LLVM::Backend::GetBuilder();

		CLEAR_VERIFY(children.size() > 1, "size must be greater than 1");

		llvm::Function* function = builder.GetInsertBlock()->getParent();

		struct Branch
		{
			llvm::BasicBlock* ConditionBlock = nullptr;
			llvm::BasicBlock* BodyBlock  = nullptr;
			int64_t ExpressionIdx = 0;
		};

		CLEAR_VERIFY(children[0]->GetType() == ASTNodeType::Expression,"");

		std::vector<Branch> branches;

		for (size_t i = 0; i + 1 < children.size(); i += 2)
		{
			Branch branch;
			branch.ConditionBlock = llvm::BasicBlock::Create(context, "if_condition");
			branch.BodyBlock      = llvm::BasicBlock::Create(context, "if_body");
			branch.ExpressionIdx  = i;

			branches.push_back(branch);
		}

		llvm::BasicBlock* elseBlock  = llvm::BasicBlock::Create(context, "else");
		llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(context, "merge");

		if(!builder.GetInsertBlock()->getTerminator())
			builder.CreateBr(branches[0].ConditionBlock);

		for (size_t i = 0; i < branches.size(); i++)
		{
			auto& branch = branches[i];

			llvm::BasicBlock* nextBranch = (i + 1) < branches.size() ? branches[i + 1].ConditionBlock : elseBlock;
			
			function->insert(function->end(), branch.ConditionBlock);
			builder.SetInsertPoint(branch.ConditionBlock);

			llvm::Value* condition = children[branch.ExpressionIdx]->Codegen();

			if (condition->getType()->isIntegerTy() && !condition->getType()->isIntegerTy(1))
			{
				condition = builder.CreateICmpNE(condition, llvm::ConstantInt::get(condition->getType(), 0));
			}
			else if (condition->getType()->isFloatingPointTy())
			{
				condition = builder.CreateFCmpONE(condition, llvm::ConstantFP::get(condition->getType(), 0.0));
			}
			else if (condition->getType()->isPointerTy())
			{
				condition = builder.CreatePtrToInt(condition, llvm::IntegerType::get(context, 64), "cast");
				condition = builder.CreateICmpNE(condition, llvm::ConstantInt::get(condition->getType(), 0));
			}

			builder.CreateCondBr(condition, branch.BodyBlock, nextBranch);

			function->insert(function->end(), branch.BodyBlock);
			builder.SetInsertPoint(branch.BodyBlock);

			CLEAR_VERIFY(branch.ExpressionIdx + 1 < children.size(), "");
			children[branch.ExpressionIdx + 1]->Codegen();

			if (!builder.GetInsertBlock()->getTerminator())
				builder.CreateBr(mergeBlock);
		}

		function->insert(function->end(), elseBlock);
		builder.SetInsertPoint(elseBlock);

		size_t last = children.size() - 1;

		if (children.size() > 2 && children[last]->GetType() == children[last - 1]->GetType())
			children[last]->Codegen();
	
		if (!builder.GetInsertBlock()->getTerminator())
			builder.CreateBr(mergeBlock);

		function->insert(function->end(), mergeBlock);
		builder.SetInsertPoint(mergeBlock);

		return nullptr;
	}

	llvm::Value* ASTWhileLoop::Codegen()
	{
		auto& builder = *LLVM::Backend::GetBuilder();
		auto& context = *LLVM::Backend::GetContext();
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() == 2, "incorrect dimension");
		CLEAR_VERIFY(children[0]->GetType() == ASTNodeType::Expression, "incorrect node type");

		llvm::Function* function = builder.GetInsertBlock()->getParent();

		llvm::BasicBlock* conditionBlock = llvm::BasicBlock::Create(context, "while_condition", function);
		llvm::BasicBlock* body  = llvm::BasicBlock::Create(context, "while_body");
		llvm::BasicBlock* end   = llvm::BasicBlock::Create(context, "while_end");

		if (!builder.GetInsertBlock()->getTerminator())
			builder.CreateBr(conditionBlock);

		builder.SetInsertPoint(conditionBlock);

		llvm::Value* condition = children[0]->Codegen();
		if (condition->getType()->isIntegerTy())
			condition = builder.CreateICmpNE(condition, llvm::ConstantInt::get(condition->getType(), 0));
		else if (condition->getType()->isFloatingPointTy())
			condition = builder.CreateFCmpONE(condition, llvm::ConstantFP::get(condition->getType(), 0.0));

		builder.CreateCondBr(condition, body, end);

		function->insert(function->end(), body);
		builder.SetInsertPoint(body);

		s_ForWhileBlocks.push({ end, conditionBlock });

		children[1]->Codegen();

		if (!builder.GetInsertBlock()->getTerminator())
			builder.CreateBr(conditionBlock);
		
		function->insert(function->end(), end);
		builder.SetInsertPoint(end);

		s_ForWhileBlocks.pop();

		return nullptr;
	}

	ASTUnaryExpression::ASTUnaryExpression(UnaryExpressionType type, const Ref<Type>& typeToCast)
		: m_Type(type)
	{
		p_MetaData.Type = typeToCast;
		p_MetaData.Debug = (int)type;
	}

	llvm::Value* ASTUnaryExpression::Codegen()
	{
		auto& builder = *LLVM::Backend::GetBuilder();
		auto& context = *LLVM::Backend::GetContext();
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() == 1, "incorrect dimensions");

		llvm::Value* operand = children[0]->Codegen();
		auto& metaData = children[0]->GetMetaData();

		switch (m_Type)
		{
			case UnaryExpressionType::BitwiseNot:
			{
				if (children[0]->GetMetaData().NeedLoading)
				{
					llvm::AllocaInst* alloc = llvm::dyn_cast<llvm::AllocaInst>(operand);
					operand = builder.CreateLoad(alloc->getAllocatedType(), alloc, "loaded_value");
				}

				return builder.CreateNot(operand, "bitwise_not");
			}
			case UnaryExpressionType::PostIncrement:
			{
				llvm::AllocaInst* inst = llvm::dyn_cast<llvm::AllocaInst>(operand);
				CLEAR_VERIFY(inst,"");

				llvm::Value* currentValue = builder.CreateLoad(inst->getAllocatedType(), inst, "saved_loaded_value");
				llvm::Value* newValue = nullptr;

				if (currentValue->getType()->isFloatingPointTy())
					newValue = builder.CreateFAdd(currentValue, llvm::ConstantFP::get(currentValue->getType(), 1.0));
				else
					newValue = builder.CreateAdd(currentValue, llvm::ConstantInt::get(currentValue->getType(), 1));

				builder.CreateStore(newValue, inst);

				return currentValue;
			}
			case UnaryExpressionType::PostDecrement:
			{
				llvm::AllocaInst* inst = llvm::dyn_cast<llvm::AllocaInst>(operand);
				CLEAR_VERIFY(inst,"");

				llvm::Value* currentValue = builder.CreateLoad(inst->getAllocatedType(), inst, "saved_loaded_value");
				llvm::Value* newValue = nullptr;

				if (currentValue->getType()->isFloatingPointTy())
					newValue = builder.CreateFSub(currentValue, llvm::ConstantFP::get(currentValue->getType(), 1.0));
				else
					newValue = builder.CreateSub(currentValue, llvm::ConstantInt::get(currentValue->getType(), 1));

				builder.CreateStore(newValue, inst);

				return currentValue;
			}
			case UnaryExpressionType::Increment:
			{
				llvm::AllocaInst* inst = llvm::dyn_cast<llvm::AllocaInst>(operand);
				CLEAR_VERIFY(inst,"");

				llvm::Value* currentValue = builder.CreateLoad(inst->getAllocatedType(), inst, "loaded_value");
				llvm::Value* newValue = nullptr;

				if (currentValue->getType()->isFloatingPointTy())
					newValue = builder.CreateFAdd(currentValue, llvm::ConstantFP::get(currentValue->getType(), 1.0));
				else
					newValue = builder.CreateAdd(currentValue, llvm::ConstantInt::get(currentValue->getType(), 1));

				builder.CreateStore(newValue, inst);

				return newValue;
			}
			case UnaryExpressionType::Decrement:
			{
				llvm::AllocaInst* inst = llvm::dyn_cast<llvm::AllocaInst>(operand);
				CLEAR_VERIFY(inst, "");

				llvm::Value* currentValue = builder.CreateLoad(inst->getAllocatedType(), inst, "loaded_value");
				llvm::Value* newValue = nullptr; 

				if(currentValue->getType()->isFloatingPointTy())
					newValue = builder.CreateFSub(currentValue, llvm::ConstantFP::get(currentValue->getType(), 1.0));
				else 
					newValue = builder.CreateSub(currentValue, llvm::ConstantInt::get(currentValue->getType(), 1));

				builder.CreateStore(newValue, inst);

				return newValue;
			}
			case UnaryExpressionType::Negation:
			{	
				if (children[0]->GetMetaData().NeedLoading)
				{
					llvm::AllocaInst* alloc = llvm::dyn_cast<llvm::AllocaInst>(operand);
					operand = builder.CreateLoad(alloc->getAllocatedType(), alloc, "loaded_value");
				}

				if (operand->getType()->isFloatingPointTy())
					return builder.CreateFNeg(operand);
				
				return builder.CreateNeg(operand);
			}
			case UnaryExpressionType::Dereference:
			{
				p_MetaData.Type = metaData.Type->GetUnderlying();
				p_MetaData.NeedLoading = metaData.NeedLoading;

				if (metaData.Type->GetID() == TypeID::Array)
				{
					CLEAR_VERIFY(metaData.Type->IsPointer(), "");
					return builder.CreateInBoundsGEP(metaData.Type->Get(), operand, {builder.getInt64(0), builder.getInt64(0)}, "array_decay");
				}

				CLEAR_VERIFY(operand->getType()->isPointerTy(), "Dereference operand must be a pointer");
				return builder.CreateLoad(metaData.Type->Get(), operand, "loaded_value");
			}
			case UnaryExpressionType::Reference:
			{
				p_MetaData.NeedLoading = false;
				p_MetaData.Type = metaData.Type;

				return operand;
			}
			case UnaryExpressionType::Cast:
			{
				if (children[0]->GetMetaData().NeedLoading)
				{
					if (children[0]->GetMetaData().Type->GetID() == TypeID::Array)
					{
						CLEAR_VERIFY(p_MetaData.Type->IsPointer(), "");
						operand = builder.CreateInBoundsGEP(children[0]->GetMetaData().Type->Get(), operand, {builder.getInt64(0), builder.getInt64(0)}, "array_decay");
					}
					else
					{
						operand = builder.CreateLoad(children[0]->GetMetaData().Type->Get(), operand, "loaded_value");
					}

				}

				Ref<Type> from = metaData.Type;
				if(!p_MetaData.Type->IsPointer())
					return Value::CastValue(operand, p_MetaData.Type, from);
					
				return operand;
			}
			case UnaryExpressionType::None:
			default:
			{
				return operand;
				break;
			}
		}

		return nullptr;
	}

	llvm::Value* ASTBreak::Codegen()
	{
		auto& builder = *LLVM::Backend::GetBuilder();

		CLEAR_VERIFY(!s_ForWhileBlocks.empty() && !builder.GetInsertBlock()->getTerminator(), "have to be in a for or a while loop");
		builder.CreateBr(s_ForWhileBlocks.top().End);
		
		return nullptr;
	}

	llvm::Value* ASTContinue::Codegen()
	{
		auto& builder = *LLVM::Backend::GetBuilder();

		CLEAR_VERIFY(!s_ForWhileBlocks.empty() && !builder.GetInsertBlock()->getTerminator(), "have to be in a for or a while loop");
		builder.CreateBr(s_ForWhileBlocks.top().Condition);

		return nullptr;
	}

	llvm::Value* ASTArrayInitializer::Codegen()
	{
		auto& builder = *LLVM::Backend::GetBuilder();
		auto& context = *LLVM::Backend::GetContext();
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() > 0, "size of children must be bigger than 1");

		llvm::Value* value = children[0]->Codegen();
		CLEAR_VERIFY(llvm::isa<llvm::AllocaInst>(value), "incorrect type");

		Ref<ASTVariableExpression> expression = DynamicCast<ASTVariableExpression>(children[0]);

		llvm::AllocaInst* allocaInstance = llvm::dyn_cast<llvm::AllocaInst>(value);

		llvm::Type* type = allocaInstance->getAllocatedType();
		CLEAR_VERIFY(llvm::isa<llvm::ArrayType>(type), "incorrect type");

		llvm::ArrayType* arrayType = llvm::dyn_cast<llvm::ArrayType>(type);

		llvm::Type* elementType = _GetElementType(arrayType);

		llvm::Constant* zeroArray = llvm::ConstantAggregateZero::get(arrayType);
		builder.CreateStore(zeroArray, allocaInstance);

		for (size_t i = 1; i < children.size(); i++)
		{
			CLEAR_VERIFY(i - 1 < m_Indices.size(), "child doesn't have an index");

			llvm::Value* childValue = children[i]->Codegen();
			CLEAR_VERIFY(childValue, "value must be proper");

			if(children[i]->GetMetaData().NeedLoading)
			{
				childValue = builder.CreateLoad(children[i]->GetMetaData().Type->Get(), childValue, "loaded_value");
			}

			if (childValue->getType() != elementType)
			{
				childValue = Value::CastValue(childValue, elementType, childValue->getType(), expression->GetMetaData().Type->IsSigned());
			}

			llvm::Value* gep = builder.CreateInBoundsGEP(arrayType, allocaInstance, m_Indices[i - 1], "array_indexing");
			builder.CreateStore(childValue, gep);
		}
		

		return nullptr;
	}

	void ASTArrayInitializer::PushElementIndex(const std::vector<size_t>& elementIndex)
	{
		auto& context = *LLVM::Backend::GetContext();

		std::vector<llvm::Value*> indices;

		for (size_t i : elementIndex)
		{
			indices.push_back(llvm::ConstantInt::get(llvm::IntegerType::get(context, 64), i));
		}

		m_Indices.push_back(indices);
	}

	llvm::Type* ASTArrayInitializer::_GetElementType(llvm::ArrayType* type)
	{
		llvm::Type* elementType = type->getElementType();

		while (llvm::ArrayType* arrayType = llvm::dyn_cast<llvm::ArrayType>(elementType)) 
		{
			elementType = arrayType->getElementType();
		}

		return elementType;
	}

    ASTMemberAccess::ASTMemberAccess(const std::string& member)
    {
		p_MetaData.Name = member;
    }

    llvm::Value *ASTMemberAccess::Codegen()
    {
        return nullptr;
    }
	

} */