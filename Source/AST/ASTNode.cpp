#include "ASTNode.h"

#include "API/LLVM/LLVMBackend.h"
#include "Core/Log.h"
#include "Core/Types.h"
#include "Core/Utils.h"

#include <iostream>
#include <map>
#include <stack>

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

	void ASTNodeBase::SetName(const std::string& name)
	{
		p_MetaData.Name = name;
	}

	void ASTNodeBase::SetType(const AbstractType& type)
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
	ASTNodeLiteral::ASTNodeLiteral(const std::string& data)
		: m_Constant(data)
	{
	}

	llvm::Value* ASTNodeLiteral::Codegen()
	{
		p_MetaData.Type = m_Constant.GetType();
		return m_Constant.Get();
	}

	ASTBinaryExpression::ASTBinaryExpression(BinaryExpressionType type, const AbstractType& expectedType)
		: m_Expression(type)
	{
		p_MetaData.Type = expectedType;
	}
	llvm::Value* ASTBinaryExpression::Codegen()
	{
		// Assumes the two values in its children are to be added in order
		auto& builder = *LLVM::Backend::GetBuilder();
		auto& context = *LLVM::Backend::GetContext();
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() == 2, "incorrect dimensions");

		llvm::Value* LHS = children[1]->Codegen();
		llvm::Value* RHS = children[0]->Codegen();

		CLEAR_VERIFY(LHS && RHS, "lhs or rhs failed to generate");

		llvm::Value* LHSRawValue = LHS;
		llvm::Value* RHSRawValue = RHS;

		llvm::Type* expectedLLVMType = p_MetaData.Type.GetLLVMType();

		if (m_Expression == BinaryExpressionType::Assignment)
		{
			if (children[0]->GetMetaData().NeedLoading)
			{
				llvm::AllocaInst* alloc = llvm::dyn_cast<llvm::AllocaInst>(RHSRawValue);
				CLEAR_VERIFY(alloc, "");
				RHSRawValue = builder.CreateLoad(alloc->getAllocatedType(), alloc, "loaded_value");
			}

			if (RHSRawValue->getType() != expectedLLVMType && p_MetaData.Type)
				RHSRawValue = Value::CastValue(RHSRawValue, p_MetaData.Type);

			return _CreateExpression(LHS, RHS, LHSRawValue, RHSRawValue, p_MetaData.Type.IsSigned());
		}

		//let the array decay to a pointer
		if (children[1]->GetMetaData().NeedLoading)
		{
			llvm::AllocaInst* alloc = llvm::dyn_cast<llvm::AllocaInst>(LHSRawValue);
			CLEAR_VERIFY(alloc, "");
			LHSRawValue = builder.CreateLoad(alloc->getAllocatedType(), alloc, "loaded_value");
		}

		if (children[0]->GetMetaData().NeedLoading)
		{
			llvm::AllocaInst* alloc = llvm::dyn_cast<llvm::AllocaInst>(RHSRawValue);
			CLEAR_VERIFY(alloc, "");
			RHSRawValue = builder.CreateLoad(alloc->getAllocatedType(), alloc, "loaded_value");
		}

		if (m_Expression == BinaryExpressionType::PositivePointerArithmetic || m_Expression == BinaryExpressionType::NegatedPointerArithmetic)
		{
			AbstractType intType = AbstractType(VariableType::Int64);

			if (RHSRawValue->getType() != intType.GetLLVMType())
				RHSRawValue = Value::CastValue(RHSRawValue, intType, children[0]->GetMetaData().Type);

			return _CreatePointerArithmeticExpression(LHSRawValue, RHSRawValue);
		}

		AbstractType fromType = children[1]->GetMetaData().Type;

		if (LHSRawValue->getType() != expectedLLVMType && p_MetaData.Type && p_MetaData.Type.Get() != VariableType::Bool)
 			LHSRawValue = Value::CastValue(LHSRawValue, p_MetaData.Type, fromType);

		fromType = children[0]->GetMetaData().Type;

		if (RHSRawValue->getType() != expectedLLVMType && p_MetaData.Type && p_MetaData.Type.Get() != VariableType::Bool)
			RHSRawValue = Value::CastValue(RHSRawValue, p_MetaData.Type, fromType);

		if (m_Expression == BinaryExpressionType::PositivePointerArithmetic || m_Expression == BinaryExpressionType::NegatedPointerArithmetic)
			p_MetaData.Type = children[1]->GetMetaData().Type;

		return _CreateExpression(LHS, RHS, LHSRawValue, RHSRawValue, p_MetaData.Type.IsSigned());
	}
	const bool ASTBinaryExpression::_IsMathExpression() const
	{
		return (int)m_Expression <= (int)BinaryExpressionType::Mod;
	}

	const bool ASTBinaryExpression::_IsCmpExpression() const
	{
		return (int)m_Expression <= (int)BinaryExpressionType::NotEq && !_IsMathExpression();
	}

	const bool ASTBinaryExpression::_IsBitwiseExpression() const
	{
		return (int)m_Expression >= (int)BinaryExpressionType::BitwiseLeftShift;
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

	llvm::Value* ASTBinaryExpression::_CreatePointerArithmeticExpression(llvm::Value* LHS, llvm::Value* RHS)
	{
		auto& builder = *LLVM::Backend::GetBuilder();

		if (m_Expression == BinaryExpressionType::NegatedPointerArithmetic)
			RHS = builder.CreateNeg(RHS);

		return builder.CreateInBoundsGEP(p_MetaData.Type.GetLLVMUnderlying(), LHS, RHS);
	}

	ASTVariableExpression::ASTVariableExpression(const std::list<std::string>& chain, bool isPointer, bool dereference)
		: m_Chain(chain), m_PointerFlag(isPointer), m_Dereference(dereference)
	{
		p_MetaData.NeedLoading = true;
	}

	llvm::Value* ASTVariableExpression::Codegen()
	{
		auto& builder = *LLVM::Backend::GetBuilder();
		auto& context = *LLVM::Backend::GetContext();

		auto& metaData = Value::GetVariableMetaData(*m_Chain.begin());
		llvm::AllocaInst* value = metaData.Alloca;

		CLEAR_VERIFY(value, "value was nullptr");

		m_Chain.pop_front();

		if (m_Chain.empty())
		{
			p_MetaData.Type = metaData.Type;
			
			if (p_MetaData.Type.Get() == VariableType::Array)
			{
				p_MetaData.NeedLoading = false;
			}

			return value; 
		}

		StructMetaData* currentRef = &AbstractType::GetStructInfo(metaData.Type.GetUserDefinedType());

		std::vector<llvm::Value*> indices =
		{
			llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 0) 
		};
		
		AbstractType typeToGet;

		for (;;)
		{
			size_t currentIndex = currentRef->Indices[m_Chain.front()];
			indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), currentIndex));
			m_Chain.pop_front();

			typeToGet = currentRef->Types[currentIndex];

			if (m_Chain.empty())
				break;

			currentRef = &AbstractType::GetStructInfo(currentRef->Types[currentIndex].GetUserDefinedType());

			if (!currentRef)
				break;
		}

		llvm::Value* gepPtr = builder.CreateInBoundsGEP(value->getAllocatedType(), value, indices, "struct_element_ptr");
		p_MetaData.Type = typeToGet;

		if (p_MetaData.Type.Get() == VariableType::Array)
		{
			p_MetaData.NeedLoading = false;
		}

		return gepPtr; 
	}

	ASTVariableDeclaration::ASTVariableDeclaration(const std::string& name, AbstractType type)
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

	ASTFunctionDefinition::ASTFunctionDefinition(const std::string& name, const AbstractType& returnType, const std::vector<Paramater>& Paramaters)
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

		llvm::Type* returnType = p_MetaData.Type.GetLLVMType();

		std::vector<llvm::Type*> ParamaterTypes;

		for (const auto& Paramater : m_Paramaters)
		{
			ParamaterTypes.push_back(Paramater.Type.GetLLVMType());
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
			llvm::AllocaInst* argAlloc = builder.CreateAlloca(Paramater.Type.GetLLVMType(), nullptr, GetName() + "::" + Paramater.Name);
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

	ASTFunctionDecleration::ASTFunctionDecleration(const std::string& name, const AbstractType& expectedReturnType, const std::vector<Paramater>& types)
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

			types.push_back(type.Type.GetLLVMType());
		}

		if (!p_MetaData.Type)
			p_MetaData.Type = VariableType::None;

		llvm::FunctionType* functionType = llvm::FunctionType::get(p_MetaData.Type.GetLLVMType(), types, isVariadic);
		module.getOrInsertFunction(p_MetaData.Name, functionType);
		return nullptr;
	}

	ASTReturnStatement::ASTReturnStatement(const AbstractType& expectedReturnType, bool createReturn)
		: m_CreateReturn(createReturn)
	{
		p_MetaData.Type = expectedReturnType;
	}

	llvm::Value* ASTReturnStatement::Codegen()
	{
		auto& builder = *LLVM::Backend::GetBuilder();

		if (GetChildren().size() > 0)
		{
			llvm::Value* returnValue = GetChildren()[0]->Codegen();

			llvm::AllocaInst* tmp = nullptr;

			if ((tmp = llvm::dyn_cast<llvm::AllocaInst>(returnValue)))
			{
				returnValue = builder.CreateLoad(tmp->getAllocatedType(), tmp, "loaded_value");
			}

			if (returnValue->getType() != p_MetaData.Type.GetLLVMType())
				returnValue = Value::CastValue(returnValue, p_MetaData.Type);
			
			CLEAR_VERIFY(returnValue->getType() == p_MetaData.Type.GetLLVMType(), "unexpected return type");

			llvm::AllocaInst* alloc = s_ReturnValues.top().Alloca;
			
			builder.CreateStore(returnValue, alloc);
		}
		
		builder.CreateBr(s_ReturnValues.top().Return);

		return nullptr;
	}
	
	ASTExpression::ASTExpression(const AbstractType& expectedType)
	{
		p_MetaData.Type = expectedType;
	}

	llvm::Value* ASTExpression::Codegen()
	{
		auto& builder  = *LLVM::Backend::GetBuilder();
		auto& children = GetChildren();

		std::stack<Ref<ASTNodeBase>> stack;

		for (const auto& child : children)
		{
			if (child->GetType() == ASTNodeType::Literal ||
				child->GetType() == ASTNodeType::VariableExpression || 
				child->GetType() == ASTNodeType::FunctionCall)
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

		return stack.top()->Codegen();
	}

	ASTStruct::ASTStruct(const std::string& name, const std::vector<AbstractType::MemberType>& fields)
		:  m_Members(fields)
	{
		p_MetaData.Name = name;
	}

	llvm::Value* ASTStruct::Codegen()
	{
		AbstractType::CreateStructType(p_MetaData.Name, m_Members);
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
				llvm::AllocaInst* tmp = llvm::dyn_cast<llvm::AllocaInst>(gen);
				CLEAR_VERIFY(tmp, "");

				gen = builder.CreateLoad(tmp->getAllocatedType(), tmp, "loaded_value");
			}
		
			if (k < expected.Parameters.size() && !expected.Parameters[k].IsVariadic && gen->getType() != expected.Parameters[k].Type.GetLLVMType())
			{
				gen = Value::CastValue(gen, expected.Parameters[k].Type);
			}

			k++;
		
			CLEAR_VERIFY(gen, "not a valid argument");
			args.push_back(gen);
		}
		
		
		llvm::Function* callee = module.getFunction(p_MetaData.Name);
		CLEAR_VERIFY(callee, "not a valid function");
		
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

		int64_t i = 0;
		for (; i + 2 < children.size(); i += 2)
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

		for (size_t j = 0; j < branches.size(); j++)
		{
			auto& branch = branches[j];

			llvm::BasicBlock* nextBranch = (j + 1) < branches.size() ? branches[j + 1].ConditionBlock : elseBlock;
			
			function->insert(function->end(), branch.ConditionBlock);
			builder.SetInsertPoint(branch.ConditionBlock);

			llvm::Value* condition = children[branch.ExpressionIdx]->Codegen();

			if (condition->getType()->isIntegerTy())
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

	ASTUnaryExpression::ASTUnaryExpression(UnaryExpressionType type, const AbstractType& typeToCast)
		: m_Type(type)
	{
		p_MetaData.Type = typeToCast;
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
				CLEAR_VERIFY(inst,"");

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
				if (children[0]->GetMetaData().NeedLoading)
				{
					llvm::AllocaInst* alloc = llvm::dyn_cast<llvm::AllocaInst>(operand);
					operand = builder.CreateLoad(alloc->getAllocatedType(), alloc, "loaded_pointer");
				}

				llvm::PointerType* ptrType = llvm::dyn_cast<llvm::PointerType>(operand->getType());
				CLEAR_VERIFY(ptrType, "Dereference operand must be a pointer");

				CLEAR_VERIFY(children[0]->GetType() == ASTNodeType::BinaryExpression || 
							 children[0]->GetType() == ASTNodeType::VariableExpression, "invalid child");

				return builder.CreateLoad(metaData.Type.GetLLVMUnderlying(), operand, "dereferenced_value");
			}
			case UnaryExpressionType::Reference:
			{
				llvm::AllocaInst* inst = llvm::dyn_cast<llvm::AllocaInst>(operand);
 				CLEAR_VERIFY(inst, "must be an alloca");

				p_MetaData.NeedLoading = false;

				return inst;
			}
			case UnaryExpressionType::Cast:
			{
				if (children[0]->GetMetaData().NeedLoading)
				{
					llvm::AllocaInst* alloc = llvm::dyn_cast<llvm::AllocaInst>(operand);

					if (alloc->getAllocatedType()->isArrayTy())
					{
						CLEAR_VERIFY(p_MetaData.Type.IsPointer(), "");
						operand = builder.CreateInBoundsGEP(alloc->getAllocatedType(), alloc, { builder.getInt64(0), builder.getInt64(0)});
					}
					else
					{
						operand = builder.CreateLoad(alloc->getAllocatedType(), alloc, "loaded_value");
					}

				}

				AbstractType from = metaData.Type;
				return Value::CastValue(operand, p_MetaData.Type, from);
			}
			case UnaryExpressionType::None:
			default:
			{
				CLEAR_UNREACHABLE("how");
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

			if (childValue->getType() != elementType)
			{
				childValue = Value::CastValue(childValue, elementType, childValue->getType(), expression->GetMetaData().Type.IsSigned());
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

}