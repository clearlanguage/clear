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
		m_Name = name;
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
		return m_Constant.Get();
	}

	ASTBinaryExpression::ASTBinaryExpression(BinaryExpressionType type, AbstractType expectedType)
		: m_Expression(type), m_ExpectedType(expectedType)
	{
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

		llvm::Type* expectedLLVMType = m_ExpectedType.GetLLVMType();

		if (m_Expression == BinaryExpressionType::Assignment)
		{
			llvm::AllocaInst* tmp = nullptr;

			if (!m_ExpectedType.IsPointer() && (tmp = llvm::dyn_cast<llvm::AllocaInst>(RHSRawValue)))
			{
				RHSRawValue = builder.CreateLoad(tmp->getAllocatedType(), tmp, "loaded_value");
			}

			if (RHSRawValue->getType() != expectedLLVMType && m_ExpectedType)
				RHSRawValue = Value::CastValue(RHSRawValue, m_ExpectedType);

			return _CreateExpression(LHS, RHS, LHSRawValue, RHSRawValue, m_ExpectedType.IsSigned());
		}

		llvm::AllocaInst* tmp1 = nullptr;
		llvm::AllocaInst* tmp2 = nullptr;

		if ((tmp1 = llvm::dyn_cast<llvm::AllocaInst>(LHSRawValue)))
		{
			LHSRawValue = builder.CreateLoad(tmp1->getAllocatedType(), tmp1, "loaded_value");
		}

		if ((tmp2 = llvm::dyn_cast<llvm::AllocaInst>(RHSRawValue)))
		{
			RHSRawValue = builder.CreateLoad(tmp2->getAllocatedType(), tmp2, "loaded_value");
		}

		if (m_ExpectedType.IsPointer())
		{
			llvm::Value* pointer = LHSRawValue->getType()->isPointerTy() ? LHSRawValue : RHSRawValue;
			llvm::Value* integer = LHSRawValue->getType()->isPointerTy() ? RHSRawValue : LHSRawValue;

			CLEAR_VERIFY(integer->getType()->isIntegerTy(), "must be integral");
			CLEAR_VERIFY(m_Expression == BinaryExpressionType::Add || m_Expression == BinaryExpressionType::Sub, "not a valid expression");

			if (m_Expression == BinaryExpressionType::Sub)
				integer = builder.CreateMul(integer, Value::GetConstant(VariableType::Int64, "-1").first);

			return builder.CreateGEP(m_ExpectedType.GetLLVMUnderlying(), pointer, integer);
		}

		AbstractType fromType;

		if (children[1]->GetType() == ASTNodeType::Literal)
		{
			Ref<ASTNodeLiteral> literal = DynamicCast<ASTNodeLiteral>(children[1]);
			fromType = literal->GetGeneratedType();
		}
		else if (children[1]->GetType() == ASTNodeType::BinaryExpression)
		{
			Ref<ASTBinaryExpression> bin = DynamicCast<ASTBinaryExpression>(children[1]);
			fromType = bin->GetExpectedType();
		}

		if (LHSRawValue->getType() != expectedLLVMType && m_ExpectedType && m_ExpectedType.Get() != VariableType::Bool)
 			LHSRawValue = Value::CastValue(LHSRawValue, m_ExpectedType, fromType);

		if (children[0]->GetType() == ASTNodeType::Literal)
		{
			Ref<ASTNodeLiteral> literal = DynamicCast<ASTNodeLiteral>(children[0]);
			fromType = literal->GetGeneratedType();
		}
		else if (children[0]->GetType() == ASTNodeType::BinaryExpression)
		{
			Ref<ASTBinaryExpression> bin = DynamicCast<ASTBinaryExpression>(children[0]);
			fromType = bin->GetExpectedType();
		}

		if (RHSRawValue->getType() != expectedLLVMType && m_ExpectedType && m_ExpectedType.Get() != VariableType::Bool)
			RHSRawValue = Value::CastValue(RHSRawValue, m_ExpectedType, fromType);

		return _CreateExpression(LHS, RHS, LHSRawValue, RHSRawValue, m_ExpectedType.IsSigned());
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
		else if (_IsCmpExpression())
			return _CreateCmpExpression(LHSRawValue, RHSRawValue);
		else if (_IsBitwiseExpression())
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

	ASTVariableExpression::ASTVariableExpression(const std::list<std::string>& chain, bool isPointer, bool dereference)
		: m_Chain(chain), m_PointerFlag(isPointer), m_Dereference(dereference)
	{
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
			m_GeneratedType = metaData.Type;
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

		llvm::Value* gepPtr = builder.CreateGEP(value->getAllocatedType(), value, indices, "struct_element_ptr");
		m_GeneratedType = typeToGet;

		return gepPtr; 
	}

	ASTVariableDecleration::ASTVariableDecleration(const std::string& name, AbstractType type)
		: m_Name(name), m_Type(type)
	{
	}

	llvm::Value* ASTVariableDecleration::Codegen()
	{		
		auto& builder = *LLVM::Backend::GetBuilder();

		auto ip = builder.saveIP();

		llvm::Function* function = builder.GetInsertBlock()->getParent();

		builder.SetInsertPoint(&function->getEntryBlock());
	
		m_Value = Value(m_Type, m_Name);

		builder.restoreIP(ip);

		return m_Value.Get();
	}

	ASTFunctionDefinition::ASTFunctionDefinition(const std::string& name, const AbstractType& returnType, const std::vector<Paramater>& Paramaters)
		: m_ReturnType(returnType), m_Paramaters(Paramaters)
	{
		g_FunctionToExpectedTypes[name] = m_Paramaters;
		SetName(name);
	}
	llvm::Value* ASTFunctionDefinition::Codegen()
	{
		auto& module = *LLVM::Backend::GetModule();
		auto& context = *LLVM::Backend::GetContext();
		auto& builder = *LLVM::Backend::GetBuilder();

		s_InsertPoints.push(builder.saveIP());

		llvm::Type* returnType = m_ReturnType.GetLLVMType();

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
		: m_Name(name), m_ExpectedReturnType(expectedReturnType), m_ExpectedTypes(types)
	{
		g_FunctionToExpectedTypes[name] = types;
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

		if (!m_ExpectedReturnType)
			m_ExpectedReturnType = VariableType::None;

		llvm::FunctionType* functionType = llvm::FunctionType::get(m_ExpectedReturnType.GetLLVMType(), types, isVariadic);
		module.getOrInsertFunction(m_Name, functionType);
		return nullptr;
	}

	ASTReturnStatement::ASTReturnStatement(const AbstractType& expectedReturnType, bool createReturn)
		: m_ExpectedReturnType(expectedReturnType), m_CreateReturn(createReturn)
	{
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

			if (returnValue->getType() != m_ExpectedReturnType.GetLLVMType())
				returnValue = Value::CastValue(returnValue, m_ExpectedReturnType);
			
			CLEAR_VERIFY(returnValue->getType() == m_ExpectedReturnType.GetLLVMType(), "unexpected return type");

			llvm::AllocaInst* alloc = s_ReturnValues.top().Alloca;
			
			builder.CreateStore(returnValue, alloc);
		}
		
		builder.CreateBr(s_ReturnValues.top().Return);

		return nullptr;
	}
	
	ASTExpression::ASTExpression(const AbstractType& expectedType)
		: m_ExpectedType(expectedType)
	{
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
		: m_Name(name), m_Members(fields)
	{
	}

	llvm::Value* ASTStruct::Codegen()
	{
		AbstractType::CreateStructType(m_Name, m_Members);
		return nullptr;
	}

	ASTFunctionCall::ASTFunctionCall(const std::string& name)
		: m_Name(name)
	{
	}

	llvm::Value* ASTFunctionCall::Codegen()
	{
		std::vector<llvm::Value*> args;

		auto& builder = *LLVM::Backend::GetBuilder();
		auto& module  = *LLVM::Backend::GetModule();
		auto& context = *LLVM::Backend::GetContext();
		auto& children = GetChildren();

		auto& expected = g_FunctionToExpectedTypes.at(m_Name);

		uint32_t k = 0;

		for (auto& child : children)
		{
			llvm::Value* gen = child->Codegen();

			llvm::AllocaInst* tmp = nullptr;
			if ((tmp = llvm::dyn_cast<llvm::AllocaInst>(gen)))
			{
				gen = builder.CreateLoad(tmp->getAllocatedType(), tmp, "loaded_value");
			}
		
			if (k < expected.size() && !expected[k].IsVariadic && gen->getType() != expected[k].Type.GetLLVMType())
			{
				gen = Value::CastValue(gen, expected[k].Type);
			}

			k++;
		
			CLEAR_VERIFY(gen, "not a valid argument");
			args.push_back(gen);
		}
		
		
		llvm::Function* callee = module.getFunction(m_Name);
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
			llvm::BasicBlock* Block = nullptr;
			size_t ExpressionIdx = 0;
		};

		CLEAR_VERIFY(children[0]->GetType() == ASTNodeType::Expression,"");

		std::vector<Branch> branches;
		branches.push_back({ llvm::BasicBlock::Create(context, "then", function), 0 });

		for (size_t i = 1; i < children.size(); i++)
		{
			if (children[i]->GetType() == ASTNodeType::Expression)
				branches.push_back({ llvm::BasicBlock::Create(context, "else_then"), i });
		}

		llvm::BasicBlock* elseBlock = llvm::BasicBlock::Create(context, "else");
		llvm::BasicBlock* mergeBranch = llvm::BasicBlock::Create(context, "merge");

		Ref<ASTNodeBase> condition = children[branches[0].ExpressionIdx];

		llvm::Value* conditionVal = condition->Codegen();

		if (conditionVal->getType()->isIntegerTy())
			conditionVal = builder.CreateICmpNE(conditionVal, llvm::ConstantInt::get(conditionVal->getType(), 0));
		else if (conditionVal->getType()->isFloatingPointTy())
			conditionVal = builder.CreateFCmpONE(conditionVal, llvm::ConstantFP::get(conditionVal->getType(), 0.0));


		builder.CreateCondBr(conditionVal, branches[0].Block, branches.size() > 1 ? branches[1].Block : elseBlock);
		builder.SetInsertPoint(branches[0].Block);

		size_t lastBranchIndex = 0;

		for (size_t i = 1; i < branches.size(); i++)
		{
			llvm::BasicBlock* nextBranch = (i + 1) == branches.size() ? elseBlock : branches[i + 1].Block;

			children[lastBranchIndex + 1]->Codegen();

			condition = children[branches[i].ExpressionIdx];

			if (!builder.GetInsertBlock()->getTerminator())
			{
				conditionVal = condition->Codegen();

				if (conditionVal->getType()->isIntegerTy())
					conditionVal = builder.CreateICmpNE(conditionVal, llvm::ConstantInt::get(conditionVal->getType(), 0));
				else if (conditionVal->getType()->isFloatingPointTy())
					conditionVal = builder.CreateFCmpONE(conditionVal, llvm::ConstantFP::get(conditionVal->getType(), 0.0));


				builder.CreateCondBr(conditionVal, branches[i].Block, nextBranch);
			}

			function->insert(function->end(), branches[i].Block);
			builder.SetInsertPoint(branches[i].Block);

			lastBranchIndex = branches[i].ExpressionIdx;
		}

		children[lastBranchIndex + 1]->Codegen();

		if (!builder.GetInsertBlock()->getTerminator())
		{
			conditionVal = children[branches.back().ExpressionIdx]->Codegen();

			if (conditionVal->getType()->isIntegerTy())
				conditionVal = builder.CreateICmpNE(conditionVal, llvm::ConstantInt::get(conditionVal->getType(), 0));
			else if (conditionVal->getType()->isFloatingPointTy())
				conditionVal = builder.CreateFCmpONE(conditionVal, llvm::ConstantFP::get(conditionVal->getType(), 0.0));

			builder.CreateCondBr(conditionVal, branches.back().Block, elseBlock);
		}

		function->insert(function->end(), elseBlock);
		builder.SetInsertPoint(elseBlock);

		size_t lastChild = children.size() - 1;
		if (children[lastChild - 1]->GetType() != ASTNodeType::Expression)
			children.back()->Codegen();

		builder.CreateBr(mergeBranch);
		
		function->insert(function->end(), mergeBranch);
		builder.SetInsertPoint(mergeBranch);

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
		: m_Type(type), m_TypeToCast(typeToCast)
	{
	}

	llvm::Value* ASTUnaryExpression::Codegen()
	{
		auto& builder = *LLVM::Backend::GetBuilder();
		auto& context = *LLVM::Backend::GetContext();
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() == 1, "incorrect dimensions");

		llvm::Value* operand = children[0]->Codegen();

		switch (m_Type)
		{
			case UnaryExpressionType::BitwiseNot:
			{
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
				if (operand->getType()->isFloatingPointTy())
					return builder.CreateFNeg(operand);
				
				return builder.CreateNeg(operand);
			}
			case UnaryExpressionType::Dereference:
			{
				llvm::PointerType* ptrType = llvm::dyn_cast<llvm::PointerType>(operand->getType());
				CLEAR_VERIFY(ptrType, "Dereference operand must be a pointer");

				CLEAR_VERIFY(children[0]->GetType() == ASTNodeType::BinaryExpression || 
							 children[0]->GetType() == ASTNodeType::VariableExpression, "invalid child");

				if (children[0]->GetType() == ASTNodeType::BinaryExpression)
				{
					Ref<ASTBinaryExpression> expression = DynamicCast<ASTBinaryExpression>(children[0]);
					llvm::Value* value = operand;
					
					if (!llvm::isa<llvm::GetElementPtrInst>(operand))
						value = builder.CreateLoad(expression->GetExpectedType().GetLLVMType(), operand, "loaded_pointer");

					return builder.CreateLoad(expression->GetExpectedType().GetLLVMUnderlying(), value, "dereferenced_value");
				}

				Ref<ASTVariableExpression> expression = DynamicCast<ASTVariableExpression>(children[0]);
				llvm::Value* value = operand;

				if (!llvm::isa<llvm::GetElementPtrInst>(operand))
					value = builder.CreateLoad(expression->GetGeneratedType().GetLLVMType(), operand, "loaded_pointer");

				return builder.CreateLoad(expression->GetGeneratedType().GetLLVMUnderlying(), value, "dereferenced_value");
			}
			case UnaryExpressionType::Reference:
			{
				llvm::AllocaInst* inst = llvm::dyn_cast<llvm::AllocaInst>(operand);
				CLEAR_VERIFY(inst, "must be an alloca");

				return inst;
			}
			case UnaryExpressionType::Cast:
			{
				AbstractType from;

				if (children[0]->GetType() == ASTNodeType::BinaryExpression)
				{
					Ref<ASTBinaryExpression> expression = DynamicCast<ASTBinaryExpression>(children[0]);
					from = expression->GetExpectedType();
				}
				else if (children[0]->GetType() == ASTNodeType::Expression)
				{
					Ref<ASTExpression> expression = DynamicCast<ASTExpression>(children[0]);
					from = expression->GetExpectedType();
				}
				else if (children[0]->GetType() == ASTNodeType::VariableExpression)
				{
					Ref<ASTVariableExpression> expression = DynamicCast<ASTVariableExpression>(children[0]);
					from = expression->GetGeneratedType();
				}

				return Value::CastValue(operand, m_TypeToCast, from);
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
				childValue = Value::CastValue(childValue, elementType, childValue->getType(), expression->GetGeneratedType().IsSigned());
			}

			llvm::Value* gep = builder.CreateGEP(arrayType, allocaInstance, m_Indices[i - 1], "array_indexing");
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