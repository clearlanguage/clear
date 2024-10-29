#include "ASTNode.h"

#include "API/LLVM/LLVMBackend.h"
#include "Core/Log.h"
#include "Core/Types.h"
#include "Core/Utils.h"

#include <iostream>
#include <map>
#include <stack>

namespace clear {

	static std::stack<llvm::IRBuilderBase::InsertPoint>  s_InsertPoints;
	static std::map<std::string, std::vector<Paramater>> s_FunctionToExpectedTypes;

	llvm::Value* ASTNodeBase::Codegen()
	{
		for (auto& child : GetChildren())
			child->Codegen();

		return nullptr;
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

		if (children.size() != 2)
			return nullptr;

		llvm::Value* LHS = children[1]->Codegen();
		llvm::Value* RHS = children[0]->Codegen();

		CLEAR_VERIFY(LHS && RHS, "lhs or rhs failed to generate");

		llvm::Value* LHSRawValue = LHS;
		llvm::Value* RHSRawValue = RHS;

		if (!m_ExpectedType)
		{
			auto& str = m_ExpectedType.GetUserDefinedType();

			std::vector<std::string> words = Split(str, '.');

			VariableMetaData& metaData = Value::GetVariableMetaData(words[0]);
			AbstractType type = metaData.Type;

			for (size_t i = 1; i < words.size(); i++)
			{
				StructMetaData& structMetaData = AbstractType::GetStructInfo(type.GetUserDefinedType());
				CLEAR_VERIFY(structMetaData.Struct, "not a valid type ", type.GetUserDefinedType());

				size_t indexToNextType = structMetaData.Indices[words[i]];
				type = structMetaData.Types[indexToNextType];
			}

			m_ExpectedType = type;
		}

		llvm::Type* expectedLLVMType = m_ExpectedType.GetLLVMType();

		if (m_Expression == BinaryExpressionType::Assignment)
		{
			if (RHSRawValue->getType() != expectedLLVMType && m_ExpectedType)
				RHSRawValue = Value::CastValue(RHSRawValue, m_ExpectedType);

			return _CreateExpression(LHS, RHS, LHSRawValue, RHSRawValue, m_ExpectedType.IsSigned());
		}

		if ((LHSRawValue->getType()->isPointerTy() || RHSRawValue->getType()->isPointerTy()) && m_ExpectedType.IsPointer())
		{
			llvm::Value* pointer = LHSRawValue->getType()->isPointerTy() ? LHSRawValue : RHSRawValue;
			llvm::Value* integer = LHSRawValue->getType()->isPointerTy() ? RHSRawValue : LHSRawValue;


			CLEAR_VERIFY(integer->getType()->isIntegerTy(), "must be integral");
			CLEAR_VERIFY(m_Expression == BinaryExpressionType::Add || m_Expression == BinaryExpressionType::Sub, "not a valid expression");

			if (m_Expression == BinaryExpressionType::Sub)
				integer = builder.CreateMul(integer, Value::GetConstant(VariableType::Int64, "-1").first);


			return builder.CreateGEP(m_ExpectedType.GetLLVMUnderlying(), pointer, integer);
		}

		if (LHSRawValue->getType() != expectedLLVMType && m_ExpectedType)
 			LHSRawValue = Value::CastValue(LHSRawValue, m_ExpectedType);

		if (RHSRawValue->getType() != expectedLLVMType && m_ExpectedType)
			RHSRawValue = Value::CastValue(RHSRawValue, m_ExpectedType);

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
			if (m_Dereference)
			{
				llvm::Value* loadedPointer = builder.CreateLoad(value->getAllocatedType(), value, "loaded_pointer");
				return builder.CreateLoad(metaData.Type.GetLLVMUnderlying(), loadedPointer, "dereferenced_value");
			}
			
			if (!m_Dereference && !m_PointerFlag)
			{
				return builder.CreateLoad(metaData.Type.GetLLVMUnderlying(), value, "loaded_value");
			}

			CLEAR_VERIFY(m_PointerFlag && !m_Dereference, "cannot dereference a non pointer type");

			return value; 
		}

		StructMetaData* currentRef = &AbstractType::GetStructInfo(metaData.Type.GetUserDefinedType());

		std::vector<llvm::Value*> indices =
		{
			llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0) 
		};

		for (;;)
		{
			size_t currentIndex = currentRef->Indices[m_Chain.front()];
			indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), currentIndex));
			m_Chain.pop_front();

			if (m_Chain.empty())
				break;

			currentRef = &AbstractType::GetStructInfo(currentRef->Types[currentIndex].GetUserDefinedType());

			if (!currentRef)
				break;
		}

		llvm::Value* gepPtr = builder.CreateGEP(value->getAllocatedType(), value, indices, "element_ptr");

		if (m_Dereference && m_PointerFlag)
		{
			llvm::Value* loadedPointer = builder.CreateLoad(gepPtr->getType(), gepPtr, "loaded_pointer");
			return builder.CreateLoad(metaData.Type.GetLLVMUnderlying(), loadedPointer, "dereferenced_value");
		}

		if (!m_Dereference && !m_PointerFlag)
		{
			return builder.CreateLoad(metaData.Type.GetLLVMUnderlying(), gepPtr, "loaded_value");
		}

		CLEAR_VERIFY(m_PointerFlag && !m_Dereference, "cannot dereference a non pointer type");

		return gepPtr; 
	}

	ASTVariableDecleration::ASTVariableDecleration(const std::string& name, AbstractType type)
		: m_Name(name), m_Type(type)
	{
	}

	llvm::Value* ASTVariableDecleration::Codegen()
	{		
		m_Value = Value(m_Type, m_Name);
		return m_Value.Get();
	}

	ASTFunctionDecleration::ASTFunctionDecleration(const std::string& name, const AbstractType& returnType, const std::vector<Paramater>& Paramaters)
		: m_ReturnType(returnType), m_Paramaters(Paramaters)
	{
		s_FunctionToExpectedTypes[name] = m_Paramaters;
		SetName(name);

		if (!s_FunctionToExpectedTypes.contains("_sleep")) //TODO: remove thhis immediately
		{
			auto& e = s_FunctionToExpectedTypes["_sleep"];
			e.push_back({ .Name = "time", .Type = AbstractType(VariableType::Int32) });
		}

		if (!s_FunctionToExpectedTypes.contains("printf"))
		{
			auto& e = s_FunctionToExpectedTypes["printf"];
			e.push_back({ .Name = "fmt", .Type = AbstractType(VariableType::String) });
			e.push_back({ .Name = "msg", .Type = AbstractType(VariableType::String) });
		}

	}
	llvm::Value* ASTFunctionDecleration::Codegen()
	{
		auto& module  = *LLVM::Backend::GetModule();
		auto& context = *LLVM::Backend::GetContext();
		auto& builder = *LLVM::Backend::GetBuilder();

		s_InsertPoints.push(builder.saveIP());

		llvm::Type* returnType = m_ReturnType.GetLLVMType();

		std::vector<llvm::Type*> ParamaterTypes;
		for (const auto& Paramater : m_Paramaters)
		{
			ParamaterTypes.push_back(GetLLVMVariableType(Paramater.Type));
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
		builder.SetInsertPoint(entry);

		uint32_t k = 0;

		for (const auto& Paramater : m_Paramaters)
		{
			llvm::AllocaInst* argAlloc = builder.CreateAlloca(GetLLVMVariableType(Paramater.Type), nullptr, Paramater.Name);
			builder.CreateStore(function->getArg(k), argAlloc);
			
			Value::RegisterVariable(argAlloc, GetName() + "::" + Paramater.Name, Paramater.Type);

			k++;
		}

		for (const auto& child : GetChildren())
		{
			child->Codegen();
		}

		for (const auto& Paramater : m_Paramaters)
		{
			Value::RemoveVariable(GetName() + "::" + Paramater.Name);
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

	ASTReturnStatement::ASTReturnStatement(const AbstractType& expectedReturnType)
		: m_ExpectedReturnType(expectedReturnType)
	{
	}

	llvm::Value* ASTReturnStatement::Codegen()
	{
		auto& builder = *LLVM::Backend::GetBuilder();

		if (GetChildren().size() > 0)
		{
			llvm::Value* returnValue = GetChildren()[0]->Codegen();
			CLEAR_VERIFY(returnValue->getType() == m_ExpectedReturnType.GetLLVMType(), "unexpected return type");

			return builder.CreateRet(returnValue);
		}

		CLEAR_VERIFY(m_ExpectedReturnType.Get() == VariableType::None, "unexpected return type");

		return builder.CreateRetVoid();
	}
	llvm::Value* ASTExpression::Codegen()
	{
		auto& builder  = *LLVM::Backend::GetBuilder();
		auto& children = GetChildren();

		std::stack<Ref<ASTNodeBase>> stack;

		for (const auto& child : children)
		{
			if (child->GetType() == ASTNodeType::Literal ||
				child->GetType() == ASTNodeType::VariableExpression)
			{
				stack.push(child);
				continue;
			}

			Ref<ASTBinaryExpression> binExp = Ref<ASTBinaryExpression>::DynamicCast<ASTBinaryExpression>(child);

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

		auto& expected = s_FunctionToExpectedTypes.at(m_Name);

		/*uint32_t k = 0;
		for (const auto& argument : m_Arguments)
		{
			if(argument.Field.GetKind() == TypeKind::RValue)
			{
				auto& variableType = argument.Field;
				auto [value, _] = Value::GetConstant(variableType, argument.Data);

				if (variableType.Get() != expected[k].Type.Get())
					value = Value::CastValue(value, expected[k].Type);

				args.push_back(value);
			}
			else
			{
				auto& variableName = argument.Data;
				auto& variableType = argument.Field;

				std::vector<std::string> chain = Split(variableName, '.');

				auto& metaData = Value::GetVariableMetaData(chain[0]);
				auto variable  = metaData.Alloca;

				if (chain.size() == 1)
				{
					llvm::Value* value = builder.CreateLoad(variable->getAllocatedType(), variable);

					if (variableType.Get() != expected[k].Type.Get()) 
						value = Value::CastValue(value, expected[k].Type);

					args.push_back(value);
				}
				else
				{
					AbstractType currentType = metaData.Type;

					std::vector<llvm::Value*> indices =
					{
						llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0)
					};

					for (size_t i = 1; i < chain.size(); i++)
					{
						auto& structMetaData = AbstractType::GetStructInfo(currentType.GetUserDefinedType());

						size_t currentIndex = structMetaData.Indices[chain[i]];
						indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), currentIndex));

						currentType = structMetaData.Types[currentIndex];
					}


					llvm::Value* elementPointer = builder.CreateGEP(variable->getAllocatedType(), variable, indices, "element_ptr");

					llvm::Value* value = builder.CreateLoad(currentType.GetLLVMType(), elementPointer);

					if (currentType.Get() != expected[k].Type.Get())
						value = Value::CastValue(value, expected[k].Type);

					args.push_back(value);
				}
			}
			k++;
		}*/

		uint32_t k = 0;

		for (auto& child : children)
		{
			llvm::Value* gen = child->Codegen();

			if (gen->getType() != expected[k].Type.GetLLVMType())
				gen = Value::CastValue(gen, expected[k].Type);

			CLEAR_VERIFY(gen, "not a valid argument");
			args.push_back(gen);
		}


		if (m_Name == "_sleep")
		{
			llvm::Function* sleepFunc = llvm::cast<llvm::Function>(
				module.getOrInsertFunction("_sleep",
					llvm::FunctionType::get(llvm::Type::getInt32Ty(module.getContext()),
						llvm::Type::getInt32Ty(module.getContext()),
						false)).getCallee());
		}
		else if (m_Name == "sleep")
		{
			llvm::Function* sleepFunc = llvm::cast<llvm::Function>(
				module.getOrInsertFunction("sleep",
					llvm::FunctionType::get(llvm::Type::getInt32Ty(module.getContext()),
						llvm::Type::getInt32Ty(module.getContext()),
						false)).getCallee());
		}
		else if (m_Name == "nanosleep")
		{
			llvm::Function* sleepFunc = llvm::cast<llvm::Function>(
				module.getOrInsertFunction("nanosleep",
					llvm::FunctionType::get(llvm::Type::getInt32Ty(module.getContext()),
						llvm::Type::getInt32Ty(module.getContext()),
						false)).getCallee());
		}
		else if (m_Name == "printf")
		{
			llvm::Function* printfFunc = llvm::cast<llvm::Function>(
				module.getOrInsertFunction("printf",
					llvm::FunctionType::get(
						llvm::Type::getInt32Ty(module.getContext()),              // Return type: int
						{ llvm::PointerType::get(llvm::Type::getInt8Ty(module.getContext()), 0) }, // First arg: const char*
						true                                                      // Is variadic
					)).getCallee());
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

		llvm::BasicBlock* thenBranch  = llvm::BasicBlock::Create(context, "then", function);
		llvm::BasicBlock* elseBranch  = llvm::BasicBlock::Create(context, "else");
		llvm::BasicBlock* mergeBranch = llvm::BasicBlock::Create(context, "merge");

		//first child is a condition
		CLEAR_VERIFY(children[0]->GetType() == ASTNodeType::Expression, "node must be an expression");

		llvm::Value* condition = children[0]->Codegen();
		builder.CreateCondBr(condition, thenBranch, elseBranch);

		builder.SetInsertPoint(thenBranch);

		//next has to be a NodeBase/FunctionCall/etc...
		llvm::Value* ifCode = children[1]->Codegen();

		if (!ifCode)
			ifCode = llvm::ConstantFP::get(context, llvm::APFloat(0.0));

		builder.CreateBr(mergeBranch);

		thenBranch = builder.GetInsertBlock();

		//TODO: add else if

		function->insert(function->end(), elseBranch);
		builder.SetInsertPoint(elseBranch);

		
		llvm::Value* elseCode = llvm::ConstantFP::get(context, llvm::APFloat(0.0));

		if (children.size() > 2)
		{
			llvm::Value* val = children[2]->Codegen();
			elseCode = val == nullptr ? elseCode : val;
		}
		
		builder.CreateBr(mergeBranch);
		elseBranch = builder.GetInsertBlock();

		function->insert(function->end(), mergeBranch);
		builder.SetInsertPoint(mergeBranch);

		llvm::PHINode* PN = builder.CreatePHI(llvm::Type::getDoubleTy(context), 2, "iftmp");

		PN->addIncoming(ifCode, thenBranch);
		PN->addIncoming(elseCode, elseBranch);

		return PN;
	}
}