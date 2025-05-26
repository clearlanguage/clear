#include "ASTNode.h"

#include "API/LLVM/LLVMBackend.h"
#include "Core/Log.h"
#include "TypeCasting.h"

#include "Core/TypeRegistry.h"

#include <stack>

namespace clear 
{
	static std::stack<llvm::IRBuilderBase::InsertPoint>  s_InsertPoints;

    ASTNodeBase::ASTNodeBase()
    {
    }

    CodegenResult ASTNodeBase::Codegen()
    {

        CodegenResult value;

		for (auto& child : GetChildren())
			value = child->Codegen();

		return value;
    }

    void ASTNodeBase::Push(const std::shared_ptr<ASTNodeBase>& child)
    {
        m_Children.push_back(child);
    }


    void ASTNodeBase::Remove(const std::shared_ptr<ASTNodeBase>& child)
    {
        if(auto pos = std::find(m_Children.begin(), m_Children.end(), child) != m_Children.end())
            m_Children.erase(m_Children.begin() + pos);
    }

    void ASTNodeBase::PropagateSymbolTableToChildren()
    {
		for(auto& child : m_Children) 
			child->PropagateSymbolTable(m_SymbolTable);
    }

    void ASTNodeBase::CreateSymbolTable()
    {
		m_SymbolTable = std::make_shared<SymbolTable>();
    }

    void ASTNodeBase::PropagateSymbolTable(const std::shared_ptr<SymbolTable>& registry)
    {
		if(m_SymbolTable) 
		{
			m_SymbolTable->SetPrevious(registry);
		}
		else 
		{
			m_SymbolTable = registry;
		}

		for(auto& child : m_Children)
		{
			child->PropagateSymbolTable(registry);
		}
    }

    ASTNodeLiteral::ASTNodeLiteral(const Token& data)
		: m_Constant(data)
	{
	}

	CodegenResult ASTNodeLiteral::Codegen()
	{
		return {m_Constant.Get(), m_Constant.GetType()};
	}

    ASTBinaryExpression::ASTBinaryExpression(BinaryExpressionType type)
		: m_Expression(type)
	{
	}
	
	CodegenResult ASTBinaryExpression::Codegen() 
	{
		auto& builder = *LLVM::Backend::GetBuilder();
		auto& context = *LLVM::Backend::GetContext();
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() == 2, "incorrect dimensions");

		auto& leftChild  = children[1];
		auto& rightChild = children[0];

		CodegenResult lhs = leftChild->Codegen();
		CodegenResult rhs = rightChild->Codegen();

        if(!lhs.CodegenValue->getType()->isPointerTy()) 
			HandleTypePromotion(lhs, rhs);

		if(IsMathExpression()) 
			return HandleMathExpression(lhs, rhs, m_Expression);

		if(IsCmpExpression()) 
			return HandleCmpExpression(lhs, rhs);

		return {}; //TODO
    }

    void ASTBinaryExpression::HandleTypePromotion(CodegenResult& lhs, CodegenResult& rhs)
    {
        auto& builder = *LLVM::Backend::GetBuilder();

        llvm::Type* lhsType = lhs.CodegenType->Get();
        llvm::Type* rhsType = rhs.CodegenType->Get();

        //same type ignore rest
        if (lhsType == rhsType)
            return;

        // int -> float
        if (lhsType->isIntegerTy() && rhsType->isFloatingPointTy()) 
        {
			if(lhs.CodegenType->IsSigned())
            	lhs.CodegenValue = builder.CreateSIToFP(lhs.CodegenValue, rhsType, "cast");
			else 
            	lhs.CodegenValue = builder.CreateUIToFP(lhs.CodegenValue, rhsType, "cast");

            lhs.CodegenType = rhs.CodegenType;
        } 
        else if (lhsType->isFloatingPointTy() && rhsType->isIntegerTy()) 
        {
			if(rhs.CodegenType->IsSigned())
            	rhs.CodegenValue = builder.CreateSIToFP(rhs.CodegenValue, lhsType, "cast");
			else 
            	rhs.CodegenValue = builder.CreateUIToFP(rhs.CodegenValue, lhsType, "cast");

            rhs.CodegenType = lhs.CodegenType;
        }
        // float -> double
        else if (lhsType->isFloatTy() && rhsType->isDoubleTy()) 
        {
            lhs.CodegenValue = builder.CreateFPExt(lhs.CodegenValue, rhsType, "cast");
            lhs.CodegenType = rhs.CodegenType;
        } 
        else if (lhsType->isDoubleTy() && rhsType->isFloatTy()) 
        {
            rhs.CodegenValue = builder.CreateFPExt(rhs.CodegenValue, lhsType, "cast");
            rhs.CodegenType = lhs.CodegenType;
        }
        // small int -> big int 
        else if (lhsType->isIntegerTy() && rhsType->isIntegerTy())
        {
            uint32_t lhsBits = lhsType->getIntegerBitWidth();
            uint32_t rhsBits = rhsType->getIntegerBitWidth();

            if (lhsBits < rhsBits) 
            {
                if(lhs.CodegenType->IsSigned())
                {
                    lhs.CodegenValue = builder.CreateSExt(lhs.CodegenValue, rhsType, "cast");
                }
                else 
                {
                    lhs.CodegenValue =  builder.CreateZExt(lhs.CodegenValue, rhsType, "cast");
                }

                lhs.CodegenType = rhs.CodegenType;
            } 
            else 
            {
                if(rhs.CodegenType->IsSigned())
                {
                    rhs.CodegenValue = builder.CreateSExt(rhs.CodegenValue, lhsType, "cast");
                }
                else 
                {
                    rhs.CodegenValue =  builder.CreateZExt(rhs.CodegenValue, lhsType, "cast");
                }

                rhs.CodegenType = lhs.CodegenType;
            }
        }
        else 
        {
            CLEAR_UNREACHABLE("unsupported type promotion");
        }
    }

    bool ASTBinaryExpression::IsMathExpression() const
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

    bool ASTBinaryExpression::IsCmpExpression() const
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

    bool ASTBinaryExpression::IsBitwiseExpression() const
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

    CodegenResult ASTBinaryExpression::HandleMathExpression(CodegenResult& lhs, CodegenResult& rhs,  BinaryExpressionType type)
    {
        if(lhs.CodegenValue->getType()->isFloatingPointTy()) 
			return HandleMathExpressionF(lhs, rhs, type);

		if(lhs.CodegenType->IsSigned() || rhs.CodegenType->IsSigned()) 
			return HandleMathExpressionSI(lhs, rhs, type);

		return HandleMathExpressionUI(lhs, rhs, type);
    }

    CodegenResult ASTBinaryExpression::HandleMathExpressionF(CodegenResult &lhs, CodegenResult &rhs, BinaryExpressionType binExpressionType)
    {
        auto& builder = *LLVM::Backend::GetBuilder();
		auto& module  = *LLVM::Backend::GetModule();

		switch (binExpressionType)
		{
			case BinaryExpressionType::Add:
			{
                return { builder.CreateFAdd(lhs.CodegenValue, rhs.CodegenValue, "faddtmp"), lhs.CodegenType };
			}
			case BinaryExpressionType::Sub:
			{
                return { builder.CreateFSub(lhs.CodegenValue, rhs.CodegenValue, "fsubtmp"), lhs.CodegenType };
			}
			case BinaryExpressionType::Mul:
			{
                return { builder.CreateFMul(lhs.CodegenValue, rhs.CodegenValue, "fmultmp"), lhs.CodegenType };
			}
			case BinaryExpressionType::Div:
			{
                return { builder.CreateFDiv(lhs.CodegenValue, rhs.CodegenValue, "fdivtmp"), lhs.CodegenType };
			}
			case BinaryExpressionType::Mod:
			{
				CLEAR_UNREACHABLE("cannot do mod on floating type");
				break;
			}
			case BinaryExpressionType::Pow:
			{
				llvm::Function* powFunction = llvm::Intrinsic::getDeclaration(&module, llvm::Intrinsic::pow, { builder.getDoubleTy() });
                return { builder.CreateCall(powFunction, {lhs.CodegenValue, rhs.CodegenValue}), 
					     TypeRegistry::GetGlobal()->GetType("float64")  };
			}
			default:
				break;
		}

		return {};
    }

    CodegenResult ASTBinaryExpression::HandleMathExpressionSI(CodegenResult& lhs, CodegenResult& rhs, BinaryExpressionType binExpressionType)
    {
        auto& builder = *LLVM::Backend::GetBuilder();
		auto& module  = *LLVM::Backend::GetModule();

		std::shared_ptr<Type> type = lhs.CodegenType->IsSigned() ? lhs.CodegenType : rhs.CodegenType;

		switch (binExpressionType)
		{
			case BinaryExpressionType::Add:
			{
                return { builder.CreateAdd(lhs.CodegenValue, rhs.CodegenValue, "faddtmp"), type };
			}
			case BinaryExpressionType::Sub:
			{
                return { builder.CreateSub(lhs.CodegenValue, rhs.CodegenValue, "fsubtmp"), type };
			}
			case BinaryExpressionType::Mul:
			{
                return { builder.CreateMul(lhs.CodegenValue, rhs.CodegenValue, "fmultmp"), type };
			}
			case BinaryExpressionType::Div:
			{
                return { builder.CreateSDiv(lhs.CodegenValue, rhs.CodegenValue, "fdivtmp"), type };
			}
			case BinaryExpressionType::Mod:
			{
                return { builder.CreateSRem(lhs.CodegenValue, rhs.CodegenValue, "modtmp"), type };
				break;
			}
			case BinaryExpressionType::Pow:
			{
				llvm::Function* powFunction = llvm::Intrinsic::getDeclaration(&module, llvm::Intrinsic::pow, { builder.getDoubleTy() });
                return { builder.CreateCall(powFunction, {lhs.CodegenValue, rhs.CodegenValue}), 
					     TypeRegistry::GetGlobal()->GetType("float64") };
			}
			default:
				break;
		}

		return {};
    }

    CodegenResult ASTBinaryExpression::HandleMathExpressionUI(CodegenResult& lhs, CodegenResult& rhs, BinaryExpressionType type)
    {
        auto& builder = *LLVM::Backend::GetBuilder();
		auto& module  = *LLVM::Backend::GetModule();

		switch (type)
		{
			case BinaryExpressionType::Add:
			{
                return { builder.CreateAdd(lhs.CodegenValue, rhs.CodegenValue, "faddtmp"), lhs.CodegenType };
			}
			case BinaryExpressionType::Sub:
			{
                return { builder.CreateSub(lhs.CodegenValue, rhs.CodegenValue, "fsubtmp"), lhs.CodegenType };
			}
			case BinaryExpressionType::Mul:
			{
                return { builder.CreateMul(lhs.CodegenValue, rhs.CodegenValue, "fmultmp"), lhs.CodegenType };
			}
			case BinaryExpressionType::Div:
			{
                return { builder.CreateUDiv(lhs.CodegenValue, rhs.CodegenValue, "fdivtmp"), lhs.CodegenType };
			}
			case BinaryExpressionType::Mod:
			{
                return { builder.CreateURem(lhs.CodegenValue, rhs.CodegenValue, "modtmp"), lhs.CodegenType };				
				break;
			}
			case BinaryExpressionType::Pow:
			{
				llvm::Function* powFunction = llvm::Intrinsic::getDeclaration(&module, llvm::Intrinsic::pow, { builder.getDoubleTy() });
                return { builder.CreateCall(powFunction, {lhs.CodegenValue, rhs.CodegenValue}), 
						 TypeRegistry::GetGlobal()->GetType("float64") };
			}
			default:
				break;
		}

		return {};
    }

    CodegenResult ASTBinaryExpression::HandleCmpExpression(CodegenResult& lhs, CodegenResult& rhs)
    {
        if(lhs.CodegenValue->getType()->isFloatingPointTy()) 
			return HandleCmpExpressionF(lhs, rhs);

		if(lhs.CodegenType->IsSigned() || rhs.CodegenType->IsSigned()) 
			return HandleCmpExpressionSI(lhs, rhs);

		return HandleCmpExpressionUI(lhs, rhs);
    }

    CodegenResult ASTBinaryExpression::HandleCmpExpressionF(CodegenResult &lhs, CodegenResult &rhs)
    {
        auto& builder = *LLVM::Backend::GetBuilder();

		switch (m_Expression)
		{
			case BinaryExpressionType::Less:
            {
                return { builder.CreateFCmpOLT(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };
            }
			case BinaryExpressionType::LessEq:
            {
                return { builder.CreateFCmpOLE(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };
            }
			case BinaryExpressionType::Greater:
            {
                return { builder.CreateFCmpOGT(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };
            }
			case BinaryExpressionType::GreaterEq:
			{
                return { builder.CreateFCmpOGE(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };

			}
			case BinaryExpressionType::Eq:
			{
                return { builder.CreateFCmpOEQ(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };
			}
			case BinaryExpressionType::NotEq:
			{
                return { builder.CreateFCmpONE(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };
			}
			default:
				break;
		}

		return {};
    }

    CodegenResult ASTBinaryExpression::HandleCmpExpressionSI(CodegenResult &lhs, CodegenResult &rhs)
    {
        auto& builder = *LLVM::Backend::GetBuilder();

		switch (m_Expression)
		{
			case BinaryExpressionType::Less:
            {
                return { builder.CreateICmpSLT(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };
            }
			case BinaryExpressionType::LessEq:
            {
                return { builder.CreateICmpSLE(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };
            }
			case BinaryExpressionType::Greater:
            {
                return { builder.CreateICmpSGT(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };
            }
			case BinaryExpressionType::GreaterEq:
			{
                return { builder.CreateICmpSGE(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };

			}
			case BinaryExpressionType::Eq:
			{
                return { builder.CreateICmpEQ(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };
			}
			case BinaryExpressionType::NotEq:
			{
                return { builder.CreateICmpNE(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };
			}
			default:
				break;
		}

		return {};
    }

    CodegenResult ASTBinaryExpression::HandleCmpExpressionUI(CodegenResult &lhs, CodegenResult &rhs)
    {
        auto& builder = *LLVM::Backend::GetBuilder();

		switch (m_Expression)
		{
			case BinaryExpressionType::Less:
            {
                return { builder.CreateICmpULT(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };
            }
			case BinaryExpressionType::LessEq:
            {
                return { builder.CreateICmpULE(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };
            }
			case BinaryExpressionType::Greater:
            {
                return { builder.CreateICmpUGT(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };
            }
			case BinaryExpressionType::GreaterEq:
			{
                return { builder.CreateICmpUGE(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };

			}
			case BinaryExpressionType::Eq:
			{
                return { builder.CreateICmpEQ(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };
			}
			case BinaryExpressionType::NotEq:
			{
                return { builder.CreateICmpNE(lhs.CodegenValue, rhs.CodegenValue), TypeRegistry::GetGlobal()->GetType("bool") };
			}
			default:
				break;
		}

		return {};
    }

    CodegenResult ASTBinaryExpression::HandleBitwiseExpression(CodegenResult &lhs, CodegenResult &rhs)
    {
		//TODO
        return {};
    }

    CodegenResult ASTBinaryExpression::HandlePointerArithmetic(CodegenResult &lhs, CodegenResult &rhs)
    {
        return {};
    }


	ASTVariableDeclaration::ASTVariableDeclaration(const std::string& name, std::shared_ptr<Type> type)
		: m_Name(name), m_Type(type)
    {
    }

	CodegenResult ASTVariableDeclaration::Codegen()
    {
		CodegenResult codegenResult;

		std::shared_ptr<SymbolTable> registry = GetSymbolTable();
		
		Allocation alloca = registry->CreateAlloca(m_Name, m_Type);
		codegenResult.CodegenValue = alloca.Alloca;
		codegenResult.CodegenType  = TypeRegistry::GetGlobal()->GetPointerTo(alloca.Type);

		return codegenResult;
    }

	ASTVariableReference::ASTVariableReference(const std::string& name)
		: m_Name(name)
    {
    }

	CodegenResult ASTVariableReference::Codegen()
    {
		CodegenResult result;

		std::shared_ptr<SymbolTable> registry = GetSymbolTable();

		Allocation alloca = registry->GetAlloca(m_Name);
		result.CodegenValue = alloca.Alloca;
		result.CodegenType  = TypeRegistry::GetGlobal()->GetPointerTo(alloca.Type);

		return result;
    }

	ASTVariableExpression::ASTVariableExpression(const std::string& name)
		: m_Name(name)
    {
    }

	CodegenResult ASTVariableExpression::Codegen()
    {
		auto& builder = *LLVM::Backend::GetBuilder();

		CodegenResult result;

		std::shared_ptr<SymbolTable> registry = GetSymbolTable();
		Allocation alloca = registry->GetAlloca(m_Name);

		result.CodegenValue = builder.CreateLoad(alloca.Type->Get(), alloca.Alloca, m_Name);
		result.CodegenType  = alloca.Type;

		return result;
    }

	ASTAssignmentOperator::ASTAssignmentOperator(AssignmentOperatorType type)
		: m_Type(type)
    {
    }

	CodegenResult ASTAssignmentOperator::Codegen()
    {
		auto& builder = *LLVM::Backend::GetBuilder();
		auto& context = *LLVM::Backend::GetContext();
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() == 2, "incorrect dimensions");
		
		CodegenResult storage = children[0]->Codegen();
		CodegenResult data    = children[1]->Codegen();

		HandleDifferentTypes(storage, data);

		CodegenResult result;

		if(m_Type == AssignmentOperatorType::Normal)
		{
			result.CodegenValue = builder.CreateStore(data.CodegenValue, storage.CodegenValue);
			result.CodegenType = storage.CodegenType;
			return result;
		}

		CodegenResult loadedValue;
		loadedValue.CodegenValue = builder.CreateLoad(storage.CodegenType->Get(), storage.CodegenValue);
		loadedValue.CodegenType = storage.CodegenType;

		CodegenResult tmp;

		if(m_Type == AssignmentOperatorType::Add)
		{
			tmp = ASTBinaryExpression::HandleMathExpression(loadedValue, data, BinaryExpressionType::Add);
		}
		else if (m_Type == AssignmentOperatorType::Sub)
		{
			tmp = ASTBinaryExpression::HandleMathExpression(loadedValue, data, BinaryExpressionType::Sub);
		}
		else if (m_Type == AssignmentOperatorType::Mul)
		{
			tmp = ASTBinaryExpression::HandleMathExpression(loadedValue, data, BinaryExpressionType::Mul);
		}
		else if (m_Type == AssignmentOperatorType::Div)
		{
			tmp = ASTBinaryExpression::HandleMathExpression(loadedValue, data, BinaryExpressionType::Div);
		}
		else if (m_Type == AssignmentOperatorType::Mod)
		{
			tmp = ASTBinaryExpression::HandleMathExpression(loadedValue, data, BinaryExpressionType::Mod);
		}
		else 
		{
			CLEAR_UNREACHABLE("invalid assignment type");
		}

		result.CodegenValue = builder.CreateStore(tmp.CodegenValue, storage.CodegenValue);
		result.CodegenType  = storage.CodegenType;

		return result;
    }

    void ASTAssignmentOperator::HandleDifferentTypes(CodegenResult& storage, CodegenResult& data)
    {
		auto& builder = *LLVM::Backend::GetBuilder();
		
		std::shared_ptr<PointerType> ptrType = std::dynamic_pointer_cast<PointerType>(storage.CodegenType);
		CLEAR_VERIFY(ptrType, "storage must have pointer type");

		std::shared_ptr<Type> underlyingStorageType = ptrType->GetBaseType();
		CLEAR_VERIFY(underlyingStorageType, "not valid storage");

        llvm::Type* storageType = underlyingStorageType->Get();
        llvm::Type* dataType = data.CodegenType->Get();

		if(storageType == dataType)
			return;

		data.CodegenValue = TypeCasting::Cast(data.CodegenValue, data.CodegenType, underlyingStorageType);
		data.CodegenType = underlyingStorageType; 
    }


	ASTFunctionDefinition::ASTFunctionDefinition(const std::string& name, const std::shared_ptr<Type>& returnType, const std::vector<Parameter>& Paramaters)
		: m_Parameters(Paramaters), m_ReturnType(returnType), m_Name(name)
	{
		CreateSymbolTable();
	}

	CodegenResult ASTFunctionDefinition::Codegen()
	{
		auto& module  = *LLVM::Backend::GetModule();
		auto& context = *LLVM::Backend::GetContext();
		auto& builder = *LLVM::Backend::GetBuilder();
		
		std::shared_ptr<SymbolTable> prev = GetSymbolTable()->GetPrevious();
		CLEAR_VERIFY(prev, "prev was null");

		FunctionData& functionData = prev->CreateFunction(m_Name, m_Parameters, m_ReturnType);
		
		s_InsertPoints.push(builder.saveIP());

		llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", functionData.Function);
		llvm::BasicBlock* body  = llvm::BasicBlock::Create(context, "body");
		
		builder.SetInsertPoint(entry);

		llvm::BasicBlock* returnBlock  = llvm::BasicBlock::Create(context, "return");
		llvm::AllocaInst* returnAlloca = m_ReturnType ? builder.CreateAlloca(m_ReturnType->Get(), nullptr, "return_value") : nullptr;
		
		uint32_t k = 0;

		for (const auto& param : m_Parameters)
		{
			llvm::AllocaInst* argAlloc = builder.CreateAlloca(param.Type->Get(), nullptr, param.Name);
			builder.CreateStore(functionData.Function->getArg(k++), argAlloc);
			
			Allocation alloca;
			alloca.Alloca = argAlloc;
			alloca.Type   = param.Type;

			GetSymbolTable()->RegisterAllocation(param.Name, alloca);
		}

		functionData.Function->insert(functionData.Function->end(), body);
		builder.SetInsertPoint(body);

		//CodegenResult returnValue; TODO

		for (const auto& child : GetChildren())
		{
			child->Codegen();

			// ADD THIS ONCE WE HAVE RETURN STATEMENTS if(std::dynamic_pointer_cast<ASTReturn>(child)) 
		}

		auto currip = builder.saveIP();

		builder.SetInsertPoint(entry);
		builder.CreateBr(body);

		builder.restoreIP(currip);

		if(!builder.GetInsertBlock()->getTerminator())
			builder.CreateBr(returnBlock);

		functionData.Function->insert(functionData.Function->end(), returnBlock);
		builder.SetInsertPoint(returnBlock);

		if (functionData.Function->getReturnType()->isVoidTy())
		{
			builder.CreateRetVoid();
		}
		else
		{   //for now will do nothing but will do something once we have return statements.
			llvm::Value* load = builder.CreateLoad(returnAlloca->getAllocatedType(), returnAlloca, "loaded_value");
			builder.CreateRet(load);
		}

		auto& ip = s_InsertPoints.top();
		builder.restoreIP(ip);
		s_InsertPoints.pop();

		return {functionData.Function, functionData.ReturnType};
	}

	ASTFunctionCall::ASTFunctionCall(const std::string &name)
		: m_Name(name)
    {
    }

	CodegenResult ASTFunctionCall::Codegen()
	{
		auto& builder  = *LLVM::Backend::GetBuilder();
		auto& module   = *LLVM::Backend::GetModule();
		auto& context  = *LLVM::Backend::GetContext();
		auto& children = GetChildren();

		std::shared_ptr<SymbolTable> symbolTable = GetSymbolTable();
		FunctionData& data = symbolTable->GetFunction(m_Name);

		CLEAR_VERIFY(data.Function, m_Name, " definition doesn't exist");

		uint32_t k = 0;

		std::vector<llvm::Value*> args;

		for (auto& child : children)
		{
			CodegenResult gen = child->Codegen();

			if(data.Parameters[k].IsVariadic)
			{
				args.push_back(gen.CodegenValue);
				continue;
			}

			if (gen.CodegenType->Get() != data.Parameters[k].Type->Get())
			{
				gen.CodegenValue = TypeCasting::Cast(gen.CodegenValue, 
												     gen.CodegenType, 
													 data.Parameters[k].Type);
			}

			args.push_back(gen.CodegenValue);

			k++;
		}

		return { builder.CreateCall(data.Function, args), data.ReturnType };
	}

    ASTFunctionDecleration::ASTFunctionDecleration(const std::string& name, const std::shared_ptr<Type>& expectedReturnType, const std::vector<Parameter>& types)
		: m_Name(name), m_Parameters(types), m_ReturnType(expectedReturnType)
    {
    }

	CodegenResult ASTFunctionDecleration::Codegen()
	{
		auto& module = *LLVM::Backend::GetModule();

		std::vector<llvm::Type*> types;

		bool isVariadic = false;

		for (auto& param : m_Parameters)
		{
			if (param.IsVariadic)
			{
				isVariadic = true;
				break;
			}

			types.push_back(param.Type->Get());
		}

		if (!m_ReturnType)
			m_ReturnType = TypeRegistry::GetGlobal()->GetType("null_type");

		llvm::FunctionType* functionType = llvm::FunctionType::get(m_ReturnType->Get(), types, isVariadic);
		llvm::FunctionCallee callee = module.getOrInsertFunction(m_Name, functionType);

		FunctionData data;
		data.FunctionType = functionType;
		data.Function = llvm::cast<llvm::Function>(callee.getCallee());
		data.Parameters = m_Parameters;
		data.ReturnType = m_ReturnType;

		GetSymbolTable()->RegisterFunction(m_Name, data);
			
		return { data.Function, m_ReturnType };	
	}

    CodegenResult ASTExpression::Codegen()
	{
		auto& builder  = *LLVM::Backend::GetBuilder();
		auto& children = GetChildren();

		std::stack<std::shared_ptr<ASTNodeBase>> stack;
		
		for (const auto& child : children)
		{
			if (child->GetType() == ASTNodeType::Literal ||
				child->GetType() == ASTNodeType::VariableExpression ||
				child->GetType() == ASTNodeType::VariableReference || 
				child->GetType() == ASTNodeType::FunctionCall)
			{
				stack.push(child);
				continue;
			}

			/* if (child->GetType() == ASTNodeType::UnaryExpression)
			{
				Ref<ASTUnaryExpression> unaryExpression = DynamicCast<ASTUnaryExpression>(child);
				CLEAR_VERIFY(unaryExpression->GetChildren().size() == 0, "");

				unaryExpression->PushChild(stack.top());
				stack.pop();

				stack.push(unaryExpression);

				continue;
			} */

			std::shared_ptr<ASTBinaryExpression> binExp = std::dynamic_pointer_cast<ASTBinaryExpression>(child);

			binExp->Push(stack.top());
			stack.pop();

			binExp->Push(stack.top());
			stack.pop();

			stack.push(binExp);
		}

		if(stack.size() > 0)
		{
			return stack.top()->Codegen();
		}


		return {};
	}


	CodegenResult ASTArrayInitializer::Codegen()
	{
		auto& builder = *LLVM::Backend::GetBuilder();
		auto& context = *LLVM::Backend::GetContext();
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() > 0, "invalid array initializer");

		CodegenResult storage = children[0]->Codegen();

		std::shared_ptr<PointerType> storageType = std::dynamic_pointer_cast<PointerType>(storage.CodegenType);
		CLEAR_VERIFY(storageType, "invalid storage type");

		CLEAR_VERIFY(children.size() - 1 == m_Indices.size(), "sizes don't match!");

		llvm::Type* intTy = llvm::Type::getInt64Ty(context);

		std::shared_ptr<ArrayType> baseType = std::dynamic_pointer_cast<ArrayType>(storageType->GetBaseType());
		CLEAR_VERIFY(baseType, "base type is not an array type");

		llvm::Constant* zeroArray = llvm::ConstantAggregateZero::get(baseType->Get());
		builder.CreateStore(zeroArray, storage.CodegenValue);

		for(size_t i = 0; i < m_Indices.size(); i++)
		{
			std::vector<llvm::Value*> indices(m_Indices[i].size());
			
			std::transform(m_Indices[i].begin(), m_Indices[i].end(), indices.begin(), 
		 		[&](size_t index)
				{
					return llvm::ConstantInt::get(intTy, index);
				}
			);

			VerifyArray(baseType, m_Indices[i]);
			
			llvm::Value* elemPtr = builder.CreateInBoundsGEP(baseType->Get(), 
													 		 storage.CodegenValue, indices, 
													 		 "get_element_ptr");
			
			std::shared_ptr<Type> innerType = GetInnerType(baseType, m_Indices[i].size() - 1);
			
			CodegenResult valueToStore = children[i + 1]->Codegen();

			if(valueToStore.CodegenType != innerType)
			{
				valueToStore.CodegenValue = TypeCasting::Cast(
							valueToStore.CodegenValue,
							valueToStore.CodegenType, 
							innerType
							);
			}

			builder.CreateStore(valueToStore.CodegenValue, elemPtr);
		}
		
		return {};
	}

    void ASTArrayInitializer::SetIndices(const std::vector<std::vector<size_t>>& indices)
    {
		m_Indices = indices;
    }

    void ASTArrayInitializer::VerifyArray(std::shared_ptr<ArrayType> type, const std::vector<size_t>& index)
    {
		//first index always guaranteed to be 0.

		for(size_t i = 1; i < index.size(); i++)
		{
			CLEAR_VERIFY(type, "invalid type"); //TODO: more formal error handling needed
			CLEAR_VERIFY(index[i] < type->GetArraySize(), "index out of bounds error");

			type = std::dynamic_pointer_cast<ArrayType>(type->GetBaseType());
		}
    }
    std::shared_ptr<Type> ASTArrayInitializer::GetElementType(std::shared_ptr<Type> type)
    {
		while(auto base = std::dynamic_pointer_cast<ArrayType>(type))
		{
			type = base->GetBaseType();
		}

        return type;
    }
    std::shared_ptr<Type> ASTArrayInitializer::GetInnerType(std::shared_ptr<Type> type, size_t index)
    {
		while(auto base = std::dynamic_pointer_cast<ArrayType>(type))
		{
			if(index == 0) break;

			index--;
			type = base->GetBaseType();
		}

		CLEAR_VERIFY(!index, "out of bounds");
        return type;
    }
}