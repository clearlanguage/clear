#include "ASTNodeN.h"

#include "API/LLVM/LLVMBackend.h"
#include "Core/Log.h"

namespace clear 
{
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
		for(auto& child : m_Children) child->PropagateSymbolTable(m_SymbolTable);
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

        if(!lhs.CodegenValue->getType()->isPointerTy()) HandleTypePromotion(lhs, rhs);

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
                return { builder.CreateCall(powFunction, {lhs.CodegenValue, rhs.CodegenValue}), std::make_shared<Type>(TypeID::Float64) };
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
                return { builder.CreateCall(powFunction, {lhs.CodegenValue, rhs.CodegenValue}), std::make_shared<Type>(TypeID::Float64) };
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
                return { builder.CreateCall(powFunction, {lhs.CodegenValue, rhs.CodegenValue}), std::make_shared<Type>(TypeID::Float64) };
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
                return { builder.CreateFCmpOLT(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };
            }
			case BinaryExpressionType::LessEq:
            {
                return { builder.CreateFCmpOLE(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };
            }
			case BinaryExpressionType::Greater:
            {
                return { builder.CreateFCmpOGT(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };
            }
			case BinaryExpressionType::GreaterEq:
			{
                return { builder.CreateFCmpOGE(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };

			}
			case BinaryExpressionType::Eq:
			{
                return { builder.CreateFCmpOEQ(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };
			}
			case BinaryExpressionType::NotEq:
			{
                return { builder.CreateFCmpONE(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };
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
                return { builder.CreateICmpSLT(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };
            }
			case BinaryExpressionType::LessEq:
            {
                return { builder.CreateICmpSLE(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };
            }
			case BinaryExpressionType::Greater:
            {
                return { builder.CreateICmpSGT(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };
            }
			case BinaryExpressionType::GreaterEq:
			{
                return { builder.CreateICmpSGE(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };

			}
			case BinaryExpressionType::Eq:
			{
                return { builder.CreateICmpEQ(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };
			}
			case BinaryExpressionType::NotEq:
			{
                return { builder.CreateICmpNE(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };
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
                return { builder.CreateICmpULT(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };
            }
			case BinaryExpressionType::LessEq:
            {
                return { builder.CreateICmpULE(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };
            }
			case BinaryExpressionType::Greater:
            {
                return { builder.CreateICmpUGT(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };
            }
			case BinaryExpressionType::GreaterEq:
			{
                return { builder.CreateICmpUGE(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };

			}
			case BinaryExpressionType::Eq:
			{
                return { builder.CreateICmpEQ(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };
			}
			case BinaryExpressionType::NotEq:
			{
                return { builder.CreateICmpNE(lhs.CodegenValue, rhs.CodegenValue), std::make_shared<Type>(TypeID::Bool) };
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
		codegenResult.CodegenType  = alloca.Type;

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
		result.CodegenType  = alloca.Type;

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

        llvm::Type* storageType = storage.CodegenType->Get();
        llvm::Type* dataType = data.CodegenType->Get();

        if (storageType == dataType)
	        return;

	    // float -> int
		if (storageType->isIntegerTy() && dataType->isFloatingPointTy()) 
		{
		    if (storage.CodegenType->IsSigned())
		        data.CodegenValue = builder.CreateFPToSI(data.CodegenValue, storageType, "cast");
		    else
		        data.CodegenValue = builder.CreateFPToUI(data.CodegenValue, storageType, "cast");
		
		    data.CodegenType = storage.CodegenType;
		} 
		// int -> float
		else if (storageType->isFloatingPointTy() && dataType->isIntegerTy()) 
		{
		    if (data.CodegenType->IsSigned())
		        data.CodegenValue = builder.CreateSIToFP(data.CodegenValue, storageType, "cast");
		    else
		        data.CodegenValue = builder.CreateUIToFP(data.CodegenValue, storageType, "cast");
		
		    data.CodegenType = storage.CodegenType;
		} 
		// float -> double
		else if (storageType->isDoubleTy() && dataType->isFloatTy()) 
		{
		    data.CodegenValue = builder.CreateFPExt(data.CodegenValue, storageType, "cast");
		    data.CodegenType = storage.CodegenType;
		} 
		// double -> float
		else if (storageType->isFloatTy() && dataType->isDoubleTy()) 
		{
		    data.CodegenValue = builder.CreateFPTrunc(data.CodegenValue, storageType, "cast");
		    data.CodegenType = storage.CodegenType;
		} 
		// smaller int -> bigger int
		else if (storageType->isIntegerTy() && dataType->isIntegerTy() && 
		         storageType->getIntegerBitWidth() > dataType->getIntegerBitWidth()) 
		{
		    if (data.CodegenType->IsSigned())
		        data.CodegenValue = builder.CreateSExt(data.CodegenValue, storageType, "cast");
		    else
		        data.CodegenValue = builder.CreateZExt(data.CodegenValue, storageType, "cast");
		
		    data.CodegenType = storage.CodegenType;
		} 
		// bigger int -> smaller int
		else if (storageType->isIntegerTy() && dataType->isIntegerTy() && 
		         storageType->getIntegerBitWidth() < dataType->getIntegerBitWidth()) 
		{
		    data.CodegenValue = builder.CreateTrunc(data.CodegenValue, storageType, "cast");
		    data.CodegenType = storage.CodegenType;
		}
    	else 
    	{
    	    CLEAR_UNREACHABLE("unsupported type conversion");
    	}
    }
}	