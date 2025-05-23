#include "ASTNodeN.h"

#include "API/LLVM/LLVMBackend.h"
#include "Core/Log.h"

namespace clear 
{   
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

    void ASTNodeBase::SetParent(const std::shared_ptr<ASTNodeBase>& parent)
	{
		m_Parent = parent;
	}

	void ASTNodeBase::RemoveParent()
	{
		m_Parent = nullptr;
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
			return HandleMathExpression(lhs, rhs);

		if(IsCmpExpression()) 
			return HandleCmpExpression(lhs, rhs);

		return {}; //TODO
    }

    void ASTBinaryExpression::HandleTypePromotion(CodegenResult& lhs, CodegenResult& rhs)
    {
        auto& builder = *LLVM::Backend::GetBuilder();

        llvm::Type* lhsType = lhs.CodegenValue->getType();
        llvm::Type* rhsType = rhs.CodegenValue->getType();

        //same type ignore rest
        if (lhsType == rhsType)
            return;

        // int -> float
        if (lhsType->isIntegerTy() && rhsType->isFloatingPointTy()) 
        {
            lhs.CodegenValue = builder.CreateSIToFP(lhs.CodegenValue, rhsType, "cast");
            lhs.CodegenType = rhs.CodegenType;
        } 
        else if (lhsType->isFloatingPointTy() && rhsType->isIntegerTy()) 
        {
            rhs.CodegenValue = builder.CreateSIToFP(rhs.CodegenValue, lhsType, "cast");
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

    CodegenResult ASTBinaryExpression::HandleMathExpression(CodegenResult& lhs, CodegenResult& rhs)
    {
        if(lhs.CodegenValue->getType()->isFloatingPointTy()) 
			return HandleMathExpressionF(lhs, rhs);

		if(lhs.CodegenType->IsSigned() || rhs.CodegenType->IsSigned()) 
			return HandleMathExpressionSI(lhs, rhs);

		return HandleMathExpressionUI(lhs, rhs);
    }

    CodegenResult ASTBinaryExpression::HandleMathExpressionF(CodegenResult &lhs, CodegenResult &rhs)
    {
        auto& builder = *LLVM::Backend::GetBuilder();
		auto& module  = *LLVM::Backend::GetModule();

		switch (m_Expression)
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

    CodegenResult ASTBinaryExpression::HandleMathExpressionSI(CodegenResult& lhs, CodegenResult& rhs)
    {
        auto& builder = *LLVM::Backend::GetBuilder();
		auto& module  = *LLVM::Backend::GetModule();

		std::shared_ptr<Type> type = lhs.CodegenType->IsSigned() ? lhs.CodegenType : rhs.CodegenType;

		switch (m_Expression)
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

    CodegenResult ASTBinaryExpression::HandleMathExpressionUI(CodegenResult& lhs, CodegenResult& rhs)
    {
        auto& builder = *LLVM::Backend::GetBuilder();
		auto& module  = *LLVM::Backend::GetModule();

		switch (m_Expression)
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
}