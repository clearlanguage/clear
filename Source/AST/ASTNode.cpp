#include "ASTNode.h"

#include "API/LLVM/LLVMInclude.h"
#include "Core/Log.h"
#include "TypeCasting.h"

#include "Core/TypeRegistry.h"
#include "Intrinsics.h"

#include <stack>

namespace clear
{
	template <typename T>
	class ValueRestoreGuard 
	{
	public:
	    ValueRestoreGuard(T& variable, T newValue)
	        : m_Reference(variable), m_OldValue(variable)
	    {
	        m_Reference = newValue;
	    }

	    ~ValueRestoreGuard()
	    {
	        m_Reference = m_OldValue;
	    }

	private:
	    T& m_Reference;
	    T m_OldValue;
	};

	static std::stack<llvm::IRBuilderBase::InsertPoint>  s_InsertPoints;

    ASTNodeBase::ASTNodeBase()
    {
    }

    CodegenResult ASTNodeBase::Codegen(CodegenContext &ctx)
    {
        CodegenResult value;

		for (auto& child : GetChildren())
		{
			bool isContinue = child->GetType() == ASTNodeType::LoopControlFlow;
			value = child->Codegen(ctx);
			if(isContinue) break;
		}

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
			child->PropagateSymbolTable(m_SymbolTable);
		}
    }

    ASTNodeLiteral::ASTNodeLiteral(const Token& data)
		: m_Token(data)
	{
	}

	CodegenResult ASTNodeLiteral::Codegen(CodegenContext& ctx)
	{
		Value value(m_Token, ctx.Registry, ctx.Context, ctx.Module);
		return {value.Get(), value.GetType()};
	}

    ASTBinaryExpression::ASTBinaryExpression(BinaryExpressionType type)
		: m_Expression(type)
	{
	}

	CodegenResult ASTBinaryExpression::Codegen(CodegenContext& ctx)
	{
		auto& builder = ctx.Builder;
		auto& context = ctx.Context;
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() == 2, "incorrect dimensions");

		auto& leftChild  = children[1];
		auto& rightChild = children[0];

		if(IsMathExpression())
			return HandleMathExpression(leftChild, rightChild, ctx);

		if(IsCmpExpression())
			return HandleCmpExpression(leftChild, rightChild, ctx);

		if(m_Expression == BinaryExpressionType::Index)
			return HandleArrayIndex(leftChild, rightChild, ctx);

    	if (IsBitwiseExpression())
			return HandleBitwiseExpression(leftChild, rightChild, ctx);

		if (IsLogicalOperator())
			return HandleLogicalExpression(leftChild, rightChild, ctx);

		if(m_Expression == BinaryExpressionType::MemberAccess)
			return HandleMemberAccess(leftChild, rightChild, ctx);


		CLEAR_UNREACHABLE("unimplmented");

		return {};
    }

    void ASTBinaryExpression::HandleTypePromotion(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx)
    {
        auto& builder = ctx.Builder;

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

    bool ASTBinaryExpression::IsLogicalOperator() const
    {
		switch(m_Expression)
		{
			case BinaryExpressionType::And:
			case BinaryExpressionType::Or:
				return true;
			default:
				break;
		}

        return false;
    }

    CodegenResult ASTBinaryExpression::HandleMathExpression(CodegenResult& lhs, CodegenResult& rhs,  BinaryExpressionType type, CodegenContext& ctx)
    {
		if(lhs.CodegenType != rhs.CodegenType)
			HandleTypePromotion(lhs, rhs, ctx);

        if(lhs.CodegenValue->getType()->isFloatingPointTy()) 
			return HandleMathExpressionF(lhs, rhs, type, ctx);

		if(lhs.CodegenType->IsSigned() || rhs.CodegenType->IsSigned()) 
			return HandleMathExpressionSI(lhs, rhs, type, ctx);

		return HandleMathExpressionUI(lhs, rhs, type, ctx);
    }

    CodegenResult ASTBinaryExpression::HandleMathExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext &ctx)
    {
		// ctx.WantAddress is set by parent to this node
		CodegenResult lhs = left->Codegen(ctx);

		// right hand side we always want a value

		CodegenResult rhs;
		{
			ValueRestoreGuard guard(ctx.WantAddress, false);
			rhs = right->Codegen(ctx);
		}

		if(lhs.CodegenType->IsPointer()) 
			return HandlePointerArithmetic(lhs, rhs, m_Expression, ctx); //internally will verify correct expression type

        return HandleMathExpression(lhs, rhs, m_Expression, ctx);
    }

    CodegenResult ASTBinaryExpression::HandleMathExpressionF(CodegenResult &lhs, CodegenResult &rhs, BinaryExpressionType binExpressionType, CodegenContext& ctx)
    {
        auto& builder = ctx.Builder;
		auto& module  = ctx.Module;

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
				llvm::Value* cast1 = TypeCasting::Cast(lhs.CodegenValue, lhs.CodegenType, ctx.Registry.GetType("float64"), ctx.Builder);
				llvm::Value* cast2 = TypeCasting::Cast(rhs.CodegenValue, rhs.CodegenType, ctx.Registry.GetType("float64"), ctx.Builder);


				llvm::Function* powFunction = llvm::Intrinsic::getDeclaration(&module, llvm::Intrinsic::pow, { builder.getDoubleTy() });
                return { builder.CreateCall(powFunction, {cast1, cast2}), 
					     ctx.Registry.GetType("float64")  };
			}
			default:
				break;
		}

		return {};
    }

    CodegenResult ASTBinaryExpression::HandleMathExpressionSI(CodegenResult& lhs, CodegenResult& rhs, BinaryExpressionType binExpressionType, CodegenContext& ctx)
    {
        auto& builder = ctx.Builder;
		auto& module  = ctx.Module;

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
				llvm::Value* cast1 = TypeCasting::Cast(lhs.CodegenValue, lhs.CodegenType, ctx.Registry.GetType("float64"), ctx.Builder);
				llvm::Value* cast2 = TypeCasting::Cast(rhs.CodegenValue, rhs.CodegenType, ctx.Registry.GetType("float64"), ctx.Builder);

				llvm::Function* powFunction = llvm::Intrinsic::getDeclaration(&module, llvm::Intrinsic::pow, { builder.getDoubleTy() });
                return { builder.CreateCall(powFunction, {cast1, cast2}), 
					     ctx.Registry.GetType("float64")  };
			}
			default:
				break;
		}

		return {};
    }

    CodegenResult ASTBinaryExpression::HandleMathExpressionUI(CodegenResult& lhs, CodegenResult& rhs, BinaryExpressionType type, CodegenContext& ctx)
    {
        auto& builder = ctx.Builder;
		auto& module  = ctx.Module;

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
				llvm::Value* cast1 = TypeCasting::Cast(lhs.CodegenValue, lhs.CodegenType, ctx.Registry.GetType("float64"), ctx.Builder);
				llvm::Value* cast2 = TypeCasting::Cast(rhs.CodegenValue, rhs.CodegenType, ctx.Registry.GetType("float64"), ctx.Builder);

				llvm::Function* powFunction = llvm::Intrinsic::getDeclaration(&module, llvm::Intrinsic::pow, { builder.getDoubleTy() });
                return { builder.CreateCall(powFunction, {cast1, cast2}), 
					     ctx.Registry.GetType("float64")  };
			}
			default:
				break;
		}

		return {};
    }

    CodegenResult ASTBinaryExpression::HandleCmpExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext &ctx)
    {
		ValueRestoreGuard guard(ctx.WantAddress, false);

		CodegenResult lhs = left->Codegen(ctx);
		CodegenResult rhs = right->Codegen(ctx);

		if(lhs.CodegenType != rhs.CodegenType)
		{
			HandleTypePromotion(lhs, rhs, ctx);
		}

        return HandleCmpExpression(lhs, rhs, ctx);
    }

    CodegenResult ASTBinaryExpression::HandleCmpExpression(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx)
    {
        if(lhs.CodegenValue->getType()->isFloatingPointTy()) 
			return HandleCmpExpressionF(lhs, rhs, ctx);

		if(lhs.CodegenType->IsSigned() || rhs.CodegenType->IsSigned()) 
			return HandleCmpExpressionSI(lhs, rhs, ctx);

		return HandleCmpExpressionUI(lhs, rhs, ctx);
    }

    CodegenResult ASTBinaryExpression::HandleCmpExpressionF(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx)
    {
        auto& builder = ctx.Builder;

		switch (m_Expression)
		{
			case BinaryExpressionType::Less:
            {
                return { builder.CreateFCmpOLT(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };
            }
			case BinaryExpressionType::LessEq:
            {
                return { builder.CreateFCmpOLE(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };
            }
			case BinaryExpressionType::Greater:
            {
                return { builder.CreateFCmpOGT(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };
            }
			case BinaryExpressionType::GreaterEq:
			{
                return { builder.CreateFCmpOGE(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };

			}
			case BinaryExpressionType::Eq:
			{
                return { builder.CreateFCmpOEQ(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };
			}
			case BinaryExpressionType::NotEq:
			{
                return { builder.CreateFCmpONE(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };
			}
			default:
				break;
		}

		return {};
    }

    CodegenResult ASTBinaryExpression::HandleCmpExpressionSI(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx)
    {
        auto& builder = ctx.Builder;

		switch (m_Expression)
		{
			case BinaryExpressionType::Less:
            {
                return { builder.CreateICmpSLT(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };
            }
			case BinaryExpressionType::LessEq:
            {
                return { builder.CreateICmpSLE(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };
            }
			case BinaryExpressionType::Greater:
            {
                return { builder.CreateICmpSGT(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };
            }
			case BinaryExpressionType::GreaterEq:
			{
                return { builder.CreateICmpSGE(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };

			}
			case BinaryExpressionType::Eq:
			{
                return { builder.CreateICmpEQ(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };
			}
			case BinaryExpressionType::NotEq:
			{
                return { builder.CreateICmpNE(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };
			}
			default:
				break;
		}

		return {};
    }

    CodegenResult ASTBinaryExpression::HandleCmpExpressionUI(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx)
    {
        auto& builder = ctx.Builder;

		switch (m_Expression)
		{
			case BinaryExpressionType::Less:
            {
                return { builder.CreateICmpULT(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };
            }
			case BinaryExpressionType::LessEq:
            {
                return { builder.CreateICmpULE(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };
            }
			case BinaryExpressionType::Greater:
            {
                return { builder.CreateICmpUGT(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };
            }
			case BinaryExpressionType::GreaterEq:
			{
                return { builder.CreateICmpUGE(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };

			}
			case BinaryExpressionType::Eq:
			{
                return { builder.CreateICmpEQ(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };
			}
			case BinaryExpressionType::NotEq:
			{
                return { builder.CreateICmpNE(lhs.CodegenValue, rhs.CodegenValue), ctx.Registry.GetType("bool") };
			}
			default:
				break;
		}

		return {};
    }

    CodegenResult ASTBinaryExpression::HandleBitwiseExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx)
    {

    	auto& builder = ctx.Builder;
    	// ctx.WantAddress is set by parent to this node
    	CodegenResult lhs = left->Codegen(ctx);

    	// right hand side we always want a value

    	CodegenResult rhs;
	    {
    		ValueRestoreGuard guard(ctx.WantAddress, false);
    		rhs = right->Codegen(ctx);
	    }
    	CLEAR_VERIFY(rhs.CodegenType->IsIntegral(),"RHS must be an int")
    	CLEAR_VERIFY(lhs.CodegenType->IsIntegral(),"LHS must be an int")
    	switch (m_Expression) 
		{
    		case BinaryExpressionType::BitwiseAnd:
    			return { builder.CreateAnd(lhs.CodegenValue, rhs.CodegenValue, "andtmp"), lhs.CodegenType };
    		case BinaryExpressionType::BitwiseOr:
    			return { builder.CreateOr(lhs.CodegenValue, rhs.CodegenValue, "ortmp"), lhs.CodegenType };
    		case BinaryExpressionType::BitwiseXor:
    			return { builder.CreateXor(lhs.CodegenValue, rhs.CodegenValue, "xortmp"), lhs.CodegenType };
    		case BinaryExpressionType::BitwiseLeftShift:
    			return { builder.CreateShl(lhs.CodegenValue, rhs.CodegenValue, "shltmp"), lhs.CodegenType };
    		case BinaryExpressionType::BitwiseRightShift:
    			return { builder.CreateLShr(lhs.CodegenValue, rhs.CodegenValue, "lshrtmp"), lhs.CodegenType };
    		case BinaryExpressionType::BitwiseNot:
    			return { builder.CreateNot(lhs.CodegenValue, "nottmp"), lhs.CodegenType };
    		default:
    			// Error handling or fallback
    				return {};
    	}

        return {};
    }

    CodegenResult ASTBinaryExpression::HandleLogicalExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext &ctx)
    {
		if(ctx.Builder.GetInsertBlock()->getTerminator()) return {};
			
		llvm::Function* function = ctx.Builder.GetInsertBlock()->getParent();

		ValueRestoreGuard guard(ctx.WantAddress, false);

		CodegenResult lhs = left->Codegen(ctx);

		lhs.CodegenValue = TypeCasting::Cast(lhs.CodegenValue, lhs.CodegenType, ctx.Registry.GetType("bool"), ctx.Builder);
		lhs.CodegenType  = ctx.Registry.GetType("bool");

		CodegenResult result;

		llvm::BasicBlock* checkSecond  = llvm::BasicBlock::Create(ctx.Context, "check_second");
		llvm::BasicBlock* trueResult   = llvm::BasicBlock::Create(ctx.Context, "true_value");
		llvm::BasicBlock* falseResult  = llvm::BasicBlock::Create(ctx.Context, "false_value");
		llvm::BasicBlock* merge  	   = llvm::BasicBlock::Create(ctx.Context, "merge");

		if(m_Expression == BinaryExpressionType::And)
			ctx.Builder.CreateCondBr(lhs.CodegenValue, checkSecond, falseResult);
		else 
			ctx.Builder.CreateCondBr(lhs.CodegenValue, trueResult, checkSecond);

		function->insert(function->end(), checkSecond);
		ctx.Builder.SetInsertPoint(checkSecond);
		
		CodegenResult rhs = right->Codegen(ctx);
		
		rhs.CodegenValue = TypeCasting::Cast(rhs.CodegenValue, rhs.CodegenType, ctx.Registry.GetType("bool"), ctx.Builder);
		rhs.CodegenType  = ctx.Registry.GetType("bool");
		
		ctx.Builder.CreateCondBr(rhs.CodegenValue, trueResult, falseResult);
		
		function->insert(function->end(), trueResult);
		ctx.Builder.SetInsertPoint(trueResult);

		ctx.Builder.CreateBr(merge);

		function->insert(function->end(), falseResult);
		ctx.Builder.SetInsertPoint(falseResult);

		ctx.Builder.CreateBr(merge);

		function->insert(function->end(), merge);
		ctx.Builder.SetInsertPoint(merge);

		auto phiNode = ctx.Builder.CreatePHI(rhs.CodegenType->Get(), 2);
		phiNode->addIncoming(ctx.Builder.getInt1(true), trueResult);
		phiNode->addIncoming(ctx.Builder.getInt1(false), falseResult);

		result.CodegenValue = phiNode;
		result.CodegenType = rhs.CodegenType;

        return result;
    }

    CodegenResult ASTBinaryExpression::HandlePointerArithmetic(CodegenResult& lhs, CodegenResult& rhs, BinaryExpressionType type, CodegenContext& ctx)
    {
		CLEAR_VERIFY(lhs.CodegenType->IsPointer(), "left hand side is not a pointer");
		CLEAR_VERIFY(rhs.CodegenType->IsIntegral(), "invalid pointer arithmetic");

		if(rhs.CodegenType->GetSize() != 64) 
		{
			rhs.CodegenValue = TypeCasting::Cast(rhs.CodegenValue, 
												 rhs.CodegenType, 
												 ctx.Registry.GetType("int64"), 
												 ctx.Builder);
		}
		
		std::shared_ptr<PointerType> ptrType = std::dynamic_pointer_cast<PointerType>(lhs.CodegenType);

		if(type == BinaryExpressionType::Add)
		{
			return { ctx.Builder.CreateGEP(ptrType->GetBaseType()->Get(), lhs.CodegenValue, rhs.CodegenValue), ptrType };
		}

		if(type == BinaryExpressionType::Sub)
		{
			rhs.CodegenValue = ctx.Builder.CreateNeg(rhs.CodegenValue);
			return { ctx.Builder.CreateGEP(ptrType->GetBaseType()->Get(), lhs.CodegenValue, rhs.CodegenValue), ptrType };
		}

		CLEAR_UNREACHABLE("invalid binary expression");

        return {};
    }

    CodegenResult ASTBinaryExpression::HandleArrayIndex(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx)
    {
		CodegenResult lhs;

		{
			ValueRestoreGuard guard(ctx.WantAddress, true);
			lhs = left->Codegen(ctx);
		}

		CodegenResult rhs;

		{
			ValueRestoreGuard guard(ctx.WantAddress, false);
			rhs = right->Codegen(ctx);
		}

		llvm::Value* gep = nullptr;
		std::shared_ptr<Type> baseType;

		//TODO: clean this up.

		if(lhs.CodegenType->IsVariadic())
		{
			if(rhs.CodegenType != ctx.Registry.GetType("int64")) 
			{
				rhs.CodegenValue = TypeCasting::Cast(rhs.CodegenValue, 
													 rhs.CodegenType, 
													 ctx.Registry.GetType("int64"), 
													 ctx.Builder);
			}

	
		
			if (auto* constIdx = llvm::dyn_cast<llvm::ConstantInt>(rhs.CodegenValue)) 
			{
    			uint64_t index = constIdx->getZExtValue();
				CLEAR_VERIFY(index < GetSymbolTable()->GetVariadicArguments().size(), "index out of range!");

				Allocation alloc = GetSymbolTable()->GetVariadicArguments()[index];
				
				gep = alloc.Alloca;
				baseType = alloc.Type;
			}
			else 
			{
				CLEAR_UNREACHABLE("only allow constant expression indexing, runtime indexing is not supported yet");
			}

			if(ctx.WantAddress)
				return {gep, ctx.Registry.GetPointerTo(baseType) };

        	return { ctx.Builder.CreateLoad(baseType->Get(), gep), baseType} ;
				
		}

		// lhs is going to be a reference to the array

		std::shared_ptr<PointerType> type = std::dynamic_pointer_cast<PointerType>(lhs.CodegenType);
		CLEAR_VERIFY(type, "invalid type");

		if(std::shared_ptr<ArrayType> arrType = std::dynamic_pointer_cast<ArrayType>(type->GetBaseType()))
		{
			llvm::Value* zero = llvm::ConstantInt::get(ctx.Builder.getInt64Ty(), 0);

			if(rhs.CodegenType != ctx.Registry.GetType("int64")) 
			{
				rhs.CodegenValue = TypeCasting::Cast(rhs.CodegenValue, 
													 rhs.CodegenType, 
													 ctx.Registry.GetType("int64"), 
													 ctx.Builder);
			}

			// if debugging enabled will do checks in array indexing here TODO (ignore for now)

			gep = ctx.Builder.CreateGEP(
        		arrType->Get(),
        		lhs.CodegenValue,
        		{ zero, rhs.CodegenValue },
        		"gep"
    		);

			baseType = arrType->GetBaseType();
		}
		else 
		{
			CLEAR_UNREACHABLE("invalid base type ", type->GetBaseType()->GetHash());
		}

		if(ctx.WantAddress)
			return {gep, ctx.Registry.GetPointerTo(baseType) };

        return { ctx.Builder.CreateLoad(baseType->Get(), gep), baseType} ;
    }

    CodegenResult ASTBinaryExpression::HandleMemberAccess(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext &ctx)
    {
		auto tbl = GetSymbolTable();

		CodegenResult lhs;

		{
			ValueRestoreGuard guard(ctx.WantAddress, true);
			lhs = left->Codegen(ctx);
		}

		if(!lhs.CodegenType->IsPointer()) 
		{
			Allocation temp = tbl->RequestTemporary(lhs.CodegenType, ctx.Builder);
			ctx.Builder.CreateStore(lhs.CodegenValue, temp.Alloca);

			lhs.CodegenValue = temp.Alloca;
			lhs.CodegenType = temp.Type;
		}

		CLEAR_VERIFY(right->GetType() == ASTNodeType::Member, "not a valid member");
		auto member = std::dynamic_pointer_cast<ASTMember>(right);

		llvm::Value* getElementPtr = lhs.CodegenValue; 
		std::shared_ptr<Type> curr = lhs.CodegenType;

		while(auto ty = std::dynamic_pointer_cast<PointerType>(curr)) // automatic derefencing if pointer
		{
			if(ty->GetBaseType()->IsCompound())
			{
				curr = ty;
				break;
			}

			getElementPtr = ctx.Builder.CreateLoad(ty->GetBaseType()->Get(), getElementPtr);
			curr = ty->GetBaseType();
		}

		std::shared_ptr<StructType> structTy;
			
		if(auto ty = std::dynamic_pointer_cast<PointerType>(curr))
			structTy = std::dynamic_pointer_cast<StructType>(ty->GetBaseType());
		else 
			structTy = std::dynamic_pointer_cast<StructType>(curr);
		
		CLEAR_VERIFY(structTy, "not a valid type ", curr->GetHash());

		size_t index = structTy->GetMemberIndex(member->GetName());
		getElementPtr = ctx.Builder.CreateStructGEP(structTy->Get(), getElementPtr, index, "gep");
		curr = structTy->GetMemberType(member->GetName());

        if(ctx.WantAddress)
		{
			return { getElementPtr, ctx.Registry.GetPointerTo(curr) };
		}

		return { ctx.Builder.CreateLoad(curr->Get(), getElementPtr), curr };
    }


    ASTVariableDeclaration::ASTVariableDeclaration(const std::string& name, const TypeDescriptor& type)
		: m_Name(name), m_Type(type)
    {
    }

	CodegenResult ASTVariableDeclaration::Codegen(CodegenContext& ctx)
    {
		CodegenResult codegenResult;

		std::shared_ptr<SymbolTable> registry = GetSymbolTable();
		
		std::shared_ptr<Type> resolvedType = ctx.Registry.ResolveType(m_Type);

        bool isGlobal = !(bool)ctx.Builder.GetInsertBlock();

		Allocation alloca;

		if(isGlobal)
		{
			alloca = registry->CreateGlobal(m_Name, resolvedType, ctx.Module);
		}
		else 
		{
			alloca = registry->CreateAlloca(m_Name, resolvedType, ctx.Builder);
		}

		codegenResult.CodegenValue = alloca.Alloca;
		codegenResult.CodegenType  = ctx.Registry.GetPointerTo(alloca.Type);

		if (alloca.Type->IsPointer()) 
		{
		    llvm::Constant* nullPtr = llvm::ConstantPointerNull::get(
		        llvm::cast<llvm::PointerType>(alloca.Type->Get())
		    );

		    ctx.Builder.CreateStore(nullPtr, codegenResult.CodegenValue);
		}
		else if (alloca.Type->IsCompound())
		{
		    llvm::Constant* zero = llvm::ConstantAggregateZero::get(alloca.Type->Get());
		    ctx.Builder.CreateStore(zero, codegenResult.CodegenValue);
		}
		else if (alloca.Type->IsArray())
		{
		    llvm::Constant* zero = llvm::ConstantAggregateZero::get(alloca.Type->Get());
		    ctx.Builder.CreateStore(zero, codegenResult.CodegenValue);
		}
		else if (alloca.Type->IsIntegral())
		{
		    llvm::Constant* zero = llvm::ConstantInt::get(alloca.Type->Get(), 0);
		    ctx.Builder.CreateStore(zero, codegenResult.CodegenValue);
		}
		else if (alloca.Type->IsFloatingPoint())
		{
		    llvm::Constant* zero = llvm::ConstantFP::get(alloca.Type->Get(), 0.0);
		    ctx.Builder.CreateStore(zero, codegenResult.CodegenValue);
		}

		return codegenResult;
    }

	ASTInferredDecleration::ASTInferredDecleration(const std::string& name, bool isConst)
		: m_Name(name), m_IsConst(isConst)
    {
    }

	CodegenResult ASTInferredDecleration::Codegen(CodegenContext& ctx)
	{
		auto& children = GetChildren();
		CLEAR_VERIFY(children.size() == 1, "invalid child size");

		ValueRestoreGuard guard(ctx.WantAddress, false);
		CodegenResult result = children[0]->Codegen(ctx);

		std::shared_ptr<SymbolTable> tbl = GetSymbolTable();
	
		if(m_IsConst)
			result.CodegenType = ctx.Registry.GetConstFrom(result.CodegenType);

		
		bool isGlobal = !(bool)ctx.Builder.GetInsertBlock();

		Allocation alloca;

		if(isGlobal)
		{
			alloca = tbl->CreateGlobal(m_Name, result.CodegenType, ctx.Module, result.CodegenValue);
		}
		else 
		{
			alloca = tbl->CreateAlloca(m_Name, result.CodegenType, ctx.Builder);
			ctx.Builder.CreateStore(result.CodegenValue, alloca.Alloca);
		}

		return {alloca.Alloca, ctx.Registry.GetPointerTo(alloca.Type)};
	}

	ASTVariable::ASTVariable(const std::string& name)
		: m_Name(name)
    {
    }

	CodegenResult ASTVariable::Codegen(CodegenContext& ctx)
    {
		auto& builder = ctx.Builder;

		CodegenResult result;

		std::shared_ptr<SymbolTable> registry = GetSymbolTable();
		Allocation alloca = registry->GetAlloca(m_Name);

		
		if(alloca.Type->IsVariadic())  // special case
		{
			return { nullptr, alloca.Type }; 
		}

		if(ctx.WantAddress)
		{
			result.CodegenValue = alloca.Alloca;
			result.CodegenType  = ctx.Registry.GetPointerTo(alloca.Type);
		}
		else 
		{
			result.CodegenValue = builder.CreateLoad(alloca.Type->Get(), alloca.Alloca, m_Name);
			result.CodegenType  = alloca.Type;
		}

		return result;
    }

	ASTAssignmentOperator::ASTAssignmentOperator(AssignmentOperatorType type)
		: m_Type(type)
    {
    }

	CodegenResult ASTAssignmentOperator::Codegen(CodegenContext& ctx)
    {
		auto& builder = ctx.Builder;
		auto& context = ctx.Context;
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() == 2, "incorrect dimensions");
	
		CodegenResult storage;
		
		{
			ValueRestoreGuard guard(ctx.WantAddress, true);
			storage = children[0]->Codegen(ctx);
		}

		CodegenResult data;

		{
			ValueRestoreGuard guard(ctx.WantAddress, false);
			data    = children[1]->Codegen(ctx);
		}

		HandleDifferentTypes(storage, data, ctx);

		CodegenResult result;

		if(m_Type == AssignmentOperatorType::Normal || m_Type == AssignmentOperatorType::Initialize)
		{
			result.CodegenValue = builder.CreateStore(data.CodegenValue, storage.CodegenValue);
			result.CodegenType = storage.CodegenType;
			return result;
		}

		std::shared_ptr<PointerType> storageType = std::dynamic_pointer_cast<PointerType>(storage.CodegenType);

		CodegenResult loadedValue;
		loadedValue.CodegenValue = builder.CreateLoad(storageType->GetBaseType()->Get(), storage.CodegenValue);
		loadedValue.CodegenType = storageType->GetBaseType();

		CodegenResult tmp;

		if(m_Type == AssignmentOperatorType::Add)
		{
			tmp = ASTBinaryExpression::HandleMathExpression(loadedValue, data, BinaryExpressionType::Add, ctx);
		}
		else if (m_Type == AssignmentOperatorType::Sub)
		{
			tmp = ASTBinaryExpression::HandleMathExpression(loadedValue, data, BinaryExpressionType::Sub, ctx);
		}
		else if (m_Type == AssignmentOperatorType::Mul)
		{
			tmp = ASTBinaryExpression::HandleMathExpression(loadedValue, data, BinaryExpressionType::Mul, ctx);
		}
		else if (m_Type == AssignmentOperatorType::Div)
		{
			tmp = ASTBinaryExpression::HandleMathExpression(loadedValue, data, BinaryExpressionType::Div, ctx);
		}
		else if (m_Type == AssignmentOperatorType::Mod)
		{
			tmp = ASTBinaryExpression::HandleMathExpression(loadedValue, data, BinaryExpressionType::Mod, ctx);
		}
		else 
		{
			CLEAR_UNREACHABLE("invalid assignment type");
		}

		result.CodegenValue = builder.CreateStore(tmp.CodegenValue, storage.CodegenValue);
		result.CodegenType  = storage.CodegenType;

		return result;
    }

    void ASTAssignmentOperator::HandleDifferentTypes(CodegenResult& storage, CodegenResult& data, CodegenContext& ctx)
    {
		auto& builder = ctx.Builder;
		
		std::shared_ptr<PointerType> ptrType = std::dynamic_pointer_cast<PointerType>(storage.CodegenType);
		CLEAR_VERIFY(ptrType, "storage must have pointer type");

		if(m_Type != AssignmentOperatorType::Initialize)
			CLEAR_VERIFY(!ptrType->GetBaseType()->IsConst(), "cannot assign to constant!");

		std::shared_ptr<Type> underlyingStorageType = ptrType->GetBaseType();
		CLEAR_VERIFY(underlyingStorageType, "not valid storage");

        llvm::Type* storageType = underlyingStorageType->Get();
        llvm::Type* dataType = data.CodegenType->Get();

		if(storageType == dataType)
			return;

		data.CodegenValue = TypeCasting::Cast(data.CodegenValue, data.CodegenType, underlyingStorageType, ctx.Builder);
		data.CodegenType = underlyingStorageType; 
    }

	ASTFunctionDefinition::ASTFunctionDefinition(const std::string& name, const TypeDescriptor& returnType, const std::vector<UnresolvedParameter>& paramaters)
		: m_ReturnType(returnType), m_Name(name), m_Parameters(paramaters)
	{
		CreateSymbolTable();
	}

	CodegenResult ASTFunctionDefinition::Codegen(CodegenContext& ctx)
	{
		auto& children = GetChildren();

		std::shared_ptr<SymbolTable> prev = GetSymbolTable()->GetPrevious();
		CLEAR_VERIFY(prev, "prev was null");

		bool isVariadic = m_Parameters.size() > 0 && m_Parameters.back().IsVariadic;

		std::transform(m_Parameters.begin(), m_Parameters.end(), std::back_inserter(m_ResolvedParams), [&](auto& a)
		{
			return Parameter{ a.Name, ctx.Registry.ResolveType(a.Type), a.IsVariadic };
		});
			
		m_ResolvedReturnType = ctx.Registry.ResolveType(m_ReturnType);
		
		std::vector<std::shared_ptr<ASTNodeBase>> defaultArgs(m_Parameters.size(), nullptr);

		size_t i = 0;
		for(; i < children.size(); i++)
		{
			if(children[i]->GetType() != ASTNodeType::DefaultArgument) 
				break;

			auto arg = std::dynamic_pointer_cast<ASTDefaultArgument>(children[i]);
			size_t argIndex = arg->GetIndex();

			CLEAR_VERIFY(argIndex < defaultArgs.size(), "invalid arg index!");
			defaultArgs[argIndex] = arg;
		}

		children.erase(children.begin(), children.begin() + i);

		prev->CreateTemplate(m_Name, m_ResolvedReturnType, m_ResolvedParams, isVariadic, defaultArgs, shared_from_this());
		
		if(m_Name == "main")
		{
			prev->InstantiateOrReturn(m_Name, m_ResolvedParams, m_ResolvedReturnType, ctx);
		}		
		
		return {};
	}

    void ASTFunctionDefinition::Instantiate(FunctionInstance& functionData, CodegenContext& ctx)
    {
		auto& module  = ctx.Module;
		auto& context = ctx.Context;
		auto& builder = ctx.Builder;

		s_InsertPoints.push(builder.saveIP());

		llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", functionData.Function);
		llvm::BasicBlock* body  = llvm::BasicBlock::Create(context, "body");
		
		builder.SetInsertPoint(entry);

		llvm::BasicBlock* returnBlock  = llvm::BasicBlock::Create(context, "return");
		llvm::AllocaInst* returnAlloca = m_ResolvedReturnType ? builder.CreateAlloca(m_ResolvedReturnType->Get(), nullptr, "return_value") : nullptr;
		
		ValueRestoreGuard guard1(ctx.ReturnType,   m_ResolvedReturnType ? m_ResolvedReturnType : ctx.Registry.GetType("void"));
		ValueRestoreGuard guard2(ctx.ReturnBlock,  returnBlock);
		ValueRestoreGuard guard3(ctx.ReturnAlloca, returnAlloca);

		uint32_t k = 0;

		llvm::AllocaInst* vaList = nullptr;

		bool hasVaArgs = false;

		std::shared_ptr<SymbolTable> tbl = GetSymbolTable();

		for (const auto& param : m_ResolvedParams)
		{
			if(param.IsVariadic)
			{
				hasVaArgs = true;
				break;
			}

			llvm::AllocaInst* argAlloc = builder.CreateAlloca(param.Type->Get(), nullptr, param.Name);
			builder.CreateStore(functionData.Function->getArg(k), argAlloc);
			
			Allocation alloca;
			alloca.Alloca = argAlloc;
			alloca.Type   = param.Type;

			tbl->RegisterAllocation(param.Name, alloca);
			k++;
		}

		auto& varArgs = tbl->GetVariadicArguments();
		varArgs.clear();
		
		if(hasVaArgs)
		{
			for(size_t i = k; i < functionData.Parameters.size(); i++)
			{
				llvm::AllocaInst* argAlloc = builder.CreateAlloca(functionData.Parameters[i].Type->Get(), nullptr, m_Parameters[k].Name);
				builder.CreateStore(functionData.Function->getArg(i), argAlloc);

				Allocation alloca;
				alloca.Alloca = argAlloc;
				alloca.Type   = functionData.Parameters[i].Type;
				varArgs.push_back(alloca);
			}

			Allocation dummy;
			dummy.Alloca = nullptr;
			dummy.Type = std::make_shared<VariadicArgumentsHolder>(); 

			tbl->RegisterAllocation(m_Parameters[k].Name, dummy);
		}

		functionData.Function->insert(functionData.Function->end(), body);
		builder.SetInsertPoint(body);

		for (const auto& child : GetChildren())
		{
			child->Codegen(ctx);
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
		{   
			llvm::Value* load = builder.CreateLoad(returnAlloca->getAllocatedType(), returnAlloca, "loaded_value");
			builder.CreateRet(load);
		}

		auto& ip = s_InsertPoints.top();
		builder.restoreIP(ip);
		s_InsertPoints.pop();
    }

    ASTFunctionCall::ASTFunctionCall(const std::string &name)
		: m_Name(name)
    {
    }

	CodegenResult ASTFunctionCall::Codegen(CodegenContext& ctx)
	{
		auto& builder  = ctx.Builder;
		auto& module   = ctx.Module;
		auto& context  = ctx.Context;

		std::shared_ptr<SymbolTable> symbolTable = GetSymbolTable();

		uint32_t k = 0;

		std::vector<llvm::Value*> args;
		std::vector<Parameter> params; // we only care about types here

		BuildArgs(ctx, args, params);

		if(Intrinsics::IsIntrinsic(m_Name))
		{
			llvm::Value* value = nullptr;

			if(args.size() > 0)
				value = Intrinsics::ApplyIntrinsic(m_Name, args[0], params[0].Type, ctx);
			else
				value = Intrinsics::ApplyIntrinsic(m_Name, nullptr, nullptr, ctx);

			if(!value) return {};

			if(m_Name == "sizeof") return { value, ctx.Registry.GetType("int64") };

			return { value, ctx.Registry.GetType(m_Name) };
		}

		FunctionTemplate& data = symbolTable->GetTemplate(m_Name, params);

		for(size_t i = args.size(); i < data.DefaultArguments.size(); i++)
		{
			CodegenResult argument = data.DefaultArguments[i]->Codegen(ctx);

			args.push_back(argument.CodegenValue);
			params.push_back({ .Type=argument.CodegenType });
		}
		
		CastArgs(ctx, args, params, data);

		// again, we need a check to make sure that return type is not a generic in the future, for now this is ok.

		if(symbolTable->HasDecleration(m_Name))
		{
			FunctionInstance& instance = symbolTable->GetDecleration(m_Name);
			return { ctx.Builder.CreateCall(instance.Function, args), instance.ReturnType };
		}

		CLEAR_VERIFY(symbolTable->GetPrevious(), "has no previous");
		FunctionInstance& instance = symbolTable->GetPrevious()->InstantiateOrReturn(m_Name, params, data.ReturnType, ctx);

		return { ctx.Builder.CreateCall(instance.Function, args), data.ReturnType };
	}

    void ASTFunctionCall::BuildArgs(CodegenContext& ctx, std::vector<llvm::Value*>& args, std::vector<Parameter>& params)
    {
		ValueRestoreGuard guard(ctx.WantAddress, false);

		for (auto& child : GetChildren())	
		{
			CodegenResult gen = child->Codegen(ctx);

			args.push_back(gen.CodegenValue);
			params.push_back({"", gen.CodegenType });
		}
    }

    void ASTFunctionCall::CastArgs(CodegenContext& ctx, std::vector<llvm::Value*>& args, std::vector<Parameter>& params, FunctionTemplate& fnTemplate)
    {
		for(size_t i = 0; i < args.size(); i++)
		{
			auto& param1 = params[i];
			auto& param2 = i < fnTemplate.Parameters.size() ? fnTemplate.Parameters[i] : fnTemplate.Parameters.back();

			if(!param2.Type) continue;

			if(param1.Type->Get() != param2.Type->Get()) 
			{
				args[i] = TypeCasting::Cast(args[i], param1.Type, param2.Type, ctx.Builder);
				params[i].Type = param2.Type;
			}
		}
    }

    ASTFunctionDecleration::ASTFunctionDecleration(const std::string& name, const TypeDescriptor& expectedReturnType, const std::vector<UnresolvedParameter>& types)
		: m_Name(name), m_Parameters(types), m_ReturnType(expectedReturnType)
    {
    }

	CodegenResult ASTFunctionDecleration::Codegen(CodegenContext& ctx)
	{
		auto& module = ctx.Module;

		std::vector<llvm::Type*> types;
		std::vector<Parameter> params;

		std::transform(m_Parameters.begin(), m_Parameters.end(), std::back_inserter(params), [&](auto& a)
		{
			return Parameter{a.Name, ctx.Registry.ResolveType(a.Type), a.IsVariadic};
		});

		bool isVariadic = false;

		for (auto& param : params)
		{
			if (!param.Type)
			{
				isVariadic = true;
				break;
			}

			types.push_back(param.Type->Get());
		}

		std::shared_ptr<Type> resolvedType = ctx.Registry.GetType("void");

		if (!m_ReturnType.Description.empty())
			resolvedType = ctx.Registry.ResolveType(m_ReturnType);

		llvm::FunctionType* functionType = llvm::FunctionType::get(resolvedType->Get(), types, isVariadic);
		llvm::FunctionCallee callee = module.getOrInsertFunction(m_Name, functionType);

		FunctionInstance data;
		data.FunctionType = functionType;
		data.Function = llvm::cast<llvm::Function>(callee.getCallee());
		data.Parameters = params;
		data.ReturnType = resolvedType;
		data.MangledName = m_Name;
		
		GetSymbolTable()->RegisterInstance(data);

		FunctionTemplate functionTemplate;
		functionTemplate.IsVariadic = params.size() > 0 && !params.back().Type;
		functionTemplate.Parameters = params;
		functionTemplate.ReturnType = resolvedType;
		functionTemplate.Root = nullptr; // external function so no root
		functionTemplate.IsExternal = true;

		GetSymbolTable()->RegisterTemplate(data.MangledName, functionTemplate);

		return { data.Function, resolvedType };	
	}

    CodegenResult ASTExpression::Codegen(CodegenContext& ctx)
	{
		auto& builder  = ctx.Builder;
		auto& children = GetChildren();

		std::stack<std::shared_ptr<ASTNodeBase>> stack;

		auto IsOperand = [](const std::shared_ptr<ASTNodeBase>& child) 
		{
			return child->GetType() == ASTNodeType::Literal || 
				   child->GetType() == ASTNodeType::Variable ||
				   child->GetType() == ASTNodeType::FunctionCall || 
				   child->GetType() == ASTNodeType::Member;
		};
		
		for (const auto& child : children)
		{
			if (IsOperand(child))
			{
				stack.push(child);
				continue;
			}

			if (std::shared_ptr<ASTUnaryExpression> unaryExpression = std::dynamic_pointer_cast<ASTUnaryExpression>(child))
			{
				unaryExpression->GetChildren().clear();

				unaryExpression->Push(stack.top());
				stack.pop();

				stack.push(unaryExpression);
				continue;
			}

			std::shared_ptr<ASTBinaryExpression> binExp = std::dynamic_pointer_cast<ASTBinaryExpression>(child);
			binExp->GetChildren().clear();

			binExp->Push(stack.top());
			stack.pop();

			binExp->Push(stack.top());
			stack.pop();

			stack.push(binExp);
		}

		if(stack.size() > 0)
		{
			CLEAR_VERIFY(stack.size() == 1, "wot");
			return stack.top()->Codegen(ctx);
		}


		return {};
	}


	CodegenResult ASTInitializerList::Codegen(CodegenContext& ctx)
	{
		auto& builder = ctx.Builder;
		auto& context = ctx.Context;
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() > 0, "invalid array initializer");

		CodegenResult storage;

		{
			ValueRestoreGuard guard(ctx.WantAddress, true);
			storage = children[0]->Codegen(ctx);
		}

		std::shared_ptr<PointerType> storageType = std::dynamic_pointer_cast<PointerType>(storage.CodegenType);
		CLEAR_VERIFY(storageType, "invalid storage type");

		CLEAR_VERIFY(children.size() - 1 == m_Indices.size(), "sizes don't match!");

		llvm::Type* intTy = llvm::Type::getInt64Ty(context);

		if(auto baseType = std::dynamic_pointer_cast<ArrayType>(storageType->GetBaseType()))
		{
			DoInitForArray(ctx, storage);
		}
		else if (auto baseType = std::dynamic_pointer_cast<StructType>(storageType->GetBaseType()))
		{
			DoInitForStruct(ctx, storage);
		}
		
		return {};
	}

    void ASTInitializerList::SetIndices(const std::vector<std::vector<size_t>>& indices)
    {
		m_Indices = indices;
    }

    void ASTInitializerList::DoInitForArray(CodegenContext& ctx, CodegenResult storage)
    {
		auto& children = GetChildren();

		llvm::Type* intTy = llvm::Type::getInt64Ty(ctx.Context);

		std::shared_ptr<PointerType> storageType = std::dynamic_pointer_cast<PointerType>(storage.CodegenType);
		std::shared_ptr<ArrayType> baseType = std::dynamic_pointer_cast<ArrayType>(storageType->GetBaseType());
		CLEAR_VERIFY(baseType, "base type is not an array type");

		llvm::Constant* zeroArray = llvm::ConstantAggregateZero::get(baseType->Get());
		ctx.Builder.CreateStore(zeroArray, storage.CodegenValue);

		ValueRestoreGuard guard(ctx.WantAddress, false);

		for(size_t i = 0; i < m_Indices.size(); i++)
		{
			llvm::Value* elemPtr = ctx.Builder.CreateInBoundsGEP(baseType->Get(), 
													 		 storage.CodegenValue, ctx.Builder.getInt64(0), 
													 		 "gep");

			auto [elemPtr1, innerType] = GetBasePointer(i, elemPtr, baseType, 1, ctx);
			elemPtr = elemPtr1;
			
			CodegenResult valueToStore = children[i + 1]->Codegen(ctx);

			if(valueToStore.CodegenType != innerType)
			{
				valueToStore.CodegenValue = TypeCasting::Cast(
							valueToStore.CodegenValue,
							valueToStore.CodegenType, 
							innerType,
							ctx.Builder);
			}

			ctx.Builder.CreateStore(valueToStore.CodegenValue, elemPtr);
		}
    }

    void ASTInitializerList::DoInitForStruct(CodegenContext &ctx, CodegenResult storage)
    {
		auto& children = GetChildren();

		//llvm::Type* intTy = llvm::Type::getInt64Ty(ctx.Context);

		std::shared_ptr<PointerType> storageType = std::dynamic_pointer_cast<PointerType>(storage.CodegenType);
		std::shared_ptr<StructType> baseType = std::dynamic_pointer_cast<StructType>(storageType->GetBaseType());
		CLEAR_VERIFY(baseType, "base type is not a struct type");

		llvm::Constant* zeroArray = llvm::ConstantAggregateZero::get(baseType->Get());
		ctx.Builder.CreateStore(zeroArray, storage.CodegenValue);

		ValueRestoreGuard guard(ctx.WantAddress, false);

		for(size_t i = 0; i < m_Indices.size(); i++)
		{
			//std::vector<llvm::Value*> indices(m_Indices[i].size());
			CLEAR_VERIFY(m_Indices[i].size() >= 2, "");

			llvm::Value* elemPtr = ctx.Builder.CreateStructGEP(baseType->Get(), storage.CodegenValue, m_Indices[i][1], "gep");
			std::shared_ptr<Type> innerType = baseType->GetMemberAtIndex(m_Indices[i][1]);

			auto [elemPtr2, innerType2] = GetBasePointer(i, elemPtr, innerType, 2, ctx);

			elemPtr = elemPtr2;
			innerType = innerType2;

			CodegenResult valueToStore = children[i + 1]->Codegen(ctx);

			if(valueToStore.CodegenType != innerType)
			{
				valueToStore.CodegenValue = TypeCasting::Cast(
							valueToStore.CodegenValue,
							valueToStore.CodegenType, 
							innerType,
							ctx.Builder);
			}

			ctx.Builder.CreateStore(valueToStore.CodegenValue, elemPtr);
		}
		
		
    }

    std::shared_ptr<Type> ASTInitializerList::GetElementType(std::shared_ptr<Type> type)
    {
		while(auto base = std::dynamic_pointer_cast<ArrayType>(type))
		{
			type = base->GetBaseType();
		}

        return type;
    }
    std::shared_ptr<Type> ASTInitializerList::GetInnerType(std::shared_ptr<Type> type, size_t index)
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

    std::pair<llvm::Value*, std::shared_ptr<Type>> ASTInitializerList::GetBasePointer(size_t index, llvm::Value* elemPtr, std::shared_ptr<Type> innerType, size_t startingIndex, CodegenContext& ctx)
    {	
		for(size_t j = startingIndex; j < m_Indices[index].size(); j++)
		{
			if(innerType->IsCompound())
			{
				auto tmp = std::dynamic_pointer_cast<StructType>(innerType);
				size_t structIndex = m_Indices[index][j] % tmp->GetMemberTypes().size();
				elemPtr = ctx.Builder.CreateStructGEP(innerType->Get(), elemPtr, structIndex, "gep");
				innerType = tmp->GetMemberAtIndex(structIndex);
			}
			else 
			{
				CLEAR_VERIFY(innerType->IsArray(), "invalid type");
				auto tmp = std::dynamic_pointer_cast<ArrayType>(innerType);
				size_t arrayIndex = m_Indices[index][j] % tmp->GetArraySize();
				elemPtr = ctx.Builder.CreateGEP(tmp->GetBaseType()->Get(), elemPtr, ctx.Builder.getInt64(arrayIndex), "gep");
				innerType = tmp->GetBaseType();
			}
		}

        return std::make_pair(elemPtr, innerType);
    }

    ASTImport::ASTImport(const std::filesystem::path& filepath, const std::string& alias)
		: m_Filepath(filepath), m_Alias(alias)
    {
    }

	CodegenResult ASTImport::Codegen(CodegenContext& ctx)
	{
		std::filesystem::path completeFilePath = ctx.CurrentDirectory / m_Filepath;

		if(completeFilePath.extension() == ".h")
		{
			CLEAR_VERIFY(std::filesystem::exists(completeFilePath), completeFilePath, " doesn't exist");
			ProcessCImport(completeFilePath, ctx);

			return {};
		}

		if(!ctx.LookupTable.contains(completeFilePath))
		{
			completeFilePath = ctx.StdLibraryDirectory / m_Filepath;
		}

		CLEAR_VERIFY(ctx.LookupTable.contains(completeFilePath), "cannot find ", completeFilePath);
		
		ProcessTypes(completeFilePath, ctx);

		auto& lookupInfo = ctx.LookupTable.at(completeFilePath);

		auto& rootChildren = lookupInfo.Node->GetChildren();
		auto rootSymbolTable = lookupInfo.Node->GetSymbolTable();


		for(const auto& child : rootChildren)
		{
			std::shared_ptr<ASTFunctionDefinition> fun = std::dynamic_pointer_cast<ASTFunctionDefinition>(child);
			
			if(!fun) 
				continue;

			auto tbl = GetSymbolTable();
			
			if(rootSymbolTable->HasInstance(fun->GetName()))
			{
				FunctionInstance& importedData = rootSymbolTable->GetInstance(fun->GetName());

				llvm::FunctionCallee callee = ctx.Module.getOrInsertFunction(importedData.MangledName, importedData.FunctionType);

				FunctionInstance registeredData;
				registeredData.FunctionType = importedData.FunctionType;
				registeredData.Function = llvm::cast<llvm::Function>(callee.getCallee());
				registeredData.Parameters = importedData.Parameters;
				
				if(!m_Alias.empty())
					registeredData.MangledName = std::format("{}.{}", m_Alias, fun->GetName());
				else 
					registeredData.MangledName = fun->GetName();

				registeredData.ReturnType = importedData.ReturnType;

				bool isVariadic = registeredData.Parameters.size() > 0 && !registeredData.Parameters.back().Type;
				tbl->RegisterDecleration(registeredData, isVariadic);
			}
			else if (rootSymbolTable->HasTemplate(fun->GetName()))
			{
				if(!m_Alias.empty())
					tbl->RegisterTemplate(std::format("{}.{}", m_Alias, fun->GetName()), rootSymbolTable->GetTemplate(fun->GetName(), fun->GetParameters()));
				else 
					tbl->RegisterTemplate(fun->GetName(), rootSymbolTable->GetTemplate(fun->GetName(), fun->GetParameters()));
			}
			else 
			{
				CLEAR_UNREACHABLE("uhhh what");
			}
		}

		return {};
	}

    void ASTImport::ProcessCImport(const std::filesystem::path& path, CodegenContext& ctx)
    {
		auto functions = ExtractFunctions(path);

		for(const auto& header : functions)
		{
			FunctionInstance function = ParseHeader(header, ctx);

			if(!m_Alias.empty())
				function.MangledName = std::format("{}.{}", m_Alias, header.name);
			else 
				function.MangledName = header.name;

			std::shared_ptr<SymbolTable> tbl = GetSymbolTable();

			bool isVariadic = function.Parameters.size() > 0 && !function.Parameters.back().Type;
			tbl->RegisterDecleration(function, isVariadic);

		}
    }

    FunctionInstance ASTImport::ParseHeader(const HeaderFunc& function, CodegenContext& ctx)
    {
		FunctionInstance functionData;
		functionData.MangledName = function.name;

		for(const auto& arg : function.args)
		{
			functionData.Parameters.push_back(GetInfoFromArg(arg, ctx));
		}

		functionData.ReturnType = GetInfoFromArg(function.returnType, ctx).Type;

		std::vector<llvm::Type*> parameterTypes;
        std::transform(functionData.Parameters.begin(), functionData.Parameters.end(), std::back_inserter(parameterTypes), [](Parameter& a) { return a.Type->Get(); });
		llvm::FunctionType* functionType = llvm::FunctionType::get(functionData.ReturnType ? functionData.ReturnType->Get() : llvm::FunctionType::getVoidTy(ctx.Context), parameterTypes, false);
		llvm::FunctionCallee callee = ctx.Module.getOrInsertFunction(function.name, functionType);

		functionData.FunctionType = functionType;
		functionData.Function = llvm::dyn_cast<llvm::Function>(callee.getCallee());

		CLEAR_VERIFY(!llvm::verifyFunction(*functionData.Function, &llvm::errs()), "failed to verify function");	
        return functionData;
    }

    Parameter ASTImport::GetInfoFromArg(const std::vector<Token>& arg, CodegenContext &ctx)
    {
		CLEAR_VERIFY(arg.size() >= 1, "args not valid");
		Parameter param;
		param.Name = "__unamed_c_parm";

		if(arg[0].TokenType == TokenType::Ellipsis)
		{
			return param;
		}

		param.Type = ctx.Registry.GetTypeFromToken(arg[0]);
		
		for(size_t i = 1; i < arg.size(); i++)
		{
			if(arg[i].TokenType == TokenType::PointerDef)
				param.Type = ctx.Registry.GetPointerTo(param.Type);
			else if (arg[i].TokenType == TokenType::StaticArrayDef)
				param.Type = ctx.Registry.GetArrayFrom(param.Type, std::stoull(arg[i].Data));
			else
				CLEAR_UNREACHABLE("invalid token");
		}

        return param;
    }

    void ASTImport::ProcessTypes(const std::filesystem::path& path, CodegenContext& ctx)
    {
		auto& lookupInfo = ctx.LookupTable.at(path);

		auto& rootChildren = lookupInfo.Node->GetChildren();
		auto rootSymbolTable = lookupInfo.Node->GetSymbolTable();

		for(const auto& [typeName, type] : lookupInfo.Registry.GetTypeTable())
		{
			if(type->IsCompound())
			{
				if(!m_Alias.empty())
					ctx.Registry.RegisterType(m_Alias + "." + typeName, type);
				else 
					ctx.Registry.RegisterType(typeName, type);
			}
		}
    }

    ASTMember::ASTMember(const std::string& name)
		: m_MemberName(name)
    {
    }

	CodegenResult ASTMember::Codegen(CodegenContext& ctx)
	{
		return {};
	}

	CodegenResult ASTReturn::Codegen(CodegenContext& ctx)
	{
		auto& children = GetChildren();

		llvm::BasicBlock* currentBlock = ctx.Builder.GetInsertBlock();

		if(currentBlock->getTerminator()) 
			return {};

		if(children.size() == 0) 
		{
			EmitDefaultReturn(ctx);
			return {};
		}

		ValueRestoreGuard guard(ctx.WantAddress, false);
		CodegenResult codegen = children[0]->Codegen(ctx);

		if(codegen.CodegenValue == nullptr)
		{
			EmitDefaultReturn(ctx);
			return {};
		}
		
		if(codegen.CodegenType != ctx.ReturnType)
		{
			codegen.CodegenValue = TypeCasting::Cast(codegen.CodegenValue, codegen.CodegenType, ctx.ReturnType, ctx.Builder);
		}

		ctx.Builder.CreateStore(codegen.CodegenValue, ctx.ReturnAlloca);
		ctx.Builder.CreateBr(ctx.ReturnBlock);

		return {};
	}

    void ASTReturn::EmitDefaultReturn(CodegenContext& ctx)
    {
		llvm::Type* retType = ctx.ReturnType->Get();
    	llvm::Value* defaultVal = llvm::UndefValue::get(retType);
    	ctx.Builder.CreateStore(defaultVal, ctx.ReturnAlloca);
    	ctx.Builder.CreateBr(ctx.ReturnBlock);
    }

    ASTUnaryExpression::ASTUnaryExpression(UnaryExpressionType type)
		: m_Type(type)
    {
    }

	CodegenResult ASTUnaryExpression::Codegen(CodegenContext& ctx)
	{
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() == 1, "incorrect dimensions");

		if(m_Type == UnaryExpressionType::Dereference)
		{
			CodegenResult result;

			{
				ValueRestoreGuard guard(ctx.WantAddress, false);
				result = children[0]->Codegen(ctx);
			}

			CLEAR_VERIFY(result.CodegenType->IsPointer(), "not a valid dereference");

			if(ctx.WantAddress)
				return result;
			
			std::shared_ptr<PointerType> ptrType = std::dynamic_pointer_cast<PointerType>(result.CodegenType);

			return {ctx.Builder.CreateLoad(ptrType->GetBaseType()->Get(), 
										   result.CodegenValue), ptrType->GetBaseType() };
		}	

		CLEAR_VERIFY(!ctx.WantAddress, "Invalid use of unary expression");

		if(m_Type == UnaryExpressionType::Reference)
		{		
			ValueRestoreGuard guard(ctx.WantAddress, true);
			CodegenResult result = children[0]->Codegen(ctx);

			return result;
		}

		if(m_Type == UnaryExpressionType::Negation)
		{			
			CodegenResult result = children[0]->Codegen(ctx);

			llvm::Value* negated;

			if(result.CodegenType->IsFloatingPoint())
			{
				negated = ctx.Builder.CreateFNeg(result.CodegenValue);
			}
			else 
			{
				negated = ctx.Builder.CreateNeg(result.CodegenValue);
				result.CodegenType = ctx.Registry.GetSignedType(result.CodegenType);
			}

			return { negated, result.CodegenType };
		}

		if(m_Type == UnaryExpressionType::BitwiseNot)
		{
			CodegenResult result = children[0]->Codegen(ctx);
			return { ctx.Builder.CreateNot(result.CodegenValue), result.CodegenType };
		}	

		if(m_Type == UnaryExpressionType::Not)
		{
			CodegenResult result = children[0]->Codegen(ctx);
			result.CodegenValue = TypeCasting::Cast(result.CodegenValue, result.CodegenType, ctx.Registry.GetType("bool"), ctx.Builder);
			result.CodegenType  = ctx.Registry.GetType("bool");

			result.CodegenValue = ctx.Builder.CreateXor(ctx.Builder.getTrue(), result.CodegenValue);

			return result;
		}
		
		CodegenResult one;
		one.CodegenType  = ctx.Registry.GetType("int32");
		one.CodegenValue = ctx.Builder.getInt32(1);

		ValueRestoreGuard guard(ctx.WantAddress, true);

		CodegenResult result = children[0]->Codegen(ctx);
		CLEAR_VERIFY(result.CodegenType->IsPointer(), "not valid type for increment");
		
		std::shared_ptr<PointerType> ty = std::dynamic_pointer_cast<PointerType>(result.CodegenType);

		CodegenResult valueToStore;
		CodegenResult returnValue;

		auto ApplyFun = [&](BinaryExpressionType type)
		{
			if(ty->GetBaseType()->IsPointer())
				valueToStore = ASTBinaryExpression::HandlePointerArithmetic(returnValue, one, type, ctx);
			else 
				valueToStore = ASTBinaryExpression::HandleMathExpression(returnValue, one, type, ctx);
		};

		if(m_Type == UnaryExpressionType::PostIncrement)
		{
			returnValue.CodegenValue = ctx.Builder.CreateLoad(ty->GetBaseType()->Get(), result.CodegenValue);
			returnValue.CodegenType = ty->GetBaseType();

			ApplyFun(BinaryExpressionType::Add);
		}
		else if (m_Type == UnaryExpressionType::PostDecrement)
		{
			returnValue.CodegenValue = ctx.Builder.CreateLoad(ty->GetBaseType()->Get(), result.CodegenValue);
			returnValue.CodegenType = ty->GetBaseType();

			ApplyFun(BinaryExpressionType::Sub);
		}
		else if (m_Type == UnaryExpressionType::PreIncrement)
		{
			returnValue.CodegenValue = ctx.Builder.CreateLoad(ty->GetBaseType()->Get(), result.CodegenValue);
			returnValue.CodegenType = ty->GetBaseType();

			ApplyFun(BinaryExpressionType::Add);
			returnValue.CodegenValue = valueToStore.CodegenValue;
		}
		else if (m_Type == UnaryExpressionType::PreDecrement)
		{
			returnValue.CodegenValue = ctx.Builder.CreateLoad(ty->GetBaseType()->Get(), result.CodegenValue);
			returnValue.CodegenType = ty->GetBaseType();

			ApplyFun(BinaryExpressionType::Sub);
			returnValue.CodegenValue = valueToStore.CodegenValue;
		}
		else 
		{
			CLEAR_UNREACHABLE("unimplemented");
		}

		valueToStore.CodegenValue = TypeCasting::Cast(valueToStore.CodegenValue, valueToStore.CodegenType, ty->GetBaseType(), ctx.Builder);

		ctx.Builder.CreateStore(valueToStore.CodegenValue, result.CodegenValue);

		return returnValue;
	}


	CodegenResult ASTIfExpression::Codegen(CodegenContext& ctx)
	{
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() > 1, "size must be greater than 1");

		llvm::Function* function = ctx.Builder.GetInsertBlock()->getParent();

		struct Branch
		{
			llvm::BasicBlock* ConditionBlock = nullptr;
			llvm::BasicBlock* BodyBlock  = nullptr;
			int64_t ExpressionIdx = 0;
		};

		CLEAR_VERIFY(children[0]->GetType() == ASTNodeType::Expression, "");

		std::vector<Branch> branches;

		for (size_t i = 0; i + 1 < children.size(); i += 2)
		{
			Branch branch;
			branch.ConditionBlock = llvm::BasicBlock::Create(ctx.Context, "if_condition");
			branch.BodyBlock      = llvm::BasicBlock::Create(ctx.Context, "if_body");
			branch.ExpressionIdx  = i;

			branches.push_back(branch);
		}

		llvm::BasicBlock* elseBlock  = llvm::BasicBlock::Create(ctx.Context, "else");
		llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(ctx.Context, "merge");

		if(!ctx.Builder.GetInsertBlock()->getTerminator())
			ctx.Builder.CreateBr(branches[0].ConditionBlock);

		for (size_t i = 0; i < branches.size(); i++)
		{
			auto& branch = branches[i];

			llvm::BasicBlock* nextBranch = (i + 1) < branches.size() ? branches[i + 1].ConditionBlock : elseBlock;
			
			function->insert(function->end(), branch.ConditionBlock);
			ctx.Builder.SetInsertPoint(branch.ConditionBlock);

			CodegenResult condition;

			{
				ValueRestoreGuard guard(ctx.WantAddress, false);
				condition = children[branch.ExpressionIdx]->Codegen(ctx);
			}

			if (condition.CodegenType->IsIntegral() && condition.CodegenType->GetSize() > 1)
			{
				condition.CodegenValue = ctx.Builder.CreateICmpNE(condition.CodegenValue, llvm::ConstantInt::get(condition.CodegenType->Get(), 0));
			}
			else if (condition.CodegenType->IsFloatingPoint())
			{
				condition.CodegenValue = ctx.Builder.CreateFCmpONE(condition.CodegenValue, llvm::ConstantFP::get(condition.CodegenType->Get(), 0.0));
			}
			else if (condition.CodegenType->IsPointer())
			{
				condition.CodegenValue = ctx.Builder.CreatePtrToInt(condition.CodegenValue, ctx.Builder.getInt64Ty(), "cast");
				condition.CodegenValue = ctx.Builder.CreateICmpNE(condition.CodegenValue, ctx.Builder.getInt64(0));
			}

			ctx.Builder.CreateCondBr(condition.CodegenValue, branch.BodyBlock, nextBranch);

			function->insert(function->end(), branch.BodyBlock);
			ctx.Builder.SetInsertPoint(branch.BodyBlock);

			CLEAR_VERIFY(branch.ExpressionIdx + 1 < children.size(), "");
			children[branch.ExpressionIdx + 1]->Codegen(ctx);

			if (!ctx.Builder.GetInsertBlock()->getTerminator())
				ctx.Builder.CreateBr(mergeBlock);
		}

		function->insert(function->end(), elseBlock);
		ctx.Builder.SetInsertPoint(elseBlock);

		size_t last = children.size() - 1;

		if (children.size() > 2 && children[last]->GetType() == children[last - 1]->GetType())
			children[last]->Codegen(ctx);
	
		if (!ctx.Builder.GetInsertBlock()->getTerminator())
			ctx.Builder.CreateBr(mergeBlock);

		function->insert(function->end(), mergeBlock);
		ctx.Builder.SetInsertPoint(mergeBlock);

		return {};
	}

	CodegenResult ASTWhileExpression::Codegen(CodegenContext& ctx)
	{
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() == 2, "incorrect dimension");
		CLEAR_VERIFY(children[0]->GetType() == ASTNodeType::Expression, "incorrect node type");

		llvm::Function* function = ctx.Builder.GetInsertBlock()->getParent();

		llvm::BasicBlock* conditionBlock = llvm::BasicBlock::Create(ctx.Context, "while_condition", function);
		llvm::BasicBlock* body  = llvm::BasicBlock::Create(ctx.Context, "while_body");
		llvm::BasicBlock* end   = llvm::BasicBlock::Create(ctx.Context, "while_end");

		if (!ctx.Builder.GetInsertBlock()->getTerminator())
			ctx.Builder.CreateBr(conditionBlock);

		ctx.Builder.SetInsertPoint(conditionBlock);

		CodegenResult condition;
			
		{
			ValueRestoreGuard guard(ctx.WantAddress, false);
			condition = children[0]->Codegen(ctx);
		}



		if (condition.CodegenType->IsIntegral())
			condition.CodegenValue = ctx.Builder.CreateICmpNE(condition.CodegenValue, llvm::ConstantInt::get(condition.CodegenType->Get(), 0));
			
		else if (condition.CodegenType->IsFloatingPoint())
			condition.CodegenValue = ctx.Builder.CreateFCmpONE(condition.CodegenValue, llvm::ConstantFP::get(condition.CodegenType->Get(), 0.0));

		if (!ctx.Builder.GetInsertBlock()->getTerminator())
			ctx.Builder.CreateCondBr(condition.CodegenValue, body, end);

		function->insert(function->end(), body);
		ctx.Builder.SetInsertPoint(body);

    	ValueRestoreGuard guard1(ctx.LoopConditionBlock, conditionBlock);
    	ValueRestoreGuard guard2(ctx.LoopEndBlock,       end);

		children[1]->Codegen(ctx);

		if (!ctx.Builder.GetInsertBlock()->getTerminator())
			ctx.Builder.CreateBr(conditionBlock);
		
		function->insert(function->end(), end);
		ctx.Builder.SetInsertPoint(end);

		return {};
	}

    ASTForExpression::ASTForExpression(const std::string& name)
		: m_Name(name)
    {
    }

    CodegenResult ASTForExpression::Codegen(CodegenContext& ctx)
    {
		auto& children = GetChildren();
		CLEAR_VERIFY(children.size() == 2, "invalid for loop");

		CodegenResult iterator;

		{
			ValueRestoreGuard guard(ctx.WantAddress, true);
			iterator = children[0]->Codegen(ctx);
		}	

		auto tbl = GetSymbolTable();

		if(iterator.CodegenType->IsVariadic())
		{
			auto& args = tbl->GetVariadicArguments();

			for(size_t i = 0; i < args.size(); i++)
			{
				tbl->RegisterAllocation(m_Name, args[i]);

				children[1]->Codegen(ctx);
			}

			return {};
		}


		CLEAR_UNREACHABLE("unimplemented");

		return {};
	}

	ASTStruct::ASTStruct(const TypeDescriptor& structTy)
		: m_StructTy(structTy)
    {
    }

	ASTLoopControlFlow::ASTLoopControlFlow(TokenType jumpTy) 
		: m_JumpTy(jumpTy)
	{
	}


	CodegenResult ASTStruct::Codegen(CodegenContext& ctx)
	{
		ctx.Registry.ResolveType(m_StructTy);
		return {};
	}

	CodegenResult ASTLoopControlFlow::Codegen(CodegenContext& ctx) 
	{
    	CLEAR_VERIFY(ctx.LoopConditionBlock, "BREAK/CONTINUE not in loop")
		
    	if (m_JumpTy == TokenType::Continue) 
			ctx.Builder.CreateBr(ctx.LoopConditionBlock);
		
    	else if(m_JumpTy == TokenType::Break) 
    		ctx.Builder.CreateBr(ctx.LoopEndBlock);
    	
    	return {};
    }

    CodegenResult ASTDefaultArgument::Codegen(CodegenContext& ctx)
    {
		auto& children = GetChildren();
		CLEAR_VERIFY(children.size() == 1, "invalid argument");
		ValueRestoreGuard guard(ctx.WantAddress, false);

        return children[0]->Codegen(ctx);
    }
}