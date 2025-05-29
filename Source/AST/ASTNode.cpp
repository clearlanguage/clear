#include "ASTNode.h"

#include "API/LLVM/LLVMInclude.h"
#include "Core/Log.h"
#include "TypeCasting.h"

#include "Core/TypeRegistry.h"

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


    CodegenResult ASTNodeBase::Codegen(CodegenContext& ctx)
    {
        CodegenResult value;

		for (auto& child : GetChildren())
			value = child->Codegen(ctx);

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

		//CodegenResult lhs = leftChild->Codegen(ctx);
		//CodegenResult rhs = rightChild->Codegen(ctx);

        //if(!lhs.CodegenValue->getType()->isPointerTy()) 
		//	HandleTypePromotion(lhs, rhs, ctx);
				
		if(IsMathExpression()) 
			return HandleMathExpression(leftChild, rightChild, ctx);

		if(IsCmpExpression())
			return HandleCmpExpression(leftChild, rightChild, ctx);

		if(m_Expression == BinaryExpressionType::Index)
			return HandleArrayIndex(leftChild, rightChild, ctx);

		//return HandleBitwiseExpression(lhs, rhs); 
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
			ValueRestoreGuard<bool> guard(ctx.WantAddress, false);
			rhs = right->Codegen(ctx);
		}

		if(lhs.CodegenType->IsPointer()) 
			return HandlePointerArithmetic(lhs, rhs, ctx); //internally will verify correct expression type

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
				llvm::Function* powFunction = llvm::Intrinsic::getDeclaration(&module, llvm::Intrinsic::pow, { builder.getDoubleTy() });
                return { builder.CreateCall(powFunction, {lhs.CodegenValue, rhs.CodegenValue}), 
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
				llvm::Function* powFunction = llvm::Intrinsic::getDeclaration(&module, llvm::Intrinsic::pow, { builder.getDoubleTy() });
                return { builder.CreateCall(powFunction, {lhs.CodegenValue, rhs.CodegenValue}), 
					     ctx.Registry.GetType("float64") };
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
				llvm::Function* powFunction = llvm::Intrinsic::getDeclaration(&module, llvm::Intrinsic::pow, { builder.getDoubleTy() });
                return { builder.CreateCall(powFunction, {lhs.CodegenValue, rhs.CodegenValue}), 
						 ctx.Registry.GetType("float64") };
			}
			default:
				break;
		}

		return {};
    }

    CodegenResult ASTBinaryExpression::HandleCmpExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext &ctx)
    {
		ValueRestoreGuard<bool> guard(ctx.WantAddress, false);

		CodegenResult lhs = left->Codegen(ctx);
		CodegenResult rhs = right->Codegen(ctx);

		if(lhs.CodegenType != rhs.CodegenType)
		{
			HandleTypePromotion(lhs, rhs, ctx);
		}

        return HandleCmpExpression(lhs, rhs, ctx);
    }

    CodegenResult ASTBinaryExpression::HandleCmpExpression(CodegenResult &lhs, CodegenResult &rhs, CodegenContext &ctx)
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

    CodegenResult ASTBinaryExpression::HandleBitwiseExpression(CodegenResult& lhs, CodegenResult& rhs)
    {
		CLEAR_UNREACHABLE("unimplemented")
        return {};
    }

    CodegenResult ASTBinaryExpression::HandlePointerArithmetic(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx)
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

		if(m_Expression == BinaryExpressionType::Add)
		{
			return { ctx.Builder.CreateGEP(ptrType->GetBaseType()->Get(), lhs.CodegenValue, rhs.CodegenValue), ptrType };
		}

		if(m_Expression == BinaryExpressionType::Sub)
		{
			rhs.CodegenValue = ctx.Builder.CreateNeg(rhs.CodegenValue);
			return { ctx.Builder.CreateGEP(ptrType->GetBaseType()->Get(), lhs.CodegenValue, rhs.CodegenValue), ptrType };
		}

		CLEAR_UNREACHABLE("invalid binary expression");

        return {};
    }

    CodegenResult ASTBinaryExpression::HandleArrayIndex(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext &ctx)
    {
		CodegenResult lhs;

		{
			ValueRestoreGuard<bool> guard(ctx.WantAddress, true);
			lhs = left->Codegen(ctx);
		}

		CodegenResult rhs;

		{
			ValueRestoreGuard<bool> guard(ctx.WantAddress, false);
			rhs = right->Codegen(ctx);
		}

		// lhs is going to be a reference to the array
		std::shared_ptr<PointerType> type = std::dynamic_pointer_cast<PointerType>(lhs.CodegenType);
		CLEAR_VERIFY(type, "invalid type");

		llvm::Value* gep = nullptr;
		std::shared_ptr<Type> baseType;

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
        		"array_index_ptr"
    		);

			baseType = arrType->GetBaseType();
		}	

		//TODO: pointers here ( will do this later when pointers implemented ignore for now)

		if(ctx.WantAddress)
			return {gep, ctx.Registry.GetPointerTo(baseType) };

        return { ctx.Builder.CreateLoad(baseType->Get(), gep), baseType} ;
    }

    ASTVariableDeclaration::ASTVariableDeclaration(const std::string& name, std::shared_ptr<Type> type)
		: m_Name(name), m_Type(type)
    {
    }

	CodegenResult ASTVariableDeclaration::Codegen(CodegenContext& ctx)
    {
		CodegenResult codegenResult;

		std::shared_ptr<SymbolTable> registry = GetSymbolTable();
		
		Allocation alloca = registry->CreateAlloca(m_Name, m_Type, ctx.Builder);
		codegenResult.CodegenValue = alloca.Alloca;
		codegenResult.CodegenType  = ctx.Registry.GetPointerTo(alloca.Type);

		return codegenResult;
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
			ValueRestoreGuard<bool> guard(ctx.WantAddress, true);
			storage = children[0]->Codegen(ctx);
		}

		CodegenResult data;

		{
			ValueRestoreGuard<bool> guard(ctx.WantAddress, false);
			data    = children[1]->Codegen(ctx);
		}

		HandleDifferentTypes(storage, data, ctx);

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

		std::shared_ptr<Type> underlyingStorageType = ptrType->GetBaseType();
		CLEAR_VERIFY(underlyingStorageType, "not valid storage");

        llvm::Type* storageType = underlyingStorageType->Get();
        llvm::Type* dataType = data.CodegenType->Get();

		if(storageType == dataType)
			return;

		data.CodegenValue = TypeCasting::Cast(data.CodegenValue, data.CodegenType, underlyingStorageType, ctx.Builder);
		data.CodegenType = underlyingStorageType; 
    }

	ASTFunctionDefinition::ASTFunctionDefinition(const std::string& name, const std::shared_ptr<Type>& returnType, const std::vector<Parameter>& Paramaters)
		: m_Parameters(Paramaters), m_ReturnType(returnType), m_Name(name)
	{
		CreateSymbolTable();
	}

	CodegenResult ASTFunctionDefinition::Codegen(CodegenContext& ctx)
	{
		auto& module  = ctx.Module;
		auto& context = ctx.Context;
		auto& builder = ctx.Builder;
		
		std::shared_ptr<SymbolTable> prev = GetSymbolTable()->GetPrevious();
		CLEAR_VERIFY(prev, "prev was null");

		FunctionData& functionData = prev->CreateFunction(m_Name, m_Parameters, m_ReturnType, ctx.Module, ctx.Context);
		
		s_InsertPoints.push(builder.saveIP());

		llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", functionData.Function);
		llvm::BasicBlock* body  = llvm::BasicBlock::Create(context, "body");
		
		builder.SetInsertPoint(entry);

		llvm::BasicBlock* returnBlock  = llvm::BasicBlock::Create(context, "return");
		llvm::AllocaInst* returnAlloca = m_ReturnType ? builder.CreateAlloca(m_ReturnType->Get(), nullptr, "return_value") : nullptr;
		
		ctx.ReturnType   = m_ReturnType ? m_ReturnType : ctx.Registry.GetType("void");
		ctx.ReturnBlock  = returnBlock;
		ctx.ReturnAlloca = returnAlloca;

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

	CodegenResult ASTFunctionCall::Codegen(CodegenContext& ctx)
	{
		auto& builder  = ctx.Builder;
		auto& module   = ctx.Module;
		auto& context  = ctx.Context;
		auto& children = GetChildren();

		std::shared_ptr<SymbolTable> symbolTable = GetSymbolTable();
		FunctionData& data = symbolTable->GetFunction(m_Name);

		CLEAR_VERIFY(data.Function, m_Name, " definition doesn't exist");

		uint32_t k = 0;

		std::vector<llvm::Value*> args;

		ValueRestoreGuard<bool> guard(ctx.WantAddress, false);

		for (auto& child : children)
		{
			CodegenResult gen = child->Codegen(ctx);

			if(data.Parameters[k].IsVariadic)
			{
				args.push_back(gen.CodegenValue);
				continue;
			}

			if (gen.CodegenType->Get() != data.Parameters[k].Type->Get())
			{
				gen.CodegenValue = TypeCasting::Cast(gen.CodegenValue, 
												     gen.CodegenType, 
													 data.Parameters[k].Type, ctx.Builder);
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

	CodegenResult ASTFunctionDecleration::Codegen(CodegenContext& ctx)
	{
		auto& module = ctx.Module;

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
			m_ReturnType = ctx.Registry.GetType("void");

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
				   child->GetType() == ASTNodeType::MemberAccess;
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
				CLEAR_VERIFY(unaryExpression->GetChildren().size() == 0, "");

				unaryExpression->Push(stack.top());
				stack.pop();

				stack.push(unaryExpression);
				continue;
			}

			std::shared_ptr<ASTBinaryExpression> binExp = std::dynamic_pointer_cast<ASTBinaryExpression>(child);

			binExp->Push(stack.top());
			stack.pop();

			binExp->Push(stack.top());
			stack.pop();

			stack.push(binExp);
		}

		if(stack.size() > 0)
		{
			return stack.top()->Codegen(ctx);
		}


		return {};
	}


	CodegenResult ASTArrayInitializer::Codegen(CodegenContext& ctx)
	{
		auto& builder = ctx.Builder;
		auto& context = ctx.Context;
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() > 0, "invalid array initializer");

		CodegenResult storage;

		{
			ValueRestoreGuard<bool> guard(ctx.WantAddress, true);
			storage = children[0]->Codegen(ctx);
		}

		std::shared_ptr<PointerType> storageType = std::dynamic_pointer_cast<PointerType>(storage.CodegenType);
		CLEAR_VERIFY(storageType, "invalid storage type");

		CLEAR_VERIFY(children.size() - 1 == m_Indices.size(), "sizes don't match!");

		llvm::Type* intTy = llvm::Type::getInt64Ty(context);

		std::shared_ptr<ArrayType> baseType = std::dynamic_pointer_cast<ArrayType>(storageType->GetBaseType());
		CLEAR_VERIFY(baseType, "base type is not an array type");

		llvm::Constant* zeroArray = llvm::ConstantAggregateZero::get(baseType->Get());
		builder.CreateStore(zeroArray, storage.CodegenValue);

		ValueRestoreGuard<bool> guard(ctx.WantAddress, false);

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
			
			CodegenResult valueToStore = children[i + 1]->Codegen(ctx);

			if(valueToStore.CodegenType != innerType)
			{
				valueToStore.CodegenValue = TypeCasting::Cast(
							valueToStore.CodegenValue,
							valueToStore.CodegenType, 
							innerType,
							ctx.Builder);
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


    ASTImport::ASTImport(const std::filesystem::path& filepath)
		: m_Filepath(filepath)
    {
    }

	CodegenResult ASTImport::Codegen(CodegenContext& ctx)
	{
		std::filesystem::path completeFilePath = ctx.CurrentDirectory / m_Filepath;
		CLEAR_VERIFY(ctx.LookupTable.contains(completeFilePath), "cannot find ", completeFilePath);

		auto& lookupInfo = ctx.LookupTable.at(completeFilePath);

		auto& rootChildren = lookupInfo.Node->GetChildren();
		auto rootSymbolTable = lookupInfo.Node->GetSymbolTable();

		for(const auto& child : rootChildren)
		{
			std::shared_ptr<ASTFunctionDefinition> fun = std::dynamic_pointer_cast<ASTFunctionDefinition>(child);
			
			if(!fun) 
				continue;
			
			FunctionData& importedData = rootSymbolTable->GetFunction(fun->GetName());

			llvm::FunctionCallee callee = ctx.Module.getOrInsertFunction(fun->GetName(), importedData.FunctionType);

			FunctionData registeredData;
			registeredData.FunctionType = importedData.FunctionType;
			registeredData.Function = llvm::cast<llvm::Function>(callee.getCallee());
			registeredData.Parameters = importedData.Parameters;
			registeredData.ReturnType = importedData.ReturnType;

			GetSymbolTable()->RegisterFunction(fun->GetName(), registeredData);

		}

		return {};
	}

    ASTMemberAccess::ASTMemberAccess()
    {
    }

    CodegenResult ASTMemberAccess::Codegen(CodegenContext& ctx)
    {
		auto& builder = ctx.Builder;
		auto& context = ctx.Context;
		auto& children = GetChildren();

		auto& typeReg = ctx.Registry;

		CLEAR_VERIFY(children.size() > 1, "invalid member access");

		CodegenResult parent;
		
		{
			ValueRestoreGuard<bool> guard(ctx.WantAddress, true);
			parent = children[0]->Codegen(ctx); 
		}

		while(auto ty = std::dynamic_pointer_cast<PointerType>(parent.CodegenType))
		{
			if(ty->GetBaseType()->IsCompound())
			{
				parent.CodegenType = ty;
				break;
			}

			parent.CodegenValue = ctx.Builder.CreateLoad(ty->GetBaseType()->Get(), parent.CodegenValue);
			parent.CodegenType = ty->GetBaseType();
		}

		std::shared_ptr<StructType> parentType;

		if(auto t = std::dynamic_pointer_cast<PointerType>(parent.CodegenType))
		{
			parentType = std::dynamic_pointer_cast<StructType>(t->GetBaseType());
		}

		CLEAR_VERIFY(parentType, "invalid type");
		
		llvm::Value* getElementPtr = parent.CodegenValue; 
		std::shared_ptr<Type> prev;

		for(size_t i = 1; i < children.size(); i++)
		{
			std::shared_ptr<ASTMember> member = std::dynamic_pointer_cast<ASTMember>(children[i]);
			CLEAR_VERIFY(member && parentType, "invalid child");

			size_t index = parentType->GetMemberIndex(member->GetName());
			getElementPtr = builder.CreateStructGEP(parentType->Get(), getElementPtr, index, "struct_get_element_ptr");

			prev = parentType->GetMemberType(member->GetName());
			parentType = std::dynamic_pointer_cast<StructType>(prev);
		}

		if(ctx.WantAddress)
		{
			return { getElementPtr, typeReg.GetPointerTo(prev) };
		}

		return { builder.CreateLoad(prev->Get(), getElementPtr), prev };
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
				ValueRestoreGuard<bool> guard(ctx.WantAddress, false);
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
			ValueRestoreGuard<bool> guard(ctx.WantAddress, true);
			CodegenResult result = children[0]->Codegen(ctx);

			return result;
		}

		if(m_Type == UnaryExpressionType::Negation)
		{			
			CodegenResult result = children[0]->Codegen(ctx);

			llvm::Value* negated;

			if(result.CodegenType->IsFloatingPoint())
				negated = ctx.Builder.CreateFNeg(result.CodegenValue);
			else 
				negated = ctx.Builder.CreateNeg(result.CodegenValue);

			return { negated, result.CodegenType };
		}

		if(m_Type == UnaryExpressionType::BitwiseNot)
		{
			CodegenResult result = children[0]->Codegen(ctx);
			return { ctx.Builder.CreateNot(result.CodegenValue), result.CodegenType };
		}	
		
		CodegenResult one;
		one.CodegenType  = ctx.Registry.GetType("int64");
		one.CodegenValue = ctx.Builder.getInt64(1);

		ValueRestoreGuard<bool> guard(ctx.WantAddress, true);

		CodegenResult result = children[0]->Codegen(ctx);
		CLEAR_VERIFY(result.CodegenType->IsPointer(), "not valid type for increment");
		std::shared_ptr<PointerType> ty = std::dynamic_pointer_cast<PointerType>(result.CodegenType);

		CodegenResult valueToStore;
		CodegenResult returnValue;

		if(m_Type == UnaryExpressionType::PostIncrement)
		{
			returnValue.CodegenValue = ctx.Builder.CreateLoad(ty->GetBaseType()->Get(), result.CodegenValue);
			returnValue.CodegenType = ty->GetBaseType();

			valueToStore = ASTBinaryExpression::HandleMathExpression(returnValue, one, BinaryExpressionType::Add, ctx);
		}
		else if (m_Type == UnaryExpressionType::PostDecrement)
		{
			returnValue.CodegenValue = ctx.Builder.CreateLoad(ty->GetBaseType()->Get(), result.CodegenValue);
			returnValue.CodegenType = ty->GetBaseType();

			valueToStore = ASTBinaryExpression::HandleMathExpression(returnValue, one, BinaryExpressionType::Sub, ctx);
		}
		else if (m_Type == UnaryExpressionType::PreIncrement)
		{
			returnValue.CodegenValue = ctx.Builder.CreateLoad(ty->GetBaseType()->Get(), result.CodegenValue);
			returnValue.CodegenType = ty->GetBaseType();

			valueToStore = ASTBinaryExpression::HandleMathExpression(returnValue, one, BinaryExpressionType::Add, ctx);
			returnValue.CodegenValue = valueToStore.CodegenValue;
		}
		else if (m_Type == UnaryExpressionType::PreDecrement)
		{
			returnValue.CodegenValue = ctx.Builder.CreateLoad(ty->GetBaseType()->Get(), result.CodegenValue);
			returnValue.CodegenType = ty->GetBaseType();

			valueToStore = ASTBinaryExpression::HandleMathExpression(returnValue, one, BinaryExpressionType::Sub, ctx);
			returnValue.CodegenValue = valueToStore.CodegenValue;
		}
		else 
		{
			CLEAR_UNREACHABLE("unimplemented");
		}

		ctx.Builder.CreateStore(valueToStore.CodegenValue, result.CodegenValue);

		return returnValue;
	}
}