#include "ASTNode.h"

#include "Core/Log.h"
#include "Core/Operator.h"
#include "Symbols/FunctionCache.h"
#include "Symbols/Symbol.h"
#include "Symbols/Type.h"
#include "Symbols/TypeCasting.h"

#include "Symbols/TypeRegistry.h"
#include "Intrinsics.h"
#include "Symbols/SymbolOperations.h"
#include "Symbols/Module.h"

#include <alloca.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Metadata.h>
#include <llvm/Support/Casting.h>

#include <memory>
#include <stack>
#include <utility>
#include <stack>
#include <print>


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

	static void PushScopeMarker(CodegenContext& ctx)
	{
		ctx.DeferredCalls.push_back(nullptr);
	}

    ASTNodeBase::ASTNodeBase()
    {
    }

    Symbol ASTNodeBase::Codegen(CodegenContext& ctx)
    {
		PushScopeMarker(ctx);

        Symbol value;

		for (auto& child : GetChildren())
		{
			bool isContinue = child->GetType() == ASTNodeType::LoopControlFlow;
			value = child->Codegen(ctx);

			if(isContinue) 
				break;
		}

		GetSymbolTable()->FlushScope(ctx);

		return value;
    }

    void ASTNodeBase::Push(const std::shared_ptr<ASTNodeBase>& child)
    {
        m_Children.push_back(child);
    }

    void ASTNodeBase::Remove(const std::shared_ptr<ASTNodeBase>& child)
    {
        auto pos = std::find(m_Children.begin(), m_Children.end(), child);
    	if(pos != m_Children.end())
    	    m_Children.erase(pos);
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

	void ASTNodeBase::SetSymbolTable(std::shared_ptr<SymbolTable> tbl)
    {
		m_SymbolTable = tbl;
    }

    void ASTNodeBase::PropagateSymbolTable(const std::shared_ptr<SymbolTable>& registry)
    {
		CLEAR_VERIFY(registry, "dont take in null");
		
		if(m_SymbolTable == registry) // already done
		{
			return;
		}

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
			if(child)
				child->PropagateSymbolTable(m_SymbolTable);
		}

    }

    ASTNodeLiteral::ASTNodeLiteral(const Token& data)
		: m_Token(data)
	{
	}

	Symbol ASTNodeLiteral::Codegen(CodegenContext& ctx)
	{
		if(m_Value.has_value())
			return Symbol::CreateValue(m_Value.value().Get(), m_Value.value().GetType());

		m_Value = Value(m_Token, ctx.ClearModule->GetTypeFromToken(m_Token), ctx.Context, ctx.Module);
		return Symbol::CreateValue(m_Value.value().Get(), m_Value.value().GetType());
	}

    ASTBinaryExpression::ASTBinaryExpression(OperatorType type)
		: m_Expression(type)
	{
	}

	Symbol ASTBinaryExpression::Codegen(CodegenContext& ctx)
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

		if(m_Expression == OperatorType::Index)
			return HandleArrayIndex(leftChild, rightChild, ctx);

    	if (IsBitwiseExpression())
			return HandleBitwiseExpression(leftChild, rightChild, ctx);

		if (IsLogicalOperator())
			return HandleLogicalExpression(leftChild, rightChild, ctx);

		if(m_Expression == OperatorType::Dot)
			return HandleMemberAccess(leftChild, rightChild, ctx);


		CLEAR_UNREACHABLE("unimplmented");

		return {};
    }


	void ASTBinaryExpression::Print()
	{
		if(m_Expression == OperatorType::Add)
		{
			std::print("+ ");
		}
		else if(m_Expression == OperatorType::Sub)
		{
			std::print("- ");
		}
		else if (m_Expression == OperatorType::LessThan)
		{
			std::print("< ");
		}
		else if (m_Expression == OperatorType::GreaterThan)
		{
			std::print("> ");
		}
	}

    bool ASTBinaryExpression::IsMathExpression() const
    {
        switch (m_Expression)
		{
			case OperatorType::Add:
            case OperatorType::Sub:
			case OperatorType::Mul:
			case OperatorType::Div:
			case OperatorType::Mod:
			//case OperatorType::Pow:
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
			case OperatorType::LessThan:
			case OperatorType::LessThanEqual:
			case OperatorType::GreaterThan:
            case OperatorType::GreaterThanEqual:
			case OperatorType::IsEqual:
			case OperatorType::NotEqual:
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
			case OperatorType::LeftShift:
			case OperatorType::RightShift:
			case OperatorType::BitwiseNot:
			case OperatorType::BitwiseAnd:
			case OperatorType::BitwiseOr:
			case OperatorType::BitwiseXor:
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
			case OperatorType::And:
			case OperatorType::Or:
				return true;
			default:
				break;
		}

        return false;
    }

    Symbol ASTBinaryExpression::HandleMathExpression(Symbol& lhs, Symbol& rhs,  OperatorType type, CodegenContext& ctx, std::shared_ptr<SymbolTable> tbl)
    {
		switch (type)
		{
			case OperatorType::Add: return SymbolOps::Add(lhs, rhs, ctx.Builder);
			case OperatorType::Sub: return SymbolOps::Sub(lhs, rhs, ctx.Builder);
			case OperatorType::Mul: return SymbolOps::Mul(lhs, rhs, ctx.Builder);
			case OperatorType::Div: return SymbolOps::Div(lhs, rhs, ctx.Builder);
			case OperatorType::Mod: return SymbolOps::Mod(lhs, rhs, ctx.Builder);
			default:
				break;
		}

		return Symbol();
    }

    Symbol ASTBinaryExpression::HandleMathExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx)
    {
		// ctx.WantAddress is set by parent to this node
		Symbol lhs = left->Codegen(ctx);

		// right hand side we always want a value

		Symbol rhs;
		{
			ValueRestoreGuard guard(ctx.WantAddress, false);
			rhs = right->Codegen(ctx);
		}

		auto [_, lhsType] = lhs.GetValue();

		if(lhsType->IsPointer()) 
			return HandlePointerArithmetic(lhs, rhs, m_Expression, ctx, GetSymbolTable()); //internally will verify correct expression type

        return HandleMathExpression(lhs, rhs, m_Expression, ctx, GetSymbolTable());
    }

    Symbol ASTBinaryExpression::HandleCmpExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext &ctx)
    {
		ValueRestoreGuard guard(ctx.WantAddress, false);

		Symbol lhs = left->Codegen(ctx);
		Symbol rhs = right->Codegen(ctx);

        return HandleCmpExpression(lhs, rhs, ctx);
    }

    Symbol ASTBinaryExpression::HandleCmpExpression(Symbol& lhs, Symbol& rhs, CodegenContext& ctx)
    {
		auto booleanType = ctx.ClearModule->Lookup("bool").GetType();

    	switch (m_Expression)
		{
			case OperatorType::LessThan: 	      return SymbolOps::Lt(lhs, rhs, booleanType, ctx.Builder);
			case OperatorType::LessThanEqual:     return SymbolOps::Lte(lhs, rhs, booleanType, ctx.Builder);
			case OperatorType::GreaterThan:       return SymbolOps::Gt(lhs, rhs, booleanType, ctx.Builder);
			case OperatorType::GreaterThanEqual:  return SymbolOps::Gte(lhs, rhs, booleanType, ctx.Builder);
			case OperatorType::IsEqual:			  return SymbolOps::Eq(lhs, rhs, booleanType, ctx.Builder);
			case OperatorType::NotEqual:		  return SymbolOps::Neq(lhs, rhs, booleanType, ctx.Builder);
			default:
				break;
		}

		return Symbol();
    }

    Symbol ASTBinaryExpression::HandleBitwiseExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx)
    {

    	auto& builder = ctx.Builder;
    	// ctx.WantAddress is set by parent to this node
    	Symbol lhs = left->Codegen(ctx);

    	// right hand side we always want a value

    	Symbol rhs;
	    {
    		ValueRestoreGuard guard(ctx.WantAddress, false);
    		rhs = right->Codegen(ctx);
	    }

    	switch (m_Expression) 
		{
    		case OperatorType::BitwiseAnd: return SymbolOps::BitAnd(lhs, rhs, ctx.Builder);
    		case OperatorType::BitwiseOr:  return SymbolOps::BitOr(lhs, rhs, ctx.Builder);
    		case OperatorType::BitwiseXor: return SymbolOps::BitXor(lhs, rhs, ctx.Builder);
    		case OperatorType::LeftShift:  return SymbolOps::Shl(lhs, rhs, ctx.Builder);
    		case OperatorType::RightShift: return SymbolOps::Shr(lhs, rhs, ctx.Builder);
    		case OperatorType::BitwiseNot: return SymbolOps::Not(lhs, ctx.Builder);
    		default: return {};
    	}

        return {};
    }

    Symbol ASTBinaryExpression::HandleLogicalExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext &ctx)
    {
		if(ctx.Builder.GetInsertBlock()->getTerminator()) 
			return {};
			
		llvm::Function* function = ctx.Builder.GetInsertBlock()->getParent();

		ValueRestoreGuard guard(ctx.WantAddress, false);

		Symbol lhs = left->Codegen(ctx);

		auto [lhsValue, lhsType] = lhs.GetValue();

		lhsValue = TypeCasting::Cast(lhsValue, lhsType, ctx.TypeReg->GetType("bool"), ctx.Builder);
		lhsType  = ctx.TypeReg->GetType("bool");


		llvm::BasicBlock* checkSecond  = llvm::BasicBlock::Create(ctx.Context, "check_second");
		llvm::BasicBlock* trueResult   = llvm::BasicBlock::Create(ctx.Context, "true_value");
		llvm::BasicBlock* falseResult  = llvm::BasicBlock::Create(ctx.Context, "false_value");
		llvm::BasicBlock* merge  	   = llvm::BasicBlock::Create(ctx.Context, "merge");

		if(m_Expression == OperatorType::And)
			ctx.Builder.CreateCondBr(lhsValue, checkSecond, falseResult);
		else 
			ctx.Builder.CreateCondBr(lhsValue, trueResult, checkSecond);

		function->insert(function->end(), checkSecond);
		ctx.Builder.SetInsertPoint(checkSecond);
		
		Symbol rhs = right->Codegen(ctx);
		
		auto [rhsValue, rhsType] = rhs.GetValue();

		rhsValue = TypeCasting::Cast(rhsValue, rhsType, ctx.TypeReg->GetType("bool"), ctx.Builder);
		rhsType  = ctx.TypeReg->GetType("bool");
		
		ctx.Builder.CreateCondBr(rhsValue, trueResult, falseResult);
		
		function->insert(function->end(), trueResult);
		ctx.Builder.SetInsertPoint(trueResult);

		ctx.Builder.CreateBr(merge);

		function->insert(function->end(), falseResult);
		ctx.Builder.SetInsertPoint(falseResult);

		ctx.Builder.CreateBr(merge);

		function->insert(function->end(), merge);
		ctx.Builder.SetInsertPoint(merge);

		auto phiNode = ctx.Builder.CreatePHI(rhsType->Get(), 2);
		phiNode->addIncoming(ctx.Builder.getInt1(true), trueResult);
		phiNode->addIncoming(ctx.Builder.getInt1(false), falseResult);

        return Symbol::CreateValue(phiNode, rhsType);
    }

    Symbol ASTBinaryExpression::HandlePointerArithmetic(Symbol& lhs, Symbol& rhs, OperatorType type, CodegenContext& ctx, std::shared_ptr<SymbolTable> tbl)
    {
		auto [lhsValue, lhsType] = lhs.GetValue();
		auto [rhsValue, rhsType] = rhs.GetValue();

		CLEAR_VERIFY(lhsType->IsPointer(), "left hand side is not a pointer");
		CLEAR_VERIFY(rhsType->IsIntegral(), "invalid pointer arithmetic");

		if(rhsType->GetSizeInBytes(ctx.Module) != 64) 
		{
			rhsValue = TypeCasting::Cast(rhsValue, 
										rhsType, 
										ctx.TypeReg->GetType("int64"), 
										ctx.Builder);

			rhsType = ctx.TypeReg->GetType("int64");
		}
		

		auto symPtrType = Symbol::CreateType(lhsType->As<PointerType>());

		if(type == OperatorType::Add)
		{
			return SymbolOps::GEP(lhs, symPtrType, { rhsValue }, ctx.Builder); 
		}

		if(type == OperatorType::Sub)
		{
			rhsValue = ctx.Builder.CreateNeg(rhsValue);
			return SymbolOps::GEP(lhs,symPtrType, { rhsValue }, ctx.Builder); 
		}

		CLEAR_UNREACHABLE("invalid binary expression");
        return {};
    }

    Symbol ASTBinaryExpression::HandleArrayIndex(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx)
    {
		Symbol lhs;

		{
			ValueRestoreGuard guard(ctx.WantAddress, true);
			lhs = left->Codegen(ctx);
		}

		Symbol rhs;

		{
			ValueRestoreGuard guard(ctx.WantAddress, false);
			rhs = right->Codegen(ctx);
		}

		auto [lhsValue, lhsType] = lhs.GetValue();
		auto [rhsValue, rhsType] = rhs.GetValue();

		if(lhsType->IsVariadic())
		{
			auto* constIdx = llvm::dyn_cast<llvm::ConstantInt>(rhsValue);
			CLEAR_VERIFY(constIdx, "only allow constant expression indexing, runtime indexing is not supported yet");

			uint64_t index = constIdx->getZExtValue();
			
			CLEAR_VERIFY(index < GetSymbolTable()->GetVariadicArguments().size(), "index out of range!");
			Allocation alloc = GetSymbolTable()->GetVariadicArguments()[index];

			llvm::Value* gep = alloc.Alloca;
			std::shared_ptr<Type> baseType = alloc.Type;

			if(ctx.WantAddress)
				return Symbol::CreateValue(gep, ctx.TypeReg->GetPointerTo(baseType));

        	return Symbol::CreateValue(ctx.Builder.CreateLoad(baseType->Get(), gep), baseType);
				
		}

		// lhs is going to be a reference to the array

		std::shared_ptr<PointerType> type = std::dynamic_pointer_cast<PointerType>(lhsType);
		CLEAR_VERIFY(type, "invalid type");
		
		std::shared_ptr<ArrayType> arrType = type->GetBaseType()->As<ArrayType>();
		CLEAR_VERIFY(arrType, "invalid base type ", type->GetBaseType()->GetHash());

		llvm::Value* zero = llvm::ConstantInt::get(ctx.Builder.getInt64Ty(), 0);

		auto basePtrTy = Symbol::CreateType(ctx.TypeReg->GetPointerTo(arrType->GetBaseType()));

		Symbol gepResult = SymbolOps::GEP(lhs, basePtrTy, { zero, rhsValue }, ctx.Builder);

		if(ctx.WantAddress)
			return  gepResult;

        return SymbolOps::Load(gepResult, ctx.Builder);
    }

    Symbol ASTBinaryExpression::HandleMemberAccess(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx)
    {
		auto tbl = GetSymbolTable();

		Symbol lhs;
		{
			ValueRestoreGuard guard(ctx.WantAddress, true);
			lhs = left->Codegen(ctx);
		}

		switch (lhs.Kind) 
		{
			case SymbolKind::Type:
			{
				auto ty = lhs.GetType();

				if(ty->IsEnum())
					return HandleMemberEnum(lhs, right, ctx);

				if(right->GetType() == ASTNodeType::FunctionCall)
				{
					CLEAR_VERIFY(ty->IsClass(), "cannot call a function on a type that is not a class");

					auto funcCall = std::dynamic_pointer_cast<ASTFunctionCall>(right);
					CLEAR_VERIFY(funcCall, "invalid function call");

					std::string name = funcCall->GetName();

					funcCall->SetName(std::format("{}.{}", ty->As<ClassType>()->GetHash(), name));
					Symbol result = funcCall->Codegen(ctx);

					funcCall->SetName(name);

					return result;
				}

				CLEAR_UNREACHABLE("unimplemented");
			}
			case SymbolKind::ClassTemplate:
			{
				CLEAR_UNREACHABLE("unimplemented");
			}
			case SymbolKind::Module:
			{
				return HandleModuleAccess(lhs, right, ctx);
			}
			case SymbolKind::Value:
			{
				auto [lhsValue, lhsType] = lhs.GetValue();

				if(!lhsType->IsPointer()) 
				{
					Allocation temp = tbl->RequestTemporary(lhsType, ctx.Builder);
					ctx.Builder.CreateStore(lhsValue, temp.Alloca);

					lhsValue = temp.Alloca;
					lhsType =  ctx.TypeReg->GetPointerTo(temp.Type);
				}

				if(right->GetType() == ASTNodeType::Member)
				{
					return HandleMember(lhs, right, ctx);
				}
				else if (right->GetType() == ASTNodeType::FunctionCall)
				{
					auto funcCall = std::dynamic_pointer_cast<ASTFunctionCall>(right);
					CLEAR_VERIFY(funcCall, "invalid function call");

					auto ptrType = std::dynamic_pointer_cast<PointerType>(lhsType);
					CLEAR_VERIFY(ptrType, "invalid pointer type for function call");

					while(!ptrType->GetBaseType()->IsCompound()) // automatic derefencing if pointer
					{
						lhs = SymbolOps::Load(lhs, ctx.Builder);
						ptrType =  std::dynamic_pointer_cast<PointerType>(lhs.GetValue().second);
					}

					auto classType = dyn_cast<ClassType>(ptrType->GetBaseType());
					CLEAR_VERIFY(classType, "invalid class type for function call");

					std::string name = funcCall->GetName();

					funcCall->PushPrefixArgument(lhs.GetValue().first, ptrType);
					funcCall->SetName(std::format("{}.{}", classType->GetHash(), name));

					Symbol result = funcCall->Codegen(ctx);

					funcCall->SetName(name);

					return result;
				}
			}
			default:	
			{
				break;
			}
		}

		CLEAR_UNREACHABLE("unimplemented");
		return Symbol();	
	}

    Symbol ASTBinaryExpression::HandleMember(Symbol& lhs, std::shared_ptr<ASTNodeBase> right, CodegenContext &ctx)
    {
        CLEAR_VERIFY(right->GetType() == ASTNodeType::Member, "not a valid member");

		auto member = std::dynamic_pointer_cast<ASTMember>(right);

		while(auto ty = std::dynamic_pointer_cast<PointerType>(lhs.GetValue().second)) // automatic derefencing if pointer
		{
			if(ty->GetBaseType()->IsCompound())
			{
				break;
			}

			lhs = SymbolOps::Load(lhs, ctx.Builder);
		}

		auto [lhsValue, lhsType] = lhs.GetValue();
		
		auto ptrType = dyn_cast<PointerType>(lhsType);
		auto structTy = dyn_cast<StructType>(ptrType->GetBaseType());
		
		CLEAR_VERIFY(structTy, "not a valid type ", lhsType->GetHash());

		size_t index = structTy->GetMemberIndex(member->GetName());

		auto resultantType = Symbol::CreateType(ctx.TypeReg->GetPointerTo(structTy->GetMemberType(member->GetName())));
		
		Symbol gep = SymbolOps::GEPStruct(lhs, resultantType, index, ctx.Builder);

        if(ctx.WantAddress)
		{
			return gep;
		}

		return SymbolOps::Load(gep, ctx.Builder);
    }

    Symbol ASTBinaryExpression::HandleMemberEnum(Symbol& lhs, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx)	
    {
		 auto member = std::dynamic_pointer_cast<ASTMember>(right);

		auto enumTy = dyn_cast<EnumType>(lhs.GetType());
		CLEAR_VERIFY(enumTy, "not a valid enum");

		auto ty = ctx.TypeReg->GetType("int64");
		return Symbol::CreateValue(ctx.Builder.getInt64(enumTy->GetEnumValue(member->GetName())), ty);
    }

    Symbol ASTBinaryExpression::HandleModuleAccess(Symbol& lhs, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx)
    {
		auto mod = lhs.GetModule();

		if(right->GetType() == ASTNodeType::Member)
		{
			auto member = std::dynamic_pointer_cast<ASTMember>(right);
			Symbol symbol = mod->Lookup(member->GetName());

			if(symbol.Kind == SymbolKind::Type) //only one LLVMContext for now so this is fine
			{
				return symbol;
			}
			else if (symbol.Kind == SymbolKind::Value) //variable
			{
				llvm::GlobalVariable* existingGV = ctx.Module.getNamedGlobal(member->GetName());
				Symbol value;

				if(existingGV)
				{
					value = Symbol::CreateValue(existingGV, symbol.GetType());
				}
				else 
				{
					llvm::GlobalVariable* gv = llvm::cast<llvm::GlobalVariable>(symbol.GetLLVMValue());

					llvm::GlobalVariable* decl = new llvm::GlobalVariable(
				    	ctx.Module,
				    	gv->getValueType(),
				    	gv->isConstant(),
				    	llvm::GlobalValue::ExternalLinkage,
				    	nullptr,
				    	gv->getName()
					);

					value = Symbol::CreateValue(decl, symbol.GetType());
				}

				if(ctx.WantAddress)
					return value;

				return SymbolOps::Load(value, ctx.Builder);
			}
		}

		if(right->GetType() == ASTNodeType::FunctionCall)
		{
			ValueRestoreGuard moduleGuard(ctx.ClearModuleSecondary, mod);
			return right->Codegen(ctx);
		}

        return Symbol();
    }

    ASTVariableDeclaration::ASTVariableDeclaration(const std::string& name)
		: m_Name(name)
    {
    }

	Symbol ASTVariableDeclaration::Codegen(CodegenContext& ctx)
    {
		auto& children = GetChildren();

		std::shared_ptr<SymbolTable> tbl = GetSymbolTable();
		
		auto& typeResolver = children[0];

		Symbol resolvedType = typeResolver->Codegen(ctx);

        bool isGlobal = !(bool)ctx.Builder.GetInsertBlock();
		Allocation alloca;

		switch (resolvedType.Kind) 
		{
			case SymbolKind::Type:
			{
				m_Type = resolvedType.GetType();

				alloca = isGlobal ? tbl->CreateGlobal(m_Name, m_Type, ctx.Module) : tbl->CreateAlloca(m_Name, m_Type, ctx.Builder);

				if(children.size() == 2)
				{
					ValueRestoreGuard guard(ctx.WantAddress, false);

					Symbol value = children[1]->Codegen(ctx);
				
					Symbol allocaSymbol = Symbol::CreateValue(alloca.Alloca, ctx.TypeReg->GetPointerTo(alloca.Type));

					if(value.GetType() == allocaSymbol.GetType())
					{
						size_t size = value.GetType()->As<PointerType>()->GetBaseType()->GetSizeInBytes(ctx.Module);
						Symbol sizeSymbol = Symbol::GetUInt64(ctx.ClearModule, ctx.Builder, (uint64_t)size);
						SymbolOps::Memcpy(allocaSymbol, value, sizeSymbol, ctx.Builder);
					}
					else 
					{
						Symbol casted = SymbolOps::Cast(value, resolvedType, ctx.Builder);
						SymbolOps::Store(allocaSymbol, casted, ctx.Builder, ctx.Module,  /* isFirstTime = */ true);
					}

				}

				break;
			}
			case SymbolKind::InferType: 
			{
				bool isConst = resolvedType.GetInferType().IsConst;

				CLEAR_VERIFY(children.size() == 2, "cannot have an inferred type without value");

				ValueRestoreGuard guard(ctx.WantAddress, false);
				Symbol value = children[1]->Codegen(ctx);
				m_Type = value.GetType();

				bool shouldMemcpy = false;

				if(value.GetType()->IsPointer())
				{
					auto baseTy = value.GetType()->As<PointerType>()->GetBaseType();
					
					if(baseTy->IsArray() || baseTy->IsCompound())
					{
						m_Type = baseTy;
						shouldMemcpy = true;
					}
				}
				
				if(isConst)
				{
					m_Type = ctx.TypeReg->GetConstFrom(m_Type);
				}

				alloca = isGlobal ? tbl->CreateGlobal(m_Name, m_Type, ctx.Module) : tbl->CreateAlloca(m_Name, m_Type, ctx.Builder);
			
				Symbol allocaSymbol = Symbol::CreateValue(alloca.Alloca, ctx.TypeReg->GetPointerTo(alloca.Type));

				if(shouldMemcpy)
				{
					size_t size = value.GetType()->As<PointerType>()->GetBaseType()->GetSizeInBytes(ctx.Module);
					Symbol sizeSymbol = Symbol::GetUInt64(ctx.ClearModule, ctx.Builder, (uint64_t)size);
					SymbolOps::Memcpy(allocaSymbol, value, sizeSymbol, ctx.Builder);
				}
				else 
				{
					SymbolOps::Store(allocaSymbol, value, ctx.Builder, ctx.Module, /* isFirstTime = */ true);
				}

				break;
			}
			default: 
			{
				CLEAR_UNREACHABLE("unimplemented");
			}
		}

		return Symbol::CreateValue(alloca.Alloca, ctx.TypeReg->GetPointerTo(m_Type));
    }

	ASTVariable::ASTVariable(const std::string& name)
		: m_Name(name)
    {
    }

	Symbol ASTVariable::Codegen(CodegenContext& ctx)
    {
		auto& builder = ctx.Builder;

		Symbol result;

		std::shared_ptr<SymbolTable> tbl = GetSymbolTable();

		if(tbl->HasAlloca(m_Name))
		{
			Allocation alloca = tbl->GetAlloca(m_Name);

			if(alloca.Type->IsVariadic())  // special case
			{
				return Symbol::CreateValue(nullptr, alloca.Type); 
			}

			if(ctx.WantAddress)
			{
				return Symbol::CreateValue(alloca.Alloca, ctx.TypeReg->GetPointerTo(alloca.Type));
			}
			else 
			{
				return Symbol::CreateValue(builder.CreateLoad(alloca.Type->Get(), alloca.Alloca, m_Name), alloca.Type);
	
			}
		}
		else if (auto ty = ctx.TypeReg->GetType(m_Name))
		{
			return Symbol::CreateType(ty);
		}
		else if (auto module = ctx.ClearModule->Return(m_Name)) 
		{
			return Symbol::CreateModule(module);
		}
	
		return Symbol();
    }

	void ASTVariable::Print()
	{
		std::print("{} ", m_Name);
	}

	ASTAssignmentOperator::ASTAssignmentOperator(AssignmentOperatorType type)
		: m_Type(type)
    {
    }

	Symbol ASTAssignmentOperator::Codegen(CodegenContext& ctx)
    {
		auto& builder = ctx.Builder;
		auto& context = ctx.Context;
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() == 2, "incorrect dimensions");
	
		Symbol storage;
		
		{
			ValueRestoreGuard guard(ctx.WantAddress, true);
			storage = children[0]->Codegen(ctx);
		}

		Symbol data;
		{
			ValueRestoreGuard guard(ctx.WantAddress, false);
			data    = children[1]->Codegen(ctx);
		}

		ValueSymbol value = data.GetValueSymbol();

		if(value.ShouldMemcpy)
		{
			CLEAR_VERIFY(storage.GetType() == data.GetType(), "");
			Symbol size = Symbol::GetUInt64(ctx.ClearModule, ctx.Builder, data.GetType()->As<PointerType>()->GetBaseType()->GetSizeInBytes(ctx.Module));
			SymbolOps::Memcpy(storage, data, size, ctx.Builder);

			return Symbol();
		}

		HandleDifferentTypes(storage, data, ctx);

		if(m_Type == AssignmentOperatorType::Normal || m_Type == AssignmentOperatorType::Initialize)
		{
			SymbolOps::Store(storage, data, ctx.Builder, ctx.Module, true);
			return Symbol();
		}

		Symbol loadedValue = SymbolOps::Load(storage, ctx.Builder);		

		Symbol tmp;

		if(m_Type == AssignmentOperatorType::Add)
		{
			tmp = SymbolOps::Add(loadedValue, data, ctx.Builder); 
		}
		else if (m_Type == AssignmentOperatorType::Sub)
		{
			tmp = SymbolOps::Sub(loadedValue, data, ctx.Builder); 
		}
		else if (m_Type == AssignmentOperatorType::Mul)
		{
			tmp = SymbolOps::Mul(loadedValue, data, ctx.Builder); 
		}
		else if (m_Type == AssignmentOperatorType::Div)
		{
			tmp = SymbolOps::Div(loadedValue, data, ctx.Builder); 
		}
		else if (m_Type == AssignmentOperatorType::Mod)
		{
			tmp = SymbolOps::Mod(loadedValue, data, ctx.Builder); 
		}
		else 
		{
			CLEAR_UNREACHABLE("invalid assignment type");
		}

		SymbolOps::Store(storage, tmp, ctx.Builder, ctx.Module);
		return Symbol();
    }

    void ASTAssignmentOperator::HandleDifferentTypes(Symbol& storage, Symbol& data, CodegenContext& ctx)
    {
		auto& builder = ctx.Builder;
		
		auto [_, storageType] = storage.GetValue();
		auto [dataValue, dataType] = data.GetValue();

		auto ptrType = dyn_cast<PointerType>(storageType);
		auto baseTy = ptrType->GetBaseType();

		if(baseTy == dataType)
			return; 

		dataValue = TypeCasting::Cast(dataValue, dataType, baseTy, ctx.Builder);
		dataType = baseTy;

		data = Symbol::CreateValue(dataValue, dataType);
    }

	ASTFunctionDefinition::ASTFunctionDefinition(const std::string& name)
		: m_Name(name)
	{
	}

	Symbol ASTFunctionDefinition::Codegen(CodegenContext& ctx)
	{		
		auto& children = GetChildren();

		std::shared_ptr<SymbolTable> prev = GetSymbolTable()->GetPrevious();
		CLEAR_VERIFY(prev, "prev was null");

		bool isVariadic = false;

		RegisterGenerics(ctx);

		m_ResolvedParams.clear();

		// resolve parameters

		for(auto& param : m_PrefixParams)
		{
			m_ResolvedParams.push_back(param);
		}

		m_PrefixParams.clear();


		size_t i = 0;
		for(; i < children.size(); i++)
		{
			if(!children[i])
				break;

			if(children[i]->GetType() != ASTNodeType::TypeSpecifier)
				break;

			auto fnParam = std::dynamic_pointer_cast<ASTTypeSpecifier>(children[i]);

			Symbol result = fnParam->Codegen(ctx);
			isVariadic = fnParam->IsVariadic;

			m_ResolvedParams.push_back({ std::string(result.Metadata.value_or(String())), result.GetType(), fnParam->IsVariadic });
		}


		if(!children[i])
			i++;

		// resolve return type
		if(i < children.size() && children[i]->GetType() == ASTNodeType::TypeResolver)
		{
			if(children[i])
				m_ResolvedReturnType = children[i]->Codegen(ctx).GetType();
		
			i++;
		}


		RemoveGenerics(ctx);
		
		// resolve default arguments
		std::vector<std::shared_ptr<ASTNodeBase>> defaultArgs(m_ResolvedParams.size(), nullptr);

		for(; i < children.size(); i++)
		{
			if(children[i]->GetType() != ASTNodeType::DefaultArgument) 
				break;

			auto arg = std::dynamic_pointer_cast<ASTDefaultArgument>(children[i]);
			size_t argIndex = arg->GetIndex();

			CLEAR_VERIFY(argIndex < defaultArgs.size(), "invalid arg index!");
			defaultArgs[argIndex] = arg;
		}

		// erase no longer needed children (params, return type and default args)
		children.erase(children.begin(), children.begin() + i);

		prev->CreateTemplate(m_Name, m_ResolvedReturnType, m_ResolvedParams, isVariadic, defaultArgs, ShallowCopy(), ctx.ClearModule);
		
		// main needs to be instantiated immedietly as nothing calls it.
		if(m_Name == "main")
		{
			prev->InstantiateOrReturn(m_Name, m_ResolvedParams, m_ResolvedReturnType, ctx);
		}		
		
		return {};
	}

    void ASTFunctionDefinition::Instantiate(FunctionInstance& functionData, CodegenContext& ctx)
    {
		PushScopeMarker(ctx);

		auto& module  = ctx.Module;
		auto& context = ctx.Context;
		auto& builder = ctx.Builder;

		s_InsertPoints.push(builder.saveIP());

		llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", functionData.Function);
		llvm::BasicBlock* body  = llvm::BasicBlock::Create(context, "body");
		
		builder.SetInsertPoint(entry);

		llvm::BasicBlock* returnBlock  = llvm::BasicBlock::Create(context, "return");
		llvm::AllocaInst* returnAlloca = m_ResolvedReturnType ? builder.CreateAlloca(functionData.ReturnType->Get(), nullptr, "return_value") : nullptr;
		
		ValueRestoreGuard guard1(ctx.ReturnType,   functionData.ReturnType);
		ValueRestoreGuard guard2(ctx.ReturnBlock,  returnBlock);
		ValueRestoreGuard guard3(ctx.ReturnAlloca, returnAlloca);
		ValueRestoreGuard guard4(ctx.Thrown,       ctx.Thrown);

		uint32_t k = 0;

		bool hasVaArgs = false;

		std::shared_ptr<SymbolTable> tbl = GetSymbolTable();

		for (const auto& param : m_ResolvedParams)
		{
			if(param.IsVariadic)
			{
				hasVaArgs = true;
				break;
			}

			auto type = param.Type;

			if(type->IsTrait() || type->IsGeneric())
			{
				type = functionData.Parameters[k].Type;
			}

			llvm::AllocaInst* argAlloc = builder.CreateAlloca(type->Get(), nullptr, param.Name);
			builder.CreateStore(functionData.Function->getArg(k), argAlloc);
			
			Allocation alloca;
			alloca.Alloca = argAlloc;
			alloca.Type   = type;

			tbl->OwnAllocation(std::string(param.Name), alloca);
			k++;
		}

		auto& varArgs = tbl->GetVariadicArguments();
		varArgs.clear();
		
		if(hasVaArgs)
		{
			for(size_t i = k; i < functionData.Parameters.size(); i++)
			{
				llvm::AllocaInst* argAlloc = builder.CreateAlloca(functionData.Parameters[i].Type->Get(), nullptr, m_ResolvedParams[k].Name);
				builder.CreateStore(functionData.Function->getArg(i), argAlloc);

				Allocation alloca;
				alloca.Alloca = argAlloc;
				alloca.Type   = functionData.Parameters[i].Type;
				varArgs.push_back(alloca);
			}

			Allocation dummy;
			dummy.Alloca = nullptr;
			dummy.Type = std::make_shared<VariadicArgumentsHolder>(); 

			tbl->TrackAllocation(std::string(m_ResolvedParams[k].Name), dummy);
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
			tbl->FlushScope(ctx);
			builder.CreateRetVoid();
		}
		else
		{   
			llvm::Value* load = builder.CreateLoad(returnAlloca->getAllocatedType(), returnAlloca, "loaded_value");
			tbl->FlushScope(ctx);
			builder.CreateRet(load);
		}

		auto& ip = s_InsertPoints.top();
		builder.restoreIP(ip);
		s_InsertPoints.pop();
    }

	void ASTFunctionDefinition::RegisterGenerics(CodegenContext& ctx)
	{
		for(const auto& generic : m_GenericTypes)
		{
			ctx.TypeReg->CreateType<GenericType>(generic, generic);
		}
	}

	void ASTFunctionDefinition::RemoveGenerics(CodegenContext& ctx)
	{
		for(const auto& generic : m_GenericTypes)
		{
			ctx.TypeReg->RemoveType(generic);
		}
	}


	std::shared_ptr<ASTFunctionDefinition> ASTFunctionDefinition::ShallowCopy()
	{
		std::shared_ptr<ASTFunctionDefinition> functionDefinition = std::make_shared<ASTFunctionDefinition>(m_Name);
		functionDefinition->SetSymbolTable(GetSymbolTable());

		functionDefinition->m_GenericTypes = m_GenericTypes;
		functionDefinition->m_PrefixParams = m_PrefixParams;
		functionDefinition->m_ResolvedParams = m_ResolvedParams;
		functionDefinition->m_ResolvedReturnType = m_ResolvedReturnType;
		functionDefinition->m_GenericTypes = m_GenericTypes;

		for(const auto& child : GetChildren())
		{
			functionDefinition->Push(child);
		}

		return functionDefinition;
	}

    ASTFunctionCall::ASTFunctionCall(const std::string& name)
		: m_Name(name)
    {
    }

	Symbol ASTFunctionCall::Codegen(CodegenContext& ctx)
	{
		auto& builder  = ctx.Builder;
		auto& module   = ctx.Module;
		auto& context  = ctx.Context;
		auto& children = GetChildren();

		std::shared_ptr<SymbolTable> symbolTable = GetSymbolTable();

		Allocation temporary;

		if(auto ty = ctx.TypeReg->GetType(m_Name)) // if the name is a type, we construct a temporary to that type and call constructor
		{
			CLEAR_VERIFY(ctx.WantAddress == false, "cannot get an address to a temporary!");

			temporary = symbolTable->RequestTemporary(ty, builder);

			if(children.size() == 0) // empty constructor so construct all children
			{
				llvm::Constant* zero = llvm::ConstantAggregateZero::get(ty->Get()); // zero initalize all elements.
		    	ctx.Builder.CreateStore(zero, temporary.Alloca);

				ASTDefaultInitializer::RecursiveCallConstructors(temporary.Alloca, ty, ctx, GetSymbolTable()); 

				return Symbol::CreateValue(ctx.Builder.CreateLoad(temporary.Type->Get(), temporary.Alloca), temporary.Type); // return value of temporary
			}

			m_Name = std::format("{}.{}", m_Name, "__construct__");
			m_PrefixArguments.push_back({ temporary.Alloca, ctx.TypeReg->GetPointerTo(ty) });
		}

		uint32_t k = 0;

		std::vector<llvm::Value*> args;
		std::vector<Parameter> params; // we only care about types here

		BuildArgs(ctx, args, params);

		if(Intrinsics::IsIntrinsic(m_Name)) // if its an intrinsic then dispatch correct function and return correct type
		{
			llvm::Value* value = nullptr;

			if(args.size() > 0)
				value = Intrinsics::ApplyIntrinsic(m_Name, args[0], params[0].Type, ctx);
			else
				value = Intrinsics::ApplyIntrinsic(m_Name, nullptr, nullptr, ctx);

			if(!value) return {};

			if(m_Name == "sizeof") return Symbol::CreateValue(value, ctx.ClearModule->Lookup("int64").GetType());
			if(m_Name == "len")    return Symbol::CreateValue(value, ctx.ClearModule->Lookup("int64").GetType());

			return Symbol::CreateValue(value, ctx.ClearModule->Lookup("int64").GetType());
		}


		// ctx.ClearModuleSecondary->GetRoot()->GetSymbolTable() hack for now until i can think of a better solution
		FunctionTemplate& data = ctx.ClearModuleSecondary->GetRoot()->GetSymbolTable()->GetTemplate(m_Name, params);
		CLEAR_VERIFY(data.Valid, "failed to find template named ", m_Name);

		// add any default arguments
		for(size_t i = args.size(); i < data.DefaultArguments.size(); i++)
		{
			if(!data.DefaultArguments[i]) 
			{
				break;
			}

			Symbol argument = data.DefaultArguments[i]->Codegen(ctx);

			auto [argValue, argType] = argument.GetValue();

			args.push_back(argValue);
			params.push_back({ .Type=argType });
		}
		
		// cast
		CastArgs(ctx, args, params, data);

		m_PrefixArguments.clear();

		// if we already have a decleration then call (external/extern function)

		if(symbolTable->HasDecleration(m_Name))
		{
			FunctionInstance& instance = symbolTable->GetDecleration(m_Name);

			if(temporary.Alloca)
			{
				ctx.Builder.CreateCall(instance.Function, args);
				return Symbol::CreateValue(ctx.Builder.CreateLoad(temporary.Type->Get(), temporary.Alloca), temporary.Type);
			}

			return Symbol::CreateValue(ctx.Builder.CreateCall(instance.Function, args), instance.ReturnType);
		}

		// instantiate and call
		FunctionInstance& instance = ctx.ClearModuleSecondary->GetRoot()->GetSymbolTable()->InstantiateOrReturn(m_Name, params, data.ReturnType, ctx);

		if(temporary.Alloca)
		{
			ctx.Builder.CreateCall(instance.Function, args);
			return Symbol::CreateValue(ctx.Builder.CreateLoad(temporary.Type->Get(), temporary.Alloca), temporary.Type);
		}

		return Symbol::CreateValue(ctx.Builder.CreateCall(instance.Function, args), instance.ReturnType);
	}

    void ASTFunctionCall::BuildArgs(CodegenContext& ctx, std::vector<llvm::Value*>& args, std::vector<Parameter>& params)
    {
		ValueRestoreGuard guard(ctx.WantAddress, false);
		
		for(auto& [value, type] : m_PrefixArguments)
		{
			args.push_back(value);
			params.push_back({ "", type });
		}

		for (auto& child : GetChildren())	
		{
			Symbol gen = child->Codegen(ctx);

			for (auto jj : gen.GetValueTuple().Values)
			{
				args.push_back(jj);
			}

			for (auto jt : gen.GetValueTuple().Types)
			{
				params.push_back({"", jt });
			}


		}
    }

    void ASTFunctionCall::CastArgs(CodegenContext& ctx, std::vector<llvm::Value*>& args, std::vector<Parameter>& params, FunctionTemplate& fnTemplate)
    {
		for(size_t i = 0; i < args.size(); i++)
		{
			auto& param1 = params[i];
			auto& param2 = i < fnTemplate.Parameters.size() ? fnTemplate.Parameters[i] : fnTemplate.Parameters.back();

			if(!param2.Type || param2.Type->IsGeneric()) 
				continue;

			if((param1.Type->Get() != param2.Type->Get()) || param2.Type->IsTrait()) 
			{
				if(param2.Type->IsTrait()) 
				{
					params[i].Type = param1.Type;
					continue;
				}

				args[i] = TypeCasting::Cast(args[i], param1.Type, param2.Type, ctx.Builder);
				params[i].Type = param2.Type;
			}

			if(param1.Type->IsPointer() && param2.Type->IsPointer())
			{
				// if both are pointers, we need to make sure that they point to the same type
				auto ptr1 = std::dynamic_pointer_cast<PointerType>(param1.Type);
				auto ptr2 = std::dynamic_pointer_cast<PointerType>(param2.Type);

				if(!ptr1->GetBaseType())
				{
					params[i].Type = param2.Type;
					continue;
				}

				if(!ptr2->GetBaseType()) // opaque pointer (which is not allowed)
				{
					CLEAR_UNREACHABLE("opaque pointer in function call, this should not happen");
					continue;
				}

				if(ptr1 && ptr2 && ptr1->GetBaseType()->Get() != ptr2->GetBaseType()->Get())
				{
					// no need to cast pointers just switch the underlying type
					params[i].Type = param2.Type;
				}
			}
		}
    }

    ASTFunctionDecleration::ASTFunctionDecleration(const std::string& name)
		: m_Name(name)
    {
    }

	Symbol ASTFunctionDecleration::Codegen(CodegenContext& ctx)
	{
		auto& children = GetChildren();
		auto& module = ctx.Module;

		std::vector<llvm::Type*> types;

		size_t i = 0;
		for(; i < children.size(); i++)
		{
			auto& child = children[i];

			if(child->GetType() != ASTNodeType::TypeSpecifier)
				break; 
			
			auto fnParam = std::dynamic_pointer_cast<ASTTypeSpecifier>(child);

			Symbol param = fnParam->Codegen(ctx);

			m_Parameters.push_back({ .Name = std::string(param.Metadata.value_or(String())), .Type = param.GetType(), .IsVariadic = fnParam->IsVariadic });
		} 

		bool isVariadic = false;

		for (auto& param : m_Parameters)
		{
			if (!param.Type)
			{
				isVariadic = true;
				break;
			}

			types.push_back(param.Type->Get());
		}

		m_ReturnType = ctx.TypeReg->GetType("void");

		if (i < children.size())
		{
			CLEAR_VERIFY(children[i]->GetType() == ASTNodeType::TypeResolver, "not a valid return type node");
			m_ReturnType = children[i]->Codegen(ctx).GetType();
		}

		if(InsertDecleration)
		{
			llvm::FunctionType* functionType = llvm::FunctionType::get(m_ReturnType->Get(), types, isVariadic);
			llvm::FunctionCallee callee = module.getOrInsertFunction(m_Name, functionType);

			FunctionInstance data;
			data.FunctionType = functionType;
			data.Function = llvm::cast<llvm::Function>(callee.getCallee());
			data.Parameters = m_Parameters;
			data.ReturnType = m_ReturnType;
			data.MangledName = m_Name;
			
			GetSymbolTable()->RegisterInstance(data);

			FunctionTemplate functionTemplate;
			functionTemplate.IsVariadic = m_Parameters.size() > 0 && !m_Parameters.back().Type;
			functionTemplate.Parameters = m_Parameters;
			functionTemplate.ReturnType = m_ReturnType;
			functionTemplate.Root = nullptr; // external function so no root
			functionTemplate.IsExternal = true;
			functionTemplate.Valid = true;

			GetSymbolTable()->RegisterTemplate(data.MangledName, functionTemplate);

			return Symbol::CreateFunction(&GetSymbolTable()->GetInstance(m_Name));	
		}

		return {};
	}	

    Symbol ASTExpression::Codegen(CodegenContext& ctx)
	{
		auto& builder  = ctx.Builder;
		auto& children = GetChildren();

		std::vector<std::shared_ptr<ASTNodeBase>> stack;

		//TODO: move this back into ParseExpression so we don't have to keep doing this every time this class's codegen gets called

		auto IsOperand = [](const std::shared_ptr<ASTNodeBase>& child) 
		{
			return child->GetType() == ASTNodeType::Literal || 
				   child->GetType() == ASTNodeType::Variable ||
				   child->GetType() == ASTNodeType::FunctionCall || 
				   child->GetType() == ASTNodeType::Member || 
				   child->GetType() == ASTNodeType::ListExpr || 
				   child->GetType() == ASTNodeType::StructExpr || 
				   child->GetType() == ASTNodeType::TypeResolver;
		};
		
		for (const auto& child : children)
		{
			if (IsOperand(child))
			{
				stack.push_back(child);
				continue;
			}

			if (std::shared_ptr<ASTUnaryExpression> unaryExpression = std::dynamic_pointer_cast<ASTUnaryExpression>(child))
			{
				unaryExpression->GetChildren().clear();

				unaryExpression->Push(stack.back());
				stack.pop_back();

				stack.push_back(unaryExpression);
				continue;
			}
			else if (std::shared_ptr<ASTBinaryExpression> binExp = std::dynamic_pointer_cast<ASTBinaryExpression>(child))
			{
				binExp->GetChildren().clear();

				binExp->Push(stack.back());
				stack.pop_back();

				binExp->Push(stack.back());
				stack.pop_back();

				stack.push_back(binExp);
			}
			else if (std::shared_ptr<ASTTernaryExpression> ternaryExpr = std::dynamic_pointer_cast<ASTTernaryExpression>(child))
			{
				ternaryExpr->GetChildren().clear();	

				ternaryExpr->Push(stack.back());
				stack.pop_back();

				ternaryExpr->Push(stack.back());
				stack.pop_back();

				ternaryExpr->Push(stack.back());
				stack.pop_back();

				stack.push_back(ternaryExpr);
			}
			else 
			{
				CLEAR_UNREACHABLE("unimplemented");
			}			
		}

		if(stack.size() > 0)
		{
			CLEAR_VERIFY(stack.size() == 1, "wot");
			return stack.back()->Codegen(ctx);
		}


		return {};
	}

	Symbol ASTListExpr::Codegen(CodegenContext& ctx)
	{
		auto& children = GetChildren();
		
		if(children.size() == 0)
		{
			return Symbol();
		}

		// collect all the values

		ValueRestoreGuard guard(ctx.WantAddress, false);

		Symbol first = children[0]->Codegen(ctx);
		
		llvm::SmallVector<llvm::Value*> values;

		values.push_back(first.GetLLVMValue());

		for(size_t i = 1; i < children.size(); i++)
		{
			Symbol value = children[i]->Codegen(ctx);
			value = SymbolOps::Cast(value, first, ctx.Builder);

			values.push_back(value.GetLLVMValue());
		}

		// create static default array and assign any constants
		std::shared_ptr<Type> arrayType = ctx.TypeReg->GetArrayFrom(first.GetType(), values.size());

		llvm::SmallVector<llvm::Constant*> constantValues;
		constantValues.resize(values.size(), llvm::ConstantAggregateZero::get(first.GetType()->Get()));
		
		bool isConst = true;

		for(size_t i = 0; i < values.size(); i++)
		{
			llvm::Constant* constant = llvm::dyn_cast<llvm::Constant>(values[i]);
			
			if(constant)
			{
				constantValues[i] = constant;
				continue;
			}

			isConst = false;
		}


		llvm::ArrayType* llvmArrayType = llvm::dyn_cast<llvm::ArrayType>(arrayType->Get());

		llvm::Constant* initializer = llvm::ConstantArray::get(llvmArrayType, constantValues);

		llvm::GlobalVariable* staticGlobal = new llvm::GlobalVariable(
		    ctx.Module,
		    llvmArrayType,
		    /* isConstant = */ true,
		    llvm::GlobalValue::PrivateLinkage,
		    initializer,
		    "const.array"
		);

		if(isConst)
		{
			Symbol valuePtr = Symbol::CreateValue(staticGlobal, ctx.TypeReg->GetPointerTo(arrayType), /* shouldMemcpy = */ true);

			if(!ctx.WantAddress)
				return SymbolOps::Load(valuePtr, ctx.Builder);

			return valuePtr;
		}

		// allocate array, copy from static to local alloca, assign any dynamic values

		llvm::Value* arrayAlloc = ctx.Builder.CreateAlloca(llvmArrayType, nullptr, "array.alloc");

		uint64_t sizeInBytes = ctx.Module.getDataLayout().getTypeAllocSize(llvmArrayType);
		llvm::Value* size = llvm::ConstantInt::get(ctx.Builder.getInt64Ty(), sizeInBytes);
			
		ctx.Builder.CreateMemCpy(
		    arrayAlloc,
		    llvm::MaybeAlign(),
		    staticGlobal,
		    llvm::MaybeAlign(),
		    size
		);

		for (size_t i = 0; i < values.size(); ++i)
		{
		    if (!llvm::isa<llvm::Constant>(values[i]))
		    {
		        llvm::Value* gep = ctx.Builder.CreateInBoundsGEP(llvmArrayType, arrayAlloc,
		            {
		                ctx.Builder.getInt64(0),
		                ctx.Builder.getInt64((uint64_t) i)
		            }
		        );
			
		        ctx.Builder.CreateStore(values[i], gep);
		    }
		}

		Symbol valuePtr = Symbol::CreateValue(arrayAlloc, ctx.TypeReg->GetPointerTo(arrayType), /* shouldMemcpy = */ true);

		if(!ctx.WantAddress)
			return SymbolOps::Load(valuePtr, ctx.Builder);

		return valuePtr;
	}

	Symbol ASTStructExpr::Codegen(CodegenContext& ctx)
	{
		auto& children = GetChildren();
		CLEAR_VERIFY(children.size() >= 1, "invalid struct expr");

		Symbol ty = children[0]->Codegen(ctx);

		std::shared_ptr<StructType> structTy = nullptr;

		llvm::SmallVector<llvm::Value*> values;
		llvm::SmallVector<std::shared_ptr<Type>> types;

		ValueRestoreGuard guard(ctx.WantAddress, false);

		for(size_t i = 1; i < children.size(); i++) 
		{
			Symbol value = children[i]->Codegen(ctx);
			values.push_back(value.GetLLVMValue());
			types.push_back(value.GetType());
		}		

		switch (ty.Kind) 
		{
			case SymbolKind::Type: 
			{
				structTy = ty.GetType()->As<StructType>();

				for(size_t i = 0; i < values.size(); i++)
				{
					auto baseTy = Symbol::CreateType(structTy->GetMemberAtIndex(i));
					Symbol value = Symbol::CreateValue(values[i], types[i]);
					values[i] = SymbolOps::Cast(value, baseTy, ctx.Builder).GetLLVMValue();
				}

				break;
			}
			case SymbolKind::ClassTemplate: 
			{
				structTy = ctx.TypeReg->GetTypeFromClassTemplate(ty.GetClassTemplate(), ctx, types)->As<StructType>();
				break;
			}
			default: 
			{
				CLEAR_UNREACHABLE("unimplemented");
			}
		}
		
		
		llvm::SmallVector<llvm::Constant*> constantValues;
		constantValues.resize(values.size());
		
		bool isConst = true;

		for(size_t i = 0; i < values.size(); i++)
		{
			llvm::Constant* constant = llvm::dyn_cast<llvm::Constant>(values[i]);
			
			if(constant)
			{
				constantValues[i] = constant;
				continue;
			}

			constantValues[i] = GetDefaultValue(structTy->GetMemberAtIndex(i)->Get());
			isConst = false;
		}

		llvm::StructType* llvmStructTy = llvm::dyn_cast<llvm::StructType>(structTy->Get());

		llvm::Constant* initializer = llvm::ConstantStruct::get(llvmStructTy, constantValues);

		llvm::GlobalVariable* staticGlobal = new llvm::GlobalVariable(
		    ctx.Module,
		    structTy->Get(),
		    /* isConstant = */ true,
		    llvm::GlobalValue::PrivateLinkage,
		    initializer,
		    "const.struct"
		);

		if(isConst)
		{
			Symbol valuePtr = Symbol::CreateValue(staticGlobal, ctx.TypeReg->GetPointerTo(structTy), /* shouldMemcpy = */ true);

			if(!ctx.WantAddress)
				return SymbolOps::Load(valuePtr, ctx.Builder);

			return valuePtr;
		}

		llvm::Value* structAlloc = ctx.Builder.CreateAlloca(llvmStructTy, nullptr, "struct.alloc");
			
		uint64_t sizeInBytes = ctx.Module.getDataLayout().getTypeAllocSize(llvmStructTy);
		llvm::Value* size = llvm::ConstantInt::get(ctx.Builder.getInt64Ty(), sizeInBytes);
			
		ctx.Builder.CreateMemCpy(
		    structAlloc,
		    llvm::MaybeAlign(),
		    staticGlobal,
		    llvm::MaybeAlign(),
		    size
		);

		for (size_t i = 0; i < values.size(); ++i)
		{
		    if (!llvm::isa<llvm::Constant>(values[i]))
		    {
		        llvm::Value* gep = ctx.Builder.CreateStructGEP(llvmStructTy, structAlloc, i);
		        ctx.Builder.CreateStore(values[i], gep);
		    }
		}

		Symbol valuePtr = Symbol::CreateValue(structAlloc, ctx.TypeReg->GetPointerTo(structTy), /* shouldMemcpy = */ true);

		if(!ctx.WantAddress)
			return SymbolOps::Load(valuePtr, ctx.Builder);

		return valuePtr;
	}

	llvm::Constant* ASTStructExpr::GetDefaultValue(llvm::Type* type)
	{
		if (type->isIntegerTy()) 
		{
        	return llvm::ConstantInt::get(type, 0);
    	} 
		else if (type->isFloatingPointTy()) 
		{
    	    return llvm::ConstantFP::get(type, 0.0);
    	} 
		else if (type->isPointerTy()) 
		{
    	    return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(type));
    	} 
		else if (type->isArrayTy() || type->isStructTy() || type->isVectorTy()) 
		{
    	    return llvm::ConstantAggregateZero::get(type);
    	}

    	return nullptr;
	}


    ASTMember::ASTMember(const std::string& name)
		: m_MemberName(name)
    {
    }

	Symbol ASTMember::Codegen(CodegenContext& ctx)
	{
		return {};
	}

	Symbol ASTReturn::Codegen(CodegenContext& ctx)
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
		Symbol codegen = children[0]->Codegen(ctx);

		if(codegen.Kind == SymbolKind::None)
		{
			EmitDefaultReturn(ctx);
			return Symbol();
		}

		auto [codegenValue, codegenType] = codegen.GetValue();

		if(codegenValue == nullptr)
		{
			EmitDefaultReturn(ctx);
			return Symbol();
		}
		
		if(codegenType->Get() != ctx.ReturnType->Get())
		{
			codegenValue = TypeCasting::Cast(codegenValue, codegenType, ctx.ReturnType, ctx.Builder);
		}

		ctx.Builder.CreateStore(codegenValue, ctx.ReturnAlloca);
		ctx.Builder.CreateBr(ctx.ReturnBlock);

		return {};
	}

    void ASTReturn::EmitDefaultReturn(CodegenContext& ctx)
    {
		if(ctx.ReturnAlloca)
		{
			llvm::Type* retType = ctx.ReturnType->Get();
    		llvm::Value* defaultVal = llvm::UndefValue::get(retType);
    		ctx.Builder.CreateStore(defaultVal, ctx.ReturnAlloca);
		}

    	ctx.Builder.CreateBr(ctx.ReturnBlock);
    }

    ASTUnaryExpression::ASTUnaryExpression(OperatorType type)
		: m_Type(type)
    {
    }

	Symbol ASTUnaryExpression::Codegen(CodegenContext& ctx)
	{
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() == 1, "incorrect dimensions");

		if(m_Type == OperatorType::Dereference)
		{
			Symbol result;

			{
				ValueRestoreGuard guard(ctx.WantAddress, false);
				result = children[0]->Codegen(ctx);
			}

			auto [resultValue, resultType] = result.GetValue();

			CLEAR_VERIFY(resultType->IsPointer(), "not a valid dereference");

			if(ctx.WantAddress)
				return result;

			return SymbolOps::Load(result, ctx.Builder);
		}	

		CLEAR_VERIFY(!ctx.WantAddress, "Invalid use of unary expression");

		if(m_Type == OperatorType::Address)
		{		
			ValueRestoreGuard guard(ctx.WantAddress, true);
			return children[0]->Codegen(ctx);;
		}

		if(m_Type == OperatorType::Negation)
		{			
			Symbol result = children[0]->Codegen(ctx);

			auto signedType = result.GetType();

			if(!signedType->IsSigned()) // uint... -> int...
			{
				signedType = ctx.ClearModule->Lookup(signedType->GetHash().substr(1)).GetType();
			}

			return SymbolOps::Neg(result, ctx.Builder, signedType); 
		}

		if(m_Type == OperatorType::Not)
		{
			Symbol result = children[0]->Codegen(ctx);
			return SymbolOps::Not(result, ctx.Builder);
		}
		
		Symbol one = Symbol::CreateValue(ctx.Builder.getInt32(1), ctx.TypeReg->GetType("int32"));

		ValueRestoreGuard guard(ctx.WantAddress, true);

		Symbol result = children[0]->Codegen(ctx);
		auto [resultValue, resultType] = result.GetValue();

		CLEAR_VERIFY(resultType->IsPointer(), "not valid type for increment");
		
		std::shared_ptr<PointerType> ty = std::dynamic_pointer_cast<PointerType>(resultType);

		Symbol valueToStore;
		Symbol returnValue;

		auto ApplyFun = [&](OperatorType type)
		{
			if(ty->GetBaseType()->IsPointer())
				valueToStore = ASTBinaryExpression::HandlePointerArithmetic(returnValue, one, type, ctx, GetSymbolTable());
			else 
				valueToStore = ASTBinaryExpression::HandleMathExpression(returnValue, one, type, ctx, GetSymbolTable());
		};

		if(m_Type == OperatorType::PostIncrement)
		{
			returnValue = SymbolOps::Load(result, ctx.Builder);
			ApplyFun(OperatorType::Add);
		}
		else if (m_Type == OperatorType::PostDecrement)
		{
			returnValue = SymbolOps::Load(result, ctx.Builder);
			ApplyFun(OperatorType::Sub);
		}
		else if (m_Type == OperatorType::Increment)
		{
			returnValue = SymbolOps::Load(result, ctx.Builder);

			ApplyFun(OperatorType::Add);
			returnValue = valueToStore;
		}
		else if (m_Type == OperatorType::Decrement)
		{
			returnValue = SymbolOps::Load(result, ctx.Builder);

			ApplyFun(OperatorType::Sub);
			returnValue = valueToStore;
		}
    	else if(m_Type == OperatorType::Ellipsis)
    	{
    		auto ptrTy = std::dynamic_pointer_cast<PointerType>(ty);
    		auto arrTy = std::dynamic_pointer_cast<ArrayType>(ptrTy->GetBaseType());

    		CLEAR_VERIFY(arrTy,"Unpack must have array");

			llvm::SmallVector<llvm::Value*> values;
			llvm::SmallVector<std::shared_ptr<Type>> types;

    		for (int i = 0; i < arrTy->GetArraySize(); i++)
    		{
    			auto pointer = ctx.Builder.CreateGEP(arrTy->Get(), resultValue, { ctx.Builder.getInt64(0),ctx.Builder.getInt64(i) });
    			auto loadedValue = ctx.Builder.CreateLoad(arrTy->GetBaseType()->Get(), pointer);
				
    			values.push_back(loadedValue);
    			types.push_back(arrTy->GetBaseType());
    		}

    		return Symbol::CreateTuple(values, types);
    	}
		else
		{
			CLEAR_UNREACHABLE("unimplemented");
		}

		auto [storedValue, storedType] = valueToStore.GetValue();
		storedValue = TypeCasting::Cast(storedValue, storedType, ty->GetBaseType(), ctx.Builder);

		ctx.Builder.CreateStore(storedValue, resultValue);

		return returnValue;
	}


	Symbol ASTIfExpression::Codegen(CodegenContext& ctx)
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
			branch.ConditionBlock = llvm::BasicBlock::Create(ctx.Context, "if.condition");
			branch.BodyBlock      = llvm::BasicBlock::Create(ctx.Context, "if.body");
			branch.ExpressionIdx  = i;

			branches.push_back(branch);
		}

		llvm::BasicBlock* elseBlock  = llvm::BasicBlock::Create(ctx.Context, "if.else");
		llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(ctx.Context, "if.merge");

		if(!ctx.Builder.GetInsertBlock()->getTerminator())
			ctx.Builder.CreateBr(branches[0].ConditionBlock);

		for (size_t i = 0; i < branches.size(); i++)
		{
			auto& branch = branches[i];

			llvm::BasicBlock* nextBranch = (i + 1) < branches.size() ? branches[i + 1].ConditionBlock : elseBlock;
			
			function->insert(function->end(), branch.ConditionBlock);
			ctx.Builder.SetInsertPoint(branch.ConditionBlock);

			Symbol condition;

			{
				ValueRestoreGuard guard(ctx.WantAddress, false);
				condition = children[branch.ExpressionIdx]->Codegen(ctx);
			}

			auto [conditionValue, conditionType] = condition.GetValue();

			if (conditionType->IsIntegral() && conditionType->GetSizeInBytes(ctx.Module) > 1)
			{
				conditionValue = ctx.Builder.CreateICmpNE(conditionValue, llvm::ConstantInt::get(conditionType->Get(), 0));
			}
			else if (conditionType->IsFloatingPoint())
			{
				conditionValue = ctx.Builder.CreateFCmpONE(conditionValue, llvm::ConstantFP::get(conditionType->Get(), 0.0));
			}
			else if (conditionType->IsPointer())
			{
				conditionValue = ctx.Builder.CreatePtrToInt(conditionValue, ctx.Builder.getInt64Ty(), "cast");
				conditionValue = ctx.Builder.CreateICmpNE(conditionValue, ctx.Builder.getInt64(0));
			}

			ctx.Builder.CreateCondBr(conditionValue, branch.BodyBlock, nextBranch);

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

    ASTWhileExpression::ASTWhileExpression()
    {
    }

	ASTTernaryExpression::ASTTernaryExpression() 
	{

	}

	Symbol ASTTernaryExpression::Codegen(CodegenContext& ctx) 
	{
		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() == 3, "invalid node");

		llvm::Function* function = ctx.Builder.GetInsertBlock()->getParent();

		Symbol condition = children.back()->Codegen(ctx);

		llvm::BasicBlock* incomingBlock  = llvm::BasicBlock::Create(ctx.Context, "ternary.incoming",  function);
		llvm::BasicBlock* falseBlock = llvm::BasicBlock::Create(ctx.Context, "ternary.false", function);
		llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(ctx.Context, "ternary.merge", function);

		Symbol trueValue, falseValue;

		Symbol trueType = Symbol::GetBooleanType(ctx.ClearModule); 
		condition = SymbolOps::Cast(condition, trueType, ctx.Builder);

		ctx.Builder.CreateCondBr(condition.GetLLVMValue(), incomingBlock, falseBlock);
		ctx.Builder.SetInsertPoint(incomingBlock);

		trueValue  = children[1]->Codegen(ctx);
		auto ip = ctx.Builder.saveIP();

		ctx.Builder.CreateBr(mergeBlock);
		incomingBlock = ctx.Builder.GetInsertBlock();

		ctx.Builder.SetInsertPoint(falseBlock);
		
		falseValue = children[0]->Codegen(ctx);

		SymbolOps::Promote(trueValue, falseValue, ctx.Builder, &ip);

		ctx.Builder.CreateBr(mergeBlock);	
		falseBlock = ctx.Builder.GetInsertBlock();

		ctx.Builder.SetInsertPoint(mergeBlock);

		// cond true false :?
		// cond true false
		// false true cond
		
		auto phiNode = ctx.Builder.CreatePHI(trueValue.GetType()->Get(), 2);

		phiNode->addIncoming(trueValue.GetLLVMValue(), incomingBlock);
		phiNode->addIncoming(falseValue.GetLLVMValue(), falseBlock);

		return Symbol::CreateValue(phiNode, trueValue.GetType());
	}

	void ASTTernaryExpression::Print()
	{
		std::print("?: ");
	}

    Symbol ASTWhileExpression::Codegen(CodegenContext &ctx)
    {
		PushScopeMarker(ctx);

		auto& children = GetChildren();

		CLEAR_VERIFY(children.size() == 2, "incorrect dimension");
		CLEAR_VERIFY(children[0]->GetType() == ASTNodeType::Expression, "incorrect node type");

		llvm::Function* function = ctx.Builder.GetInsertBlock()->getParent();

		llvm::BasicBlock* conditionBlock = llvm::BasicBlock::Create(ctx.Context, "while.condition", function);
		llvm::BasicBlock* body  = llvm::BasicBlock::Create(ctx.Context, "while.body");
		llvm::BasicBlock* end   = llvm::BasicBlock::Create(ctx.Context, "while.merge");

		if (!ctx.Builder.GetInsertBlock()->getTerminator())
			ctx.Builder.CreateBr(conditionBlock);

		ctx.Builder.SetInsertPoint(conditionBlock);

		Symbol condition;
			
		{
			ValueRestoreGuard guard(ctx.WantAddress, false);
			condition = children[0]->Codegen(ctx);
		}
		auto [conditionValue, conditionType] = condition.GetValue();


		if (conditionType->IsIntegral())
			conditionValue = ctx.Builder.CreateICmpNE(conditionValue, llvm::ConstantInt::get(conditionType->Get(), 0));
			
		else if (conditionType->IsFloatingPoint())
			conditionValue = ctx.Builder.CreateFCmpONE(conditionValue, llvm::ConstantFP::get(conditionType->Get(), 0.0));

		if (!ctx.Builder.GetInsertBlock()->getTerminator())
			ctx.Builder.CreateCondBr(conditionValue, body, end);

		function->insert(function->end(), body);
		ctx.Builder.SetInsertPoint(body);

    	ValueRestoreGuard guard1(ctx.LoopConditionBlock, conditionBlock);
    	ValueRestoreGuard guard2(ctx.LoopEndBlock,       end);

		children[1]->Codegen(ctx);


		if (!ctx.Builder.GetInsertBlock()->getTerminator())
		{
			GetSymbolTable()->FlushScope(ctx);
			ctx.Builder.CreateBr(conditionBlock);
		}
		
		function->insert(function->end(), end);
		ctx.Builder.SetInsertPoint(end);
		

		return {};
	}

    ASTForExpression::ASTForExpression(const std::string& name)
		: m_Name(name)
    {
    }

    Symbol ASTForExpression::Codegen(CodegenContext& ctx)
    {
		auto& children = GetChildren();
		CLEAR_VERIFY(children.size() == 2, "invalid for loop");

		Symbol iterator;

		{
			ValueRestoreGuard guard(ctx.WantAddress, true);
			iterator = children[0]->Codegen(ctx);
		}	

		auto tbl = GetSymbolTable();

		auto [iterValue, iterType] = iterator.GetValue();

		if(iterType->IsVariadic())
		{
			auto& args = tbl->GetVariadicArguments();

			for(size_t i = 0; i < args.size(); i++)
			{
				tbl->TrackAllocation(m_Name, args[i]);
				children[1]->Codegen(ctx);
			}

			return {};
		}

		CLEAR_UNREACHABLE("unimplemented");

		return {};
	}

	ASTLoopControlFlow::ASTLoopControlFlow(std::string jumpTy)
		: m_JumpTy(jumpTy)
	{
	}

    ASTStruct::ASTStruct(const std::string& name)
		: m_Name(name)
    {
    }

    Symbol ASTStruct::Codegen(CodegenContext& ctx)
    {
		auto& children = GetChildren();

		// create the struct type
		auto structTy = ctx.TypeReg->CreateType<StructType>(m_Name, m_Name, ctx.Context);
		std::vector<std::pair<std::string, std::shared_ptr<Type>>> members;

		size_t i = 0;
		for(; i < children.size(); i++)
		{
			if(!children[i]) break;
			if(children[i]->GetType() != ASTNodeType::TypeSpecifier) break;

			auto typeSpec = std::dynamic_pointer_cast<ASTTypeSpecifier>(children[i]);
			Symbol result = typeSpec->Codegen(ctx);

			members.emplace_back(std::string(result.Metadata.value_or(String())), result.GetType());
		}

		structTy->SetBody(members);

		// set its default values

		ValueRestoreGuard guard(ctx.WantAddress, false);

		for(const auto& [memberName, memberType] : members)
		{
			CLEAR_VERIFY(i < children.size(), "haven't added all the default values");

			if(!children[i])
			{
				i++;
				structTy->AddDefaultValue(memberName, llvm::Constant::getNullValue(memberType->Get()));
				continue;
			}

			Symbol result = 	children[i++]->Codegen(ctx);
			auto [resultValue, resultType] = result.GetValue();
			resultValue = TypeCasting::Cast(resultValue, resultType, memberType, ctx.Builder);
			structTy->AddDefaultValue(memberName, resultValue);
		}

		return {};
	}

	Symbol ASTLoopControlFlow::Codegen(CodegenContext& ctx) 
	{
    	CLEAR_VERIFY(ctx.LoopConditionBlock, "BREAK/CONTINUE not in loop")
		
		GetSymbolTable()->FlushScope(ctx);

    	if (m_JumpTy == "continue")
			ctx.Builder.CreateBr(ctx.LoopConditionBlock);

    	else if(m_JumpTy == "break")
    		ctx.Builder.CreateBr(ctx.LoopEndBlock);

    	return {};
    }

    Symbol ASTDefaultArgument::Codegen(CodegenContext& ctx)
    {
		auto& children = GetChildren();
		CLEAR_VERIFY(children.size() == 1, "invalid argument");
		ValueRestoreGuard guard(ctx.WantAddress, false);

        return children[0]->Codegen(ctx);
    }
    
	ASTClass::ASTClass(const std::string& name)
		: m_Name(name)
    {
    }

    Symbol ASTClass::Codegen(CodegenContext& ctx)
    {
		if(m_Generics.empty())
		{
			Instantiate(ctx);
		}
		else  
		{
			ctx.TypeReg->CreateClassTemplate(m_Name, std::dynamic_pointer_cast<ASTClass>(shared_from_this()), m_Generics);
		}

		return {};
    }

	void ASTClass::Instantiate(CodegenContext& ctx, llvm::ArrayRef<std::shared_ptr<Type>> aliasTypes)
	{
		ValueRestoreGuard guard1(m_Name, m_Name);

		for(size_t i = 0; i < m_Generics.size(); i++)
		{
			CLEAR_VERIFY(i <= aliasTypes.size(), "index out of bounds");
			ctx.ClearModule->CreateAlias(m_Generics[i], aliasTypes[i]->GetHash());
			m_Name += aliasTypes[i]->GetHash();
		}

		auto& children = GetChildren();

		// create the struct type
		auto structTy = std::make_shared<StructType>(m_Name, ctx.Context);
		auto classTy  = ctx.TypeReg->CreateType<ClassType>(m_Name, structTy);

		std::vector<std::pair<std::string, std::shared_ptr<Type>>> members;

		int64_t i = children.size() - 1; // params start from back so we walk backwards from end
		for(; i >= 0; i--)
		{
			if(!children[i]) break;
			if(children[i]->GetType() != ASTNodeType::TypeSpecifier) break;

			auto typeSpec = std::dynamic_pointer_cast<ASTTypeSpecifier>(children[i]);
			Symbol result = typeSpec->Codegen(ctx);

			members.emplace_back(std::string(result.Metadata.value_or(String())), result.GetType());
		}

		// we need to reverse members so they are in correct order as written in the language
		std::reverse(members.begin(), members.end());
		structTy->SetBody(members);

		// set its default values

		ValueRestoreGuard guard2(ctx.WantAddress, false);

		for(const auto& [memberName, memberType] : members)
		{
			CLEAR_VERIFY(i >= 0, "haven't added all the default values");

			if(!children[i])
			{
				i--;
				structTy->AddDefaultValue(memberName, llvm::Constant::getNullValue(memberType->Get()));

				continue;
			}

			CLEAR_VERIFY(children[i]->GetType() == ASTNodeType::Expression, "haven't added all the default values");
			

			Symbol result = children[i--]->Codegen(ctx);

			auto [resultValue, resultType] = result.GetValue();

			resultValue = TypeCasting::Cast(resultValue, resultType, memberType, ctx.Builder);
			structTy->AddDefaultValue(memberName, resultValue);
		}

		// handle all function definitions

		for(const auto& definition : GetChildren())
		{
			if(!definition || definition->GetType() != ASTNodeType::FunctionDefinition)
				continue;

			auto functionDefinition = std::dynamic_pointer_cast<ASTFunctionDefinition>(definition);
		
			std::string functionName = functionDefinition->GetName();
			std::string qualifiedFunctionName = m_Name + "." + functionName;

			functionDefinition->SetName(qualifiedFunctionName);
	
			functionDefinition->Codegen(ctx);

			functionDefinition->SetName(functionName);
	
			const auto& returnType = functionDefinition->GetReturnType();
			const auto& params = functionDefinition->GetParameters();

			std::string mangledName = FunctionCache::GetMangledName(qualifiedFunctionName, params, returnType);
			classTy->PushFunction(mangledName);
		}

		for(size_t i = 0; i < m_Generics.size(); i++)
		{
			ctx.ClearModule->RemoveAlias(m_Generics[i]);
		}
	}

    ASTTrait::ASTTrait(const std::string& name)
		: m_Name(name)
    {
    }

    Symbol ASTTrait::Codegen(CodegenContext& ctx)
    {
		auto& children = GetChildren();

		std::vector<std::string> functions;
        std::vector<std::pair<std::string, std::shared_ptr<Type>>> members; 

		for(const auto& child : children)
		{
			if(child->GetType() == ASTNodeType::VariableDecleration)
			{
				auto decleration = std::dynamic_pointer_cast<ASTVariableDeclaration>(child);
				members.emplace_back(decleration->GetName(), decleration->GetResolvedType());
			}
			else if(child->GetType() == ASTNodeType::FunctionDecleration)
			{
				auto decleration = std::dynamic_pointer_cast<ASTFunctionDecleration>(child);
				decleration->InsertDecleration = false;

				decleration->Codegen(ctx);
				functions.push_back(FunctionCache::GetMangledName(decleration->GetName(), 
																  decleration->GetParameters(), 
																  decleration->GetReturnType()));
			}
			else 
			{
				CLEAR_UNREACHABLE("unimplemented type");
			}
		}

		ctx.TypeReg->CreateType<TraitType>(m_Name, functions, members, m_Name);
        return Symbol();
    }

    Symbol ASTRaise::Codegen(CodegenContext& ctx)
    {
		CLEAR_UNREACHABLE("unimplemented");
		ctx.Thrown = true;

        return Symbol();
    }

    Symbol ASTTryCatch::Codegen(CodegenContext &)
    {
		CLEAR_UNREACHABLE("unimplemented");
        return Symbol();
    }

    Symbol ASTDefaultInitializer::Codegen(CodegenContext& ctx)
    {
		auto& children = GetChildren();
		auto tbl = GetSymbolTable();

		CLEAR_VERIFY(children.size() == 1, "invalid node");

		ValueRestoreGuard guard(ctx.WantAddress, true);
		Symbol variable = children[0]->Codegen(ctx);
		
		auto [varValue, varType] = variable.GetValue();
		CLEAR_VERIFY(varType->IsPointer(), "cannot assign to a value");

		auto pointerTy = dyn_cast<PointerType>(varType);
		auto baseTy = pointerTy->GetBaseType();

		bool isGlobal = llvm::isa<llvm::GlobalVariable>(varValue);

		if (baseTy->IsPointer()) 
		{
		    llvm::Constant* nullPtr = llvm::ConstantPointerNull::get(
		        llvm::cast<llvm::PointerType>(baseTy->Get())
		    );
		
		    if (isGlobal)
		    {
		        llvm::cast<llvm::GlobalVariable>(varValue)->setInitializer(nullPtr);
		    }
		    else
		    {
		        ctx.Builder.CreateStore(nullPtr, varValue);
		    }
		}
		else if (baseTy->IsCompound() || baseTy->IsArray())
		{
			llvm::ConstantAggregateZero* zero = llvm::ConstantAggregateZero::get(baseTy->Get());

			if (isGlobal)
		    {
		        llvm::cast<llvm::GlobalVariable>(varValue)->setInitializer(zero);
		    }
			else
		    {
		        ctx.Builder.CreateStore(zero, varValue);
		    }
		}
		else if (baseTy->IsIntegral())
		{
		    llvm::Constant* zero = llvm::ConstantInt::get(baseTy->Get(), 0);
		
		    if (isGlobal)
		    {
		        llvm::cast<llvm::GlobalVariable>(varValue)->setInitializer(zero);
		    }
		    else
		    {
		        ctx.Builder.CreateStore(zero, varValue);
		    }
		}
		else if (baseTy->IsFloatingPoint())
		{
		    llvm::Constant* zero = llvm::ConstantFP::get(baseTy->Get(), 0.0);
		
		    if (isGlobal)
		    {
		        llvm::cast<llvm::GlobalVariable>(varValue)->setInitializer(zero);
		    }
		    else
		    {
		        ctx.Builder.CreateStore(zero, varValue);
		    }
		}

        return Symbol();
    }

    void ASTDefaultInitializer::RecursiveCallConstructors(llvm::Value* value, std::shared_ptr<Type> type, CodegenContext& ctx, std::shared_ptr<SymbolTable> tbl, bool isGlobal)
    {
		if(type->IsArray())
		{
			auto arrayTy = dyn_cast<ArrayType>(type);
			auto baseTy  = arrayTy->GetBaseType();

			if(!baseTy->IsCompound())
				return;

			for(size_t i = 0; i < arrayTy->GetArraySize(); i++)
			{
				llvm::Value* gep = nullptr;

				std::vector<llvm::Value*> indices = {
				        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.Context), 0), 
				        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.Context), i) 
				};

				if (isGlobal)
				{
					CLEAR_VERIFY(llvm::cast<llvm::Constant>(value), "cannot have global that is not a constant");

				    gep = llvm::ConstantExpr::getGetElementPtr(
				        arrayTy->Get(),                           
				        llvm::cast<llvm::Constant>(value),         
				        indices
				    );
				}
				else 
				{
				    gep = ctx.Builder.CreateGEP(
				        arrayTy->Get(),  
				        value,            
				        indices             
				    );
				}

				RecursiveCallConstructors(gep, baseTy, ctx, tbl, isGlobal);
			}
			
			return;
		}

		CLEAR_VERIFY(type->IsCompound(), "compound type");

		std::shared_ptr<StructType> structTy = nullptr;
		std::string functionName;  

		if(type->IsClass())
		{
			auto classTy = dyn_cast<ClassType>(type);
			structTy = classTy->GetBaseType();

			functionName = classTy->ConvertFunctionToClassFunction("_CLR__construct__$%");

			if(!tbl->HasTemplateMangled(functionName))
			{
				return;
			}
		}
		else 
		{
			structTy = dyn_cast<StructType>(type);
		}

		CLEAR_VERIFY(structTy, "not a valid type");

		const auto& memberIndices = structTy->GetMemberIndices();
		const auto& memberTypes   = structTy->GetMemberTypes();

		for(const auto& [name, subType] : memberTypes)
		{
			size_t index = memberIndices.at(name);
			llvm::Value* gep = nullptr;

			if (isGlobal)
			{
			    std::vector<llvm::Constant*> indices = {
			        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.Context), 0), 
			        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.Context), index) 
				};
				
				CLEAR_VERIFY(llvm::cast<llvm::Constant>(value), "cannot have global that is not a constant");

			    gep = llvm::ConstantExpr::getGetElementPtr(
			        structTy->Get(),                           
			        llvm::cast<llvm::Constant>(value),         
			        indices
			    );
			}
			else 
			{
			    gep = ctx.Builder.CreateStructGEP(
			        structTy->Get(),  
			        value,            
			        index             
			    );
			}

			if(!subType->IsCompound()) 
			{
				ctx.Builder.CreateStore(structTy->GetDefaultValue(name), gep);
				continue;
			}

			RecursiveCallConstructors(gep, subType, ctx, tbl, isGlobal);
		}

		if(type->IsClass())
		{
			auto classTy = dyn_cast<ClassType>(type);

			Parameter param;
			param.Name = "this";
			param.Type = ctx.TypeReg->GetPointerTo(classTy);

			std::string name = classTy->GetHash() + "." + "__construct__";

			auto function = tbl->InstantiateOrReturn(name, { param }, nullptr, ctx);

			if(isGlobal)
			{
				CLEAR_UNREACHABLE("unimplemented");
				static thread_local int32_t s_Index = 0;

				CLEAR_VERIFY(llvm::cast<llvm::Constant>(value), "value not a constant");
				llvm::appendToGlobalCtors(ctx.Module, function.Function, s_Index++, llvm::cast<llvm::Constant>(value));
			}
			else 
			{
				ctx.Builder.CreateCall(function.Function, { value });
			}

		}
    }

    ASTEnum::ASTEnum(const std::string& enumName, const std::vector<std::string>& names)
		: m_Names(names), m_EnumName(enumName)
    {
    }

    Symbol ASTEnum::Codegen(CodegenContext& ctx)
    {
		std::shared_ptr<EnumType> type = std::make_shared<EnumType>(ctx.TypeReg->GetType("int64"), m_EnumName);

		auto& children = GetChildren();

		int64_t previous = 0;

		for(size_t i = 0; i < m_Names.size(); i++)
		{
			CLEAR_VERIFY(i < children.size(), "invalid enum node");

			if(!children[i])
			{
				type->InsertEnumValue(m_Names[i], ++previous);
				continue;
			}

			Symbol result = children[i]->Codegen(ctx);

			auto casted = llvm::dyn_cast<llvm::ConstantInt>(result.GetValue().first);
			CLEAR_VERIFY(casted, "not a valid enum value!");

			type->InsertEnumValue(m_Names[i], casted->getSExtValue());
			previous = casted->getSExtValue();
		}

		ctx.TypeReg->RegisterType(m_EnumName, type);
        return Symbol();
    }

    Symbol ASTDefer::Codegen(CodegenContext& ctx)
    {
		auto& children = GetChildren();
		CLEAR_VERIFY(children.size() == 1, "cannot have defer node with more than one child");

		ctx.DeferredCalls.push_back(children[0]);
        return Symbol();
    }

    ASTTypeResolver::ASTTypeResolver(const std::vector<Token>& tokens)
		: m_Tokens(tokens)
    {
    }

    Symbol ASTTypeResolver::Codegen(CodegenContext& ctx)
    {
		CLEAR_VERIFY(m_Tokens.size() > 0, "not a valid type resolver");

		if(Symbol inferred = Inferred(); inferred.Kind != SymbolKind::None)
		{
			return inferred;
		}

		auto& children = GetChildren();

		int64_t k = (size_t)children.size() - 1;
		size_t i = 0;

		std::reverse(m_Tokens.begin(), m_Tokens.end());

		std::shared_ptr<Type> type;

		if(m_Tokens[i].IsType(TokenType::RightBracket))
		{
			type = ResolveArray(ctx, i, k);
		}
		else if (m_Tokens[i].IsType(TokenType::GreaterThan))
		{
			i += 2; // <>

			Symbol symbol = ctx.ClearModule->Lookup(m_Tokens[i].GetData());
			CLEAR_VERIFY(symbol.Kind == SymbolKind::ClassTemplate, "not a valid template");

			i++;
		
			type = ResolveGeneric(symbol, ctx, i, k);
		}
		else
		{
			Symbol symbol = ctx.ClearModule->Lookup(m_Tokens[i].GetData());

			if(symbol.Kind == SymbolKind::ClassTemplate)
				return symbol;

			type = symbol.GetType();
			i++;
		}


		for(; i < m_Tokens.size(); i++)
		{
			if(m_Tokens[i].IsType(TokenType::Star))
			{
				type = ctx.TypeReg->GetPointerTo(type);
			}
			else if (m_Tokens[i].IsType(TokenType::RightBracket))
			{
				type = ResolveArray(ctx, i, k);
				i--;
			}
			else if (m_Tokens[i].GetData() == "const")
			{
				type = ctx.TypeReg->GetConstFrom(type);
			}
			else 
			{
				CLEAR_UNREACHABLE("invalid token in type ", m_Tokens[i].GetData());
			}
		}

        return Symbol::CreateType(type);
    }

	Symbol ASTTypeResolver::Inferred()
	{
		if(m_Tokens[0].GetData() == "let") 
		{
			CLEAR_VERIFY(m_Tokens.size() == 1, "cannot have anything past let");
			return Symbol::CreateInferType(/* isConst = */ false);
		}

		if(m_Tokens[0].GetData() == "const" && m_Tokens.size() == 1)
		{
			return Symbol::CreateInferType(/* isConst = */ true);
		} 

		return Symbol();
	}

	std::shared_ptr<Type> ASTTypeResolver::ResolveArray(CodegenContext& ctx, size_t& i, int64_t& k)
	{
		auto& children = GetChildren();

		Symbol baseType = children[k]->Codegen(ctx);
		CLEAR_VERIFY(k > 0 && baseType.Kind == SymbolKind::Type, "");
		
		k--;

		Symbol size = children[k]->Codegen(ctx);
		CLEAR_VERIFY(size.Kind == SymbolKind::Value, "");

		k--;

		i += 2; // []

		llvm::ConstantInt* value = llvm::dyn_cast<llvm::ConstantInt>(size.GetLLVMValue());
		CLEAR_VERIFY(value, "cannot define an array of dynamic size, consider using a dynamic list instead");

		return ctx.TypeReg->GetArrayFrom(baseType.GetType(), value->getZExtValue());
	}

	std::shared_ptr<Type> ASTTypeResolver::ResolveGeneric(Symbol& symbol, CodegenContext& ctx, size_t& i, int64_t& k)
	{		
		auto& children = GetChildren();

		ClassTemplate classTemplate = symbol.GetClassTemplate();

		std::string typeName = classTemplate.Name;

		llvm::SmallVector<std::shared_ptr<Type>> types;

		for(; k >= 0; k--)
		{
			if(children[k]->GetType() != ASTNodeType::TypeResolver)
				break;
			
			std::shared_ptr<Type> subType = children[k]->Codegen(ctx).GetType();
			types.push_back(subType);
		}

		std::reverse(types.begin(), types.end());

		for(auto& ty : types)
		{
			typeName += ty->GetHash();
		}

		std::shared_ptr<Type> type = ctx.TypeReg->GetType(typeName);

		if(!type)
		{
			auto classNode = std::dynamic_pointer_cast<ASTClass>(classTemplate.Class);
			classNode->Instantiate(ctx, types);

			type = ctx.TypeReg->GetType(typeName);
		}

		return type;
	}

	Symbol ASTTypeSpecifier::Codegen(CodegenContext& ctx) 
	{
		auto& children = GetChildren();

		if(children.empty())
		{
			Symbol type = Symbol::CreateType(nullptr);
			type.Metadata = m_Name;

			return type;
		}

		CLEAR_VERIFY(children.size() == 1, "invalid parameter node");

		Symbol type = children[0]->Codegen(ctx);
		type.Metadata = m_Name;
		return type;
	}

	ASTTypeSpecifier::ASTTypeSpecifier(const std::string& name)
		 : m_Name(name)
	{
	}

	Symbol ASTSwitch::Codegen(CodegenContext& ctx)
	{
		llvm::Function* function = ctx.Builder.GetInsertBlock()->getParent();

		auto& children = GetChildren(); 

		// default case must be at back
		llvm::BasicBlock* continueBlock = llvm::BasicBlock::Create(ctx.Context, "continue");
		llvm::BasicBlock* defaultCase   = llvm::BasicBlock::Create(ctx.Context, "default");
		auto defaultCaseCode = children.back();

		auto savedIp = ctx.Builder.saveIP();

		ctx.Builder.SetInsertPoint(defaultCase);
		defaultCaseCode->Codegen(ctx);
		
		if(!ctx.Builder.GetInsertBlock()->getTerminator())
			ctx.Builder.CreateBr(continueBlock);

		ctx.Builder.restoreIP(savedIp);

		// first child must be value
		Symbol value = children[0]->Codegen(ctx);

		llvm::SwitchInst* switchStatement = ctx.Builder.CreateSwitch(value.GetLLVMValue(), defaultCase);
		function->insert(function->end(), defaultCase);

		llvm::SmallVector<llvm::Value*> values;

		// rest should be value ... code
		for(size_t i = 1; i < children.size() - 1; i++)
		{
			auto child = children[i];

			if(child->GetType() == ASTNodeType::Expression)
			{
				ValueRestoreGuard guard(ctx.WantAddress, false);
				values.push_back(child->Codegen(ctx).GetLLVMValue());
				continue;
			}

			llvm::BasicBlock* block = llvm::BasicBlock::Create(ctx.Context, "switch_case", function);
			ctx.Builder.SetInsertPoint(block);
			child->Codegen(ctx);
			
			if(!ctx.Builder.GetInsertBlock()->getTerminator())
				ctx.Builder.CreateBr(continueBlock);

			while(!values.empty())
			{
				llvm::ConstantInt* casted = llvm::dyn_cast<llvm::ConstantInt>(values.back());
				CLEAR_VERIFY(casted, "not a constant int!");

				switchStatement->addCase(casted, block);
				values.pop_back();
			}
		}

		function->insert(function->end(), continueBlock);
		ctx.Builder.SetInsertPoint(continueBlock);

		return Symbol();
	}
}