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
#include "Sema/Sema.h"

#include <alloca.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Metadata.h>
#include <llvm/MC/MCInstrDesc.h>
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

	static Symbol CreateTemporary(std::shared_ptr<Type> type, CodegenContext& ctx)
	{
		llvm::BasicBlock* insertBlock = ctx.Builder.GetInsertBlock();
        
        CLEAR_VERIFY(insertBlock, "cannot create an alloca without function");  
	    auto ip = ctx.Builder.saveIP(); 
	    llvm::Function* function = insertBlock->getParent();    
	    ctx.Builder.SetInsertPoint(&function->getEntryBlock());
		
		Symbol symbol = Symbol::CreateValue(ctx.Builder.CreateAlloca(type->Get(), nullptr, "tmp"), ctx.ClearModule->GetTypeRegistry()->GetPointerTo(type)); 

	    ctx.Builder.restoreIP(ip);  
		return symbol;
	}

    ASTNodeBase::ASTNodeBase()
    {
    }

	Symbol ASTNodeBase::Codegen(CodegenContext& ctx)
	{
		return Symbol();
	}

    void ASTNodeBase::PropagateSymbolTableToChildren()
    {
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
    }
	
	ASTBlock::ASTBlock()
	{
		CreateSymbolTable();
	}

	Symbol ASTBlock::Codegen(CodegenContext& ctx)
	{
		for (auto child : Children)
			child->Codegen(ctx);

		return Symbol();
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
		CLEAR_VERIFY(LeftSide && RightSide, "Cannot be null");

		auto& leftChild  = LeftSide;
		auto& rightChild = RightSide;

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
		auto booleanType = ctx.ClearModule->Lookup("bool").value().GetType();

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

				if(ty->IsClass())
				{
					std::shared_ptr<ASTMember> member = std::dynamic_pointer_cast<ASTMember>(right);
					CLEAR_VERIFY(member, "");
						
					std::shared_ptr<Symbol> memberSymbol = ty->As<ClassType>()->GetMember(member->GetName()).value();

					if (memberSymbol->Kind == SymbolKind::Function)
						return Symbol::CreateCallee(memberSymbol, nullptr);
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

					//std::string name = funcCall->GetName();

					//funcCall->PushPrefixArgument(lhs.GetValue().first, ptrType);
					//funcCall->SetName(std::format("{}.{}", classType->GetHash(), name));

					Symbol result = funcCall->Codegen(ctx);

					//funcCall->SetName(name);

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

    Symbol ASTBinaryExpression::HandleMember(Symbol& lhs, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx)
    {
		auto member = std::dynamic_pointer_cast<ASTMember>(right);
		auto lhsType = lhs.GetType();
		
		while (lhsType->IsPointer())
			lhsType = lhsType->As<PointerType>()->GetBaseType();

		auto memberSymbol = lhsType->As<ClassType>()->GetMember(member->GetName()).value();

		if (memberSymbol->Kind == SymbolKind::Function)
		{
			std::shared_ptr<Type> targetType = memberSymbol->GetFunctionSymbol().FunctionNode->Arguments[0]->TypeResolver->ConstructedType.GetType();

			while(lhs.GetType() != targetType)
			{
				lhs = SymbolOps::Load(lhs, ctx.Builder);
			}
			
			return Symbol::CreateCallee(memberSymbol, std::make_shared<Symbol>(lhs));
		}
		
		auto memberPtrType = Symbol::CreateType(ctx.TypeReg->GetPointerTo(memberSymbol->GetType()));

		size_t index = lhsType->As<ClassType>()->GetMemberValueIndex(member->GetName()).value();	
		
		if (lhs.GetType()->IsClass())
		{
			Symbol storage = CreateTemporary(lhsType, ctx);
			SymbolOps::Store(storage, lhs, ctx.Builder, ctx.Module, true);
			lhs = storage;
		}
		
		while (lhs.GetType()->IsPointer())
		{
			if (lhs.GetType()->As<PointerType>()->GetBaseType()->IsClass())
				break;
			
			lhs = SymbolOps::Load(lhs, ctx.Builder);
		}
		
		return SymbolOps::GEPStruct(lhs, memberPtrType, index, ctx.Builder);
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
			Symbol symbol = mod->Lookup(member->GetName()).value();

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

    ASTVariableDeclaration::ASTVariableDeclaration(const Token& name)
		: m_Name(name)
    {
    }

	Symbol ASTVariableDeclaration::Codegen(CodegenContext& ctx)
    {
		Symbol resolvedType = TypeResolver->Codegen(ctx);
		Symbol initializer = Initializer ? Initializer->Codegen(ctx) : Symbol();
		
        bool isGlobal = !(bool)ctx.Builder.GetInsertBlock();
			
		if (isGlobal)
		{
		
			llvm::Value* allocaInst = new llvm::GlobalVariable(
				ctx.Module, 
				resolvedType.GetType()->Get(),
				resolvedType.GetType()->IsConst(),
				llvm::GlobalValue::InternalLinkage,
				initializer.Kind != SymbolKind::None ? llvm::dyn_cast<llvm::Constant>(initializer.GetLLVMValue()) : nullptr,
				m_Name.GetData()
			);

			*Variable = Symbol::CreateValue(allocaInst, ctx.TypeReg->GetPointerTo(resolvedType.GetType()));

			if (initializer.Kind != SymbolKind::None && !llvm::dyn_cast<llvm::Constant>(initializer.GetLLVMValue()))
			{
				SymbolOps::Store(*Variable, initializer, ctx.Builder, ctx.Module, true);
			}
		}
		else
		{
			llvm::BasicBlock* insertBlock = ctx.Builder.GetInsertBlock();
        
			CLEAR_VERIFY(insertBlock, "cannot create an alloca without function");  
			auto ip = ctx.Builder.saveIP(); 
			llvm::Function* function = insertBlock->getParent();    
			ctx.Builder.SetInsertPoint(&function->getEntryBlock());
			
			llvm::Value* allocaInst = ctx.Builder.CreateAlloca(resolvedType.GetType()->Get(), nullptr, m_Name.GetData());
			*Variable = Symbol::CreateValue(allocaInst, ctx.TypeReg->GetPointerTo(resolvedType.GetType()));
				
			ctx.Builder.restoreIP(ip);

			if (initializer.Kind != SymbolKind::None)
				SymbolOps::Store(*Variable, initializer, ctx.Builder, ctx.Module, true);
		}
		

		return *Variable;
    }

	ASTVariable::ASTVariable(const Token& name)
		: m_Name(name)
    {
    }

	Symbol ASTVariable::Codegen(CodegenContext& ctx)
    {
		if (Variable->Kind == SymbolKind::Function)
			return Symbol::CreateCallee(Variable, nullptr);

		return *Variable;
	}
	
	void ASTVariable::Print()
	{
		std::print("{} ", m_Name.GetData());
	}

	ASTAssignmentOperator::ASTAssignmentOperator(AssignmentOperatorType type)
		: m_Type(type)
    {
    }

	Symbol ASTAssignmentOperator::Codegen(CodegenContext& ctx)
    {
		auto& builder = ctx.Builder;
		auto& context = ctx.Context;

		CLEAR_VERIFY(Storage && Value, "Assigment operator must have a storage and value");
	
		Symbol storage;
		
		{
			ValueRestoreGuard guard(ctx.WantAddress, true);
			storage = Storage->Codegen(ctx);
		}

		Symbol data;
		{
			ValueRestoreGuard guard(ctx.WantAddress, false);
			data    = Value->Codegen(ctx);
		}

		ValueSymbol value = data.GetValueSymbol();

		//TODO: remove should be handled by semantic analyzer
		if(value.ShouldMemcpy)
		{
			CLEAR_VERIFY(storage.GetType() == data.GetType(), "");
			Symbol size = Symbol::GetUInt64(ctx.ClearModule, ctx.Builder, data.GetType()->As<PointerType>()->GetBaseType()->GetSizeInBytes(ctx.Module));
			SymbolOps::Memcpy(storage, data, size, ctx.Builder);

			return Symbol();
		}

		//TODO: this will be handled by semantic analyzer
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
		auto& module  = ctx.Module;
		auto& context = ctx.Context;
		auto& builder = ctx.Builder;
		
		auto& functionSymbol = FunctionSymbol->GetFunctionSymbol();
		
		llvm::SmallVector<llvm::Type*> argTypes;
		std::transform(Arguments.begin(), Arguments.end(), std::back_inserter(argTypes), [](std::shared_ptr<ASTVariableDeclaration> decl)
				 {
					return decl->TypeResolver->ConstructedType.GetType()->Get();
				 });
		
		std::shared_ptr<Type> returnType = ReturnType ? ReturnType->Codegen(ctx).GetType() : nullptr;

		functionSymbol.FunctionType = llvm::FunctionType::get(returnType ? returnType->Get() : llvm::FunctionType::getVoidTy(context), argTypes, false);
		functionSymbol.FunctionPtr = llvm::Function::Create(functionSymbol.FunctionType, Linkage, m_Name, ctx.Module);

		s_InsertPoints.push(builder.saveIP());

		llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", functionSymbol.FunctionPtr);
		llvm::BasicBlock* body  = llvm::BasicBlock::Create(context, "body");
		
		builder.SetInsertPoint(entry);

		llvm::BasicBlock* returnBlock  = llvm::BasicBlock::Create(context, "return");
		llvm::AllocaInst* returnAlloca = returnType ? builder.CreateAlloca(returnType->Get(), nullptr, "return_value") : nullptr;
		
		ValueRestoreGuard guard1(ctx.ReturnType,   returnType);
		ValueRestoreGuard guard2(ctx.ReturnBlock,  returnBlock);
		ValueRestoreGuard guard3(ctx.ReturnAlloca, returnAlloca);
		ValueRestoreGuard guard4(ctx.Thrown,       ctx.Thrown);

		size_t k = 0;
		for (const auto& arg : Arguments)
		{
			Symbol argAlloc = arg->Codegen(ctx);
			Symbol argValue = Symbol::CreateValue(functionSymbol.FunctionPtr->getArg(k++), arg->TypeResolver->ConstructedType.GetType());
			SymbolOps::Store(argAlloc, argValue, ctx.Builder, ctx.Module, true);
		}

		functionSymbol.FunctionPtr->insert(functionSymbol.FunctionPtr->end(), body);
		builder.SetInsertPoint(body);

		CodeBlock->Codegen(ctx);

		auto currip = builder.saveIP();

		builder.SetInsertPoint(entry);
		builder.CreateBr(body);

		builder.restoreIP(currip);

		if(!builder.GetInsertBlock()->getTerminator())
			builder.CreateBr(returnBlock);

		functionSymbol.FunctionPtr->insert(functionSymbol.FunctionPtr->end(), returnBlock);
		builder.SetInsertPoint(returnBlock);
		

		if (functionSymbol.FunctionPtr->getReturnType()->isVoidTy())
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

		return *FunctionSymbol;
	}

	void ASTFunctionDefinition::Instantiate(FunctionInstance& functionData, CodegenContext& ctx)
    {
		PushScopeMarker(ctx);
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
		functionDefinition->Arguments = Arguments;
		functionDefinition->CodeBlock = CodeBlock;
		functionDefinition->ReturnType = ReturnType;

		return functionDefinition;
	}

	Symbol ASTFunctionCall::Codegen(CodegenContext& ctx)
	{
		std::vector<llvm::Value*> args;
		std::vector<std::shared_ptr<Type>> types;
		
		CalleeSymbol calleeSymbol = Callee->Codegen(ctx).GetCalleeSymbol();
		FunctionSymbol& functionSymbol = calleeSymbol.FunctionSymbol->GetFunctionSymbol();

		if (calleeSymbol.Receiver)
		{
			args.push_back(calleeSymbol.Receiver->GetLLVMValue());
			types.push_back(calleeSymbol.Receiver->GetType());
		}

		BuildArgs(ctx, args, types);

		if (!functionSymbol.FunctionPtr)
		{
			CodegenContext contextFromOther = functionSymbol.FunctionNode->SourceModule->GetCodegenContext();
			functionSymbol.FunctionNode->Codegen(contextFromOther);
		}	
	
		llvm::Function* functionPtr = functionSymbol.FunctionPtr;
		llvm::FunctionType* functionType = functionSymbol.FunctionType;

		if (functionSymbol.FunctionNode->SourceModule != ctx.ClearModule)
		{
			functionPtr = ctx.Module.getFunction(functionSymbol.FunctionNode->GetName());
			
			if (!functionPtr)
			{
				functionPtr = llvm::Function::Create(
					functionType,
					llvm::Function::ExternalLinkage,
					functionSymbol.FunctionNode->GetName(),
					ctx.Module
				);
		}
		}

		llvm::Value* returnValue = ctx.Builder.CreateCall(functionPtr, args);

		if (!functionSymbol.FunctionNode->ReturnType)
			return Symbol();
		
		return Symbol::CreateValue(returnValue, functionSymbol.FunctionNode->ReturnType->ConstructedType.GetType());
	}



    void ASTFunctionCall::BuildArgs(CodegenContext& ctx, std::vector<llvm::Value*>& args, std::vector<std::shared_ptr<Type>>& types)
    {
		ValueRestoreGuard guard(ctx.WantAddress, false);
		
		for (auto& child : Arguments)	
		{
			Symbol gen = child->Codegen(ctx);

			for (auto value : gen.GetValueTuple().Values)
			{
				args.push_back(value);
			}

			for (auto type : gen.GetValueTuple().Types)
			{
				types.push_back(type);
			}
		}
    }

	std::shared_ptr<ASTBinaryExpression> ASTFunctionCall::IsMemberFunction()
	{
		if (auto memberAccess = std::dynamic_pointer_cast<ASTBinaryExpression>(Callee); memberAccess && memberAccess->GetExpression() == OperatorType::Dot)
			return memberAccess;
		
		return nullptr;
	}

    ASTFunctionDeclaration::ASTFunctionDeclaration(const std::string& name)
		: m_Name(name)
    {
    }

	Symbol ASTFunctionDeclaration::Codegen(CodegenContext& ctx)
	{
		auto& module = ctx.Module;
		std::vector<llvm::Type*> types;

		for (auto arg : Arguments)
		{
			auto param = arg->Codegen(ctx);
			m_Parameters.push_back({ .Name = std::string(param.Metadata.value_or(String())), .Type = param.GetType(), .IsVariadic = arg->IsVariadic });
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

		m_ReturnType = ctx.ClearModule->Lookup("void").value().GetType();

		if (ReturnType)
			m_ReturnType = ReturnType->Codegen(ctx).GetType();

		if(InsertDecleration)
		{
			llvm::FunctionType* functionType = llvm::FunctionType::get(m_ReturnType->Get(), types, isVariadic);
			llvm::FunctionCallee callee = module.getOrInsertFunction(m_Name, functionType);
			
			*DeclSymbol = Symbol::CreateFunction(nullptr);
			auto& funcSymbol = DeclSymbol->GetFunctionSymbol();
			
			funcSymbol.FunctionPtr = llvm::dyn_cast<llvm::Function>(callee.getCallee());
			funcSymbol.FunctionType = functionType;
			funcSymbol.FunctionNode = std::make_shared<ASTFunctionDefinition>(m_Name);
			funcSymbol.FunctionNode->SourceModule = ctx.ClearModule;
			funcSymbol.FunctionNode->ReturnType = std::make_shared<ASTType>();
			funcSymbol.FunctionNode->ReturnType->ConstructedType = Symbol::CreateType(m_ReturnType);	
			
			return *DeclSymbol;
		}

		return {};
	}	

    Symbol ASTExpression::Codegen(CodegenContext& ctx)
	{
		CLEAR_VERIFY(RootExpr, "root expr cannot be null");
		return RootExpr->Codegen(ctx);
	}


	std::shared_ptr<ASTExpression> ASTExpression::AssembleFromRPN(llvm::ArrayRef<std::shared_ptr<ASTNodeBase>> nodes)
	{
		llvm::SmallVector<std::shared_ptr<ASTNodeBase>> stack;

		auto IsOperand = [](std::shared_ptr<ASTNodeBase> child) 
		{
			return child->GetType() == ASTNodeType::Literal || 
				   child->GetType() == ASTNodeType::Variable ||
				   child->GetType() == ASTNodeType::Member || 
				   child->GetType() == ASTNodeType::ListExpr || 
				   child->GetType() == ASTNodeType::StructExpr || 
				   child->GetType() == ASTNodeType::TypeResolver;
		};
		
		for (auto child : nodes)
		{
			if (IsOperand(child))
			{
				stack.push_back(child);
				continue;
			}

			if (std::shared_ptr<ASTUnaryExpression> unaryExpression = std::dynamic_pointer_cast<ASTUnaryExpression>(child))
			{
				unaryExpression->Operand = stack.back();
				stack.pop_back();

				stack.push_back(unaryExpression);
				continue;
			}
			else if (std::shared_ptr<ASTFunctionCall> functionCall = std::dynamic_pointer_cast<ASTFunctionCall>(child))
			{
				functionCall->Callee = stack.back();
				stack.pop_back();

				stack.push_back(functionCall);
			}
			else if (std::shared_ptr<ASTBinaryExpression> binExp = std::dynamic_pointer_cast<ASTBinaryExpression>(child))
			{
				binExp->RightSide = stack.back();
				stack.pop_back();

				binExp->LeftSide = stack.back();
				stack.pop_back();

				stack.push_back(binExp);
			}
			else if (std::shared_ptr<ASTTernaryExpression> ternaryExpr = std::dynamic_pointer_cast<ASTTernaryExpression>(child))
			{
				ternaryExpr->Falsy = stack.back();
				stack.pop_back();

				ternaryExpr->Truthy = stack.back();
				stack.pop_back();

				ternaryExpr->Condition = stack.back();
				stack.pop_back();

				stack.push_back(ternaryExpr);
			}
			else 
			{
				CLEAR_UNREACHABLE("unimplemented");
			}			
		}

		CLEAR_VERIFY(stack.size() == 1, "wot");

		std::shared_ptr<ASTExpression> expr = std::make_shared<ASTExpression>();
		expr->RootExpr = stack.back();
		return expr;
	}

	Symbol ASTListExpr::Codegen(CodegenContext& ctx)
	{
		if(Values.size() == 0)
		{
			return Symbol();
		}

		// collect all the values

		ValueRestoreGuard guard(ctx.WantAddress, false);

		Symbol first = Values[0]->Codegen(ctx);
		
		llvm::SmallVector<llvm::Value*> values;

		values.push_back(first.GetLLVMValue());

		for(size_t i = 1; i < Values.size(); i++)
		{
			Symbol value = Values[i]->Codegen(ctx);
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
		Symbol ty = TargetType->Codegen(ctx);

		std::shared_ptr<ClassType> structTy = nullptr;

		llvm::SmallVector<llvm::Value*> values;
		llvm::SmallVector<std::shared_ptr<Type>> types;

		ValueRestoreGuard guard(ctx.WantAddress, false);

		for (auto value : Values)
		{
			Symbol valueSymbol = value->Codegen(ctx);
			values.push_back(valueSymbol.GetLLVMValue());
			types.push_back(valueSymbol.GetType());
		}		

		switch (ty.Kind) 
		{
			case SymbolKind::Type: 
			{
				structTy = ty.GetType()->As<ClassType>();

				for(size_t i = 0; i < values.size(); i++)
				{
					auto baseTy = *structTy->GetMemberValueByIndex(i).value();
					Symbol value = Symbol::CreateValue(values[i], types[i]);
					values[i] = SymbolOps::Cast(value, baseTy, ctx.Builder).GetLLVMValue();
				}

				break;
			}
			case SymbolKind::ClassTemplate: 
			{
				CLEAR_UNREACHABLE("TODO");
				//structTy = ctx.TypeReg->GetTypeFromClassTemplate(ty.GetClassTemplate(), ctx, types)->As<StructType>();
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

			constantValues[i] = GetDefaultValue(structTy->GetMemberValueByIndex(i).value()->GetType()->Get());
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
		
		auto ip = ctx.Builder.saveIP(); 

		llvm::Function* function = ctx.Builder.GetInsertBlock()->getParent();    

		ctx.Builder.SetInsertPoint(&function->getEntryBlock());
		llvm::Value* structAlloc = ctx.Builder.CreateAlloca(llvmStructTy, nullptr, "struct.alloc");
		ctx.Builder.restoreIP(ip);	

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
		llvm::BasicBlock* currentBlock = ctx.Builder.GetInsertBlock();

		if(currentBlock->getTerminator()) 
			return {};

		if (!ReturnValue)
		{
			EmitDefaultReturn(ctx);
			return {};
		}

		ValueRestoreGuard guard(ctx.WantAddress, false);
		Symbol codegen = ReturnValue->Codegen(ctx);

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
		CLEAR_VERIFY(Operand, "incorrect dimensions");

		if(m_Type == OperatorType::Dereference)
		{
			Symbol result;

			{
				ValueRestoreGuard guard(ctx.WantAddress, false);
				result = Operand->Codegen(ctx);
			}

			auto [resultValue, resultType] = result.GetValue();

			CLEAR_VERIFY(resultType->IsPointer(), "not a valid dereference");
			return SymbolOps::Load(result, ctx.Builder);
		}	

		CLEAR_VERIFY(!ctx.WantAddress, "Invalid use of unary expression");

		if(m_Type == OperatorType::Address)
		{		
			return Operand->Codegen(ctx);;
		}

		if(m_Type == OperatorType::Negation)
		{			
			Symbol result = Operand->Codegen(ctx);

			auto signedType = result.GetType();

			if(!signedType->IsSigned()) // uint... -> int...
			{
				signedType = ctx.ClearModule->Lookup(signedType->GetHash().substr(1)).value().GetType();
			}

			return SymbolOps::Neg(result, ctx.Builder, signedType); 
		}

		if(m_Type == OperatorType::Not)
		{
			Symbol result = Operand->Codegen(ctx);
			return SymbolOps::Not(result, ctx.Builder);
		}
		
		Symbol one = Symbol::CreateValue(ctx.Builder.getInt32(1), ctx.ClearModule->Lookup("int32").value().GetType());

		Symbol result = Operand->Codegen(ctx);
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

	Symbol ASTLoad::Codegen(CodegenContext& ctx)
	{
		Symbol operand = Operand->Codegen(ctx);
		return SymbolOps::Load(operand, ctx.Builder);
	}

	Symbol ASTIfExpression::Codegen(CodegenContext& ctx)
	{
		llvm::Function* function = ctx.Builder.GetInsertBlock()->getParent();

		struct Branch
		{
			llvm::BasicBlock* ConditionBlock = nullptr;
			llvm::BasicBlock* BodyBlock  = nullptr;
			int64_t ExpressionIdx = 0;
		};

		std::vector<Branch> branches;

		for (size_t i = 0; i < ConditionalBlocks.size(); i++)
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
				condition = ConditionalBlocks[i].Condition->Codegen(ctx);
			}

			auto [conditionValue, conditionType] = condition.GetValue();
			//TODO: sema pass should insert casts, these casts should be handled by sema pass

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
			
			ConditionalBlocks[i].CodeBlock->Codegen(ctx);
			
			if (!ctx.Builder.GetInsertBlock()->getTerminator())
				ctx.Builder.CreateBr(mergeBlock);
		}

		function->insert(function->end(), elseBlock);
		ctx.Builder.SetInsertPoint(elseBlock);


		if (ElseBlock)
			ElseBlock->Codegen(ctx);
	
		if (!ctx.Builder.GetInsertBlock()->getTerminator())
			ctx.Builder.CreateBr(mergeBlock);

		function->insert(function->end(), mergeBlock);
		ctx.Builder.SetInsertPoint(mergeBlock);

		return {};
	}

    ASTWhileExpression::ASTWhileExpression()
    {
    }
	
    Symbol ASTWhileExpression::Codegen(CodegenContext& ctx)
    {
		PushScopeMarker(ctx);

		CLEAR_VERIFY(WhileBlock.Condition && WhileBlock.CodeBlock, "Cannot have null values here");

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
			condition = WhileBlock.Condition->Codegen(ctx);
		}

		auto [conditionValue, conditionType] = condition.GetValue();
		
		//TODO: Move casting to semantic analyzer
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

		WhileBlock.CodeBlock->Codegen(ctx);

		if (!ctx.Builder.GetInsertBlock()->getTerminator())
		{
			ctx.Builder.CreateBr(conditionBlock);
		}
		
		function->insert(function->end(), end);
		ctx.Builder.SetInsertPoint(end);
		
		return {};
	}


	ASTTernaryExpression::ASTTernaryExpression() 
	{
	}

	Symbol ASTTernaryExpression::Codegen(CodegenContext& ctx) 
	{
		llvm::Function* function = ctx.Builder.GetInsertBlock()->getParent();

		Symbol condition = Condition->Codegen(ctx);

		llvm::BasicBlock* incomingBlock  = llvm::BasicBlock::Create(ctx.Context, "ternary.incoming",  function);
		llvm::BasicBlock* falseBlock = llvm::BasicBlock::Create(ctx.Context, "ternary.false", function);
		llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(ctx.Context, "ternary.merge", function);

		Symbol trueValue, falseValue;

		Symbol trueType = Symbol::GetBooleanType(ctx.ClearModule); 
		condition = SymbolOps::Cast(condition, trueType, ctx.Builder);

		ctx.Builder.CreateCondBr(condition.GetLLVMValue(), incomingBlock, falseBlock);
		ctx.Builder.SetInsertPoint(incomingBlock);

		trueValue  = Truthy->Codegen(ctx);
		auto ip = ctx.Builder.saveIP();

		ctx.Builder.CreateBr(mergeBlock);
		incomingBlock = ctx.Builder.GetInsertBlock();

		ctx.Builder.SetInsertPoint(falseBlock);
		
		falseValue = Falsy->Codegen(ctx);

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

    ASTForExpression::ASTForExpression(const std::string& name)
		: m_Name(name)
    {
    }

    Symbol ASTForExpression::Codegen(CodegenContext& ctx)
    {
		Symbol iterator;

		{
			ValueRestoreGuard guard(ctx.WantAddress, true);
			iterator = Iterator->Codegen(ctx);
		}	

		auto tbl = GetSymbolTable();

		auto [iterValue, iterType] = iterator.GetValue();

		if(iterType->IsVariadic())
		{
			auto& args = tbl->GetVariadicArguments();

			for(size_t i = 0; i < args.size(); i++)
			{
				tbl->TrackAllocation(m_Name, args[i]);
				CodeBlock->Codegen(ctx);
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
		// // create the struct type
		// auto structTy = ctx.TypeReg->CreateType<StructType>(m_Name, m_Name, ctx.Context);
		// std::vector<std::pair<std::string, std::shared_ptr<Type>>> members;
		//
		// for (auto value : Members)
		// {
		// 	Symbol result = value->Codegen(ctx);
		// 	members.emplace_back(std::string(result.Metadata.value_or(String())), result.GetType());
		// }
		//
		// structTy->SetBody(members);
		//
		// // set its default values
		//
		// ValueRestoreGuard guard(ctx.WantAddress, false);
		//
		// size_t i = 0;
		// for(const auto& [memberName, memberType] : members)
		// {
		// 	if(!DefaultValues[i])
		// 	{
		// 		i++;
		// 		structTy->AddDefaultValue(memberName, llvm::Constant::getNullValue(memberType->Get()));
		// 		continue;
		// 	}
		//
		// 	Symbol result = DefaultValues[i++]->Codegen(ctx);
		//
		// 	auto [resultValue, resultType] = result.GetValue();
		// 	resultValue = TypeCasting::Cast(resultValue, resultType, memberType, ctx.Builder);
		// 	structTy->AddDefaultValue(memberName, resultValue);
		// }
		//
		// return Symbol::CreateType(structTy);
		CLEAR_UNREACHABLE("TODO MOVE INTO CLASS NODE");	
		return Symbol();
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
		CLEAR_VERIFY(Value, "invalid argument");
		ValueRestoreGuard guard(ctx.WantAddress, false);

        return Value->Codegen(ctx);
    }
    
	ASTClass::ASTClass(const std::string& name)
		: m_Name(name)
    {
    }

    Symbol ASTClass::Codegen(CodegenContext& ctx)
    {
		for (auto func : MemberFunctions)
		{
			func->Codegen(ctx);
		}
	
		return Symbol::CreateType(ClassTy);
   }
	
	//TODO: type substitution should be handled by sema, in future node will be deep copied before instantiation
	void ASTClass::Instantiate(CodegenContext& ctx, llvm::ArrayRef<std::shared_ptr<Type>> aliasTypes)
	{
		CLEAR_UNREACHABLE("TODO move this into sema");
	}

    ASTTrait::ASTTrait(const std::string& name)
		: m_Name(name)
    {
    }

    Symbol ASTTrait::Codegen(CodegenContext& ctx)
    {
		std::vector<std::string> functions;
        std::vector<std::pair<std::string, std::shared_ptr<Type>>> members; 

		for (auto child : VariableDeclarations)
			members.emplace_back(child->GetName().GetData(), child->GetResolvedType());
		
		for (auto child : FunctionDeclarations)
		{
			child->InsertDecleration = false;
			child->Codegen(ctx);
			functions.push_back(FunctionCache::GetMangledName(child->GetName(), 
															  child->GetParameters(), 
															  child->GetReturnType()));
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
		auto tbl = GetSymbolTable();

		CLEAR_VERIFY(Storage, "invalid node");

		ValueRestoreGuard guard(ctx.WantAddress, true);
		Symbol variable = Storage->Codegen(ctx);
		
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
    }

    ASTEnum::ASTEnum(const std::string& enumName, const std::vector<std::string>& names)
		: m_Names(names), m_EnumName(enumName)
    {
    }

    Symbol ASTEnum::Codegen(CodegenContext& ctx)
    {
		std::shared_ptr<EnumType> type = std::make_shared<EnumType>(ctx.TypeReg->GetType("int64"), m_EnumName);

		int64_t previous = 0;

		for(size_t i = 0; i < m_Names.size(); i++)
		{
			CLEAR_VERIFY(i < EnumValues.size(), "invalid enum node");

			if(!EnumValues[i])
			{
				type->InsertEnumValue(m_Names[i], ++previous);
				continue;
			}

			Symbol result = EnumValues[i]->Codegen(ctx);

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
		ctx.DeferredCalls.push_back(Expr);
        return Symbol();
    }

    ASTType::ASTType(const std::vector<Token>& tokens)
		: m_Tokens(tokens)
    {
    }

    Symbol ASTType::Codegen(CodegenContext& ctx)
    {
		return ConstructedType;
		
#if OLD
		CLEAR_VERIFY(m_Tokens.size() > 0, "not a valid type resolver");

		if(Symbol inferred = Inferred(); inferred.Kind != SymbolKind::None)
		{
			return inferred;
		}

		auto& children = Children;

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
#endif   
	}



	Symbol ASTType::Inferred()
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

	std::shared_ptr<Type> ASTType::ResolveArray(CodegenContext& ctx, size_t& i, int64_t& k)
	{
		auto& children = Children;

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

	std::shared_ptr<Type> ASTType::ResolveGeneric(Symbol& symbol, CodegenContext& ctx, size_t& i, int64_t& k)
	{		
		auto& children = Children;

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
		if (!TypeResolver)
		{
			Symbol type = Symbol::CreateType(nullptr);
			type.Metadata = m_Name;

			return type;
		}

		Symbol type = TypeResolver->Codegen(ctx);
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

		// default case must be at back
		llvm::BasicBlock* continueBlock = llvm::BasicBlock::Create(ctx.Context, "switch.continue");
		llvm::BasicBlock* defaultCase   = llvm::BasicBlock::Create(ctx.Context, "switch.default");

		auto savedIp = ctx.Builder.saveIP();

		ctx.Builder.SetInsertPoint(defaultCase);
		
		DefaultCaseCodeBlock->Codegen(ctx);

		if(!ctx.Builder.GetInsertBlock()->getTerminator())
			ctx.Builder.CreateBr(continueBlock);

		ctx.Builder.restoreIP(savedIp);

		Symbol value = Value->Codegen(ctx);

		llvm::SwitchInst* switchStatement = ctx.Builder.CreateSwitch(value.GetLLVMValue(), defaultCase);
		function->insert(function->end(), defaultCase);

		llvm::SmallVector<llvm::Value*> values;

		for (const auto& [caseValues, codeBlock] : Cases)
		{
			llvm::BasicBlock* block = llvm::BasicBlock::Create(ctx.Context, "switch.case", function);
			ctx.Builder.SetInsertPoint(block);
			codeBlock->Codegen(ctx);
			
			if(!ctx.Builder.GetInsertBlock()->getTerminator())
				ctx.Builder.CreateBr(continueBlock);

			for(const auto value : caseValues)
			{
				llvm::ConstantInt* casted = llvm::dyn_cast<llvm::ConstantInt>(value->Codegen(ctx).GetLLVMValue());
				CLEAR_VERIFY(casted, "not a constant int!");

				switchStatement->addCase(casted, block);
			}
		}

		function->insert(function->end(), continueBlock);
		ctx.Builder.SetInsertPoint(continueBlock);

		return Symbol();
	}
}

