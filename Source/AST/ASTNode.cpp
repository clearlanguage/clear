#include "ASTNode.h"

#include "Core/Log.h"
#include "Core/Operator.h"
#include "Symbols/Symbol.h"
#include "Symbols/Type.h"
#include "Symbols/TypeCasting.h"
#include "Symbols/Module.h"
#include "Symbols/SymbolOperations.h"

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

	ASTBlock::ASTBlock()
	{
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

    Symbol ASTBinaryExpression::HandleMathExpression(Symbol& lhs, Symbol& rhs,  OperatorType type, CodegenContext& ctx)
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
			return HandlePointerArithmetic(lhs, rhs, m_Expression, ctx); //internally will verify correct expression type

        return HandleMathExpression(lhs, rhs, m_Expression, ctx);
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
		auto booleanType = ctx.ClearModule->Lookup("bool").value()->GetType();

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

    Symbol ASTBinaryExpression::HandlePointerArithmetic(Symbol& lhs, Symbol& rhs, OperatorType type, CodegenContext& ctx)
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

	Symbol ASTBinaryExpression::HandleMemberAccess(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx)
    {
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

				if(ty->IsClass())
				{
					std::shared_ptr<ASTVariable> member = std::dynamic_pointer_cast<ASTVariable>(right);
					CLEAR_VERIFY(member, "");
						
					std::shared_ptr<Symbol> memberSymbol = ty->As<ClassType>()->GetMember(member->GetName().GetData()).value();

					if (memberSymbol->Kind == SymbolKind::Function)
						return Symbol::CreateCallee(memberSymbol, nullptr);
					

					if (memberSymbol->Kind == SymbolKind::Type)
					{
						Symbol resPtrType = Symbol::CreateType(ctx.ClearModule->GetTypeRegistry()->GetPointerTo(memberSymbol->GetType()));
						return SymbolOps::GEPStruct(lhs, resPtrType, ty->As<ClassType>()->GetMemberValueIndex(member->GetName().GetData()).value(), ctx.Builder);
					}
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

				if(right->GetType() == ASTNodeType::Variable)
				{
					return HandleMember(lhs, right, ctx);
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
		auto member = std::dynamic_pointer_cast<ASTVariable>(right);
		auto lhsType = lhs.GetType();
		
		while (lhsType->IsPointer())
			lhsType = lhsType->As<PointerType>()->GetBaseType();

		auto memberSymbol = lhsType->As<ClassType>()->GetMember(member->GetName().GetData()).value();

		if (memberSymbol->Kind == SymbolKind::Function)
		{
			std::shared_ptr<Type> targetType = memberSymbol->GetFunctionSymbol().FunctionNode->Arguments[0]->ResolvedType;

			while(lhs.GetType() != targetType)
			{
				lhs = SymbolOps::Load(lhs, ctx.Builder);
			}
			
			return Symbol::CreateCallee(memberSymbol, std::make_shared<Symbol>(lhs));
		}
		
		auto memberPtrType = Symbol::CreateType(ctx.TypeReg->GetPointerTo(memberSymbol->GetType()));

		size_t index = lhsType->As<ClassType>()->GetMemberValueIndex(member->GetName().GetData()).value();	
		
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


    Symbol ASTBinaryExpression::HandleModuleAccess(Symbol& lhs, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx)
    {
		auto mod = lhs.GetModule();
		auto member = std::dynamic_pointer_cast<ASTVariable>(right);

		std::shared_ptr<Symbol> symbol = mod->GetExposedSymbols().at(member->GetName().GetData());

		if(symbol->Kind == SymbolKind::Type) //only one LLVMContext for now so this is fine
		{
			return *symbol;
		}
		else if (symbol->Kind == SymbolKind::Value) //variable
		{
			llvm::GlobalVariable* existingGV = ctx.Module.getNamedGlobal(member->GetName().GetData());
			Symbol value;

			if(existingGV)
			{
				value = Symbol::CreateValue(existingGV, symbol->GetType());
			}
			else 
			{
				llvm::GlobalVariable* gv = llvm::cast<llvm::GlobalVariable>(symbol->GetLLVMValue());

				llvm::GlobalVariable* decl = new llvm::GlobalVariable(
					ctx.Module,
					gv->getValueType(),
					gv->isConstant(),
					llvm::GlobalValue::ExternalLinkage,
					nullptr,
					gv->getName()
				);

				value = Symbol::CreateValue(decl, symbol->GetType());
			}

			if(ctx.WantAddress)
				return value;

			return SymbolOps::Load(value, ctx.Builder);
		}
		else if (symbol->Kind == SymbolKind::Function)
		{
			return Symbol::CreateCallee(symbol, nullptr);
		}

        return Symbol();
    }

    ASTVariableDeclaration::ASTVariableDeclaration(const Token& name)
		: m_Name(name)
    {
    }

	Symbol ASTVariableDeclaration::Codegen(CodegenContext& ctx)
    {
		Symbol resolvedType = Symbol::CreateType(ResolvedType);
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
					return decl->ResolvedType->Get();
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
			Symbol argValue = Symbol::CreateValue(functionSymbol.FunctionPtr->getArg(k++), arg->ResolvedType);
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
		
		return Symbol::CreateValue(returnValue, functionSymbol.FunctionNode->ReturnTypeVal);
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

	Symbol ASTSubscript::Codegen(CodegenContext& ctx)
	{
		switch (Meaning)
		{
			case SubscriptSemantic::ArrayIndex:
			{
				Symbol operand = Target->Codegen(ctx);
				
				llvm::SmallVector<llvm::Value*> indices;
				indices.push_back(ctx.Builder.getInt64(0));
					
				std::shared_ptr<Type> resPtrType = operand.GetType()->As<PointerType>()->GetBaseType(); 

				for (auto index : SubscriptArgs)
				{
					indices.push_back(index->Codegen(ctx).GetLLVMValue());
					resPtrType = resPtrType->IsArray() ? resPtrType->As<ArrayType>()->GetBaseType() : resPtrType->As<PointerType>()->GetBaseType();
				}

				resPtrType = ctx.ClearModule->GetTypeRegistry()->GetPointerTo(resPtrType);
			
				Symbol resPtrTypeSymbol = Symbol::CreateType(resPtrType);
				return SymbolOps::GEP(operand, resPtrTypeSymbol, indices, ctx.Builder);
			}
			case SubscriptSemantic::Generic:
			{
				return *GeneratedType; //Type is generated during semantic analysis
			}
			default:
			{
				CLEAR_UNREACHABLE("unimplemented");
				break;
			}
		}

		return Symbol();
	}

    ASTFunctionDeclaration::ASTFunctionDeclaration(const std::string& name)
		: m_Name(name)
    {
    }

	Symbol ASTFunctionDeclaration::Codegen(CodegenContext& ctx)
	{
		struct Parameter 
		{
			std::string Name;
			std::shared_ptr<Type> Type;
			bool IsVariadic;
		};

		auto& module = ctx.Module;
		llvm::SmallVector<llvm::Type*> types;
		llvm::SmallVector<Parameter> params;

		for (auto arg : Arguments)
		{
			auto param = arg->Codegen(ctx);
			params.push_back({ .Name = std::string(param.Metadata.value_or(String())), .Type = param.GetType(), .IsVariadic = arg->IsVariadic });
		} 

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

		if(InsertDecleration)
		{
			llvm::FunctionType* functionType = llvm::FunctionType::get(ReturnType->Get(), types, isVariadic);
			llvm::FunctionCallee callee = module.getOrInsertFunction(m_Name, functionType);
			
			*DeclSymbol = Symbol::CreateFunction(nullptr);
			auto& funcSymbol = DeclSymbol->GetFunctionSymbol();
			
			funcSymbol.FunctionPtr = llvm::dyn_cast<llvm::Function>(callee.getCallee());
			funcSymbol.FunctionType = functionType;
			funcSymbol.FunctionNode = std::make_shared<ASTFunctionDefinition>(m_Name);
			funcSymbol.FunctionNode->SourceModule = ctx.ClearModule;
			funcSymbol.FunctionNode->ReturnTypeVal = ReturnType;	
			
			return *DeclSymbol;
		}

		return {};
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

		std::shared_ptr<Type> arrayType = ListType;

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

			if (result.Kind == SymbolKind::Type)
				return Symbol::CreateType(ctx.ClearModule->GetTypeRegistry()->GetPointerTo(result.GetType()));

			auto [resultValue, resultType] = result.GetValue();

			CLEAR_VERIFY(resultType->IsPointer(), "not a valid dereference");
			return SymbolOps::Load(result, ctx.Builder);
		}	

		CLEAR_VERIFY(!ctx.WantAddress, "Invalid use of unary expression");

		if(m_Type == OperatorType::Address)
		{		
			return Operand->Codegen(ctx);
		}

		if(m_Type == OperatorType::Negation)
		{			
			Symbol result = Operand->Codegen(ctx);

			auto signedType = result.GetType();

			if(!signedType->IsSigned()) // uint... -> int...
			{
				signedType = ctx.ClearModule->Lookup(signedType->GetHash().substr(1)).value()->GetType();
			}

			return SymbolOps::Neg(result, ctx.Builder, signedType); 
		}

		if(m_Type == OperatorType::Not)
		{
			Symbol result = Operand->Codegen(ctx);
			return SymbolOps::Not(result, ctx.Builder);
		}
		
		Symbol one = Symbol::CreateValue(ctx.Builder.getInt32(1), ctx.ClearModule->Lookup("int32").value()->GetType());

		Symbol result = Operand->Codegen(ctx);
		auto [resultValue, resultType] = result.GetValue();

		CLEAR_VERIFY(resultType->IsPointer(), "not valid type for increment");
		
		std::shared_ptr<PointerType> ty = std::dynamic_pointer_cast<PointerType>(resultType);

		Symbol valueToStore;
		Symbol returnValue;

		auto ApplyFun = [&](OperatorType type)
		{
			if(ty->GetBaseType()->IsPointer())
				valueToStore = ASTBinaryExpression::HandlePointerArithmetic(returnValue, one, type, ctx);
			else 
				valueToStore = ASTBinaryExpression::HandleMathExpression(returnValue, one, type, ctx);
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

	ASTLoopControlFlow::ASTLoopControlFlow(std::string jumpTy)
		: m_JumpTy(jumpTy)
	{
	}

	Symbol ASTLoopControlFlow::Codegen(CodegenContext& ctx) 
	{
    	CLEAR_VERIFY(ctx.LoopConditionBlock, "BREAK/CONTINUE not in loop")
		
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
	
    Symbol ASTDefaultInitializer::Codegen(CodegenContext& ctx)
    {
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

	Symbol ASTTypeSpecifier::Codegen(CodegenContext& ctx) 
	{
		if (!TypeResolver)
		{
			Symbol type = Symbol::CreateType(nullptr);
			type.Metadata = m_Name;

			return type;
		}

		return Symbol::CreateType(ResolvedType);
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

	std::string ASTGenericTemplate::GetName()
	{
		switch (TemplateNode->GetType()) 
		{
			case ASTNodeType::Class: return std::dynamic_pointer_cast<ASTClass>(TemplateNode)->GetName();
			default:
				break;
		}
		
		CLEAR_UNREACHABLE("unhandled type");
		return "";
	}
}

