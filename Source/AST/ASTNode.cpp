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
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/Intrinsics.h>
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

	static Symbol CreateAlloca(std::shared_ptr<Type> type, CodegenContext& ctx)
	{
		llvm::BasicBlock* insertBlock = ctx.Builder.GetInsertBlock();
        
        CLEAR_VERIFY(insertBlock, "cannot create an alloca without function");  
	    auto ip = ctx.Builder.saveIP(); 
	    llvm::Function* function = insertBlock->getParent();    
		llvm::BasicBlock& entry = function->getEntryBlock();

		// keep every alloca at the very top of the entry block so LLVM can promote them to registers
	    ctx.Builder.SetInsertPoint(&entry, entry.getFirstInsertionPt());
		
		Symbol symbol = Symbol::CreateValue(ctx.Builder.CreateAlloca(type->Get(), nullptr, "alloca"), ctx.ClearModule->GetTypeRegistry()->GetPointerTo(type)); 

	    ctx.Builder.restoreIP(ip);  
		return symbol;
	}

	static Symbol UseGlobalHere(const Symbol& symbol, CodegenContext& ctx);

    ASTNodeBase::ASTNodeBase()
    {
    }

	Token GetNodeLocation(const std::shared_ptr<ASTNodeBase>& node)
	{
		if (!node)
			return Token();

		if (!node->Location.GetSourceFile().empty())
			return node->Location;

		switch (node->GetType())
		{
			case ASTNodeType::Load:				return GetNodeLocation(std::dynamic_pointer_cast<ASTLoad>(node)->Operand);
			case ASTNodeType::UnaryExpression:	return GetNodeLocation(std::dynamic_pointer_cast<ASTUnaryExpression>(node)->Operand);
			case ASTNodeType::BinaryExpression:	return GetNodeLocation(std::dynamic_pointer_cast<ASTBinaryExpression>(node)->LeftSide);
			case ASTNodeType::FunctionCall:		return GetNodeLocation(std::dynamic_pointer_cast<ASTFunctionCall>(node)->Callee);
			case ASTNodeType::CastExpr:			return GetNodeLocation(std::dynamic_pointer_cast<ASTCastExpr>(node)->Object);
			case ASTNodeType::Subscript:		return GetNodeLocation(std::dynamic_pointer_cast<ASTSubscript>(node)->Target);
			case ASTNodeType::StructExpr:		return GetNodeLocation(std::dynamic_pointer_cast<ASTStructExpr>(node)->TargetType);
			case ASTNodeType::Variable:			return std::dynamic_pointer_cast<ASTVariable>(node)->GetName();
			case ASTNodeType::Literal:			return std::dynamic_pointer_cast<ASTNodeLiteral>(node)->GetData();
			default:							break;
		}

		return Token();
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
		bool inFunction = ctx.Builder.GetInsertBlock() != nullptr;

		if (!inFunction)
		{
			// top level: external declarations, then globals (their initializers may call anything), then the rest
			auto emitWhere = [&](auto predicate)
			{
				for (auto& child : Children)
				{
					if (child && predicate(child->GetType()))
						child->Codegen(ctx);
				}
			};

			emitWhere([](ASTNodeType type) { return type == ASTNodeType::FunctionDecleration; });
			emitWhere([](ASTNodeType type) { return type == ASTNodeType::VariableDecleration; });
			emitWhere([](ASTNodeType type) { return type != ASTNodeType::FunctionDecleration && type != ASTNodeType::VariableDecleration; });

			return Symbol();
		}

		ctx.Defers->emplace_back();

		for (auto child : Children)
		{
			// code after return/break/continue is unreachable, emitting it would produce invalid IR
			if (llvm::BasicBlock* block = ctx.Builder.GetInsertBlock(); block && block->getTerminator())
				break;

			child->Codegen(ctx);
		}

		if (inFunction)
		{
			// falling off the end of the block runs its defers (return/break/continue already ran them)
			if (llvm::BasicBlock* block = ctx.Builder.GetInsertBlock(); block && !block->getTerminator())
				EmitDefers(ctx, ctx.Defers->size() - 1);

			ctx.Defers->pop_back();
		}

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

		if (m_Expression == OperatorType::Power)
			return HandlePower(leftChild, rightChild, ctx);

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

	// + - * / % with the run-time checks the build asks for: signed overflow, division by zero
	static Symbol Arithmetic(Symbol lhs, Symbol rhs, OperatorType op, CodegenContext& ctx, const Token& location)
	{
		if (lhs.GetType()->IsPointer())
			return ASTBinaryExpression::HandlePointerArithmetic(lhs, rhs, op, ctx);

		SymbolOps::Promote(lhs, rhs, ctx.Builder);

		auto type = lhs.GetType();
		llvm::Value* left = lhs.GetLLVMValue();
		llvm::Value* right = rhs.GetLLVMValue();
		bool isInteger = left->getType()->isIntegerTy() && !left->getType()->isIntegerTy(1);

		if (ctx.RuntimeChecks && isInteger)
		{
			if (op == OperatorType::Div || op == OperatorType::Mod)
			{
				EmitCheck(ctx, ctx.Builder.CreateICmpNE(right, llvm::ConstantInt::get(right->getType(), 0)), "division by zero", location);

				// INT_MIN / -1 does not fit either
				if (type->IsSigned())
				{
					unsigned bits = left->getType()->getIntegerBitWidth();
					llvm::Value* isMin = ctx.Builder.CreateICmpEQ(left, llvm::ConstantInt::get(left->getType(), llvm::APInt::getSignedMinValue(bits)));
					llvm::Value* isMinusOne = ctx.Builder.CreateICmpEQ(right, llvm::ConstantInt::get(right->getType(), -1, true));
					EmitCheck(ctx, ctx.Builder.CreateNot(ctx.Builder.CreateAnd(isMin, isMinusOne)), "integer overflow in division", location);
				}
			}
			else if (type->IsSigned() && (op == OperatorType::Add || op == OperatorType::Sub || op == OperatorType::Mul))
			{
				llvm::Intrinsic::ID id = op == OperatorType::Add ? llvm::Intrinsic::sadd_with_overflow 
									   : op == OperatorType::Sub ? llvm::Intrinsic::ssub_with_overflow : llvm::Intrinsic::smul_with_overflow;

				llvm::Value* pair = ctx.Builder.CreateBinaryIntrinsic(id, left, right);
				llvm::Value* overflowed = ctx.Builder.CreateExtractValue(pair, 1);
				EmitCheck(ctx, ctx.Builder.CreateNot(overflowed), "integer overflow", location);

				return Symbol::CreateValue(ctx.Builder.CreateExtractValue(pair, 0), type);
			}
		}

		return ASTBinaryExpression::HandleMathExpression(lhs, rhs, op, ctx);
	}

    Symbol ASTBinaryExpression::HandleMathExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx)
    {
		Symbol lhs = left->Codegen(ctx);
		Symbol rhs = right->Codegen(ctx);

        return Arithmetic(lhs, rhs, m_Expression, ctx, Location);
    }

    Symbol ASTBinaryExpression::HandleCmpExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext &ctx)
    {
		Symbol lhs = left->Codegen(ctx);
		Symbol rhs = right->Codegen(ctx);

        return HandleCmpExpression(lhs, rhs, ctx);
    }

    Symbol ASTBinaryExpression::HandlePower(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx)
    {
		Symbol lhs = left->Codegen(ctx);
		Symbol rhs = right->Codegen(ctx);
		SymbolOps::Promote(lhs, rhs, ctx.Builder);

		auto [base, type] = lhs.GetValue();
		llvm::Value* exponent = rhs.GetLLVMValue();

		if (type->IsFloatingPoint())
			return Symbol::CreateValue(ctx.Builder.CreateBinaryIntrinsic(llvm::Intrinsic::pow, base, exponent), type);

		// integers: exponentiation by squaring in a small loop (a negative exponent gives 0, like integer division)
		llvm::Function* function = ctx.Builder.GetInsertBlock()->getParent();
		llvm::Type* intType = base->getType();

		llvm::BasicBlock* before = ctx.Builder.GetInsertBlock();
		llvm::BasicBlock* loop   = llvm::BasicBlock::Create(ctx.Context, "pow.loop", function);
		llvm::BasicBlock* done   = llvm::BasicBlock::Create(ctx.Context, "pow.done", function);

		llvm::Value* negative = type->IsSigned() ? ctx.Builder.CreateICmpSLT(exponent, llvm::ConstantInt::get(intType, 0)) : ctx.Builder.getFalse();
		llvm::Value* start = ctx.Builder.CreateSelect(negative, llvm::ConstantInt::get(intType, 0), exponent);
		ctx.Builder.CreateBr(loop);

		ctx.Builder.SetInsertPoint(loop);
		llvm::PHINode* result = ctx.Builder.CreatePHI(intType, 2, "pow.result");
		llvm::PHINode* factor = ctx.Builder.CreatePHI(intType, 2, "pow.factor");
		llvm::PHINode* remaining = ctx.Builder.CreatePHI(intType, 2, "pow.remaining");

		result->addIncoming(llvm::ConstantInt::get(intType, 1), before);
		factor->addIncoming(base, before);
		remaining->addIncoming(start, before);

		llvm::Value* isOdd = ctx.Builder.CreateTrunc(remaining, ctx.Builder.getInt1Ty());
		llvm::Value* nextResult = ctx.Builder.CreateSelect(isOdd, ctx.Builder.CreateMul(result, factor), result);
		llvm::Value* nextFactor = ctx.Builder.CreateMul(factor, factor);
		llvm::Value* nextRemaining = ctx.Builder.CreateLShr(remaining, 1);

		llvm::BasicBlock* loopEnd = ctx.Builder.GetInsertBlock();
		result->addIncoming(nextResult, loopEnd);
		factor->addIncoming(nextFactor, loopEnd);
		remaining->addIncoming(nextRemaining, loopEnd);

		llvm::Value* finished = ctx.Builder.CreateICmpEQ(nextRemaining, llvm::ConstantInt::get(intType, 0));
		ctx.Builder.CreateCondBr(finished, done, loop);

		ctx.Builder.SetInsertPoint(done);
		llvm::PHINode* value = ctx.Builder.CreatePHI(intType, 1);
		value->addIncoming(nextResult, loopEnd);

		// a negative exponent skips straight to 0
		llvm::Value* finalValue = ctx.Builder.CreateSelect(negative, llvm::ConstantInt::get(intType, 0), value);
		return Symbol::CreateValue(finalValue, type);
    }

    Symbol ASTBinaryExpression::HandleCmpExpression(Symbol& lhs, Symbol& rhs, CodegenContext& ctx)
    {
		auto booleanType = ctx.ClearModule->Lookup("bool").value()->GetType();

		// str values compare their contents, unless one side is null
		bool isStr = lhs.GetType()->GetHash() == "str" || rhs.GetType()->GetHash() == "str";
		bool isNull = llvm::isa<llvm::ConstantPointerNull>(lhs.GetLLVMValue()) || llvm::isa<llvm::ConstantPointerNull>(rhs.GetLLVMValue());

		if (isStr && !isNull && lhs.GetLLVMValue()->getType()->isPointerTy() && rhs.GetLLVMValue()->getType()->isPointerTy())
		{
			llvm::FunctionCallee strcmp = ctx.Module.getOrInsertFunction("strcmp", llvm::FunctionType::get(ctx.Builder.getInt32Ty(), { ctx.Builder.getPtrTy(), ctx.Builder.getPtrTy() }, false));
			llvm::Value* order = ctx.Builder.CreateCall(strcmp, { lhs.GetLLVMValue(), rhs.GetLLVMValue() }, "strcmp");
			llvm::Value* zero = ctx.Builder.getInt32(0);
			llvm::Value* result = nullptr;

			switch (m_Expression)
			{
				case OperatorType::IsEqual:          result = ctx.Builder.CreateICmpEQ(order, zero); break;
				case OperatorType::NotEqual:         result = ctx.Builder.CreateICmpNE(order, zero); break;
				case OperatorType::LessThan:         result = ctx.Builder.CreateICmpSLT(order, zero); break;
				case OperatorType::LessThanEqual:    result = ctx.Builder.CreateICmpSLE(order, zero); break;
				case OperatorType::GreaterThan:      result = ctx.Builder.CreateICmpSGT(order, zero); break;
				case OperatorType::GreaterThanEqual: result = ctx.Builder.CreateICmpSGE(order, zero); break;
				default: break;
			}

			return Symbol::CreateValue(result, booleanType);
		}

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
    	Symbol lhs = left->Codegen(ctx);

    	// right hand side we always want a value

    	Symbol rhs;
		rhs = right->Codegen(ctx);

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

		Symbol lhs = left->Codegen(ctx);

		auto [lhsValue, lhsType] = lhs.GetValue();

		lhsValue = TypeCasting::Cast(lhsValue, lhsType, Symbol::GetBooleanType(ctx.ClearModule).GetType(), ctx.Builder);
		lhsType  = Symbol::GetBooleanType(ctx.ClearModule).GetType();


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

		rhsValue = TypeCasting::Cast(rhsValue, rhsType, Symbol::GetBooleanType(ctx.ClearModule).GetType(), ctx.Builder);
		rhsType  = Symbol::GetBooleanType(ctx.ClearModule).GetType();
		
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
		lhs = left->Codegen(ctx);

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

		// a temporary (e.g. `Pair { 1, 2 }.sum()`) needs a home in memory before it can be addressed
		if (lhs.GetType()->IsClass())
		{
			Symbol storage = CreateAlloca(lhsType, ctx);
			SymbolOps::Store(storage, lhs, ctx.Builder, ctx.Module, true);
			lhs = storage;
		}

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

		bool throughPointer = false;

		while (lhs.GetType()->IsPointer())
		{
			if (lhs.GetType()->As<PointerType>()->GetBaseType()->IsClass())
				break;
			
			lhs = SymbolOps::Load(lhs, ctx.Builder);
			throughPointer = true;
		}

		// p.x where p is a pointer: p must not be null
		if (throughPointer && ctx.RuntimeChecks)
			EmitCheck(ctx, ctx.Builder.CreateIsNotNull(lhs.GetLLVMValue()), "accessing a field through a null pointer", member->GetName());
		
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
			return UseGlobalHere(*symbol, ctx);
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
        bool isGlobal = !(bool)ctx.Builder.GetInsertBlock();
			
		if (isGlobal)
		{
			llvm::Type* llvmType = resolvedType.GetType()->Get();

			// qualified by the module so files can each have their own `count`; external so other files can use it
			// (executables internalize everything after linking, so this costs nothing)
			llvm::GlobalVariable* global = new llvm::GlobalVariable(
				ctx.Module, 
				llvmType,
				/* isConstant = */ false,
				llvm::GlobalValue::ExternalLinkage,
				llvm::Constant::getNullValue(llvmType),
				std::format("{}.{}", ctx.ClearModule->GetName(), m_Name.GetData())
			);

			*Variable = Symbol::CreateValue(global, ctx.TypeReg->GetPointerTo(resolvedType.GetType()));

			if (!Initializer)
				return *Variable;

			// initializers that are not constants run before main, inside the global initializer function
			llvm::Function* init = SymbolOps::GetInitGlobalsFunction(ctx.Module);
			auto savedIp = ctx.Builder.saveIP();
			ctx.Builder.SetInsertPoint(&init->back());

			Symbol initializer = Initializer->Codegen(ctx);

			if (auto constant = llvm::dyn_cast<llvm::Constant>(initializer.GetLLVMValue()); constant && !llvm::isa<llvm::GlobalValue>(constant))
			{
				global->setInitializer(constant);
				global->setConstant(IsConst);
			}
			else
			{
				ctx.Builder.CreateStore(initializer.GetLLVMValue(), global);
			}

			ctx.Builder.restoreIP(savedIp);
			return *Variable;
		}
		else
		{
			Symbol initializer = Initializer ? Initializer->Codegen(ctx) : Symbol();

			*Variable = CreateAlloca(resolvedType.GetType(), ctx);

			if (initializer.Kind != SymbolKind::None)
				SymbolOps::Store(*Variable, initializer, ctx.Builder, ctx.Module, true);
		}
		

		return *Variable;
    }

	ASTVariable::ASTVariable(const Token& name)
		: m_Name(name)
    {
    }

	// a global defined in another module is used through a declaration in this one
	static Symbol UseGlobalHere(const Symbol& symbol, CodegenContext& ctx)
	{
		if (symbol.Kind != SymbolKind::Value)
			return symbol;

		auto global = llvm::dyn_cast_or_null<llvm::GlobalVariable>(symbol.GetLLVMValue());

		if (!global || global->getParent() == &ctx.Module)
			return symbol;

		llvm::GlobalVariable* local = ctx.Module.getNamedGlobal(global->getName());

		if (!local)
		{
			local = new llvm::GlobalVariable(ctx.Module, global->getValueType(), global->isConstant(), 
											 llvm::GlobalValue::ExternalLinkage, nullptr, global->getName());
		}

		return Symbol::CreateValue(local, symbol.GetType());
	}

	Symbol ASTVariable::Codegen(CodegenContext& ctx)
    {
		if (Variable->Kind == SymbolKind::Function)
			return Symbol::CreateCallee(Variable, nullptr);

		return UseGlobalHere(*Variable, ctx);
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
		storage = Storage->Codegen(ctx);

		Symbol data;
		data    = Value->Codegen(ctx);

		ValueSymbol value = data.GetValueSymbol();

		if(m_Type == AssignmentOperatorType::Normal || m_Type == AssignmentOperatorType::Initialize)
		{
			SymbolOps::Store(storage, data, ctx.Builder, ctx.Module, true);
			return Symbol();
		}

		Symbol loadedValue = SymbolOps::Load(storage, ctx.Builder);		

		Symbol tmp;

		if(m_Type == AssignmentOperatorType::Add)      tmp = Arithmetic(loadedValue, data, OperatorType::Add, ctx, Location);
		else if (m_Type == AssignmentOperatorType::Sub) tmp = Arithmetic(loadedValue, data, OperatorType::Sub, ctx, Location);
		else if (m_Type == AssignmentOperatorType::Mul) tmp = Arithmetic(loadedValue, data, OperatorType::Mul, ctx, Location);
		else if (m_Type == AssignmentOperatorType::Div) tmp = Arithmetic(loadedValue, data, OperatorType::Div, ctx, Location);
		else if (m_Type == AssignmentOperatorType::Mod) tmp = Arithmetic(loadedValue, data, OperatorType::Mod, ctx, Location);
		else if (m_Type == AssignmentOperatorType::BitAnd) tmp = SymbolOps::BitAnd(loadedValue, data, ctx.Builder);
		else if (m_Type == AssignmentOperatorType::BitOr)  tmp = SymbolOps::BitOr(loadedValue, data, ctx.Builder);
		else if (m_Type == AssignmentOperatorType::BitXor) tmp = SymbolOps::BitXor(loadedValue, data, ctx.Builder);
		else if (m_Type == AssignmentOperatorType::Shl)    tmp = SymbolOps::Shl(loadedValue, data, ctx.Builder);
		else if (m_Type == AssignmentOperatorType::Shr)    tmp = SymbolOps::Shr(loadedValue, data, ctx.Builder);
		else 
		{
			CLEAR_UNREACHABLE("invalid assignment type");
		}

		// the arithmetic may have been done in a wider type, store it back in the variable's own type
		Symbol baseType = Symbol::CreateType(storage.GetType()->As<PointerType>()->GetBaseType());
		tmp = SymbolOps::Cast(tmp, baseType, ctx.Builder);

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

		// a function used before its definition was generated at the first call already
		if (functionSymbol.FunctionPtr && !functionSymbol.FunctionPtr->isDeclaration())
			return *FunctionSymbol;
		
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
		ValueRestoreGuard guard4(ctx.FunctionDeferBase, ctx.Defers->size());

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

		// falling off the end (only main may do that with a return type) returns zero
		if(!builder.GetInsertBlock()->getTerminator())
		{
			if (returnAlloca)
				builder.CreateStore(llvm::Constant::getNullValue(returnAlloca->getAllocatedType()), returnAlloca);

			builder.CreateBr(returnBlock);
		}

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
		if (IsBuiltinPrint)
		{
			llvm::SmallVector<Symbol> values;

			for (auto& argument : Arguments)
				values.push_back(argument->Codegen(ctx));

			EmitBuiltinPrint(ctx, values);
			return Symbol();
		}

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

		ConvertArguments(ctx, functionType, args, types);

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

		if (!functionSymbol.FunctionNode->ReturnTypeVal)
			return Symbol();
		
		return Symbol::CreateValue(returnValue, functionSymbol.FunctionNode->ReturnTypeVal);
	}

    void ASTFunctionCall::BuildArgs(CodegenContext& ctx, std::vector<llvm::Value*>& args, std::vector<std::shared_ptr<Type>>& types)
    {
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

	void ASTFunctionCall::ConvertArguments(CodegenContext& ctx, llvm::FunctionType* functionType, std::vector<llvm::Value*>& args, std::vector<std::shared_ptr<Type>>& types)
	{
		auto registry = ctx.ClearModule;

		for (size_t i = 0; i < args.size() && i < types.size(); i++)
		{
			llvm::Type* argType = args[i]->getType();

			if (i < functionType->getNumParams())
			{
				llvm::Type* paramType = functionType->getParamType(i);

				if (argType == paramType || !types[i])
					continue;

				// find the clear type matching the parameter so signedness is respected
				std::shared_ptr<Type> target;

				if (paramType->isIntegerTy())
				{
					std::string name = paramType->isIntegerTy(1) ? "bool" : std::format("{}int{}", types[i]->IsSigned() || !types[i]->IsIntegral() ? "" : "u", paramType->getIntegerBitWidth());
					target = registry->Lookup(name).value()->GetType();
				}
				else if (paramType->isDoubleTy()) target = registry->Lookup("float64").value()->GetType();
				else if (paramType->isFloatTy())  target = registry->Lookup("float32").value()->GetType();

				if (target)
				{
					args[i] = TypeCasting::Cast(args[i], types[i], target, ctx.Builder);
					types[i] = target;
				}

				continue;
			}

			// C default argument promotions for variadic arguments: float -> double, small ints -> int
			if (argType->isFloatTy())
			{
				args[i] = ctx.Builder.CreateFPExt(args[i], ctx.Builder.getDoubleTy(), "vararg.promote");
				types[i] = registry->Lookup("float64").value()->GetType();
			}
			else if (argType->isIntegerTy() && argType->getIntegerBitWidth() < 32)
			{
				bool isSigned = types[i] && types[i]->IsSigned() && !argType->isIntegerTy(1);
				args[i] = isSigned ? ctx.Builder.CreateSExt(args[i], ctx.Builder.getInt32Ty(), "vararg.promote")
				                   : ctx.Builder.CreateZExt(args[i], ctx.Builder.getInt32Ty(), "vararg.promote");
				types[i] = registry->Lookup("int32").value()->GetType();
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
				// `current` always holds the address of the value being indexed
				Symbol current = Target->Codegen(ctx);
				auto registry = ctx.ClearModule->GetTypeRegistry();

				for (auto index : SubscriptArgs)
				{
					Symbol indexSymbol = index->Codegen(ctx);
					llvm::Value* indexValue = indexSymbol.GetLLVMValue();

					// widen to 64 bits so negative/large indices behave the same on every target
					if (indexValue->getType()->getIntegerBitWidth() < 64)
					{
						indexValue = indexSymbol.GetType()->IsSigned() ? ctx.Builder.CreateSExt(indexValue, ctx.Builder.getInt64Ty())
						                                               : ctx.Builder.CreateZExt(indexValue, ctx.Builder.getInt64Ty());
					}

					std::shared_ptr<Type> base = current.GetType()->As<PointerType>()->GetBaseType();

					if (base->IsArray())
					{
						std::shared_ptr<Type> element = base->As<ArrayType>()->GetBaseType();

						if (ctx.RuntimeChecks)
						{
							// unsigned comparison also catches negative indices
							uint64_t size = base->As<ArrayType>()->GetArraySize();
							llvm::Value* inRange = ctx.Builder.CreateICmpULT(indexValue, ctx.Builder.getInt64(size));
							EmitCheck(ctx, inRange, std::format("index out of range for an array of {}", size), GetNodeLocation(index));
						}

						llvm::Value* address = ctx.Builder.CreateInBoundsGEP(base->Get(), current.GetLLVMValue(), { ctx.Builder.getInt64(0), indexValue }, "index");
						current = Symbol::CreateValue(address, registry->GetPointerTo(element));
					}
					else
					{
						// indexing through a pointer: load it, then offset by the index
						Symbol pointer = SymbolOps::Load(current, ctx.Builder);
						std::shared_ptr<Type> element = base->As<PointerType>()->GetBaseType();

						if (ctx.RuntimeChecks)
							EmitCheck(ctx, ctx.Builder.CreateIsNotNull(pointer.GetLLVMValue()), "indexing a null pointer", GetNodeLocation(Target));
						llvm::Value* address = ctx.Builder.CreateInBoundsGEP(element->Get(), pointer.GetLLVMValue(), { indexValue }, "index");
						current = Symbol::CreateValue(address, registry->GetPointerTo(element));
					}
				}

				return current;
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
			
			auto& funcSymbol = DeclSymbol->GetFunctionSymbol();
			
			funcSymbol.FunctionPtr = llvm::dyn_cast<llvm::Function>(callee.getCallee());
			funcSymbol.FunctionType = functionType;

			// keep the node semantic analysis made (it knows the parameters), just complete it
			if (!funcSymbol.FunctionNode)
				funcSymbol.FunctionNode = std::make_shared<ASTFunctionDefinition>(m_Name);

			funcSymbol.FunctionNode->SetName(m_Name);
			funcSymbol.FunctionNode->SourceModule = ctx.ClearModule;
			funcSymbol.FunctionNode->ReturnTypeVal = ReturnType->Get()->isVoidTy() ? nullptr : ReturnType;
			
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
			return SymbolOps::Load(valuePtr, ctx.Builder);
		}

		// allocate array, copy from static to local alloca, assign any dynamic values

		// in the entry block, an alloca inside a loop would grow the stack on every iteration
		llvm::Value* arrayAlloc = CreateAlloca(arrayType, ctx).GetLLVMValue();

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
		return SymbolOps::Load(valuePtr, ctx.Builder);
	}

	Symbol ASTStructExpr::Codegen(CodegenContext& ctx)
	{
		Symbol ty = TargetType->Codegen(ctx);

		std::shared_ptr<ClassType> structTy = nullptr;

		llvm::SmallVector<llvm::Value*> values;
		llvm::SmallVector<std::shared_ptr<Type>> types;

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
			return SymbolOps::Load(valuePtr, ctx.Builder);
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

		Symbol valuePtr = Symbol::CreateValue(structAlloc, ctx.TypeReg->GetPointerTo(structTy));
		return SymbolOps::Load(valuePtr, ctx.Builder);
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

		CLEAR_VERIFY(codegenType->Get() == ctx.ReturnType->Get(), "return value has the wrong type");	

		// the value is computed before any defer runs, so `defer` cannot change what is returned
		ctx.Builder.CreateStore(codegenValue, ctx.ReturnAlloca);
		EmitDefers(ctx, ctx.FunctionDeferBase);
		ctx.Builder.CreateBr(ctx.ReturnBlock);

		return {};
	}



    void ASTReturn::EmitDefaultReturn(CodegenContext& ctx)
    {
		if(ctx.ReturnAlloca)
		{
			// reaching the end of main means success, like C
			llvm::Type* retType = ctx.ReturnType->Get();
    		llvm::Value* defaultVal = llvm::Constant::getNullValue(retType);
    		ctx.Builder.CreateStore(defaultVal, ctx.ReturnAlloca);
		}

		EmitDefers(ctx, ctx.FunctionDeferBase);
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
			result = Operand->Codegen(ctx);

			if (result.Kind == SymbolKind::Type)
				return Symbol::CreateType(ctx.ClearModule->GetTypeRegistry()->GetPointerTo(result.GetType()));

			if (ctx.RuntimeChecks && result.GetLLVMValue()->getType()->isPointerTy())
				EmitCheck(ctx, ctx.Builder.CreateIsNotNull(result.GetLLVMValue()), "dereferencing a null pointer", Location.GetSourceFile().empty() ? GetNodeLocation(Operand) : Location);

			if (IsStorage)
				return result; // the pointer value is the storage location

			auto [resultValue, resultType] = result.GetValue();

			CLEAR_VERIFY(resultType->IsPointer(), "not a valid dereference");
			return SymbolOps::Load(result, ctx.Builder);
		}	

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
			// logical not: compare against zero first so `not 5` is false rather than ~5
			Symbol result = Operand->Codegen(ctx);
			Symbol boolType = Symbol::GetBooleanType(ctx.ClearModule);
			Symbol asBool = SymbolOps::Cast(result, boolType, ctx.Builder);
			return SymbolOps::Not(asBool, ctx.Builder);
		}

		if(m_Type == OperatorType::BitwiseNot)
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
				valueToStore = Arithmetic(returnValue, one, type, ctx, Location);
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
			condition = ConditionalBlocks[i].Condition->Codegen(ctx);

			auto [conditionValue, conditionType] = condition.GetValue();
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
		condition = WhileBlock.Condition->Codegen(ctx);

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
    	ValueRestoreGuard guard3(ctx.LoopDeferBase,      ctx.Defers->size());

		WhileBlock.CodeBlock->Codegen(ctx);

		if (!ctx.Builder.GetInsertBlock()->getTerminator())
		{
			ctx.Builder.CreateBr(conditionBlock);
		}
		
		function->insert(function->end(), end);
		ctx.Builder.SetInsertPoint(end);
		
		return {};
	}


	Symbol ASTForExpression::Codegen(CodegenContext& ctx)
	{
		llvm::Function* function = ctx.Builder.GetInsertBlock()->getParent();
		auto registry = ctx.ClearModule->GetTypeRegistry();

		llvm::BasicBlock* conditionBlock = llvm::BasicBlock::Create(ctx.Context, "for.condition");
		llvm::BasicBlock* bodyBlock      = llvm::BasicBlock::Create(ctx.Context, "for.body");
		llvm::BasicBlock* stepBlock      = llvm::BasicBlock::Create(ctx.Context, "for.step");
		llvm::BasicBlock* endBlock       = llvm::BasicBlock::Create(ctx.Context, "for.end");

		Symbol variable = CreateAlloca(VariableType, ctx);
		*Variable = variable;

		// ranges count the variable itself, arrays count a hidden index
		Symbol counter;
		llvm::Value* limit = nullptr;
		bool isSigned = true;
		Symbol iterable;

		if (Iterable)
		{
			iterable = Iterable->Codegen(ctx);
			counter = CreateAlloca(ctx.ClearModule->Lookup("uint64").value()->GetType(), ctx);
			ctx.Builder.CreateStore(ctx.Builder.getInt64(0), counter.GetLLVMValue());
			limit = ctx.Builder.getInt64(IterableType->As<ArrayType>()->GetArraySize());
			isSigned = false;
		}
		else
		{
			Symbol start = Start->Codegen(ctx);
			ctx.Builder.CreateStore(start.GetLLVMValue(), variable.GetLLVMValue());

			// the end is evaluated once, before the first iteration
			limit = End->Codegen(ctx).GetLLVMValue();
			counter = variable;
			isSigned = VariableType->IsSigned();
		}

		ctx.Builder.CreateBr(conditionBlock);

		function->insert(function->end(), conditionBlock);
		ctx.Builder.SetInsertPoint(conditionBlock);

		llvm::Type* counterType = Iterable ? ctx.Builder.getInt64Ty() : VariableType->Get();
		llvm::Value* current = ctx.Builder.CreateLoad(counterType, counter.GetLLVMValue(), "for.counter");
		llvm::Value* keepGoing = nullptr;

		if (Inclusive)
			keepGoing = isSigned ? ctx.Builder.CreateICmpSLE(current, limit) : ctx.Builder.CreateICmpULE(current, limit);
		else
			keepGoing = isSigned ? ctx.Builder.CreateICmpSLT(current, limit) : ctx.Builder.CreateICmpULT(current, limit);

		ctx.Builder.CreateCondBr(keepGoing, bodyBlock, endBlock);

		function->insert(function->end(), bodyBlock);
		ctx.Builder.SetInsertPoint(bodyBlock);

		if (Iterable)
		{
			// copy the current element into the loop variable
			llvm::Value* index = ctx.Builder.CreateLoad(ctx.Builder.getInt64Ty(), counter.GetLLVMValue());
			llvm::Value* address = ctx.Builder.CreateInBoundsGEP(IterableType->Get(), iterable.GetLLVMValue(), { ctx.Builder.getInt64(0), index }, "for.element");
			llvm::Value* element = ctx.Builder.CreateLoad(VariableType->Get(), address);
			ctx.Builder.CreateStore(element, variable.GetLLVMValue());
		}

		{
			ValueRestoreGuard continueGuard(ctx.LoopConditionBlock, stepBlock);
			ValueRestoreGuard breakGuard(ctx.LoopEndBlock, endBlock);
			ValueRestoreGuard deferGuard(ctx.LoopDeferBase, ctx.Defers->size());

			CodeBlock->Codegen(ctx);
		}

		if (!ctx.Builder.GetInsertBlock()->getTerminator())
			ctx.Builder.CreateBr(stepBlock);

		function->insert(function->end(), stepBlock);
		ctx.Builder.SetInsertPoint(stepBlock);

		llvm::Value* value = ctx.Builder.CreateLoad(counterType, counter.GetLLVMValue());
		llvm::Value* next = ctx.Builder.CreateAdd(value, llvm::ConstantInt::get(counterType, 1), "for.next", /* NUW = */ false, /* NSW = */ isSigned);
		ctx.Builder.CreateStore(next, counter.GetLLVMValue());

		// an inclusive range ending at the type's maximum would wrap around, stop after the last value instead
		if (Inclusive)
		{
			llvm::Value* wasLast = ctx.Builder.CreateICmpEQ(value, limit);
			ctx.Builder.CreateCondBr(wasLast, endBlock, conditionBlock);
		}
		else
		{
			ctx.Builder.CreateBr(conditionBlock);
		}

		function->insert(function->end(), endBlock);
		ctx.Builder.SetInsertPoint(endBlock);

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

		auto phiNode = ctx.Builder.CreatePHI(trueValue.GetType()->Get(), 2);

		phiNode->addIncoming(trueValue.GetLLVMValue(), incomingBlock);
		phiNode->addIncoming(falseValue.GetLLVMValue(), falseBlock);

		return Symbol::CreateValue(phiNode, trueValue.GetType());
	}

	void ASTTernaryExpression::Print()
	{
		std::print("?: ");
	}

	ASTLoopControlFlow::ASTLoopControlFlow(std::string jumpTy, const Token& token)
		: m_JumpTy(jumpTy), m_Token(token)
	{
	}

	Symbol ASTLoopControlFlow::Codegen(CodegenContext& ctx) 
	{
    	CLEAR_VERIFY(ctx.LoopConditionBlock, "BREAK/CONTINUE not in loop")
		
		EmitDefers(ctx, ctx.LoopDeferBase);

    	if (m_JumpTy == "continue")
			ctx.Builder.CreateBr(ctx.LoopConditionBlock);

    	else if(m_JumpTy == "break")
    		ctx.Builder.CreateBr(ctx.LoopEndBlock);

    	return {};
    }

    Symbol ASTDefaultArgument::Codegen(CodegenContext& ctx)
    {
		CLEAR_VERIFY(Value, "invalid argument");
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

		Symbol value = Value->Codegen(ctx);
		llvm::Type* valueType = value.GetLLVMValue()->getType();

		llvm::BasicBlock* endBlock     = llvm::BasicBlock::Create(ctx.Context, "switch.end");
		llvm::BasicBlock* defaultBlock = DefaultCaseCodeBlock ? llvm::BasicBlock::Create(ctx.Context, "switch.default") : endBlock;

		llvm::SwitchInst* switchInst = ctx.Builder.CreateSwitch(value.GetLLVMValue(), defaultBlock, (unsigned)Cases.size());

		for (auto& switchCase : Cases)
		{
			llvm::BasicBlock* caseBlock = llvm::BasicBlock::Create(ctx.Context, "switch.case", function);

			for (int64_t constant : switchCase.Constants)
				switchInst->addCase(llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(valueType, constant, true)), caseBlock);

			// no fallthrough: every case jumps to the end when it finishes
			ctx.Builder.SetInsertPoint(caseBlock);
			switchCase.CodeBlock->Codegen(ctx);

			if (!ctx.Builder.GetInsertBlock()->getTerminator())
				ctx.Builder.CreateBr(endBlock);
		}

		if (DefaultCaseCodeBlock)
		{
			function->insert(function->end(), defaultBlock);
			ctx.Builder.SetInsertPoint(defaultBlock);
			DefaultCaseCodeBlock->Codegen(ctx);

			if (!ctx.Builder.GetInsertBlock()->getTerminator())
				ctx.Builder.CreateBr(endBlock);
		}

		function->insert(function->end(), endBlock);
		ctx.Builder.SetInsertPoint(endBlock);

		return Symbol();
	}

	Symbol ASTZero::Codegen(CodegenContext& ctx)
	{
		return Symbol::CreateValue(llvm::Constant::getNullValue(ValueType->Get()), ValueType);
	}

	Symbol ASTConstruct::Codegen(CodegenContext& ctx)
	{
		Symbol storage = CreateAlloca(ClassTy, ctx);
		Symbol initial = Initial->Codegen(ctx);
		SymbolOps::Store(storage, initial, ctx.Builder, ctx.Module, true);

		Self->Value = storage;
		InitCall->Codegen(ctx);

		return SymbolOps::Load(storage, ctx.Builder);
	}

	void EmitPanic(CodegenContext& ctx, const std::string& message, const Token& location, llvm::Value* detail)
	{
		auto& builder = ctx.Builder;
		llvm::FunctionCallee dprintf = ctx.Module.getOrInsertFunction("dprintf", llvm::FunctionType::get(builder.getInt32Ty(), { builder.getInt32Ty(), builder.getPtrTy() }, true));
		llvm::FunctionCallee abort = ctx.Module.getOrInsertFunction("abort", llvm::FunctionType::get(builder.getVoidTy(), false));

		std::string where = location.GetSourceFile().empty() ? std::string("unknown location") 
			: std::format("{}:{}:{}", location.GetSourceFile().filename().string(), location.LineNumber + 1, location.ColumnNumber + 1);

		std::string format = detail ? "panic: %s (%s): %s\n" : "panic: %s (%s)\n";
		llvm::SmallVector<llvm::Value*> args = { builder.getInt32(2), builder.CreateGlobalStringPtr(format), 
												 builder.CreateGlobalStringPtr(message), builder.CreateGlobalStringPtr(where) };

		if (detail)
			args.push_back(detail);

		// output printed before the panic must not be lost in stdout's buffer
		llvm::FunctionCallee fflush = ctx.Module.getOrInsertFunction("fflush", llvm::FunctionType::get(builder.getInt32Ty(), { builder.getPtrTy() }, false));
		builder.CreateCall(fflush, { llvm::ConstantPointerNull::get(builder.getPtrTy()) });

		builder.CreateCall(dprintf, args);
		llvm::CallInst* call = builder.CreateCall(abort);
		call->setDoesNotReturn();
		builder.CreateUnreachable();
	}

	void EmitCheck(CodegenContext& ctx, llvm::Value* ok, const std::string& message, const Token& location, llvm::Value* detail)
	{
		llvm::Function* function = ctx.Builder.GetInsertBlock()->getParent();
		llvm::BasicBlock* failBlock = llvm::BasicBlock::Create(ctx.Context, "check.fail", function);
		llvm::BasicBlock* okBlock = llvm::BasicBlock::Create(ctx.Context, "check.ok", function);

		// tell the optimizer the failure is rare so the happy path stays straight-line code
		llvm::MDBuilder weights(ctx.Context);
		ctx.Builder.CreateCondBr(ok, okBlock, failBlock, weights.createBranchWeights(1 << 20, 1));

		ctx.Builder.SetInsertPoint(failBlock);
		EmitPanic(ctx, message, location, detail);

		ctx.Builder.SetInsertPoint(okBlock);
	}

	Symbol ASTAssert::Codegen(CodegenContext& ctx)
	{
		// like Python's -O, asserts disappear (unevaluated) when run-time checks are off
		if (!ctx.RuntimeChecks)
			return Symbol();

		Symbol condition = Condition->Codegen(ctx);
		Symbol boolType = Symbol::GetBooleanType(ctx.ClearModule);
		condition = SymbolOps::Cast(condition, boolType, ctx.Builder);

		llvm::Value* detail = Message ? Message->Codegen(ctx).GetLLVMValue() : nullptr;
		EmitCheck(ctx, condition.GetLLVMValue(), "assertion failed", Location, detail);

		return Symbol();
	}

	Symbol ASTContains::Codegen(CodegenContext& ctx)
	{
		Symbol needle = Needle->Codegen(ctx);
		Symbol haystack = Haystack->Codegen(ctx);

		auto arrayType = ArrayTy->As<ArrayType>();
		auto elementType = arrayType->GetBaseType();
		auto boolType = ctx.ClearModule->Lookup("bool").value()->GetType();

		// a short unrolled chain of comparisons; LLVM turns small arrays into straight-line code
		llvm::Value* found = ctx.Builder.getFalse();

		for (size_t i = 0; i < arrayType->GetArraySize(); i++)
		{
			llvm::Value* address = ctx.Builder.CreateInBoundsGEP(arrayType->Get(), haystack.GetLLVMValue(), { ctx.Builder.getInt64(0), ctx.Builder.getInt64(i) });
			llvm::Value* element = ctx.Builder.CreateLoad(elementType->Get(), address);
			llvm::Value* equal = element->getType()->isFloatingPointTy() ? ctx.Builder.CreateFCmpOEQ(element, needle.GetLLVMValue()) 
																		: ctx.Builder.CreateICmpEQ(element, needle.GetLLVMValue());
			found = ctx.Builder.CreateOr(found, equal);
		}

		if (Negate)
			found = ctx.Builder.CreateNot(found);

		return Symbol::CreateValue(found, boolType);
	}

	Symbol ASTIntrinsic::Codegen(CodegenContext& ctx)
	{
		auto& builder = ctx.Builder;
		llvm::SmallVector<llvm::Value*> args;

		for (auto& argument : Arguments)
			args.push_back(argument->Codegen(ctx).GetLLVMValue());

		if (Name == "strlen")
		{
			llvm::FunctionCallee strlen = ctx.Module.getOrInsertFunction("strlen", llvm::FunctionType::get(builder.getInt64Ty(), { builder.getPtrTy() }, false));
			return Symbol::CreateValue(builder.CreateCall(strlen, args), ResultType);
		}

		if (Name == "str_contains")
		{
			llvm::FunctionCallee strstr = ctx.Module.getOrInsertFunction("strstr", llvm::FunctionType::get(builder.getPtrTy(), { builder.getPtrTy(), builder.getPtrTy() }, false));
			llvm::Value* position = builder.CreateCall(strstr, { args[1], args[0] });
			return Symbol::CreateValue(builder.CreateIsNotNull(position), ResultType);
		}

		CLEAR_UNREACHABLE("unknown intrinsic ", Name);
		return Symbol();
	}

	Symbol ASTTemporary::Codegen(CodegenContext& ctx)
	{
		Symbol value = Operand->Codegen(ctx);
		Symbol storage = CreateAlloca(ValueType, ctx);
		SymbolOps::Store(storage, value, ctx.Builder, ctx.Module, true);

		return storage;
	}

	Symbol ASTConstantValue::Codegen(CodegenContext& ctx)
	{
		return Symbol::CreateValue(llvm::ConstantInt::get(ValueType->Get(), Value, /* isSigned = */ true), ValueType);
	}

	Symbol ASTDefer::Codegen(CodegenContext& ctx)
	{
		// nothing runs now, the expression is emitted at every exit of the enclosing block
		CLEAR_VERIFY(!ctx.Defers->empty(), "defer outside of a block");
		ctx.Defers->back().push_back(Expr);

		return Symbol();
	}

	void EmitDefers(CodegenContext& ctx, size_t downTo)
	{
		auto& defers = *ctx.Defers;

		for (size_t scope = defers.size(); scope-- > downTo; )
		{
			// copy, emitting a deferred expression must not be affected by changes to the stack
			auto pending = defers[scope];

			for (auto it = pending.rbegin(); it != pending.rend(); it++)
				(*it)->Codegen(ctx);
		}
	}

	std::string ASTGenericTemplate::GetName()
	{
		switch (TemplateNode->GetType()) 
		{
			case ASTNodeType::Class: return std::dynamic_pointer_cast<ASTClass>(TemplateNode)->GetName();
			case ASTNodeType::FunctionDefinition: return std::dynamic_pointer_cast<ASTFunctionDefinition>(TemplateNode)->GetName();
			default:
				break;
		}
		
		CLEAR_UNREACHABLE("unhandled type");
		return "";
	}

	Symbol ASTCastExpr::Codegen(CodegenContext& ctx)
	{
		Symbol result = Object->Codegen(ctx);
		Symbol type = Symbol::CreateType(TargetType);
		return SymbolOps::Cast(result, type, ctx.Builder);
	}

	Symbol ASTSizeofExpr::Codegen(CodegenContext& ctx)
	{
		return Symbol::CreateValue(ctx.Builder.getInt64(Size), ctx.ClearModule->Lookup("uint64").value()->GetType());
	}

	Symbol ASTIsExpr::Codegen(CodegenContext& ctx)
	{
		return Symbol::CreateValue(ctx.Builder.getInt1(AreTypesSame), ctx.ClearModule->Lookup("bool").value()->GetType());
	}
}

