#include "Core/CrashHandler.h"
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

			NoteProgress("generating code for", child->Location.GetSourceFile(), child->Location.LineNumber, child->Location.ColumnNumber);
			Symbol result = child->Codegen(ctx);

			// `make_list()` on its own line: the value it made is cleaned up straight away
			bool fresh = child->GetType() == ASTNodeType::FunctionCall || child->GetType() == ASTNodeType::Construct || child->GetType() == ASTNodeType::StructExpr;

			if (fresh && result.Kind == SymbolKind::Value && result.GetType() && IsOwning(result.GetType()) && result.GetLLVMValue() && 
				(!result.GetLLVMValue()->getType()->isPointerTy() || std::dynamic_pointer_cast<CoroutineType>(result.GetType())))
			{
				Symbol slot = CreateAlloca(result.GetType(), ctx);
				ctx.Builder.CreateStore(result.GetLLVMValue(), slot.GetLLVMValue());
				EmitDestroy(ctx, result.GetType(), slot.GetLLVMValue());
			}
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

		// str values compare their contents: bytes first (memcmp), then the shorter one comes first
		bool isStr = lhs.GetType()->GetHash() == "str" && rhs.GetType()->GetHash() == "str";

		if (isStr)
		{
			auto& b = ctx.Builder;
			llvm::FunctionCallee memcmp = ctx.Module.getOrInsertFunction("memcmp", llvm::FunctionType::get(b.getInt32Ty(), { b.getPtrTy(), b.getPtrTy(), b.getInt64Ty() }, false));
			llvm::Value* leftLength = b.CreateExtractValue(lhs.GetLLVMValue(), 1);
			llvm::Value* rightLength = b.CreateExtractValue(rhs.GetLLVMValue(), 1);
			llvm::Value* shorter = b.CreateSelect(b.CreateICmpULT(leftLength, rightLength), leftLength, rightLength);
			llvm::Value* bytes = b.CreateCall(memcmp, { b.CreateExtractValue(lhs.GetLLVMValue(), 0), b.CreateExtractValue(rhs.GetLLVMValue(), 0), shorter }, "memcmp");
			llvm::Value* byLength = b.CreateSelect(b.CreateICmpULT(leftLength, rightLength), b.getInt32(-1), 
												   b.CreateSelect(b.CreateICmpUGT(leftLength, rightLength), b.getInt32(1), b.getInt32(0)));
			llvm::Value* order = b.CreateSelect(b.CreateICmpNE(bytes, b.getInt32(0)), bytes, byLength);
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

			// the receiver is passed as a pointer to the object (a *Dog also serves a method taking *Animal),
			// or by value when the method takes `self` by value
			auto isObjectPointer = [](const std::shared_ptr<Type>& type)
			{
				return type->IsPointer() && type->As<PointerType>()->GetBaseType() && type->As<PointerType>()->GetBaseType()->IsClass();
			};

			if (targetType && isObjectPointer(targetType))
			{
				while (!isObjectPointer(lhs.GetType()))
					lhs = SymbolOps::Load(lhs, ctx.Builder);
			}
			else
			{
				while (lhs.GetType() != targetType && lhs.GetType()->IsPointer())
					lhs = SymbolOps::Load(lhs, ctx.Builder);
			}
			
			return Symbol::CreateCallee(memberSymbol, std::make_shared<Symbol>(lhs));
		}
		
		auto memberPtrType = Symbol::CreateType(ctx.TypeReg->GetPointerTo(memberSymbol->GetType()));

		size_t index = lhsType->As<ClassType>()->GetMemberValueIndex(member->GetName().GetData()).value();
		bool isUnion = lhsType->As<ClassType>()->IsUnion;

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

		// every field of a union lives at the start of its storage
		if (isUnion)
			return Symbol::CreateValue(lhs.GetLLVMValue(), memberPtrType.GetType());
		
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

			if (IsAlias)
			{
				*Variable = Symbol::CreateValue(initializer.GetLLVMValue(), ctx.TypeReg->GetPointerTo(resolvedType.GetType()));
				return *Variable;
			}

			*Variable = CreateAlloca(resolvedType.GetType(), ctx);

			if (initializer.Kind != SymbolKind::None)
				SymbolOps::Store(*Variable, initializer, ctx.Builder, ctx.Module, true);

			// an owning local is cleaned up when its block ends, however it ends
			if (IsOwning(ResolvedType) && !ctx.Defers->empty())
			{
				auto destroy = std::make_shared<ASTDestroy>();
				destroy->Address = Variable->GetLLVMValue();
				destroy->ValueType = ResolvedType;
				ctx.Defers->back().push_back(destroy);
			}
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
			// the new value is ready (and its source emptied, if it was moved): now the old one can go
			if (DestroyOld)
				EmitDestroy(ctx, storage.GetType()->As<PointerType>()->GetBaseType(), storage.GetLLVMValue());

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

	// generators and async functions are LLVM coroutines (switch-resumed). The call allocates the frame
	// (LLVM removes the allocation when the coroutine does not outlive its caller), runs up to the first
	// suspension and returns the handle; resuming continues from the last suspension.
	// destroyed while suspended (a loop over a generator ends with break): clean up what is alive at this point,
	// as a return from here would, before the frame is freed
	static llvm::BasicBlock* AbandonBlock(CodegenContext& ctx)
	{
		auto& builder = ctx.Builder;
		auto saved = builder.saveIP();

		llvm::BasicBlock* abandon = llvm::BasicBlock::Create(ctx.Context, "coro.abandon", builder.GetInsertBlock()->getParent());
		builder.SetInsertPoint(abandon);
		EmitDefers(ctx, ctx.FunctionDeferBase);
		builder.CreateBr(ctx.Coroutine->Cleanup);

		builder.restoreIP(saved);
		return abandon;
	}

	static llvm::Value* CoroutineSuspend(CodegenContext& ctx, bool final, llvm::BasicBlock* resume)
	{
		auto& builder = ctx.Builder;
		llvm::BasicBlock* destroyed = final ? ctx.Coroutine->Cleanup : AbandonBlock(ctx);
		llvm::Value* state = builder.CreateIntrinsic(llvm::Intrinsic::coro_suspend, {}, { llvm::ConstantTokenNone::get(ctx.Context), builder.getInt1(final) });
		llvm::SwitchInst* branch = builder.CreateSwitch(state, ctx.Coroutine->Suspend, 2);
		branch->addCase(builder.getInt8(0), resume);
		branch->addCase(builder.getInt8(1), destroyed);
		return state;
	}

	void ASTFunctionDefinition::BeginCoroutine(CodegenContext& ctx, CodegenContext::CoroutineState& coroutine, llvm::BasicBlock* entry, llvm::BasicBlock* returnBlock)
	{
		auto& builder = ctx.Builder;
		llvm::Function* function = builder.GetInsertBlock()->getParent();
		function->addFnAttr(llvm::Attribute::PresplitCoroutine);

		// the promise holds what was yielded, or the task's result; the caller reads it through the handle
		if (CoroutineValue)
		{
			llvm::IRBuilder<> entryBuilder(entry, entry->getFirstInsertionPt());
			coroutine.Promise = entryBuilder.CreateAlloca(CoroutineValue->Get(), nullptr, "promise");
			coroutine.Promise->setAlignment(llvm::Align(16));
		}

		llvm::Value* nullPointer = llvm::ConstantPointerNull::get(builder.getPtrTy());
		llvm::Value* id = builder.CreateIntrinsic(llvm::Intrinsic::coro_id, {}, 
			{ builder.getInt32(16), coroutine.Promise ? (llvm::Value*)coroutine.Promise : nullPointer, nullPointer, nullPointer });

		llvm::BasicBlock* start = builder.GetInsertBlock();
		llvm::BasicBlock* allocate = llvm::BasicBlock::Create(ctx.Context, "coro.alloc", function);
		llvm::BasicBlock* begin = llvm::BasicBlock::Create(ctx.Context, "coro.begin", function);
		builder.CreateCondBr(builder.CreateIntrinsic(llvm::Intrinsic::coro_alloc, {}, { id }), allocate, begin);

		builder.SetInsertPoint(allocate);
		llvm::FunctionCallee malloc = ctx.Module.getOrInsertFunction("malloc", llvm::FunctionType::get(builder.getPtrTy(), { builder.getInt64Ty() }, false));
		llvm::Value* memory = builder.CreateCall(malloc, { builder.CreateIntrinsic(llvm::Intrinsic::coro_size, { builder.getInt64Ty() }, {}) });
		builder.CreateBr(begin);

		builder.SetInsertPoint(begin);
		llvm::PHINode* frame = builder.CreatePHI(builder.getPtrTy(), 2);
		frame->addIncoming(nullPointer, start);
		frame->addIncoming(memory, allocate);
		coroutine.Handle = builder.CreateIntrinsic(llvm::Intrinsic::coro_begin, {}, { id, frame });

		// a generator owns the value it yielded last: it starts empty, so replacing or cleaning it up is safe
		if (CoroutineKind == 1 && coroutine.Promise && IsOwning(CoroutineValue))
			builder.CreateStore(llvm::Constant::getNullValue(CoroutineValue->Get()), coroutine.Promise);

		// destroying the coroutine frees its frame; suspending returns the handle to the caller
		coroutine.Cleanup = llvm::BasicBlock::Create(ctx.Context, "coro.cleanup", function);
		coroutine.Suspend = llvm::BasicBlock::Create(ctx.Context, "coro.suspend", function);
		llvm::BasicBlock* release = llvm::BasicBlock::Create(ctx.Context, "coro.free", function);

		llvm::IRBuilder<> cleanup(coroutine.Cleanup);

		// the last value a generator yielded is cleaned up with it
		if (CoroutineKind == 1 && coroutine.Promise && IsOwning(CoroutineValue))
		{
			llvm::BasicBlock* freeFrame = llvm::BasicBlock::Create(ctx.Context, "coro.free_frame", function);
			auto saved = builder.saveIP();
			builder.SetInsertPoint(coroutine.Cleanup);
			EmitDestroy(ctx, CoroutineValue, coroutine.Promise);
			builder.CreateBr(freeFrame);
			builder.restoreIP(saved);
			cleanup.SetInsertPoint(freeFrame);
		}

		llvm::Value* toFree = cleanup.CreateIntrinsic(llvm::Intrinsic::coro_free, {}, { id, coroutine.Handle });
		cleanup.CreateCondBr(cleanup.CreateIsNotNull(toFree), release, coroutine.Suspend);

		llvm::IRBuilder<> freeing(release);
		llvm::FunctionCallee free = ctx.Module.getOrInsertFunction("free", llvm::FunctionType::get(freeing.getVoidTy(), { freeing.getPtrTy() }, false));
		freeing.CreateCall(free, { toFree });
		freeing.CreateBr(coroutine.Suspend);

		llvm::IRBuilder<> suspend(coroutine.Suspend);
		suspend.CreateIntrinsic(llvm::Intrinsic::coro_end, {}, { coroutine.Handle, suspend.getInt1(false), llvm::ConstantTokenNone::get(ctx.Context) });
		suspend.CreateRet(coroutine.Handle);

		// nothing runs until the first resume (so a generator does no work before it is iterated)
		llvm::BasicBlock* run = llvm::BasicBlock::Create(ctx.Context, "coro.start", function);
		CoroutineSuspend(ctx, false, run);
		builder.SetInsertPoint(run);

		// `return value` in a task stores the result in the promise, then reaches the final suspension
		ctx.ReturnAlloca = CoroutineKind == 2 ? coroutine.Promise : nullptr;
		ctx.ReturnType = CoroutineKind == 2 ? CoroutineValue : nullptr;
		ctx.ReturnBlock = returnBlock;
	}

	void ASTFunctionDefinition::EndCoroutine(CodegenContext& ctx, CodegenContext::CoroutineState& coroutine)
	{
		// the final suspension: done() is true from here on, resuming again is not allowed
		auto& builder = ctx.Builder;
		llvm::BasicBlock* invalid = llvm::BasicBlock::Create(ctx.Context, "coro.resumed_after_end", builder.GetInsertBlock()->getParent());
		CoroutineSuspend(ctx, true, invalid);

		builder.SetInsertPoint(invalid);
		builder.CreateUnreachable();
	}

	Symbol ASTYield::Codegen(CodegenContext& ctx)
	{
		Symbol value = Value->Codegen(ctx);

		// the previous value is replaced: clean it up first (it starts out empty)
		if (value.GetType() && IsOwning(value.GetType()))
			EmitDestroy(ctx, value.GetType(), ctx.Coroutine->Promise);

		ctx.Builder.CreateStore(value.GetLLVMValue(), ctx.Coroutine->Promise);

		llvm::BasicBlock* resume = llvm::BasicBlock::Create(ctx.Context, "yield.resume", ctx.Builder.GetInsertBlock()->getParent());
		CoroutineSuspend(ctx, false, resume);
		ctx.Builder.SetInsertPoint(resume);
		return Symbol();
	}

	Symbol ASTAwait::Codegen(CodegenContext& ctx)
	{
		auto& builder = ctx.Builder;
		llvm::Function* function = builder.GetInsertBlock()->getParent();

		if (IsPause)
		{
			llvm::BasicBlock* resume = llvm::BasicBlock::Create(ctx.Context, "pause.resume", function);
			CoroutineSuspend(ctx, false, resume);
			builder.SetInsertPoint(resume);
			return Symbol();
		}

		// run the task; each time it suspends, suspend this one too (whoever runs us decides when to continue)
		llvm::Value* task = Operand->Codegen(ctx).GetLLVMValue();

		llvm::BasicBlock* step = llvm::BasicBlock::Create(ctx.Context, "await.step", function);
		llvm::BasicBlock* wait = llvm::BasicBlock::Create(ctx.Context, "await.wait", function);
		llvm::BasicBlock* finished = llvm::BasicBlock::Create(ctx.Context, "await.done", function);
		llvm::BasicBlock* abandon = llvm::BasicBlock::Create(ctx.Context, "await.abandon", function);
		builder.CreateBr(step);

		builder.SetInsertPoint(step);
		builder.CreateIntrinsic(llvm::Intrinsic::coro_resume, {}, { task });
		builder.CreateCondBr(builder.CreateIntrinsic(llvm::Intrinsic::coro_done, {}, { task }), finished, wait);

		// destroyed while waiting: the awaited task goes too
		builder.SetInsertPoint(abandon);
		builder.CreateIntrinsic(llvm::Intrinsic::coro_destroy, {}, { task });
		builder.CreateBr(AbandonBlock(ctx));

		builder.SetInsertPoint(wait);
		llvm::Value* state = builder.CreateIntrinsic(llvm::Intrinsic::coro_suspend, {}, { llvm::ConstantTokenNone::get(ctx.Context), builder.getInt1(false) });
		llvm::SwitchInst* branch = builder.CreateSwitch(state, ctx.Coroutine->Suspend, 2);
		branch->addCase(builder.getInt8(0), step);
		branch->addCase(builder.getInt8(1), abandon);

		builder.SetInsertPoint(finished);
		llvm::Value* result = nullptr;

		if (ValueType)
		{
			llvm::Value* promise = builder.CreateIntrinsic(llvm::Intrinsic::coro_promise, {}, { task, builder.getInt32(16), builder.getInt1(false) });
			result = builder.CreateLoad(ValueType->Get(), promise, "await.result");
		}

		builder.CreateIntrinsic(llvm::Intrinsic::coro_destroy, {}, { task });

		return ValueType ? Symbol::CreateValue(result, ValueType) : Symbol();
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
		
		// the analysed return type (lambdas have no return type written out)
		std::shared_ptr<Type> returnType = ReturnTypeVal && !ReturnTypeVal->Get()->isVoidTy() ? ReturnTypeVal : nullptr;

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

		// parameters taken by value are this function's to clean up: they get a scope of their own
		ctx.Defers->emplace_back();
		struct PopFrame { CodegenContext& Ctx; ~PopFrame() { Ctx.Defers->pop_back(); } } popParameters { ctx };

		size_t k = 0;
		for (const auto& arg : Arguments)
		{
			Symbol argAlloc = arg->Codegen(ctx);
			Symbol argValue = Symbol::CreateValue(functionSymbol.FunctionPtr->getArg(k++), arg->ResolvedType);
			SymbolOps::Store(argAlloc, argValue, ctx.Builder, ctx.Module, true);
		}

		functionSymbol.FunctionPtr->insert(functionSymbol.FunctionPtr->end(), body);
		builder.SetInsertPoint(body);

		CodegenContext::CoroutineState coroutine;
		ValueRestoreGuard guard5(ctx.Coroutine, CoroutineKind ? &coroutine : nullptr);

		if (CoroutineKind)
			BeginCoroutine(ctx, coroutine, entry, returnBlock);

		CodeBlock->Codegen(ctx);

		auto currip = builder.saveIP();

		builder.SetInsertPoint(entry);
		builder.CreateBr(body);

		builder.restoreIP(currip);

		if (CoroutineKind)
		{
			if (!builder.GetInsertBlock()->getTerminator())
			{
				EmitDefers(ctx, ctx.FunctionDeferBase);
				builder.CreateBr(returnBlock);
			}

			functionSymbol.FunctionPtr->insert(functionSymbol.FunctionPtr->end(), returnBlock);
			builder.SetInsertPoint(returnBlock);
			EndCoroutine(ctx, coroutine);

			auto& ip = s_InsertPoints.top();
			builder.restoreIP(ip);
			s_InsertPoints.pop();
			return *FunctionSymbol;
		}

		// falling off the end (only main may do that with a return type) returns zero
		if(!builder.GetInsertBlock()->getTerminator())
		{
			EmitDefers(ctx, ctx.FunctionDeferBase);

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

	// the function behind a symbol, generated on first use and declared in this module if it lives in another
	llvm::Function* GetFunctionHere(std::shared_ptr<Symbol> symbol, CodegenContext& ctx)
	{
		FunctionSymbol& functionSymbol = symbol->GetFunctionSymbol();

		if (!functionSymbol.FunctionPtr)
		{
			CodegenContext contextFromOther = functionSymbol.FunctionNode->SourceModule->GetCodegenContext();
			functionSymbol.FunctionNode->Codegen(contextFromOther);
		}

		if (functionSymbol.FunctionNode->SourceModule == ctx.ClearModule)
			return functionSymbol.FunctionPtr;

		llvm::Function* local = ctx.Module.getFunction(functionSymbol.FunctionNode->GetName());

		if (!local)
			local = llvm::Function::Create(functionSymbol.FunctionType, llvm::Function::ExternalLinkage, functionSymbol.FunctionNode->GetName(), ctx.Module);

		return local;
	}

	Symbol ASTFunctionRef::Codegen(CodegenContext& ctx)
	{
		return Symbol::CreateValue(GetFunctionHere(Function, ctx), FunctionTy);
	}

	Symbol ASTVTableRef::Codegen(CodegenContext& ctx)
	{
		// one constant table per class and module: [n x ptr] holding the class's version of each virtual method
		std::string name = std::format("{}.vtable", ClassTy->GetHash());
		llvm::GlobalVariable* table = ctx.Module.getNamedGlobal(name);

		if (!table)
		{
			llvm::SmallVector<llvm::Constant*> slots;

			for (auto& function : ClassTy->VTable)
				slots.push_back(GetFunctionHere(function, ctx));

			auto arrayType = llvm::ArrayType::get(ctx.Builder.getPtrTy(), slots.size());
			table = new llvm::GlobalVariable(ctx.Module, arrayType, true, llvm::GlobalValue::LinkOnceODRLinkage, llvm::ConstantArray::get(arrayType, slots), name);
		}

		return Symbol::CreateValue(table, PointerTy);
	}

	Symbol ASTFunctionCall::Codegen(CodegenContext& ctx)
	{
		if (IndirectType)
		{
			// calling a function value
			auto functionType = IndirectType->As<FunctionPointerType>();
			llvm::Value* target = Callee->Codegen(ctx).GetLLVMValue();

			std::vector<llvm::Value*> args;
			std::vector<std::shared_ptr<Type>> types;
			BuildArgs(ctx, args, types);
			ConvertArguments(ctx, functionType->GetFunctionType(), args, types);

			if (ctx.RuntimeChecks)
				EmitCheck(ctx, ctx.Builder.CreateIsNotNull(target), "calling a null function", GetNodeLocation(Callee));

			llvm::Value* result = ctx.Builder.CreateCall(functionType->GetFunctionType(), target, args);

			if (!functionType->GetReturnType())
				return Symbol();

			return Symbol::CreateValue(result, functionType->GetReturnType());
		}

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

		// virtual: receiver->__vtable[slot](receiver, ...)
		if (VirtualSlot >= 0 && calleeSymbol.Receiver)
		{
			llvm::Value* receiver = calleeSymbol.Receiver->GetLLVMValue();
			llvm::Value* table = ctx.Builder.CreateLoad(ctx.Builder.getPtrTy(), receiver, "vtable");
			llvm::Value* slot = ctx.Builder.CreateConstInBoundsGEP1_64(ctx.Builder.getPtrTy(), table, (uint64_t)VirtualSlot, "vslot");
			llvm::Value* target = ctx.Builder.CreateLoad(ctx.Builder.getPtrTy(), slot, "vfunc");
			llvm::Value* result = ctx.Builder.CreateCall(functionType, target, args);

			if (!functionSymbol.FunctionNode->ReturnTypeVal)
				return Symbol();

			return Symbol::CreateValue(result, functionSymbol.FunctionNode->ReturnTypeVal);
		}

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

				// a computed value has no address yet: give it one, so it is indexed like a variable
				if (TargetIsValue)
				{
					Symbol slot = CreateAlloca(current.GetType(), ctx);
					ctx.Builder.CreateStore(current.GetLLVMValue(), slot.GetLLVMValue());
					current = slot;
				}

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

		// a small literal is built as a value: its constant part, with the computed items put in (no memory involved)
		if (ctx.Module.getDataLayout().getTypeAllocSize(llvmArrayType) <= 256)
		{
			llvm::Value* aggregate = initializer;

			for (size_t i = 0; i < values.size(); i++)
			{
				if (!llvm::isa<llvm::Constant>(values[i]))
					aggregate = ctx.Builder.CreateInsertValue(aggregate, values[i], { (unsigned)i });
			}

			return Symbol::CreateValue(aggregate, arrayType);
		}

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

		// a small literal is built as a value: its constant part, with the computed fields put in (no memory involved)
		if (ctx.Module.getDataLayout().getTypeAllocSize(llvmStructTy) <= 256)
		{
			llvm::Value* aggregate = initializer;

			for (size_t i = 0; i < values.size(); i++)
			{
				if (!llvm::isa<llvm::Constant>(values[i]))
					aggregate = ctx.Builder.CreateInsertValue(aggregate, values[i], { (unsigned)i });
			}

			return Symbol::CreateValue(aggregate, structTy);
		}

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

			// for x in {1, 2, 3}: a computed array is kept in a slot so its items can be addressed
			if (!iterable.GetLLVMValue()->getType()->isPointerTy())
			{
				Symbol slot = CreateAlloca(IterableType, ctx);
				ctx.Builder.CreateStore(iterable.GetLLVMValue(), slot.GetLLVMValue());
				iterable = slot;
			}

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
			// objects are visited in place (the loop variable is the element), plain values are copied
			if (VariableType->IsClass() && !IterableIsTemporary)
			{
				*Variable = Symbol::CreateValue(address, ctx.TypeReg->GetPointerTo(VariableType));
			}
			else
			{
				llvm::Value* element = ctx.Builder.CreateLoad(VariableType->Get(), address);
				ctx.Builder.CreateStore(element, variable.GetLLVMValue());
			}
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
		if (IsTrait)
			return Symbol::CreateType(ClassTy);

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

		// every case is handled: tell LLVM no other value can occur
		if (IsExhaustive && !DefaultCaseCodeBlock)
		{
			defaultBlock = llvm::BasicBlock::Create(ctx.Context, "switch.impossible", function);
			llvm::IRBuilder<> impossible(defaultBlock);
			impossible.CreateUnreachable();
		}

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

		// the message is a str: its bytes (messages are literals, so they end with a zero)
		llvm::Value* detail = Message ? Message->Codegen(ctx).GetLLVMValue() : nullptr;

		if (detail && detail->getType()->isStructTy())
			detail = ctx.Builder.CreateExtractValue(detail, 0);

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

		// slices: { pointer to the first item, number of items }
		if (Name == "slice_retype")
			return Symbol::CreateValue(args[0], ResultType); // str and []int8 are laid out the same

		if (Name == "slice_data")
			return Symbol::CreateValue(builder.CreateExtractValue(args[0], 0), ResultType);

		// str -> char*: the bytes, which C needs to end with a zero (true of literals and String's text, not of
		// every part of a text)
		if (Name == "str_c")
		{
			llvm::Value* data = builder.CreateExtractValue(args[0], 0);

			if (ctx.RuntimeChecks && !llvm::isa<llvm::Constant>(args[0]))
			{
				llvm::Function* function = builder.GetInsertBlock()->getParent();
				llvm::BasicBlock* look = llvm::BasicBlock::Create(ctx.Context, "str.look", function);
				llvm::BasicBlock* done = llvm::BasicBlock::Create(ctx.Context, "str.checked", function);
				llvm::BasicBlock* start = builder.GetInsertBlock();
				builder.CreateCondBr(builder.CreateIsNull(data), done, look);

				builder.SetInsertPoint(look);
				llvm::Value* end = builder.CreateLoad(builder.getInt8Ty(), builder.CreateInBoundsGEP(builder.getInt8Ty(), data, { builder.CreateExtractValue(args[0], 1) }));
				llvm::Value* terminated = builder.CreateICmpEQ(end, builder.getInt8(0));
				llvm::BasicBlock* lookEnd = builder.GetInsertBlock();
				builder.CreateBr(done);

				builder.SetInsertPoint(done);
				llvm::PHINode* ok = builder.CreatePHI(builder.getInt1Ty(), 2);
				ok->addIncoming(builder.getTrue(), start);
				ok->addIncoming(terminated, lookEnd);
				EmitCheck(ctx, ok, "a str passed to C must end with a zero byte (part of a text does not: use String(text).c_str())", Location, nullptr);
			}

			return Symbol::CreateValue(data, ResultType);
		}

		// char* -> str: measured with strlen (null stays an empty str)
		if (Name == "str_from_c")
		{
			llvm::FunctionCallee strlen = ctx.Module.getOrInsertFunction("strlen", llvm::FunctionType::get(builder.getInt64Ty(), { builder.getPtrTy() }, false));
			llvm::Function* function = builder.GetInsertBlock()->getParent();
			llvm::BasicBlock* measure = llvm::BasicBlock::Create(ctx.Context, "str.measure", function);
			llvm::BasicBlock* done = llvm::BasicBlock::Create(ctx.Context, "str.measured", function);
			llvm::BasicBlock* start = builder.GetInsertBlock();
			builder.CreateCondBr(builder.CreateIsNull(args[0]), done, measure);

			builder.SetInsertPoint(measure);
			llvm::Value* counted = builder.CreateCall(strlen, { args[0] });
			llvm::BasicBlock* measureEnd = builder.GetInsertBlock();
			builder.CreateBr(done);

			builder.SetInsertPoint(done);
			llvm::PHINode* length = builder.CreatePHI(builder.getInt64Ty(), 2);
			length->addIncoming(builder.getInt64(0), start);
			length->addIncoming(counted, measureEnd);

			llvm::Value* value = llvm::UndefValue::get(ResultType->Get());
			value = builder.CreateInsertValue(value, args[0], 0);
			return Symbol::CreateValue(builder.CreateInsertValue(value, length, 1), ResultType);
		}

		if (Name == "make_slice" || Name == "slice_of_array" || Name == "slice_len" || Name == "slice_at" || Name == "slice_range")
		{
			auto slice = [&](llvm::Value* data, llvm::Value* length)
			{
				llvm::Value* value = llvm::UndefValue::get(ResultType->Get());
				value = builder.CreateInsertValue(value, data, 0);
				return builder.CreateInsertValue(value, length, 1);
			};

			if (Name == "make_slice")
				return Symbol::CreateValue(slice(args[0], args[1]), ResultType);

			if (Name == "slice_of_array")
				return Symbol::CreateValue(slice(args[0], args[1]), ResultType); // an array's address is its first item's

			if (Name == "slice_len")
				return Symbol::CreateValue(builder.CreateExtractValue(args[0], 1), ResultType);

			llvm::Value* data = builder.CreateExtractValue(args[0], 0);
			llvm::Value* length = builder.CreateExtractValue(args[0], 1);

			if (Name == "slice_at")
			{
				auto element = ResultType->As<PointerType>()->GetBaseType();

				if (ctx.RuntimeChecks)
					EmitCheck(ctx, builder.CreateICmpULT(args[1], length), "index out of range for a slice", Location, nullptr);

				return Symbol::CreateValue(builder.CreateInBoundsGEP(element->Get(), data, { args[1] }), ResultType);
			}

			// slice_range(s, start, end): 0 <= start <= end <= len(s)
			auto element = ResultType->As<SliceType>()->GetBaseType();

			if (ctx.RuntimeChecks)
				EmitCheck(ctx, builder.CreateAnd(builder.CreateICmpULE(args[1], args[2]), builder.CreateICmpULE(args[2], length)), "slice bounds out of range", Location, nullptr);

			return Symbol::CreateValue(slice(builder.CreateInBoundsGEP(element->Get(), data, { args[1] }), builder.CreateSub(args[2], args[1])), ResultType);
		}

		if (Name == "strlen")
		{
			llvm::FunctionCallee strlen = ctx.Module.getOrInsertFunction("strlen", llvm::FunctionType::get(builder.getInt64Ty(), { builder.getPtrTy() }, false));
			return Symbol::CreateValue(builder.CreateCall(strlen, args), ResultType);
		}

		if (Name == "str_contains")
		{
			// needle in haystack: memmem over the bytes (an empty needle is found everywhere)
			llvm::FunctionCallee memmem = ctx.Module.getOrInsertFunction("memmem", llvm::FunctionType::get(builder.getPtrTy(), 
				{ builder.getPtrTy(), builder.getInt64Ty(), builder.getPtrTy(), builder.getInt64Ty() }, false));
			llvm::Value* needleLength = builder.CreateExtractValue(args[0], 1);
			llvm::Value* position = builder.CreateCall(memmem, { builder.CreateExtractValue(args[1], 0), builder.CreateExtractValue(args[1], 1), 
																	builder.CreateExtractValue(args[0], 0), needleLength });
			llvm::Value* found = builder.CreateOr(builder.CreateIsNotNull(position), builder.CreateICmpEQ(needleLength, builder.getInt64(0)));
			return Symbol::CreateValue(found, ResultType);
		}

		// Generator[T] / Task[T] handles
		if (Name.starts_with("coro_") || Name == "task_run")
		{
			llvm::Function* function = builder.GetInsertBlock()->getParent();
			llvm::Value* handle = args[0];
			auto done = [&]() { return builder.CreateIntrinsic(llvm::Intrinsic::coro_done, {}, { handle }); };
			auto value = [&](std::shared_ptr<Type> type)
			{
				llvm::Value* promise = builder.CreateIntrinsic(llvm::Intrinsic::coro_promise, {}, { handle, builder.getInt32(16), builder.getInt1(false) });
				return builder.CreateLoad(type->Get(), promise, "coro.value");
			};

			if (Name == "coro_done")
				return Symbol::CreateValue(done(), ResultType);

			if (Name == "coro_value")
				return Symbol::CreateValue(value(ResultType), ResultType);

			if (Name == "coro_value_address")
				return Symbol::CreateValue(builder.CreateIntrinsic(llvm::Intrinsic::coro_promise, {}, { handle, builder.getInt32(16), builder.getInt1(false) }), ResultType);

			if (Name == "coro_destroy")
			{
				llvm::BasicBlock* destroy = llvm::BasicBlock::Create(ctx.Context, "coro.destroy", function);
				llvm::BasicBlock* after = llvm::BasicBlock::Create(ctx.Context, "coro.destroyed", function);
				builder.CreateCondBr(builder.CreateIsNotNull(handle), destroy, after);
				builder.SetInsertPoint(destroy);
				builder.CreateIntrinsic(llvm::Intrinsic::coro_destroy, {}, { handle });
				builder.CreateBr(after);
				builder.SetInsertPoint(after);
				return Symbol();
			}

			// resume (unless already finished), then report: resume -> finished?  advance -> a new value?
			if (Name == "coro_resume" || Name == "coro_advance")
			{
				llvm::BasicBlock* resume = llvm::BasicBlock::Create(ctx.Context, "coro.resume", function);
				llvm::BasicBlock* after = llvm::BasicBlock::Create(ctx.Context, "coro.resumed", function);
				llvm::BasicBlock* before = builder.GetInsertBlock();
				builder.CreateCondBr(done(), after, resume);

				builder.SetInsertPoint(resume);
				builder.CreateIntrinsic(llvm::Intrinsic::coro_resume, {}, { handle });
				llvm::Value* finishedNow = done();
				llvm::BasicBlock* resumed = builder.GetInsertBlock();
				builder.CreateBr(after);

				builder.SetInsertPoint(after);
				llvm::PHINode* finished = builder.CreatePHI(builder.getInt1Ty(), 2);
				finished->addIncoming(builder.getTrue(), before);
				finished->addIncoming(finishedNow, resumed);

				llvm::Value* result = Name == "coro_resume" ? (llvm::Value*)finished : builder.CreateNot(finished);
				return Symbol::CreateValue(result, ResultType);
			}

			if (Name == "task_run")
			{
				llvm::BasicBlock* check = llvm::BasicBlock::Create(ctx.Context, "run.check", function);
				llvm::BasicBlock* step = llvm::BasicBlock::Create(ctx.Context, "run.step", function);
				llvm::BasicBlock* after = llvm::BasicBlock::Create(ctx.Context, "run.done", function);
				builder.CreateBr(check);

				builder.SetInsertPoint(check);
				builder.CreateCondBr(done(), after, step);

				builder.SetInsertPoint(step);
				builder.CreateIntrinsic(llvm::Intrinsic::coro_resume, {}, { handle });
				builder.CreateBr(check);

				// the frame is freed by whoever owns the task (a variable, or the temporary when it ends)
				builder.SetInsertPoint(after);
				llvm::Value* result = ResultType ? value(ResultType) : nullptr;
				return ResultType ? Symbol::CreateValue(result, ResultType) : Symbol();
			}
		}

		if (Name == "clone")
			return Symbol::CreateValue(EmitCopy(ctx, ResultType, builder.CreateLoad(ResultType->Get(), args[0], "original")), ResultType);

		if (Name == "take")
			return Symbol::CreateValue(builder.CreateLoad(ResultType->Get(), args[0], "taken"), ResultType);

		if (Name == "hash_int")
		{
			// any scalar: its bits, mixed (splitmix64) so nearby values spread over the whole range
			llvm::Value* value = args[0];

			if (value->getType()->isPointerTy())
				value = builder.CreatePtrToInt(value, builder.getInt64Ty());
			else if (value->getType()->isFloatingPointTy())
				value = builder.CreateBitCast(value, builder.getIntNTy((unsigned)value->getType()->getPrimitiveSizeInBits()));

			value = builder.CreateZExtOrTrunc(value, builder.getInt64Ty());
			value = builder.CreateXor(value, builder.CreateLShr(value, 30));
			value = builder.CreateMul(value, builder.getInt64(0xbf58476d1ce4e5b9ULL));
			value = builder.CreateXor(value, builder.CreateLShr(value, 27));
			value = builder.CreateMul(value, builder.getInt64(0x94d049bb133111ebULL));
			value = builder.CreateXor(value, builder.CreateLShr(value, 31));
			return Symbol::CreateValue(value, ResultType);
		}

		if (Name == "hash_str")
		{
			// FNV-1a over the str's bytes, in a small helper shared by the whole module
			llvm::Function* helper = ctx.Module.getFunction("clear.hash_bytes");

			if (!helper)
			{
				auto type = llvm::FunctionType::get(builder.getInt64Ty(), { builder.getPtrTy(), builder.getInt64Ty() }, false);
				helper = llvm::Function::Create(type, llvm::Function::LinkOnceODRLinkage, "clear.hash_bytes", ctx.Module);

				llvm::IRBuilder<> local(ctx.Context);
				auto entry = llvm::BasicBlock::Create(ctx.Context, "entry", helper);
				auto loop = llvm::BasicBlock::Create(ctx.Context, "loop", helper);
				auto body = llvm::BasicBlock::Create(ctx.Context, "body", helper);
				auto done = llvm::BasicBlock::Create(ctx.Context, "done", helper);

				local.SetInsertPoint(entry);
				local.CreateBr(loop);

				local.SetInsertPoint(loop);
				auto hash = local.CreatePHI(local.getInt64Ty(), 2, "hash");
				auto index = local.CreatePHI(local.getInt64Ty(), 2, "index");
				hash->addIncoming(local.getInt64(0xcbf29ce484222325ULL), entry);
				index->addIncoming(local.getInt64(0), entry);
				local.CreateCondBr(local.CreateICmpULT(index, helper->getArg(1)), body, done);

				local.SetInsertPoint(body);
				auto byte = local.CreateLoad(local.getInt8Ty(), local.CreateInBoundsGEP(local.getInt8Ty(), helper->getArg(0), { index }), "byte");
				auto mixed = local.CreateMul(local.CreateXor(hash, local.CreateZExt(byte, local.getInt64Ty())), local.getInt64(0x100000001b3ULL));
				hash->addIncoming(mixed, body);
				index->addIncoming(local.CreateAdd(index, local.getInt64(1)), body);
				local.CreateBr(loop);

				local.SetInsertPoint(done);
				local.CreateRet(hash);
			}

			return Symbol::CreateValue(builder.CreateCall(helper, { builder.CreateExtractValue(args[0], 0), builder.CreateExtractValue(args[0], 1) }), ResultType);
		}

		CLEAR_UNREACHABLE("unknown intrinsic ", Name);
		return Symbol();
	}

	llvm::Value* LoadVariantPayload(CodegenContext& ctx, std::shared_ptr<Type> variantType, size_t caseIndex, llvm::Value* storage)
	{
		auto& variantCase = variantType->As<ClassType>()->Cases[caseIndex];
		llvm::Value* payload = ctx.Builder.CreateStructGEP(variantType->Get(), storage, 1, "payload");
		return ctx.Builder.CreateLoad(variantCase.Payload, payload);
	}

	// keeps a value in a stack slot so parts of it can be addressed
	static llvm::Value* SpillToStack(CodegenContext& ctx, std::shared_ptr<Type> type, llvm::Value* value)
	{
		Symbol slot = CreateAlloca(type, ctx);
		ctx.Builder.CreateStore(value, slot.GetLLVMValue());
		return slot.GetLLVMValue();
	}

	Symbol ASTVariantConstruct::Codegen(CodegenContext& ctx)
	{
		auto classType = VariantTy->As<ClassType>();
		auto& variantCase = classType->Cases[CaseIndex];

		// fill the case's payload, then store tag and payload into a zeroed value
		llvm::Value* payload = llvm::UndefValue::get(variantCase.Payload);

		for (size_t i = 0; i < Values.size(); i++)
		{
			Symbol value = Values[i]->Codegen(ctx);
			Symbol fieldType = Symbol::CreateType(variantCase.Fields[i].second);
			value = SymbolOps::Cast(value, fieldType, ctx.Builder);
			payload = ctx.Builder.CreateInsertValue(payload, value.GetLLVMValue(), { (unsigned)i });
		}

		llvm::Value* storage = SpillToStack(ctx, VariantTy, llvm::Constant::getNullValue(VariantTy->Get()));
		ctx.Builder.CreateStore(ctx.Builder.getInt32((uint32_t)CaseIndex), ctx.Builder.CreateStructGEP(VariantTy->Get(), storage, 0));

		if (!Values.empty())
			ctx.Builder.CreateStore(payload, ctx.Builder.CreateStructGEP(VariantTy->Get(), storage, 1));

		return Symbol::CreateValue(ctx.Builder.CreateLoad(VariantTy->Get(), storage), VariantTy);
	}

	Symbol ASTVariantField::Codegen(CodegenContext& ctx)
	{
		Symbol subject = Subject->Codegen(ctx);
		auto fieldType = VariantTy->As<ClassType>()->Cases[CaseIndex].Fields[FieldIndex].second;

		if (AsAddress)
		{
			auto& variantCase = VariantTy->As<ClassType>()->Cases[CaseIndex];
			llvm::Value* payloadAddress = ctx.Builder.CreateStructGEP(VariantTy->Get(), subject.GetLLVMValue(), 1, "payload");
			llvm::Value* field = ctx.Builder.CreateStructGEP(variantCase.Payload, payloadAddress, (unsigned)FieldIndex, "case.field");
			return Symbol::CreateValue(field, ctx.TypeReg->GetPointerTo(fieldType));
		}

		llvm::Value* payload = LoadVariantPayload(ctx, VariantTy, CaseIndex, subject.GetLLVMValue());
		return Symbol::CreateValue(ctx.Builder.CreateExtractValue(payload, { (unsigned)FieldIndex }), fieldType);
	}

	Symbol ASTVariantTag::Codegen(CodegenContext& ctx)
	{
		Symbol subject = Subject->Codegen(ctx);
		return Symbol::CreateValue(ctx.Builder.CreateExtractValue(subject.GetLLVMValue(), { 0u }, "tag"), TagType);
	}

	Symbol ASTOptionalUnwrap::Codegen(CodegenContext& ctx)
	{
		Symbol subject = Subject->Codegen(ctx);
		auto classType = OptionalTy->As<ClassType>();
		size_t someIndex = CaseIndex >= 0 ? (size_t)CaseIndex : classType->FindCase("some").value();

		// a variant always checks: reading the wrong type would reinterpret its bytes
		if (ctx.RuntimeChecks || classType->IsTypeVariant)
		{
			llvm::Value* tag = ctx.Builder.CreateExtractValue(subject.GetLLVMValue(), { 0u });
			std::string message = classType->IsTypeVariant ? std::format("reading {} from a {} that holds another type", classType->Cases[someIndex].Name, classType->GetHash())
														   : std::string("unwrapping an optional that is none");
			EmitCheck(ctx, ctx.Builder.CreateICmpEQ(tag, ctx.Builder.getInt32((uint32_t)someIndex)), message, Location);
		}

		llvm::Value* storage = SpillToStack(ctx, OptionalTy, subject.GetLLVMValue());
		llvm::Value* payload = LoadVariantPayload(ctx, OptionalTy, someIndex, storage);
		auto valueType = classType->Cases[someIndex].Fields[0].second;

		return Symbol::CreateValue(ctx.Builder.CreateExtractValue(payload, { 0u }), valueType);
	}

	Symbol ASTOptionalValueOr::Codegen(CodegenContext& ctx)
	{
		Symbol subject = Subject->Codegen(ctx);
		auto classType = OptionalTy->As<ClassType>();
		size_t someIndex = classType->FindCase("some").value();
		auto valueType = classType->Cases[someIndex].Fields[0].second;

		llvm::Value* storage = SpillToStack(ctx, OptionalTy, subject.GetLLVMValue());
		llvm::Value* payload = LoadVariantPayload(ctx, OptionalTy, someIndex, storage);
		llvm::Value* value = ctx.Builder.CreateExtractValue(payload, { 0u });

		Symbol fallback = Default->Codegen(ctx);
		Symbol valueTypeSymbol = Symbol::CreateType(valueType);
		fallback = SymbolOps::Cast(fallback, valueTypeSymbol, ctx.Builder);

		llvm::Value* tag = ctx.Builder.CreateExtractValue(subject.GetLLVMValue(), { 0u });
		llvm::Value* hasValue = ctx.Builder.CreateICmpEQ(tag, ctx.Builder.getInt32((uint32_t)someIndex));

		return Symbol::CreateValue(ctx.Builder.CreateSelect(hasValue, value, fallback.GetLLVMValue()), valueType);
	}

	Symbol ASTUnionConstruct::Codegen(CodegenContext& ctx)
	{
		llvm::Value* storage = SpillToStack(ctx, UnionTy, llvm::Constant::getNullValue(UnionTy->Get()));

		if (Value)
		{
			Symbol value = Value->Codegen(ctx);
			Symbol fieldType = Symbol::CreateType(FieldTy);
			value = SymbolOps::Cast(value, fieldType, ctx.Builder);
			ctx.Builder.CreateStore(value.GetLLVMValue(), storage);
		}

		return Symbol::CreateValue(ctx.Builder.CreateLoad(UnionTy->Get(), storage), UnionTy);
	}

	Symbol ASTTupleExpr::Codegen(CodegenContext& ctx)
	{
		if (IsType)
			return Symbol::CreateType(TupleTy);

		// build the value in registers, element by element
		auto& elements = TupleTy->As<TupleType>()->GetElements();
		llvm::Value* tuple = llvm::UndefValue::get(TupleTy->Get());

		for (size_t i = 0; i < Values.size(); i++)
		{
			Symbol value = Values[i]->Codegen(ctx);
			Symbol elementType = Symbol::CreateType(elements[i]);
			value = SymbolOps::Cast(value, elementType, ctx.Builder);
			tuple = ctx.Builder.CreateInsertValue(tuple, value.GetLLVMValue(), { (unsigned)i });
		}

		return Symbol::CreateValue(tuple, TupleTy);
	}

	Symbol ASTTupleGet::Codegen(CodegenContext& ctx)
	{
		Symbol tuple = Tuple->Codegen(ctx);
		auto elementType = TupleTy->As<TupleType>()->GetElements()[Index];

		if (!TupleIsStorage)
			return Symbol::CreateValue(ctx.Builder.CreateExtractValue(tuple.GetLLVMValue(), { (unsigned)Index }), elementType);

		llvm::Value* address = ctx.Builder.CreateStructGEP(TupleTy->Get(), tuple.GetLLVMValue(), (unsigned)Index);
		auto pointerType = ctx.ClearModule->GetTypeRegistry()->GetPointerTo(elementType);

		if (WantAddress)
			return Symbol::CreateValue(address, pointerType);

		return Symbol::CreateValue(ctx.Builder.CreateLoad(elementType->Get(), address), elementType);
	}

	Symbol ASTTemporary::Codegen(CodegenContext& ctx)
	{
		Symbol value = Operand->Codegen(ctx);
		Symbol storage = CreateAlloca(ValueType, ctx);
		SymbolOps::Store(storage, value, ctx.Builder, ctx.Module, true);

		if (DestroyAtScopeEnd && !ctx.Defers->empty())
		{
			auto destroy = std::make_shared<ASTDestroy>();
			destroy->Address = storage.GetLLVMValue();
			destroy->ValueType = ValueType;
			ctx.Defers->back().push_back(destroy);
		}

		return storage;
	}

	Symbol ASTConstantValue::Codegen(CodegenContext& ctx)
	{
		return Symbol::CreateValue(llvm::ConstantInt::get(ValueType->Get(), Value, /* isSigned = */ true), ValueType);
	}

	Symbol ASTMove::Codegen(CodegenContext& ctx)
	{
		// read the value, then leave the source all zero: its own cleanup becomes a no-op
		Symbol value = Value->Codegen(ctx);
		Symbol storage = Storage->Codegen(ctx);
		auto storedType = storage.GetType()->As<PointerType>()->GetBaseType();
		ctx.Builder.CreateStore(llvm::Constant::getNullValue(storedType->Get()), storage.GetLLVMValue());
		return value;
	}

	Symbol ASTCopy::Codegen(CodegenContext& ctx)
	{
		Symbol value = Value->Codegen(ctx);

		// the variable's last use: take the value and leave the variable empty, as a move does
		if (MoveFrom)
		{
			Symbol storage = MoveFrom->Codegen(ctx);
			ctx.Builder.CreateStore(llvm::Constant::getNullValue(ValueType->Get()), storage.GetLLVMValue());
			return value;
		}

		return Symbol::CreateValue(EmitCopy(ctx, ValueType, value.GetLLVMValue()), ValueType);
	}

	llvm::Value* EmitCopy(CodegenContext& ctx, const std::shared_ptr<Type>& type, llvm::Value* value)
	{
		if (!IsOwning(type))
			return value;

		auto& builder = ctx.Builder;
		llvm::Function* function = builder.GetInsertBlock()->getParent();

		if (auto array = std::dynamic_pointer_cast<ArrayType>(type))
		{
			for (unsigned i = 0; i < array->GetArraySize(); i++)
				value = builder.CreateInsertValue(value, EmitCopy(ctx, array->GetBaseType(), builder.CreateExtractValue(value, { i })), { i });
			return value;
		}

		if (auto tuple = std::dynamic_pointer_cast<TupleType>(type))
		{
			for (unsigned i = 0; i < tuple->GetElements().size(); i++)
				value = builder.CreateInsertValue(value, EmitCopy(ctx, tuple->GetElements()[i], builder.CreateExtractValue(value, { i })), { i });
			return value;
		}

		auto classType = type->As<ClassType>();
		Symbol result = CreateAlloca(type, ctx);
		builder.CreateStore(value, result.GetLLVMValue());

		// operator copy decides
		if (auto copy = classType->MemberFunctions.find("__copy__"); copy != classType->MemberFunctions.end())
		{
			llvm::Function* callee = GetFunctionHere(copy->second, ctx);
			return builder.CreateCall(callee, { result.GetLLVMValue() }, "copy");
		}

		// variants and optionals: copy what the case they hold owns
		if (classType->IsVariant)
		{
			llvm::BasicBlock* done = llvm::BasicBlock::Create(ctx.Context, "copy.done", function);
			llvm::Value* tag = builder.CreateExtractValue(value, { 0u }, "tag");
			llvm::SwitchInst* branch = builder.CreateSwitch(tag, done);

			for (size_t i = 0; i < classType->Cases.size(); i++)
			{
				auto& variantCase = classType->Cases[i];

				if (std::none_of(variantCase.Fields.begin(), variantCase.Fields.end(), [](auto& field) { return IsOwning(field.second); }))
					continue;

				llvm::BasicBlock* block = llvm::BasicBlock::Create(ctx.Context, "copy.case", function);
				branch->addCase(builder.getInt32((uint32_t)i), block);
				builder.SetInsertPoint(block);

				llvm::Value* payload = builder.CreateStructGEP(classType->Get(), result.GetLLVMValue(), 1);

				for (size_t f = 0; f < variantCase.Fields.size(); f++)
				{
					auto fieldType = variantCase.Fields[f].second;

					if (!IsOwning(fieldType))
						continue;

					llvm::Value* address = builder.CreateStructGEP(variantCase.Payload, payload, (unsigned)f);
					builder.CreateStore(EmitCopy(ctx, fieldType, builder.CreateLoad(fieldType->Get(), address)), address);
				}

				builder.CreateBr(done);
			}

			builder.SetInsertPoint(done);
			return builder.CreateLoad(type->Get(), result.GetLLVMValue());
		}

		// field by field: plain fields as they are, owning fields copied
		unsigned index = 0;
		for (const auto& [name, fieldType] : classType->GetMemberValues())
		{
			if (IsOwning(fieldType))
			{
				llvm::Value* address = builder.CreateStructGEP(classType->Get(), result.GetLLVMValue(), index);
				builder.CreateStore(EmitCopy(ctx, fieldType, builder.CreateLoad(fieldType->Get(), address)), address);
			}

			index++;
		}

		return builder.CreateLoad(type->Get(), result.GetLLVMValue());
	}

	Symbol ASTOnce::Codegen(CodegenContext& ctx)
	{
		llvm::Function* function = ctx.Builder.GetInsertBlock()->getParent();

		if (ComputedIn != function)
		{
			Computed = Operand->Codegen(ctx);
			ComputedIn = function;
		}

		return Computed;
	}

	Symbol ASTDestroy::Codegen(CodegenContext& ctx)
	{
		llvm::Value* address = Address ? Address : Pointer->Codegen(ctx).GetLLVMValue();
		EmitDestroy(ctx, ValueType, address);
		return Symbol();
	}

	void EmitDestroy(CodegenContext& ctx, const std::shared_ptr<Type>& type, llvm::Value* address)
	{
		if (!IsOwning(type))
			return;

		auto& builder = ctx.Builder;
		llvm::Function* function = builder.GetInsertBlock()->getParent();

		if (auto array = std::dynamic_pointer_cast<ArrayType>(type))
		{
			for (size_t i = 0; i < array->GetArraySize(); i++)
				EmitDestroy(ctx, array->GetBaseType(), builder.CreateConstInBoundsGEP2_64(array->Get(), address, 0, i));
			return;
		}

		if (auto tuple = std::dynamic_pointer_cast<TupleType>(type))
		{
			for (unsigned i = 0; i < tuple->GetElements().size(); i++)
				EmitDestroy(ctx, tuple->GetElements()[i], builder.CreateStructGEP(tuple->Get(), address, i));
			return;
		}

		// a generator or task: free its frame (if it still has one) and forget it, so a second cleanup does nothing
		if (std::dynamic_pointer_cast<CoroutineType>(type))
		{
			llvm::BasicBlock* destroy = llvm::BasicBlock::Create(ctx.Context, "coro.destroy", function);
			llvm::BasicBlock* done = llvm::BasicBlock::Create(ctx.Context, "coro.destroyed", function);
			llvm::Value* handle = builder.CreateLoad(builder.getPtrTy(), address, "handle");
			builder.CreateCondBr(builder.CreateIsNotNull(handle), destroy, done);

			builder.SetInsertPoint(destroy);
			builder.CreateIntrinsic(llvm::Intrinsic::coro_destroy, {}, { handle });
			builder.CreateStore(llvm::ConstantPointerNull::get(builder.getPtrTy()), address);
			builder.CreateBr(done);

			builder.SetInsertPoint(done);
			return;
		}

		auto classType = type->As<ClassType>();

		// variants and optionals: clean up the case they hold
		if (classType->IsVariant)
		{
			llvm::BasicBlock* done = llvm::BasicBlock::Create(ctx.Context, "destroy.done", function);
			llvm::Value* tag = builder.CreateLoad(builder.getInt32Ty(), builder.CreateStructGEP(classType->Get(), address, 0), "tag");
			llvm::SwitchInst* branch = builder.CreateSwitch(tag, done);

			for (size_t i = 0; i < classType->Cases.size(); i++)
			{
				auto& variantCase = classType->Cases[i];
				bool owns = std::any_of(variantCase.Fields.begin(), variantCase.Fields.end(), [](auto& field) { return IsOwning(field.second); });

				if (!owns)
					continue;

				llvm::BasicBlock* block = llvm::BasicBlock::Create(ctx.Context, "destroy.case", function);
				branch->addCase(builder.getInt32((uint32_t)i), block);
				builder.SetInsertPoint(block);

				llvm::Value* payload = builder.CreateStructGEP(classType->Get(), address, 1);

				for (size_t f = 0; f < variantCase.Fields.size(); f++)
					EmitDestroy(ctx, variantCase.Fields[f].second, builder.CreateStructGEP(variantCase.Payload, payload, (unsigned)f));

				builder.CreateBr(done);
			}

			builder.SetInsertPoint(done);
			return;
		}

		// operator destruct first, then the fields that own something (last field first)
		if (auto destruct = classType->MemberFunctions.find("__destruct__"); destruct != classType->MemberFunctions.end())
		{
			llvm::Function* callee = GetFunctionHere(destruct->second, ctx);
			builder.CreateCall(callee, { address });
		}

		auto& members = classType->GetMemberValues();
		std::vector<std::pair<std::string, std::shared_ptr<Type>>> fields(members.begin(), members.end());

		for (size_t i = fields.size(); i-- > 0; )
		{
			if (IsOwning(fields[i].second))
				EmitDestroy(ctx, fields[i].second, builder.CreateStructGEP(classType->Get(), address, (unsigned)i));
		}
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

