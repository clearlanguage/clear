#include "SymbolOperations.h"

#include "Symbols/FunctionCache.h"
#include "Type.h"
#include "Core/Log.h"
#include <llvm/IR/IRBuilder.h>

namespace clear 
{
    template<typename FIntOp, typename FFloatOp>
    static Symbol BinaryNumericOp(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder, 
                                  FIntOp intOp, FFloatOp floatOp, const char* opName)
    {
        SymbolOps::Promote(lhs, rhs, builder);

        auto [valuel, typel] = lhs.GetValue();
        auto [valuer, typer] = rhs.GetValue();

        if (typel->IsFloatingPoint())
        {
            return Symbol::CreateValue(floatOp(builder, valuel, valuer, opName), typel);
        }

        auto typeToReturn = typel->IsSigned() ? typel : typer;
        return Symbol::CreateValue(intOp(builder, valuel, valuer, opName), typeToReturn);
    }

    template<typename FUIntOp, typename FSIntOp, typename FFloatOp>
    static Symbol BinaryNumericOpSignednessAware(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder, 
                                                 FUIntOp uIntOp, FSIntOp sIntOp, FFloatOp floatOp, const char* opName)
    {
        SymbolOps::Promote(lhs, rhs, builder);

        auto [valuel, typel] = lhs.GetValue();
        auto [valuer, typer] = rhs.GetValue();

        if (typel->IsFloatingPoint())
        {
            return Symbol::CreateValue(floatOp(builder, valuel, valuer, opName), typel);
        }

        auto typeToReturn = typel->IsSigned() ? typel : typer;

        if(typeToReturn->IsSigned())
            return Symbol::CreateValue(sIntOp(builder, valuel, valuer, opName), typeToReturn);

        return Symbol::CreateValue(uIntOp(builder, valuel, valuer, opName), typeToReturn);
    }

    template<typename FUIntOp, typename FSIntOp, typename FFloatOp>
    static Symbol BinaryBooleanOpSignednessAware(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder, std::shared_ptr<Type> booleanType,
                                                 FUIntOp uIntOp, FSIntOp sIntOp, FFloatOp floatOp, const char* opName)
    {
        SymbolOps::Promote(lhs, rhs, builder);

        auto [valuel, typel] = lhs.GetValue();
        auto [valuer, typer] = rhs.GetValue();

        if (typel->IsFloatingPoint())
        {
            return Symbol::CreateValue(floatOp(builder, valuel, valuer, opName), booleanType);
        }

        auto typeToReturn = typel->IsSigned() ? typel : typer;

        if(typeToReturn->IsSigned())
            return Symbol::CreateValue(sIntOp(builder, valuel, valuer, opName), booleanType);

        return Symbol::CreateValue(uIntOp(builder, valuel, valuer, opName), booleanType);
    }

    template<typename Op> 
    static Symbol BinaryIntOp(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder, 
                              Op op, const char* opName)
    {
        SymbolOps::Promote(lhs, rhs, builder);

        auto [valuel, typel] = lhs.GetValue();
        auto [valuer, typer] = rhs.GetValue();

        return Symbol::CreateValue(op(builder, valuel, valuer, opName), typel);
    }

    template<typename Op> 
    static Symbol BinaryIntBooleanOp(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder, 
                                     std::shared_ptr<Type> booleanType, Op op, const char* opName)
    {
        SymbolOps::Promote(lhs, rhs, builder);

        auto [valuel, typel] = lhs.GetValue();
        auto [valuer, typer] = rhs.GetValue();

        return Symbol::CreateValue(op(builder, valuel, valuer, opName), booleanType);
    }

    template<typename UOp, typename SOp> 
    static Symbol BinaryIntOpSignednessAware(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder, 
                                             UOp uOp, SOp sOp, const char* opName)
    {
        SymbolOps::Promote(lhs, rhs, builder);

        auto [valuel, typel] = lhs.GetValue();
        auto [valuer, typer] = rhs.GetValue();

        auto typeToReturn = typel->IsSigned() ? typel : typer;

        if(typeToReturn->IsSigned())
            return Symbol::CreateValue(sOp(builder, valuel, valuer, opName), typeToReturn);

        return Symbol::CreateValue(uOp(builder, valuel, valuer, opName), typeToReturn);
    }


    Symbol SymbolOps::Add(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder)
    {
        return BinaryNumericOp(
            lhs, rhs, builder,
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateAdd(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateFAdd(l, r, name); },
            "add"
        );
    }
    
    Symbol SymbolOps::Sub(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder)
    {
        return BinaryNumericOp(
            lhs, rhs, builder,
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateSub(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateFSub(l, r, name); },
            "sub"
        );
    }
    
    Symbol SymbolOps::Mul(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder)
    {
        return BinaryNumericOp(
            lhs, rhs, builder,
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateMul(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateFMul(l, r, name); },
            "mul"
        );
    }
    
    Symbol SymbolOps::Div(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder)
    {
        return BinaryNumericOpSignednessAware(
            lhs, rhs, builder,
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateUDiv(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateSDiv(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateFDiv(l, r, name); },
            "div"
        );
    }
    
    Symbol SymbolOps::Mod(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder)
    {
        return BinaryNumericOpSignednessAware(
            lhs, rhs, builder,
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateURem(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateSRem(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateFRem(l, r, name); },
            "mod"
        );
    }
    
    Symbol SymbolOps::BitAnd(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder)
    {
        return BinaryIntOp(
            lhs, rhs, builder, 
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateAnd(l, r, name); },
            "bitand"
        );
    }
    
    Symbol SymbolOps::BitOr(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder)
    {
        return BinaryIntOp(
            lhs, rhs, builder, 
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateOr(l, r, name); },
            "bitor"
        );
    }
    
    Symbol SymbolOps::BitXor(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder)
    {
        return BinaryIntOp(
            lhs, rhs, builder, 
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateXor(l, r, name); },
            "bitxor"
        );
    }
    
    Symbol SymbolOps::BitNot(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder)
    {
        return BinaryIntOp(
            lhs, rhs, builder, 
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateXor(l, r, name); },
            "bitnot"
        );
    }
    
    Symbol SymbolOps::Shl(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder)
    {
        return BinaryIntOp(
            lhs, rhs, builder, 
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateShl(l, r, name); },
            "shl"
        );
    }
    
    Symbol SymbolOps::Shr(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder)
    {
        return BinaryIntOpSignednessAware(
            lhs, rhs, builder,
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateAShr(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateLShr(l, r, name); },
            "shr"
        );
    }
    
    Symbol SymbolOps::And(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType, llvm::IRBuilder<>& builder)
    {
        return BinaryIntBooleanOp(
            lhs, rhs, builder, booleanType,
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateLogicalAnd(l, r, name); },
            "logicaland"
        );
    }
    
    Symbol SymbolOps::Or(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType, llvm::IRBuilder<>& builder)
    {
        return BinaryIntBooleanOp(
            lhs, rhs, builder, booleanType,
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateLogicalOr(l, r, name); },
            "logicalor"
        );
    }
    
    Symbol SymbolOps::Eq(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType, llvm::IRBuilder<>& builder)
    {
        return BinaryBooleanOpSignednessAware(
            lhs, rhs, builder, booleanType,
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateICmpEQ(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateICmpEQ(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateFCmpOEQ(l, r, name); },
            "eq"
        );
    }
    
    Symbol SymbolOps::Neq(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType, llvm::IRBuilder<>& builder)
    {
         return BinaryBooleanOpSignednessAware(
            lhs, rhs, builder, booleanType,
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateICmpNE(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateICmpNE(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateFCmpONE(l, r, name); },
            "neq"
        );
    }
    
    Symbol SymbolOps::Lt(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType, llvm::IRBuilder<>& builder)
    {
        return BinaryBooleanOpSignednessAware(
            lhs, rhs, builder, booleanType,
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateICmpULT(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateICmpSLT(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateFCmpOLT(l, r, name); },
            "lt"
        );
    }
    
    Symbol SymbolOps::Lte(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType, llvm::IRBuilder<>& builder)
    {
        return BinaryBooleanOpSignednessAware(
            lhs, rhs, builder, booleanType,
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateICmpULE(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateICmpSLE(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateFCmpOLE(l, r, name); },
            "lte"
        );
    }
    
    Symbol SymbolOps::Gt(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType,  llvm::IRBuilder<>& builder)
    {
        return BinaryBooleanOpSignednessAware(
            lhs, rhs, builder, booleanType,
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateICmpUGT(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateICmpSGT(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateFCmpOGT(l, r, name); },
            "gt"
        );
    }
    
    Symbol SymbolOps::Gte(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType, llvm::IRBuilder<>& builder)
    {
        return BinaryBooleanOpSignednessAware(
            lhs, rhs, builder, booleanType,
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateICmpUGE(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateICmpSGE(l, r, name); },
            [](llvm::IRBuilder<>& bd, llvm::Value* l, llvm::Value* r, const char* name) { return bd.CreateFCmpOGE(l, r, name); },
            "gte"
        );
    }
    
    Symbol SymbolOps::Neg(Symbol& operand, llvm::IRBuilder<>& builder)
    {
        auto [value, type] = operand.GetValue();

        return Symbol::CreateValue(builder.CreateNeg(value, "neg"), type);
    }
    
    Symbol SymbolOps::Not(Symbol& operand, llvm::IRBuilder<>& builder)
    {
        auto [value, type] = operand.GetValue();

        return Symbol::CreateValue(builder.CreateNot(value, "not"), type);
    }
    
    Symbol SymbolOps::Call(Symbol& fn, const llvm::SmallVector<llvm::Value*>& args, llvm::IRBuilder<>& builder)
    {
        FunctionInstance* instance = fn.GetFunction();
        return Symbol::CreateValue(builder.CreateCall(instance->Function, args), instance->ReturnType);
    }
    
    Symbol SymbolOps::Load(Symbol& ptr, llvm::IRBuilder<>& builder)
    {
        auto [value, type] = ptr.GetValue();
        CLEAR_VERIFY(type->IsPointer(), "cannot load from a value that is not a pointer");

        auto ptrType = dyn_cast<PointerType>(type);
        return Symbol::CreateValue(builder.CreateLoad(ptrType->GetBaseType()->Get(), value, "load"), ptrType->GetBaseType());
    }
    
    void SymbolOps::Store(Symbol& ptr, Symbol& value, llvm::IRBuilder<>& builder, bool isFirstTime)
    {
        auto [storage, storageType] = ptr.GetValue();
        auto [val, type] = value.GetValue();

        CLEAR_VERIFY(storageType->IsPointer(), "cannot store in value that is not a pointer type!");
        auto ptrTy = dyn_cast<PointerType>(storageType); 
        auto baseTy = ptrTy->GetBaseType();

        if(!isFirstTime)
        {   
            CLEAR_VERIFY(!baseTy->IsConst(), "cannot change a constant value!");
        }
        
        builder.CreateStore(val, storage);
    }
    
    Symbol SymbolOps::GEPStruct(Symbol& basePtr, Symbol& resPtrType, size_t index, llvm::IRBuilder<>& builder)
    {
        auto [array, arrayPtr] = basePtr.GetValue();
        
        auto ptrTy = dyn_cast<PointerType>(arrayPtr);
        CLEAR_VERIFY(ptrTy, "not a valid pointer");


        auto baseTy = ptrTy->GetBaseType();

        llvm::Value* gep = builder.CreateStructGEP(
				        baseTy->Get(),  
				        array,            
				        index        
				    );
                    
        
        return Symbol::CreateValue(gep, resPtrType.GetType());
    }

    
    Symbol SymbolOps::GEP(Symbol& basePtr, Symbol& resPtrType, const llvm::SmallVector<llvm::Value*>& indices, llvm::IRBuilder<>& builder)
    {
        auto [array, arrayPtr] = basePtr.GetValue();
        
        auto ptrTy = dyn_cast<PointerType>(arrayPtr);
        CLEAR_VERIFY(ptrTy, "not a valid pointer");


        auto baseTy = ptrTy->GetBaseType();

        llvm::Value* gep = builder.CreateGEP(
				        baseTy->Get(),  
				        array,            
				        indices             
				    );
        
        return Symbol::CreateValue(gep, resPtrType.GetType());
    }
    
    
    void SymbolOps::Promote(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder)
    {
        auto [valuel, typel] = lhs.GetValue();
        auto [valuer, typer] = rhs.GetValue();


        llvm::Type* lhsType = typel->Get();
        llvm::Type* rhsType = typer->Get();

        //same type ignore rest
        if (lhsType == rhsType)
            return;

        // int -> float
        if (lhsType->isIntegerTy() && rhsType->isFloatingPointTy())
        {
			if(typel->IsSigned())
            	lhs = Symbol::CreateValue(builder.CreateSIToFP(valuel, rhsType, "cast"), typer);
			else
            	lhs = Symbol::CreateValue(builder.CreateUIToFP(valuel, rhsType, "cast"), typer);

        }
        else if (lhsType->isFloatingPointTy() && rhsType->isIntegerTy())
        {
			if(typer->IsSigned())
            	rhs = Symbol::CreateValue(builder.CreateSIToFP(valuer, lhsType, "cast"), typel);
			else
            	rhs = Symbol::CreateValue(builder.CreateUIToFP(valuer, lhsType, "cast"), typel);

        }        
        // float -> double
        else if (lhsType->isFloatTy() && rhsType->isDoubleTy())
        {
            lhs = Symbol::CreateValue(builder.CreateFPExt(valuel, rhsType, "cast"), typer);
        }
        else if (lhsType->isDoubleTy() && rhsType->isFloatTy())
        {
            rhs = Symbol::CreateValue(builder.CreateFPExt(valuer, lhsType, "cast"), typel);
        }
        // small int -> big int
        else if (lhsType->isIntegerTy() && rhsType->isIntegerTy())
        {
            uint32_t lhsBits = lhsType->getIntegerBitWidth();
            uint32_t rhsBits = rhsType->getIntegerBitWidth();

            if (lhsBits < rhsBits)
            {
                if(typel->IsSigned())
                {
                    lhs = Symbol::CreateValue(builder.CreateSExt(valuel, rhsType, "cast"), typer);
                }
                else
                {
                    lhs = Symbol::CreateValue(builder.CreateZExt(valuel, rhsType, "cast"), typer);
                }
            }
            else
            {
                if(typer->IsSigned())
                {
                    rhs = Symbol::CreateValue(builder.CreateSExt(valuer, lhsType, "cast"), typel);
                }
                else
                {
                    rhs = Symbol::CreateValue(builder.CreateZExt(valuer, lhsType, "cast"), typel);
                }

            }
        }
        else
        {
            CLEAR_UNREACHABLE("unsupported type promotion");
        }
    }
    
}