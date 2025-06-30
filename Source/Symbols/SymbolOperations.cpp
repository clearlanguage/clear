#include "SymbolOperations.h"

#include "Type.h"

namespace clear 
{
    

    Symbol SymbolOps::Add(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder)
    {
        
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