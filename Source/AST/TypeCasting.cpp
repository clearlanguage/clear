#include "TypeCasting.h"

#include "Core/Log.h"

namespace clear 
{
    llvm::Value* TypeCasting::Cast(llvm::Value* value, 
                                   std::shared_ptr<Type> src, 
                                   std::shared_ptr<Type> dst, 
                                   llvm::IRBuilder<>& builder)
    {
        llvm::Type* srcType = src->Get();
        llvm::Type* dstType = dst->Get();

        if (srcType == dstType)
            return value;

        llvm::Value* castedValue = nullptr;

        // int -> float
        if (srcType->isIntegerTy() && dstType->isFloatingPointTy()) 
        {
            if (src->IsSigned())
                castedValue = builder.CreateSIToFP(value, dstType, "cast");
            else
                castedValue = builder.CreateUIToFP(value, dstType, "cast");
        } 
        // float -> int
        else if (srcType->isFloatingPointTy() && dstType->isIntegerTy()) 
        {
            if (dst->IsSigned())
                castedValue = builder.CreateFPToSI(value, dstType, "cast");
            else
                castedValue = builder.CreateFPToUI(value, dstType, "cast");
        } 
        // float -> double
        else if (srcType->isFloatTy() && dstType->isDoubleTy()) 
        {
            castedValue = builder.CreateFPExt(value, dstType, "cast");
        } 
        // double -> float
        else if (srcType->isDoubleTy() && dstType->isFloatTy()) 
        {
            castedValue = builder.CreateFPTrunc(value, dstType, "cast");
        } 
        // smaller int -> bigger int
        else if (srcType->isIntegerTy() && dstType->isIntegerTy() && 
                 dstType->getIntegerBitWidth() > srcType->getIntegerBitWidth()) 
        {
            if (src->IsSigned())
                castedValue = builder.CreateSExt(value, dstType, "cast");
            else
                castedValue = builder.CreateZExt(value, dstType, "cast");

        } 
        // bigger int -> smaller int
        else if (srcType->isIntegerTy() && dstType->isIntegerTy() && 
                 dstType->getIntegerBitWidth() < srcType->getIntegerBitWidth()) 
        {
            castedValue = builder.CreateTrunc(value, dstType, "cast");
        }
        // fallback for bitcastable types 
        else if (srcType->canLosslesslyBitCastTo(dstType)) 
        {
            castedValue = builder.CreateBitCast(value, dstType, "cast");
        }
        else 
        {
            CLEAR_UNREACHABLE("unsupported type conversion between ", src->GetHash(), " and ", dst->GetHash());
        }

        return castedValue;
    }

    bool TypeCasting::CanBeCasted(std::shared_ptr<Type> src, std::shared_ptr<Type> dst)
    {
        llvm::Type* srcType = src->Get();
        llvm::Type* dstType = dst->Get();

        if (srcType == dstType)
            return true;

        if (srcType->isIntegerTy() && dstType->isFloatingPointTy()) return true;
        else if (srcType->isFloatingPointTy() && dstType->isIntegerTy()) return true;
        else if (srcType->isFloatTy() && dstType->isDoubleTy()) return true;
        else if (srcType->isDoubleTy() && dstType->isFloatTy()) return true;
        else if (srcType->isIntegerTy() && dstType->isIntegerTy()) return true;
        else if (srcType->canLosslesslyBitCastTo(dstType)) return true;

        return false;
    }

    bool TypeCasting::CanBePromoted(std::shared_ptr<Type> src, std::shared_ptr<Type> dst)
    {
        llvm::Type* lhsType = src->Get();
        llvm::Type* rhsType = dst->Get();

        if (lhsType == rhsType)
            return true;

        if (lhsType->isIntegerTy() && rhsType->isFloatingPointTy()) return false;
        else if (lhsType->isFloatingPointTy() && rhsType->isIntegerTy()) return false;
        else if (lhsType->isFloatTy() && rhsType->isDoubleTy()) return true;
        else if (lhsType->isDoubleTy() && rhsType->isFloatTy()) return false;
        else if (lhsType->isIntegerTy() && rhsType->isIntegerTy())
        {
            uint32_t lhsBits = lhsType->getIntegerBitWidth();
            uint32_t rhsBits = rhsType->getIntegerBitWidth();

            if (lhsBits < rhsBits) return true;
        }

        return false;
    }
}