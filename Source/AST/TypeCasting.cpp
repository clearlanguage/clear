#include "TypeCasting.h"

#include "Core/Log.h"

namespace clear 
{
    llvm::Value* TypeCasting::Cast(llvm::Value* value, std::shared_ptr<Type> src, std::shared_ptr<Type> dst)
    {
        auto& builder = *LLVM::Backend::GetBuilder();

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
            CLEAR_UNREACHABLE("unsupported type conversion");
        }

        return castedValue;
    }

}