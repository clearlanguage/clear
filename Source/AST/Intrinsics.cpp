#include "Intrinsics.h"

#include "Core/Log.h"
#include "Symbols/TypeCasting.h"

namespace clear 
{
    llvm::Value* Intrinsics::ApplyIntrinsic(const std::string& name, llvm::Value* value, std::shared_ptr<Type> type, CodegenContext& ctx)
    {
        CLEAR_VERIFY(IsIntrinsic(name), "not a valid intrinsic");

        if(name == "__trap")
        {
            llvm::Function* trapFunc = llvm::Intrinsic::getDeclaration(&ctx.Module, llvm::Intrinsic::trap);
            ctx.Builder.CreateCall(trapFunc);
            ctx.Builder.CreateUnreachable();
            return nullptr;
        }
        
        if(name == "sizeof")
        {
            return ctx.Builder.getInt64(ctx.Module.getDataLayout().getTypeSizeInBits(value->getType()) / 8);
        }

        if (name == "len") 
        {
            CLEAR_VERIFY(type->IsArray(),"Not a valid array");
            auto x = std::dynamic_pointer_cast<ArrayType>(type);
            return ctx.Builder.getInt64(x->GetArraySize());
        }

        CLEAR_VERIFY(value && type, "value or type was invalid");

        /* auto ty = ctx.Registry.GetType(name); */
        //CLEAR_VERIFY(TypeCasting::CanBeCasted(type, ty), "type ", type->GetHash(), " cannot be casted to ", ty->GetHash());
        CLEAR_UNREACHABLE("unimplemented");
        return nullptr;
        
        //return TypeCasting::Cast(value, type, ty, ctx.Builder);
    }

    bool Intrinsics::IsIntrinsic(const std::string& name)
    {
        static std::unordered_set<std::string> s_Intrinsics = {
            "int8", "int16", "int32", "int64",
            "uint8", "uint16", "uint32", "uint64",
            "bool", "float32", "float64", "int", "float", "uint",
             "__trap", "sizeof","len"
        };

        return s_Intrinsics.contains(name);
    }


}