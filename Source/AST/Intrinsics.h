#pragma once 

#include "ASTNode.h"

namespace clear 
{
    class Intrinsics
    {
    public:
        static llvm::Value* ApplyIntrinsic(const std::string& name, llvm::Value* value, std::shared_ptr<Type> type, CodegenContext& ctx);
        static bool IsIntrinsic(const std::string& name);

    };
}