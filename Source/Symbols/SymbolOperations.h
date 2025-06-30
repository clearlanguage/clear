#pragma once 

#include "Symbol.h"
#include <llvm/IR/IRBuilder.h>

namespace clear 
{
    struct SymbolOps
    {
        static Symbol Add(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Sub(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Mul(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Div(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Mod(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol And(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Or(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);

        static Symbol BitAnd(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol BitOr(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol BitXor(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol BitNot(const Symbol& operand, llvm::IRBuilder<>& builder);
        static Symbol Shl(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Shr(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);

        static Symbol Eq(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Neq(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Lt(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Lte(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Gt(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Gte(const Symbol& lhs, const Symbol& rhs, llvm::IRBuilder<>& builder);

        static Symbol Neg(const Symbol& operand, llvm::IRBuilder<>& builder);
        static Symbol Not(const Symbol& operand, llvm::IRBuilder<>& builder);

        static Symbol Call(const Symbol& fn, const llvm::SmallVector<llvm::Value*>& args, llvm::IRBuilder<>& builder);

        static Symbol Cast(const Symbol& value, const Symbol& type, llvm::IRBuilder<>& builder);

        static Symbol Load(const Symbol& ptr, llvm::IRBuilder<>& builder);
        static void   Store(const Symbol& ptr, const Symbol& value, llvm::IRBuilder<>& builder);
        static Symbol GEP(const Symbol& basePtr, const llvm::SmallVector<size_t>& indices, llvm::IRBuilder<>& builder);

        void Promote(Symbol& value1, Symbol& value2, llvm::IRBuilder<>& builder);
    };
}