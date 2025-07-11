#pragma once 

#include "Symbol.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>

namespace clear 
{
    struct SymbolOps
    {
        static Symbol Add(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Sub(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Mul(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Div(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Mod(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder);

        static Symbol BitAnd(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol BitOr(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol BitXor(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder);

        static Symbol Shl(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder);
        static Symbol Shr(Symbol& lhs, Symbol& rhs, llvm::IRBuilder<>& builder);

        static Symbol And(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType, llvm::IRBuilder<>& builder);
        static Symbol Or(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType, llvm::IRBuilder<>& builder);

        static Symbol Eq(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType, llvm::IRBuilder<>& builder);
        static Symbol Neq(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType, llvm::IRBuilder<>& builder);
        static Symbol Lt(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType, llvm::IRBuilder<>& builder);
        static Symbol Lte(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType, llvm::IRBuilder<>& builder);
        static Symbol Gt(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType,  llvm::IRBuilder<>& builder);
        static Symbol Gte(Symbol& lhs, Symbol& rhs, std::shared_ptr<Type> booleanType, llvm::IRBuilder<>& builder);

        static Symbol Neg(Symbol& operand, llvm::IRBuilder<>& builder, std::shared_ptr<Type> signedType);
        static Symbol Not(Symbol& operand, llvm::IRBuilder<>& builder);

        static Symbol Call(Symbol& fn, const llvm::SmallVector<llvm::Value*>& args, llvm::IRBuilder<>& builder);

        static Symbol Load(Symbol& ptr, llvm::IRBuilder<>& builder);
        static void   Store(Symbol& ptr, Symbol& value, llvm::IRBuilder<>& builder, llvm::Module& module, bool isFirstTime = false);
        
        static Symbol GEPStruct(Symbol& basePtr, Symbol& resPtrType, size_t index, llvm::IRBuilder<>& builder);
        static Symbol GEP(Symbol& basePtr, Symbol& resPtrType, const llvm::SmallVector<llvm::Value*>& indices, llvm::IRBuilder<>& builder);

        static void Promote(Symbol& value1, Symbol& value2, llvm::IRBuilder<>& builder);


        static llvm::Function* GetInitGlobalsFunction(llvm::Module& module);
    };
}