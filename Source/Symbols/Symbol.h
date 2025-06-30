#pragma once 

#include <API/LLVM/LLVMInclude.h>

#include "Module.h"
#include "Type.h"
#include "FunctionCache.h"

#include <variant>

namespace clear 
{
    enum class SymbolKind 
    {
        None=0,
        Function, 
        Module, 
        Type,
        Value
    };

    struct FunctionSymbol 
    {
        FunctionInstance* Instance;
    };

    struct ValueSymbol 
    {
        llvm::SmallVector<llvm::Value*> Values;
        llvm::SmallVector<std::shared_ptr<Type>> Types;
    };

    struct ModuleSymbol 
    {
        std::shared_ptr<Module> Module_;
    };

    struct TypeSymbol 
    {
        std::shared_ptr<Type> Type_;
    };

    using SymbolData = std::variant<
        std::monostate,
        FunctionSymbol,
        ValueSymbol,
        ModuleSymbol, 
        TypeSymbol>;

    struct Symbol 
    {
        SymbolKind Kind = SymbolKind::None;
        std::string Name;
        SymbolData  Data;

        static Symbol CreateType(std::shared_ptr<Type> type);
        static Symbol CreateModule(std::shared_ptr<Module> module_);
        static Symbol CreateValue(llvm::Value* value, std::shared_ptr<Type> type);
        static Symbol CreateFunction(FunctionInstance* instance);
        static Symbol CreateTuple(const std::vector<llvm::Value*>& values, const std::vector<std::shared_ptr<Type>>& types);
    
        std::shared_ptr<Type>   GetType();
        std::shared_ptr<Module> GetModule();
        std::pair<llvm::Value*, std::shared_ptr<Type>> GetValue();
        FunctionInstance* GetFunction();
        ValueSymbol& GetValueTuple();
    };
}