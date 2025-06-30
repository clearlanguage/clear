#pragma once 

#include <API/LLVM/LLVMInclude.h>

#include <variant>

namespace clear 
{
    class Module;
    class Type;
    class FunctionInstance;

    using String = llvm::SmallString<64>;
    using StringRef = llvm::StringRef;

    enum class SymbolKind 
    {
        None=0,
        Function, 
        Module, 
        Type,
        Value, 
        Identifier
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
        TypeSymbol, 
        String>;

    struct Symbol 
    {
        SymbolKind Kind = SymbolKind::None;
        std::optional<String> Metadata;
        SymbolData  Data;

        static Symbol CreateType(std::shared_ptr<Type> type);
        static Symbol CreateModule(std::shared_ptr<Module> module_);
        static Symbol CreateValue(llvm::Value* value, std::shared_ptr<Type> type);
        static Symbol CreateVariable(StringRef name, llvm::Value* value, std::shared_ptr<Type> type);
        static Symbol CreateFunction(FunctionInstance* instance);
        static Symbol CreateTuple(const std::vector<llvm::Value*>& values, const std::vector<std::shared_ptr<Type>>& types);
        static Symbol CreateIdentifier(StringRef identifierName);

        std::shared_ptr<Type>   GetType() const;
        std::shared_ptr<Module> GetModule() const;
        std::pair<llvm::Value*, std::shared_ptr<Type>> GetValue() const;
        FunctionInstance* GetFunction() const;
        const ValueSymbol& GetValueTuple() const;
    };
}