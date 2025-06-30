#include "Symbol.h"

namespace clear 
{
    Symbol Symbol::CreateType(std::shared_ptr<Type> type)
    {
        TypeSymbol symbol = { type };

        return Symbol {
            .Kind = SymbolKind::Type,
            .Data = symbol
        };
    }

    Symbol Symbol::CreateModule(std::shared_ptr<Module> module_)
    {
        ModuleSymbol symbol = { module_ };

        return Symbol {
            .Kind = SymbolKind::Module,
            .Data = symbol
        };
    }

    Symbol Symbol::CreateValue(llvm::Value* value, std::shared_ptr<Type> type)
    {
        ValueSymbol symbol;

        symbol.Values.push_back(value);
        symbol.Types.push_back(type);


        return Symbol {
            .Kind = SymbolKind::Value,
            .Data = symbol
        };
    }

    Symbol Symbol::CreateFunction(FunctionInstance* instance)
    {
        FunctionSymbol symbol = { instance };

        return Symbol {
            .Kind = SymbolKind::Function,
            .Data = symbol
        };
    }

    Symbol Symbol::CreateTuple(const std::vector<llvm::Value*>& values, const std::vector<std::shared_ptr<Type>>& types)
    {
        ValueSymbol symbol;

        symbol.Values.insert(symbol.Values.begin(), values.begin(), values.end());;
        symbol.Types.insert(symbol.Types.begin(), types.begin(), types.end());

        return Symbol {
            .Kind = SymbolKind::Value,
            .Data = symbol
        };
    }

    std::shared_ptr<Type> Symbol::GetType()
    {
        CLEAR_VERIFY(Kind == SymbolKind::Type, "cannot call Symbol::GetType() when kind is not Type");
        return std::get<TypeSymbol>(Data).Type_;
    }

    std::shared_ptr<Module> Symbol::GetModule()
    {
        CLEAR_VERIFY(Kind == SymbolKind::Module, "cannot call Symbol::GetModule() when kind is not Module");
        return std::get<ModuleSymbol>(Data).Module_;
    }

    std::pair<llvm::Value*, std::shared_ptr<Type>> Symbol::GetValue()
    {
        CLEAR_VERIFY(Kind == SymbolKind::Value, "cannot call Symbol::GetValue() when kind is not Value");
        auto& value = std::get<ValueSymbol>(Data);

        return std::make_pair(value.Values[0], value.Types[0]);
    }

    FunctionInstance* Symbol::GetFunction()
    {
        CLEAR_VERIFY(Kind == SymbolKind::Function, "cannot call Symbol::GetFunction() when kind is not Function");
        return std::get<FunctionSymbol>(Data).Instance;
    }

    ValueSymbol& Symbol::GetValueTuple()
    {
        CLEAR_VERIFY(Kind == SymbolKind::Value, "cannot call Symbol::GetValueTuple() when kind is not Value");
        return std::get<ValueSymbol>(Data);;
    }
}