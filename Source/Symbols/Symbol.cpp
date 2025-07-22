#include "Symbol.h"

#include "Module.h"
#include "Type.h"
#include "FunctionCache.h"
#include <llvm/IR/IRBuilder.h>


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

    Symbol Symbol::CreateValue(llvm::Value* value, std::shared_ptr<Type> type, bool shouldMemcpy)
    {
        ValueSymbol symbol;

        symbol.Values.push_back(value);
        symbol.Types.push_back(type);
        symbol.ShouldMemcpy = shouldMemcpy;

        return Symbol {
            .Kind = SymbolKind::Value,
            .Data = symbol
        };
    }

    Symbol Symbol::CreateVariable(StringRef name, llvm::Value* value, std::shared_ptr<Type> type)
    {
        Symbol symbol = CreateValue(value, type);
        symbol.Metadata = name;

        return symbol;
    }

    Symbol Symbol::CreateFunction(FunctionInstance* instance)
    {
        FunctionSymbol symbol = { instance };

        return Symbol {
            .Kind = SymbolKind::Function,
            .Data = symbol
        };
    }
    
    Symbol Symbol::CreateFunctionTemplate(FunctionTemplate* template_)
    {
        FunctionTemplateSymbol symbol = { template_ };

        return Symbol {
            .Kind = SymbolKind::FunctionTemplate,
            .Data = symbol
        };
    }

    Symbol Symbol::CreateTuple(const llvm::SmallVector<llvm::Value*>& values, const llvm::SmallVector<std::shared_ptr<Type>>& types)
    {
        ValueSymbol symbol;

        symbol.Values.insert(symbol.Values.begin(), values.begin(), values.end());;
        symbol.Types.insert(symbol.Types.begin(), types.begin(), types.end());

        return Symbol {
            .Kind = SymbolKind::Value,
            .Data = symbol
        };
    }

    Symbol Symbol::CreateIdentifier(StringRef identifierName)
    {
        return Symbol {
            .Kind = SymbolKind::Identifier,
            .Data = String(identifierName)
        };
    }

    Symbol Symbol::CreateClassTemplate(const ClassTemplate& classTemplate)
    {
        return Symbol {
            .Kind = SymbolKind::ClassTemplate,
            .Data = classTemplate
        };
    }
    
    Symbol Symbol::CreateInferType(bool IsConst)
    {
        return Symbol {
            .Kind = SymbolKind::InferType, 
            .Data = InferTypeSymbol { IsConst }
        };
    }

    Symbol Symbol::GetUInt64(std::shared_ptr<Module> module_, llvm::IRBuilder<>& builder, uint64_t value)
    {
        return Symbol::CreateValue(builder.getInt64(value), module_->Lookup("uint64").GetType());
    }
    
    Symbol Symbol::GetBooleanType(std::shared_ptr<Module> module_)
    {
        return Symbol::CreateType(module_->Lookup("bool").GetType());
    }
    
    llvm::Value* Symbol::GetLLVMValue()
    {
        CLEAR_VERIFY(Kind == SymbolKind::Value, "cannot call Symbol::GetValue() when kind is not Value");
        auto& value = std::get<ValueSymbol>(Data);
        return value.Values[0];
    }

    std::shared_ptr<Type> Symbol::GetType() const
    {
        CLEAR_VERIFY(Kind == SymbolKind::Type || Kind == SymbolKind::Value, "cannot call Symbol::GetType() when kind is not Type or Value");

        if(Kind == SymbolKind::Type)
            return std::get<TypeSymbol>(Data).Type_;

        auto& value = std::get<ValueSymbol>(Data);
        return value.Types[0];
    }

    std::shared_ptr<Module> Symbol::GetModule() const
    {
        CLEAR_VERIFY(Kind == SymbolKind::Module, "cannot call Symbol::GetModule() when kind is not Module");
        return std::get<ModuleSymbol>(Data).Module_;
    }

    std::pair<llvm::Value*, std::shared_ptr<Type>> Symbol::GetValue() const
    {
        CLEAR_VERIFY(Kind == SymbolKind::Value, "cannot call Symbol::GetValue() when kind is not Value");
        auto& value = std::get<ValueSymbol>(Data);

        return std::make_pair(value.Values[0], value.Types[0]);
    }

    FunctionInstance* Symbol::GetFunction() const
    {
        CLEAR_VERIFY(Kind == SymbolKind::Function, "cannot call Symbol::GetFunction() when kind is not Function");
        return std::get<FunctionSymbol>(Data).Instance;
    }
    
    ValueSymbol Symbol::GetValueSymbol()
    {
        CLEAR_VERIFY(Kind == SymbolKind::Value, "cannot call Symbol::GetValueSymbol() when kind is not Value");
        return std::get<ValueSymbol>(Data);
    }

    const ValueSymbol& Symbol::GetValueTuple() const
    {
        CLEAR_VERIFY(Kind == SymbolKind::Value, "cannot call Symbol::GetValueTuple() when kind is not Value");
        return std::get<ValueSymbol>(Data);
    }
    
    FunctionTemplate* Symbol::GetFunctionTemplate() const
    {
        CLEAR_VERIFY(Kind == SymbolKind::FunctionTemplate, "cannot call Symbol::GetFunctionTemplate() when kind is not FunctionTemplate");
        return std::get<FunctionTemplateSymbol>(Data).Template;
    }

    ClassTemplate Symbol::GetClassTemplate()
    {
        CLEAR_VERIFY(Kind == SymbolKind::ClassTemplate, "cannot call Symbol::GetClassTemplate() when kind is not ClassTemplate");
        return std::get<ClassTemplate>(Data);
    }
    
    InferTypeSymbol Symbol::GetInferType()
    {
        CLEAR_VERIFY(Kind == SymbolKind::InferType, "cannot call Symbol::GetInferType() when kind is not InferType");
        return std::get<InferTypeSymbol>(Data);
    }
}