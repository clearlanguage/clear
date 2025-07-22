#pragma once 

#include <API/LLVM/LLVMInclude.h>

#include <llvm/IR/IRBuilder.h>
#include <variant>

namespace clear 
{
    class Module;
    class Type;
    class FunctionInstance;
    class FunctionTemplate;
    class ASTNodeBase;

    using String = llvm::SmallString<64>;
    using StringRef = llvm::StringRef;

    enum class SymbolKind 
    {
        None=0,
        Function, 
        FunctionTemplate,
        Module, 
        Type,
        Value, 
        Identifier, 
        ClassTemplate, 
        InferType
    };

    struct FunctionSymbol 
    {
        FunctionInstance* Instance;
    };

    struct FunctionTemplateSymbol 
    {
        FunctionTemplate* Template;
    };

    struct ValueSymbol 
    {
        llvm::SmallVector<llvm::Value*> Values;
        llvm::SmallVector<std::shared_ptr<Type>> Types;
        bool ShouldMemcpy = false;
    };

    struct ModuleSymbol 
    {
        std::shared_ptr<Module> Module_;
    };

    struct TypeSymbol 
    {
        std::shared_ptr<Type> Type_;
    };

    struct InferTypeSymbol 
    {
        bool IsConst = false;
    };

    struct ClassTemplate
    {
        std::string Name;
        std::shared_ptr<ASTNodeBase> Class;
        llvm::SmallVector<std::string> Generics;
    };

    using SymbolData = std::variant<
        std::monostate,
        FunctionSymbol,
        FunctionTemplateSymbol,
        ValueSymbol,
        ModuleSymbol, 
        TypeSymbol, 
        String, 
        ClassTemplate, 
        InferTypeSymbol>;

    struct Symbol 
    {
        SymbolKind Kind = SymbolKind::None;
        std::optional<String> Metadata;
        SymbolData  Data;

        static Symbol CreateType(std::shared_ptr<Type> type);
        static Symbol CreateModule(std::shared_ptr<Module> module_);
        static Symbol CreateValue(llvm::Value* value, std::shared_ptr<Type> type, bool shouldMemcpy = false);
        static Symbol CreateVariable(StringRef name, llvm::Value* value, std::shared_ptr<Type> type);
        static Symbol CreateFunction(FunctionInstance* instance);
        static Symbol CreateFunctionTemplate(FunctionTemplate* template_);
        static Symbol CreateTuple(const llvm::SmallVector<llvm::Value*>& values, const llvm::SmallVector<std::shared_ptr<Type>>& types);
        static Symbol CreateIdentifier(StringRef identifierName);
        static Symbol CreateClassTemplate(const ClassTemplate& classTemplate);
        static Symbol CreateInferType(bool IsConst);

        // helpers
        static Symbol GetUInt64(std::shared_ptr<Module> module_, llvm::IRBuilder<>& builder, uint64_t value);
        static Symbol GetBooleanType(std::shared_ptr<Module> module_);

        llvm::Value* GetLLVMValue();
        std::shared_ptr<Type>   GetType() const;
        std::shared_ptr<Module> GetModule() const;
        std::pair<llvm::Value*, std::shared_ptr<Type>> GetValue() const;
        ValueSymbol GetValueSymbol();
        FunctionInstance* GetFunction() const;
        FunctionTemplate* GetFunctionTemplate() const;
        const ValueSymbol& GetValueTuple() const;
        ClassTemplate GetClassTemplate();
        InferTypeSymbol GetInferType();
    };
}