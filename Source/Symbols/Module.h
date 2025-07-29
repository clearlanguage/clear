#pragma once 

#include "AST/ASTNode.h"
#include "Compilation/BuildConfig.h"
#include "Symbols/FunctionCache.h"
#include "Symbols/SymbolTable.h"
#include "Symbols/TypeRegistry.h"

#include "Symbols/Symbol.h"

#include <llvm/IR/Module.h>
#include <memory>
#include <unordered_map>

namespace clear 
{
    class Module : public std::enable_shared_from_this<Module>
    {
    public:
        Module(const std::string& name, std::shared_ptr<llvm::LLVMContext> context, std::shared_ptr<Module> builtins);
        Module(Module* parent, const std::string& name, std::shared_ptr<Module> builtins);
        ~Module() = default;

        void PropagateSymbolTables();

        std::shared_ptr<Module> EmplaceOrReturn(const std::string& moduleName);
        std::shared_ptr<Module> Return(const std::string& moduleName);
        void InsertModule(const std::string& name, std::shared_ptr<Module> module_);

        void Codegen(const BuildConfig& config);
        void Link();

        llvm::Module* GetModule()  { return m_Module.get(); }
        std::unique_ptr<llvm::Module> TakeModule() { return std::move(m_Module); }
        
        std::shared_ptr<llvm::LLVMContext> GetContext() { return m_Context; }
        std::shared_ptr<ASTNodeBase> GetRoot() { return m_Root; }
		std::shared_ptr<TypeRegistry> GetTypeRegistry() { return m_TypeRegistry; }

        CodegenContext GetCodegenContext();

        Symbol Lookup(const std::string& symbol);
        Symbol Lookup(const std::string& fn, const std::vector<Parameter>& params);

        // NOTE: for now only types but may be extended to support more symbols
        void CreateAlias(const std::string& aliasName, const std::string& symbolName);
        void RemoveAlias(const std::string& aliasName);

        std::shared_ptr<Type> GetTypeFromToken(const Token& token);

    private:
        std::string m_ModuleName;

        std::unique_ptr<llvm::Module> m_Module;
        std::shared_ptr<llvm::LLVMContext> m_Context;
        std::shared_ptr<llvm::IRBuilder<>> m_Builder;
        
        std::unordered_map<std::string, std::shared_ptr<Module>> m_ContainedModules;
        std::shared_ptr<ASTNodeBase> m_Root;

        std::shared_ptr<TypeRegistry> m_TypeRegistry;

        bool m_CodeGenerated = false;
        bool m_IsBuiltin = false;
    };
}
