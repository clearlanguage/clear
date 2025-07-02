#pragma once 

#include "AST/ASTNode.h"
#include "Compilation/BuildConfig.h"
#include "Symbols/SymbolTable.h"

#include <llvm/IR/Module.h>
#include <memory>
#include <unordered_map>

namespace clear 
{
    class Module : public std::enable_shared_from_this<Module>
    {
    public:
        Module(const std::string& name = "");
        Module(Module* parent, const std::string& name = "");
        ~Module() = default;

        void PushNode(const std::shared_ptr<ASTNodeBase>& node);
        void PropagateSymbolTables();

        std::shared_ptr<Module> EmplaceOrReturn(const std::string& moduleName);
        std::shared_ptr<Module> Return(const std::string& moduleName);

        void Codegen(const BuildConfig& config);
        void Link();

        llvm::Module* GetModule()   { return m_Module.get(); }
        std::unique_ptr<llvm::Module> TakeModule() { return std::move(m_Module); }
        std::shared_ptr<llvm::LLVMContext> GetContext() { return m_Context; }

        CodegenContext GetCodegenContext();

        //NOTE: hack until export keyword is implemented
        std::shared_ptr<SymbolTable> GetSymbolTable() { return m_Nodes[0]->GetSymbolTable(); } 

    private:
        std::string m_ModuleName;

        std::unique_ptr<llvm::Module> m_Module;
        std::shared_ptr<llvm::LLVMContext> m_Context;
        std::shared_ptr<llvm::IRBuilder<>> m_Builder;
        
        std::unordered_map<std::string, std::shared_ptr<Module>> m_ContainedModules;
        std::vector<std::shared_ptr<ASTNodeBase>> m_Nodes;

        std::shared_ptr<SymbolTable> m_SymbolTable;
    };
}