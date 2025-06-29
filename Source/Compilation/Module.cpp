#include "Module.h"

namespace clear 
{
    Module::Module(const std::string& name)
        : m_ModuleName(name)
    {
        m_Context = std::make_shared<llvm::LLVMContext>();
		m_Builder = std::make_shared<llvm::IRBuilder<>>(*m_Context);
        m_Module  = std::make_unique<llvm::Module>(name, *m_Context);

        m_SymbolTable = std::make_shared<SymbolTable>(m_Context);
        m_SymbolTable->GetTypeRegistry().RegisterBuiltinTypes();
    }

    Module::Module(Module* parent, const std::string& name)
    {   
        m_Context = parent->m_Context;
        m_Builder = std::make_shared<llvm::IRBuilder<>>(*m_Context);
        m_Module  = std::make_unique<llvm::Module>(name, *m_Context);

        m_SymbolTable = std::make_shared<SymbolTable>(m_Context);
        m_SymbolTable->SetPrevious(parent->m_SymbolTable);
    }

    void Module::PushNode(const std::shared_ptr<ASTNodeBase>& node)
    {
        m_Nodes.push_back(node);

        if(auto tbl = node->GetSymbolTable())
        {
            tbl->SetPrevious(m_SymbolTable);
        }
    }

    void Module::PropagateSymbolTables()
    {
        for(auto& node : m_Nodes)
        {
            node->PropagateSymbolTableToChildren();
        }

        for(auto& [_, mod] : m_ContainedModules)
        {
            mod->PropagateSymbolTables();
        }
    }

    void Module::Codegen(const BuildConfig& config)
    {
        CodegenContext context(m_ModuleName, *m_Context, *m_Builder, *m_Module);

        for(auto& node : m_Nodes)
        {
            node->Codegen(context);
        }
    }
}