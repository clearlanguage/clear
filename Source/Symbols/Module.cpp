#include "Module.h"
#include "AST/ASTNode.h"
#include "Symbols/FunctionCache.h"
#include "Symbols/TypeRegistry.h"
#include "Symbols/SymbolTable.h"
#include <memory>

namespace clear 
{
    Module::Module(const std::string& name, std::shared_ptr<llvm::LLVMContext> context, std::shared_ptr<Module> builtins)
        : m_ModuleName(name)
    {
        m_Context = context;
		m_Builder = std::make_shared<llvm::IRBuilder<>>(*m_Context);
        m_Module  = std::make_unique<llvm::Module>(name, *m_Context);

        m_Root = std::make_shared<ASTNodeBase>();
        m_Root->CreateSymbolTable();

        m_TypeRegistry = std::make_shared<TypeRegistry>(m_Context);

        if(builtins)
        {
            m_ContainedModules["__clrt_internal"] = builtins;
        }
        else
        {
            m_TypeRegistry->RegisterBuiltinTypes();
            m_IsBuiltin = true;
        } 

    }

    Module::Module(Module* parent, const std::string& name, std::shared_ptr<Module> builtins)
        : m_ModuleName(name)
    {
        m_Context = parent->m_Context;
        m_Builder = std::make_shared<llvm::IRBuilder<>>(*m_Context);
        m_Module  = std::make_unique<llvm::Module>(name, *m_Context);

        m_TypeRegistry = std::make_shared<TypeRegistry>(m_Context);
        m_ContainedModules["__clrt_internal"] = builtins;

        m_Root = std::make_shared<ASTNodeBase>();
        m_Root->CreateSymbolTable();
        m_Root->GetSymbolTable()->SetPrevious(parent->GetRoot()->GetSymbolTable());
    }

 
    void Module::PropagateSymbolTables()
    {
        m_Root->PropagateSymbolTableToChildren();
       
        for(auto& [_, mod] : m_ContainedModules)
        {
            mod->PropagateSymbolTables();
        }
    }

    std::shared_ptr<Module> Module::EmplaceOrReturn(const std::string& moduleName)
    {
        auto [it, sucess] = m_ContainedModules.try_emplace(moduleName, std::make_shared<Module>(this, moduleName, m_ContainedModules["__clrt_internal"]));
        return it->second;
    }
    
    std::shared_ptr<Module> Module::Return(const std::string& moduleName)
    {
        return m_ContainedModules[moduleName];
    }
    
    void Module::InsertModule(const std::string& name, std::shared_ptr<Module> module_)
    {
        m_ContainedModules[name] = module_;
    }

    void Module::Codegen(const BuildConfig& config)
    {
        if(m_CodeGenerated)
            return;

        m_CodeGenerated = true;

        for(const auto& [_, mod] : m_ContainedModules)
        {
            mod->Codegen(config);
        }

        CodegenContext context = GetCodegenContext();
        m_Root->Codegen(context);
    }
    
    void Module::Link()
    {
        for(const auto& [name, mod] : m_ContainedModules)
        {
            mod->Link();
        }

        if(!m_Module) // module has already been taken and linked elsewhere so should merge gracefully later
            return;

        llvm::Linker linker(*m_Module);

        for(const auto& [name, mod] : m_ContainedModules)
        {
            if(!mod->GetModule()) // same as above
                continue;
            
            linker.linkInModule(mod->TakeModule());
        }
    }
    
    CodegenContext Module::GetCodegenContext()
    {
        CodegenContext ctx(m_ModuleName, *m_Context, *m_Builder, *m_Module);
        
        ctx.ClearModule = shared_from_this();
        ctx.ClearModuleSecondary = shared_from_this();
        ctx.TypeReg = m_TypeRegistry;

        return ctx;
    }
    
    Symbol Module::Lookup(const std::string& symbolName)
    {
        if(auto ty = m_TypeRegistry->GetType(symbolName))
            return Symbol::CreateType(ty);
    
        // TODO: any function/variable should be exported which will move it to the modules symbol table
        // for now everything is public
        
        auto tbl = m_Root->GetSymbolTable();
        
        Allocation alloca = tbl->GetAlloca(symbolName);

        if(alloca.Alloca)
        {
            return Symbol::CreateValue(alloca.Alloca, m_TypeRegistry->GetPointerTo(alloca.Type));
        }


        for(const auto& [name, containedModule] : m_ContainedModules)
        {
            if(name == symbolName)
                return Symbol::CreateModule(containedModule);

            Symbol symbol = containedModule->Lookup(symbolName);

            if(symbol.Kind != SymbolKind::None)
                return symbol;
        }

        return Symbol();
    }
    
    Symbol Module::Lookup(const std::string& fn, const std::vector<Parameter>& params)
    {
        FunctionTemplate& template_ = m_Root->GetSymbolTable()->GetTemplate(fn, params);

        if(template_.SourceModule)
            return Symbol::CreateFunctionTemplate(&template_);

        for(const auto& [name, containedModule] : m_ContainedModules)
        {
            Symbol symbol = containedModule->Lookup(fn, params);

            if(symbol.Kind != SymbolKind::None)
                return symbol;
        }

        return Symbol();
    }

    void Module::CreateAlias(const std::string& aliasName, const std::string& symbolName)
    {
        Symbol symbol = Lookup(symbolName);
        m_TypeRegistry->RegisterType(aliasName, symbol.GetType());
    }
    
    void Module::RemoveAlias(const std::string& aliasName)
    {
        m_TypeRegistry->RemoveType(aliasName);
    }

    std::shared_ptr<Type> Module::GetTypeFromToken(const Token& token)
    {
        if(auto ty = m_TypeRegistry->GetTypeFromToken(token))
            return ty;

        return m_ContainedModules["__clrt_internal"]->GetTypeFromToken(token);
    }
}