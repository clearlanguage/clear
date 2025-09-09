#include "Module.h"
#include "AST/ASTNode.h"
#include "Core/Log.h"
#include "Symbols/FunctionCache.h"
#include "Symbols/TypeRegistry.h"
#include <memory>
#include <optional>

namespace clear 
{
    Module::Module(const std::string& name, std::shared_ptr<llvm::LLVMContext> context, std::shared_ptr<Module> builtins, const std::filesystem::path& path)
        : m_ModuleName(name), m_ModulePath(path)
    {
        m_Context = context;
		m_Builder = std::make_shared<llvm::IRBuilder<>>(*m_Context);
        m_Module  = std::make_unique<llvm::Module>(name, *m_Context);

        m_Root = std::make_shared<ASTBlock>();

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

        m_Root = std::make_shared<ASTBlock>();
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
		CLEAR_UNREACHABLE("depricated");

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
    
	std::optional<std::shared_ptr<Symbol>> Module::Lookup(llvm::StringRef symbolName)
    {
        if(auto ty = m_TypeRegistry->GetType(symbolName.str()))
            return std::make_shared<Symbol>(Symbol::CreateType(ty));

        for(const auto& [name, containedModule] : m_ContainedModules)
        {
            if(name == symbolName)
                return std::make_shared<Symbol>(Symbol::CreateModule(containedModule));

			auto symbol = containedModule->Lookup(symbolName.str());

            if(symbol.has_value())
                return symbol;
        }

        return std::nullopt;
    }
    
	std::optional<Symbol> Module::Lookup(llvm::StringRef fn, llvm::ArrayRef<Parameter> params)
    {
		return Symbol();
    }

    void Module::CreateAlias(const std::string& aliasName, const std::string& symbolName)
    {
    }
    
    void Module::RemoveAlias(const std::string& aliasName)
    {
    }

    std::shared_ptr<Type> Module::GetTypeFromToken(const Token& token)
    {
        if(auto ty = m_TypeRegistry->GetTypeFromToken(token))
            return ty;

        return m_ContainedModules["__clrt_internal"]->GetTypeFromToken(token);
    }

	void Module::ExposeSymbol(llvm::StringRef name, std::shared_ptr<Symbol> sym)
	{
		m_ExposedSymbols[name.str()] = sym;
	}
}
