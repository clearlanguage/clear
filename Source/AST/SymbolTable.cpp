#include "SymbolTable.h"

#include "ASTNode.h"
#include "Core/Log.h"

namespace clear 
{
    SymbolTable::SymbolTable(const std::shared_ptr<SymbolTable>& other)
    {
        m_Previous = other;
    }

    Allocation SymbolTable::RequestTemporary(const std::shared_ptr<Type>& type, llvm::IRBuilder<>& builder)
    {
        auto [it, inserted] = m_Temporaries.try_emplace(type->Get(), Allocation{});
        if (!inserted)
           return it->second;

        llvm::BasicBlock* insertBlock = builder.GetInsertBlock();
        
        CLEAR_VERIFY(insertBlock, "cannot create an alloca without function");  
	    auto ip = builder.saveIP(); 
	    llvm::Function* function = insertBlock->getParent();    
	    builder.SetInsertPoint(&function->getEntryBlock());

	    Allocation allocation;
        allocation.Alloca = builder.CreateAlloca(type->Get(), nullptr, std::format("{}.{}", "tmp", type->GetHash()));
        allocation.Type = type; 
	    builder.restoreIP(ip);  
        m_Temporaries[type->Get()] = allocation;

        return allocation;
    }

    Allocation SymbolTable::CreateGlobal(const std::string &name, std::shared_ptr<Type> type, llvm::Module &module, llvm::Value *value)
    {
        Allocation alloca;

        alloca.Alloca = new llvm::GlobalVariable(
            module,
            type->Get(),
            false,
            llvm::GlobalValue::ExternalLinkage,
            value ? llvm::cast<llvm::Constant>(value) : nullptr,
            name
        );
        
        alloca.Type = type;
        m_Variables[name] = alloca;

        return alloca;
    }

    Allocation SymbolTable::CreateAlloca(const std::string& name, std::shared_ptr<Type> type, llvm::IRBuilder<>& builder)
    {
        if(name == "test")
        {
            
        }

        llvm::BasicBlock* insertBlock = builder.GetInsertBlock();
        
        CLEAR_VERIFY(insertBlock, "cannot create an alloca without function");

		auto ip = builder.saveIP();

		llvm::Function* function = insertBlock->getParent();

		builder.SetInsertPoint(&function->getEntryBlock());
	
		Allocation allocation;
        allocation.Alloca = builder.CreateAlloca(type->Get(), nullptr, name);
        allocation.Type = type;

		builder.restoreIP(ip);

        m_Variables[name] = allocation;
        return allocation;
    }


    void SymbolTable::RegisterAllocation(const std::string& name, Allocation allocation)
    {
        m_Variables[name] = allocation;
    }

    Allocation SymbolTable::GetAlloca(const std::string& name)
    {
        if(m_Variables.contains(name)) return m_Variables.at(name);

        std::shared_ptr<SymbolTable> ptr = m_Previous;

        while(ptr)
        {
            if(ptr->m_Variables.contains(name)) 
                return ptr->m_Variables.at(name);
                
            ptr = ptr->m_Previous;
        }

        CLEAR_UNREACHABLE("unable to find variable ", name);
        return {};
    }

    void SymbolTable::SetPrevious(const std::shared_ptr<SymbolTable>& previous)
    {
        m_Previous = previous;
    }

    void SymbolTable::FlushDestructors(CodegenContext& ctx)
    {
        for(const auto& [name, allocation] : m_Variables)
        {
            if(allocation.Type->IsClass())
            {
                auto classType = std::dynamic_pointer_cast<ClassType>(allocation.Type);

                std::string mangledName = std::format("{}.{}", allocation.Type->GetHash(), "__destruct__");

                if(HasTemplate(mangledName))
                {
                    Parameter param;
                    param.Name = "this";
                    param.Type = ctx.Registry.GetPointerTo(classType);

                    FunctionInstance& instance = InstantiateOrReturn(mangledName, { param }, nullptr, ctx);
                    ctx.Builder.CreateCall( instance.Function, { allocation.Alloca });
                }
            }
        }

        for(const auto& [type, allocation] : m_Temporaries)
        {
            if(allocation.Type->IsClass())
            {
                auto classType = std::dynamic_pointer_cast<ClassType>(allocation.Type);

                std::string mangledName = std::format("{}.{}", allocation.Type->GetHash(), "__destruct__");

                if(HasTemplate(mangledName))
                {
                    Parameter param;
                    param.Name = "this";
                    param.Type = ctx.Registry.GetPointerTo(classType);

                    FunctionInstance& instance = InstantiateOrReturn(mangledName, { param }, nullptr, ctx);
                    ctx.Builder.CreateCall( instance.Function, { allocation.Alloca });
                }
            }
        }

    }

    FunctionInstance& SymbolTable::GetInstance(const std::string& instanceName)
    {
        if(m_FunctionCache.HasInstance(instanceName)) return m_FunctionCache.GetInstance(instanceName);

        std::shared_ptr<SymbolTable> ptr = m_Previous;

        while(ptr)
        {
            if(ptr->m_FunctionCache.HasInstance(instanceName)) 
                return ptr->m_FunctionCache.GetInstance(instanceName);
                
            ptr = ptr->m_Previous;
        }

        CLEAR_UNREACHABLE("unable to find instance ", instanceName);
        static FunctionInstance s_NullInstance;
        return s_NullInstance;
    }

    FunctionInstance& SymbolTable::GetDecleration(const std::string& declerationName)
    {
        if(m_FunctionCache.HasDecleration(declerationName)) return m_FunctionCache.GetDecleration(declerationName);

        std::shared_ptr<SymbolTable> ptr = m_Previous;

        while(ptr)
        {
            if(ptr->m_FunctionCache.HasDecleration(declerationName)) 
                return ptr->m_FunctionCache.GetDecleration(declerationName);
                
            ptr = ptr->m_Previous;
        }

        CLEAR_UNREACHABLE("unable to find decleration ", declerationName);

        static FunctionInstance s_NullInstance;
        return s_NullInstance;
    }

    FunctionTemplate& SymbolTable::GetTemplate(const std::string& templateName,  const std::vector<Parameter>& params)
    {
        if(m_FunctionCache.HasTemplate(templateName)) return m_FunctionCache.GetTemplate(templateName, params);

        std::shared_ptr<SymbolTable> ptr = m_Previous;

        while(ptr)
        {
            if(ptr->m_FunctionCache.HasTemplate(templateName)) 
                return ptr->m_FunctionCache.GetTemplate(templateName, params);
                
            ptr = ptr->m_Previous;
        }

        CLEAR_UNREACHABLE("unable to find decleration ", templateName);

        static FunctionTemplate s_NullTemplate;
        return s_NullTemplate;
    }

    bool SymbolTable::HasInstance(const std::string& instanceName)
    {
         if(m_FunctionCache.HasInstance(instanceName)) return true;

        std::shared_ptr<SymbolTable> ptr = m_Previous;

        while(ptr)
        {
            if(ptr->m_FunctionCache.HasInstance(instanceName)) 
                return true;
                
            ptr = ptr->m_Previous;
        }


        return false;
    }

    bool SymbolTable::HasDecleration(const std::string& declerationName)
    {
         if(m_FunctionCache.HasDecleration(declerationName)) return true;

        std::shared_ptr<SymbolTable> ptr = m_Previous;

        while(ptr)
        {
            if(ptr->m_FunctionCache.HasDecleration(declerationName)) 
                return true;
                
            ptr = ptr->m_Previous;
        }

        return false;
    }

    bool SymbolTable::HasTemplate(const std::string& templateName)
    {
        if(m_FunctionCache.HasTemplate(templateName)) return true;

        std::shared_ptr<SymbolTable> ptr = m_Previous;

        while(ptr)
        {
            if(ptr->m_FunctionCache.HasTemplate(templateName)) 
                return true;
                
            ptr = ptr->m_Previous;
        }

        return false;
    }

    bool SymbolTable::HasTemplateMangled(const std::string& templateName)
    {
        if(m_FunctionCache.HasTemplateMangled(templateName)) return true;

        std::shared_ptr<SymbolTable> ptr = m_Previous;

        while(ptr)
        {
            if(ptr->m_FunctionCache.HasTemplateMangled(templateName)) 
                return true;
                
            ptr = ptr->m_Previous;
        }

        return false;
    }

    bool SymbolTable::HasAlloca(const std::string& name)
    {
        if(m_Variables.contains(name)) return true;

        std::shared_ptr<SymbolTable> ptr = m_Previous;

        while(ptr)
        {
            if(ptr->m_Variables.contains(name)) 
                return true;
                
            ptr = ptr->m_Previous;
        }

        return false;
    }

    void SymbolTable::CreateTemplate(const std::string& templateName, std::shared_ptr<Type> returnType, const std::vector<Parameter>& params, bool isVariadic,  const std::vector<std::shared_ptr<ASTNodeBase>>& defaultArgs, std::shared_ptr<ASTNodeBase> root)
    {
        m_FunctionCache.CreateTemplate(templateName, returnType, params, isVariadic, defaultArgs, root);
    }

    FunctionInstance& SymbolTable::InstantiateOrReturn(const std::string& templateName, const std::vector<Parameter>& params, std::shared_ptr<Type> returnType, CodegenContext& context)
    {
         if(m_FunctionCache.HasTemplate(templateName)) return  m_FunctionCache.InstantiateOrReturn(templateName, params, returnType, context);

        std::shared_ptr<SymbolTable> ptr = m_Previous;

        while(ptr)
        {
            if(ptr->m_FunctionCache.HasTemplate(templateName)) 
                return ptr->m_FunctionCache.InstantiateOrReturn(templateName, params, returnType, context);
                
            ptr = ptr->m_Previous;
        }

        CLEAR_UNREACHABLE("unable to find template ", templateName);
        return m_FunctionCache.InstantiateOrReturn(templateName, params, returnType, context);
    }

    void SymbolTable::RegisterTemplate(const std::string& templateName, const FunctionTemplate& functionTemplate)
    {
        m_FunctionCache.RegisterTemplate(templateName, functionTemplate);
    }

    void SymbolTable::RegisterInstance(const FunctionInstance& instance)
    {
        m_FunctionCache.RegisterInstance(instance);
    }

    void SymbolTable::RegisterDecleration(const FunctionInstance& decleration, bool isVariadic)
    {
        m_FunctionCache.RegisterDecleration(decleration, isVariadic);
    }
}