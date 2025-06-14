#include "SymbolTable.h"

#include "Core/Log.h"

namespace clear 
{
    SymbolTable::SymbolTable(const std::shared_ptr<SymbolTable>& other)
    {
        m_Previous = other;
    }

    Allocation SymbolTable::CreateGlobal(const std::string& name, std::shared_ptr<Type> type, llvm::Module& module, llvm::Value* value)
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
}