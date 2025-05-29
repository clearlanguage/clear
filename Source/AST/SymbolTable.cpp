#include "SymbolTable.h"

#include "Core/Log.h"

namespace clear 
{
    SymbolTable::SymbolTable(const std::shared_ptr<SymbolTable>& other)
    {
        m_Previous = other;
    }

    Allocation SymbolTable::CreateAlloca(const std::string& name, std::shared_ptr<Type> type, llvm::IRBuilder<>& builder)
    {
		auto ip = builder.saveIP();

		llvm::Function* function = builder.GetInsertBlock()->getParent();

		builder.SetInsertPoint(&function->getEntryBlock());
	
		Allocation allocation;
        allocation.Alloca = builder.CreateAlloca(type->Get(), nullptr, name);
        allocation.Type = type;

		builder.restoreIP(ip);

        m_Variables[name] = allocation;
        return allocation;
    }


    FunctionData& SymbolTable::CreateFunction(const std::string& name, 
                                              std::vector<Parameter>& parameters, 
                                              const std::shared_ptr<Type>& returnType, 
                                              llvm::Module& module, 
                                              llvm::LLVMContext& context)
    {
		std::vector<llvm::Type*> parameterTypes;
        std::transform(parameters.begin(), parameters.end(), std::back_inserter(parameterTypes), [](Parameter& a) { return a.Type->Get(); });

		llvm::FunctionType* functionType = llvm::FunctionType::get(returnType ? returnType->Get() : llvm::FunctionType::getVoidTy(context), parameterTypes, false);

		llvm::Function* function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, module);

		FunctionData functionData;
        functionData.Function     = function;
        functionData.FunctionType = functionType;
        functionData.ReturnType   = returnType;
        functionData.Parameters   = parameters;

        m_Functions[name] = functionData;
        return m_Functions[name];
    }

    void SymbolTable::RegisterAllocation(const std::string& name, Allocation allocation)
    {
        m_Variables[name] = allocation;
    }

    void SymbolTable::RegisterFunction(const std::string& name, const FunctionData& data)
    {
        if(!m_Functions.contains(name))
        {
            m_Functions[name] = data;
            return;
        }
        
        CLEAR_LOG_WARNING("registered function with name ", name, " multiple times");
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

    FunctionData& SymbolTable::GetFunction(const std::string& name)
    {
       if(m_Functions.contains(name)) return m_Functions.at(name);

        std::shared_ptr<SymbolTable> ptr = m_Previous;

        while(ptr)
        {
            if(ptr->m_Functions.contains(name)) return ptr->m_Functions.at(name);
            ptr = ptr->m_Previous;
        }

        CLEAR_UNREACHABLE("unable to find function");

        static FunctionData s_NullFunction;
        return s_NullFunction;
    }

    void SymbolTable::SetPrevious(const std::shared_ptr<SymbolTable>& previous)
    {
        m_Previous = previous;
    }
}