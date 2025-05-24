#include "SymbolTable.h"

#include "Core/Log.h"

namespace clear 
{
    SymbolTable::SymbolTable(const std::shared_ptr<SymbolTable>& other)
    {
        m_Previous = other;
    }

    Allocation SymbolTable::CreateAlloca(const std::string& name, std::shared_ptr<Type> type)
    {
        auto& builder = *LLVM::Backend::GetBuilder();

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

    StructData& SymbolTable::CreateStruct(const std::string &name, const std::vector<Member> &members)
    {
        // TODO: insert return statement here
        StructData data;
        return data;
    }

    Allocation SymbolTable::GetAlloca(const std::string& name)
    {
        if(m_Variables.contains(name)) return m_Variables.at(name);

        std::shared_ptr<SymbolTable> ptr = m_Previous;

        while(ptr)
        {
            if(ptr->m_Variables.contains(name)) return ptr->m_Variables.at(name);
            ptr = ptr->m_Previous;
        }

        CLEAR_UNREACHABLE("unable to find variable");
        return {};
    }

    StructData& SymbolTable::GetStruct(const std::string &name)
    {
        StructData data; //TODO
        return data;
    }

    void SymbolTable::SetPrevious(const std::shared_ptr<SymbolTable>& previous)
    {
        m_Previous = previous;
    }
}