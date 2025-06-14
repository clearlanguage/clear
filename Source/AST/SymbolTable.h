#pragma once

#include "API/LLVM/LLVMInclude.h"
#include "Core/Type.h"
#include "FunctionCache.h"

#include <unordered_map>
#include <vector>

namespace clear 
{
    struct Allocation
    {
        llvm::Value* Alloca = nullptr;
        std::shared_ptr<Type> Type;
        bool IsVariadicList = false;
    };

    struct Member
    {
        std::string Name;
        std::shared_ptr<Type> Type;
    };

    struct FunctionData
    {
        llvm::FunctionType* FunctionType;
        llvm::Function* Function;
        std::shared_ptr<Type> ReturnType;
        std::vector<Parameter> Parameters;
        std::string MangledName;
    };

    class SymbolTable
    {
    public:
        SymbolTable()  = default;
        SymbolTable(const std::shared_ptr<SymbolTable>& other);
        ~SymbolTable() = default;
        
        Allocation  CreateGlobal(const std::string& name, std::shared_ptr<Type> type, llvm::Module& module, llvm::Value* value = nullptr); //TODO: add linkage and threading options
        Allocation  CreateAlloca(const std::string& name, std::shared_ptr<Type> type, llvm::IRBuilder<>& builder);
        Allocation  GetAlloca(const std::string& name);
        void        RegisterAllocation(const std::string& name, Allocation allocation);

        FunctionInstance& GetInstance(const std::string& instanceName);
        FunctionInstance& GetDecleration(const std::string& decleration);
        FunctionTemplate& GetTemplate(const std::string& templateName, const std::vector<Parameter>& params);

        bool HasInstance(const std::string& instanceName);
        bool HasDecleration(const std::string& instanceName);
        bool HasTemplate(const std::string& instanceName);

        void SetPrevious(const std::shared_ptr<SymbolTable>& previous);
        std::shared_ptr<SymbolTable> GetPrevious() {return m_Previous;}

        FunctionCache& GetFunctionCache() { return m_FunctionCache; }
        std::vector<Allocation>& GetVariadicArguments() { return m_VariadicArguments; }

    private:

        std::unordered_map<std::string, Allocation>   m_Variables; 
        std::vector<Allocation> m_VariadicArguments;

        FunctionCache m_FunctionCache;

        std::shared_ptr<SymbolTable> m_Previous;
    };
}