#include "API/LLVM/LLVMBackend.h"
#include "Core/Type.h"

#include <unordered_map>

namespace clear 
{
    struct Allocation
    {
        llvm::AllocaInst* Alloca = nullptr;
        std::shared_ptr<Type> Type;
    };

    class SymbolTable
    {
    public:
        SymbolTable()  = default;
        SymbolTable(const std::shared_ptr<SymbolTable>& other);
        ~SymbolTable() = default;

        Allocation CreateAlloca(const std::string& name, std::shared_ptr<Type> type);
        //void CreateType(...)

        Allocation GetAlloca(const std::string& name);
        //void GetType(...)

        void SetPrevious(const std::shared_ptr<SymbolTable>& previous);
        std::shared_ptr<SymbolTable> GetPrevious() {return m_Previous;}

    private:
        //Type registry goes here as well once done 
        std::unordered_map<std::string, Allocation> m_Variables; 
        std::shared_ptr<SymbolTable> m_Previous;
    };
}