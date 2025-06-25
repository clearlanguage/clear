#pragma once

#include "API/LLVM/LLVMInclude.h"
#include "Core/Type.h"
#include "FunctionCache.h"

#include <unordered_map>
#include <vector>

namespace clear 
{
    struct CodegenResult;
    class ASTNodeBase;

    struct Allocation
    {
        llvm::Value* Alloca = nullptr;
        std::shared_ptr<Type> Type;
        bool IsVariadicList = false;
        bool IsGlobal = false;
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
        
        Allocation  RequestTemporary(const std::shared_ptr<Type>& type, llvm::IRBuilder<>& builder);
        Allocation  CreateGlobal(const std::string& name, std::shared_ptr<Type> type, llvm::Module& module, llvm::Value* value = nullptr); //TODO: add linkage and threading options
        Allocation  CreateAlloca(const std::string& name, std::shared_ptr<Type> type, llvm::IRBuilder<>& builder);
        Allocation  GetAlloca(const std::string& name);

        void        TrackAllocation(const std::string& name, Allocation allocation);
        void        OwnAllocation(const std::string& name, Allocation allocation);

        FunctionInstance& GetInstance(const std::string& instanceName);
        FunctionInstance& GetDecleration(const std::string& decleration);
        FunctionTemplate& GetTemplate(const std::string& templateName, const std::vector<Parameter>& params);
        
        void CreateTemplate(const std::string& templateName, 
                            std::shared_ptr<Type> returnType, 
                            const std::vector<Parameter>& params, 
                            bool isVariadic, const std::vector<std::shared_ptr<ASTNodeBase>>& defaultArgs,
                            std::shared_ptr<ASTNodeBase> root);

        FunctionInstance& InstantiateOrReturn(const std::string& templateName, 
                                              const std::vector<Parameter>& params, 
                                              std::shared_ptr<Type> returnType, 
                                              CodegenContext& context);
        
        bool HasInstance(const std::string& instanceName);
        bool HasDecleration(const std::string& instanceName);
        bool HasTemplate(const std::string& instanceName);
        bool HasTemplateMangled(const std::string& name);
        bool HasAlloca(const std::string& name);

        void RegisterTemplate(const std::string& templateName, const FunctionTemplate& functionTemplate);
        void RegisterInstance(const FunctionInstance& instance);
        void RegisterDecleration(const FunctionInstance& decleration, bool isVariadic);

        void SetPrevious(const std::shared_ptr<SymbolTable>& previous);
        std::shared_ptr<SymbolTable> GetPrevious() {return m_Previous;}

        FunctionCache& GetFunctionCache() { return m_FunctionCache; }
        std::vector<Allocation>& GetVariadicArguments() { return m_VariadicArguments; }        

        void FlushScope(CodegenContext& ctx);
        void RecursiveCallDestructors(llvm::Value* value, std::shared_ptr<Type> type, CodegenContext& ctx, bool isGlobal = false);    

    private:
        std::vector<Allocation> m_Allocations;

        std::unordered_map<std::string, size_t>  m_Variables;  // key to index in allocation vector
        std::unordered_map<llvm::Type*, size_t>  m_Temporaries; 

        std::unordered_map<std::string, Allocation> m_TrackedAllocations;

        std::vector<Allocation> m_VariadicArguments;
        FunctionCache m_FunctionCache;

        std::shared_ptr<SymbolTable> m_Previous;
    };
}