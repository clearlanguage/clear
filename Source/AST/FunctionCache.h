#pragma once 

#include "API/LLVM/LLVMInclude.h"
#include "Core/TypeRegistry.h"

#include <unordered_map>

namespace clear 
{
    struct Parameter
	{
		std::string Name;
		std::shared_ptr<Type> Type;
        bool IsVariadic = false;
	};

    struct UnresolvedParameter
    {
        std::string Name;
		TypeDescriptor Type;
        bool IsVariadic = false;
    };

    class ASTNodeBase; 

    struct FunctionTemplate
    {
        std::shared_ptr<Type> ReturnType;
        std::vector<Parameter> Parameters;
        std::shared_ptr<ASTNodeBase> Root;
        std::vector<std::shared_ptr<ASTNodeBase>> DefaultArguments;
        bool IsVariadic = false;
        bool IsExternal = false;
    };

    struct FunctionInstance
    {
        llvm::FunctionType* FunctionType;
        llvm::Function* Function;
        std::shared_ptr<Type> ReturnType;
        std::vector<Parameter> Parameters;
        std::string MangledName;
    };


    struct CodegenContext;
    struct CodegenResult;

    class FunctionCache 
    {
    public:
        FunctionCache() = default;
        ~FunctionCache() = default;

        void CreateTemplate(const std::string& templateName,
                            std::shared_ptr<Type> returnType,
                            const std::vector<Parameter>& params,
                            bool isVariadic, const std::vector<std::shared_ptr<ASTNodeBase>>& defaultArgs,
                            std::shared_ptr<ASTNodeBase> root);

        FunctionInstance& InstantiateOrReturn(const std::string& templateName, 
                                              std::vector<Parameter> params, 
                                              std::shared_ptr<Type> returnType, 
                                              CodegenContext& context);

        
        FunctionInstance& GetInstance(const std::string& instanceName);
        FunctionInstance& GetDecleration(const std::string& decleration);
        FunctionTemplate& GetTemplate(const std::string& templateName, const std::vector<Parameter>& params);

        bool HasInstance(const std::string& instanceName);
        bool HasDecleration(const std::string& instanceName);
        bool HasTemplate(const std::string& instanceName);

        void RegisterTemplate(const std::string& templateName, const FunctionTemplate& functionTemplate);
        void RegisterInstance(const FunctionInstance& instance);
        void RegisterDecleration(const FunctionInstance& decleration, bool isVariadic);

        static std::string GetMangledName(const std::string& templateName, 
                                          const std::vector<Parameter>& params, 
                                          std::shared_ptr<Type> returnType);

    private:
        std::unordered_map<std::string, std::vector<FunctionTemplate>> m_Templates;
        std::unordered_map<std::string, FunctionInstance> m_Instances;
        std::unordered_map<std::string, FunctionInstance> m_Declerations; // external function/already instantiated (printf from c, scanf etc...)

    };

}