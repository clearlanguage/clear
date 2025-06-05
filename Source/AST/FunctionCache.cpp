#include "FunctionCache.h"

#include "Core/Log.h"

#include "ASTNode.h"

namespace clear 
{
    FunctionTemplate& FunctionCache::CreateTemplate(const std::string& templateName, std::shared_ptr<Type> returnType, const std::vector<Parameter>& params, bool isVariadic, std::shared_ptr<ASTNodeBase> root)
    {
        CLEAR_VERIFY(!m_Templates.contains(templateName), "conflicting template function name!");
        m_Templates[templateName] = {returnType, params, root, isVariadic};
        return m_Templates[templateName];
    }

    FunctionInstance& FunctionCache::InstantiateOrReturn(const std::string& templateName, std::vector<Parameter> params, std::shared_ptr<Type> returnType, CodegenContext& context)
    {
        CLEAR_VERIFY(m_Templates.contains(templateName), "missing function template to instantiate");

        std::string mangledName = GetMangledName(templateName, params, returnType);

        if(m_Instances.contains(templateName)) 
            return GetInstance(templateName);

        FunctionTemplate& functionTemplate = m_Templates.at(templateName);

        std::vector<Parameter> types;

        auto it1 = params.begin();
        auto it2 = functionTemplate.Parameters.begin();

        while(it1 != params.end() && it2 != functionTemplate.Parameters.end())
        {
            // in future transferring of types from generics to real will happen here.
            if(!it2->Type) break;

            types.push_back(*it2);

            it1++;
            it2++;
        }

        while(it1 != params.end()) // we have variadic args, we will need to build a struct type to hold the arguments.
        {               
            types.push_back({ "", it1->Type } );
            it1++;
        }

        std::vector<llvm::Type*> parameterTypes;
        std::transform(types.begin(), types.end(), std::back_inserter(parameterTypes), [](auto& a) { return a.Type->Get(); });

		llvm::FunctionType* functionType = llvm::FunctionType::get(returnType ? returnType->Get() : llvm::FunctionType::getVoidTy(context.Context), parameterTypes, false);

		FunctionInstance functionData;
        functionData.FunctionType = functionType;
        functionData.ReturnType   = returnType;
        functionData.Parameters   = types;
        functionData.MangledName  = mangledName;

		llvm::Function* function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, functionData.MangledName, context.Module);
        functionData.Function     = function;
        m_Instances[mangledName] = functionData;
        
        std::shared_ptr<ASTFunctionDefinition> definition = std::dynamic_pointer_cast<ASTFunctionDefinition>(functionTemplate.Root);
        CLEAR_VERIFY(definition, "invalid object");

        definition->SetName(mangledName);
        definition->Instantiate(functionData, context);

        
        return m_Instances[mangledName];
    }

    FunctionInstance& FunctionCache::GetInstance(const std::string& instanceName)
    {
        if(m_Instances.contains(instanceName)) return m_Instances.at(instanceName);

        CLEAR_UNREACHABLE("instance name ", instanceName, " doesn't exist");
        static FunctionInstance s_NullInstance;
        return s_NullInstance;
    }

    FunctionInstance& FunctionCache::GetDecleration(const std::string& decleration)
    {
        if(m_Declerations.contains(decleration)) return m_Declerations.at(decleration);

        CLEAR_UNREACHABLE("decleration name ", decleration, " doesn't exist");
        static FunctionInstance s_NullDecleration;
        return s_NullDecleration;    
    }

    FunctionTemplate& FunctionCache::GetTemplate(const std::string& templateName)
    {
         if(m_Templates.contains(templateName)) return m_Templates.at(templateName);

        CLEAR_UNREACHABLE("template name ", templateName, " doesn't exist");
        static FunctionTemplate s_NullTemplate;
        return s_NullTemplate;
    }

    bool FunctionCache::HasInstance(const std::string& instanceName)
    {
        return m_Instances.contains(instanceName);
    }

    bool FunctionCache::HasDecleration(const std::string& instanceName)
    {
        return m_Declerations.contains(instanceName);
    }

    bool FunctionCache::HasTemplate(const std::string& instanceName)
    {
        return m_Templates.contains(instanceName);
    }

    void FunctionCache::RegisterTemplate(const std::string& templateName, const FunctionTemplate& functionTemplate)
    {
        if(m_Templates.contains(templateName))
        {
            CLEAR_LOG_WARNING("template name conflict not registering ", templateName);
            return;
        }

        m_Templates[templateName] = functionTemplate;
    }

    void FunctionCache::RegisterInstance(const FunctionInstance &instance)
    {
        if(m_Instances.contains(instance.MangledName)) 
        {
            CLEAR_LOG_WARNING("instance name conflict not registering instance ", instance.MangledName);
            return;
        }
        
        m_Instances[instance.MangledName] = instance;
    }

    void FunctionCache::RegisterDecleration(const FunctionInstance& decleration, bool isVariadic)
    {
        if(m_Declerations.contains(decleration.MangledName)) 
        {
            CLEAR_LOG_WARNING("instance name conflict not registering instance ", decleration.MangledName);
            return;
        }
        
        m_Declerations[decleration.MangledName] = decleration;

        FunctionTemplate templateF;
        templateF.Parameters = decleration.Parameters;
        templateF.IsVariadic = isVariadic;
        templateF.ReturnType = decleration.ReturnType;
        templateF.Root = nullptr; // declerations have no responsibility to instantiate functions

        m_Templates[decleration.MangledName] = templateF;
    }

    std::string FunctionCache::GetMangledName(const std::string& templateName, const std::vector<Parameter>& params, std::shared_ptr<Type> returnType)
    {
        if(templateName == "main") return templateName;

        std::string mangledName = "_CLR";
        mangledName += templateName;

        for(const auto& param : params)
        {
            mangledName += param.Type->GetShortHash();
        }

        if(returnType)
            mangledName += "rt" + returnType->GetShortHash();

        return mangledName;
    }
}