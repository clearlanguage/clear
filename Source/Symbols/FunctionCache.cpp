#include "FunctionCache.h"

#include "Core/Log.h"

#include "AST/ASTNode.h"
#include "TypeCasting.h"
#include "Module.h"
#include "Symbols/Type.h"

#include <llvm/ADT/SmallVector.h>
#include <queue>

namespace clear 
{
    void FunctionCache::CreateTemplate(const std::string& templateName, 
                                       std::shared_ptr<Type> returnType, 
                                       const std::vector<Parameter>& params, 
                                       bool isVariadic, 
                                       const std::vector<std::shared_ptr<ASTNodeBase>>& defaultArgs, 
                                       std::shared_ptr<ASTNodeBase> root, 
                                       std::shared_ptr<Module> sourceModule)
    {
        FunctionTemplate templateF;
        templateF.MangledName = GetMangledName(templateName, params, returnType);
        templateF.ReturnType = returnType;
        templateF.Root = root;
        templateF.Parameters = params;
        templateF.IsVariadic = isVariadic;
        templateF.IsExternal = false;
        templateF.DefaultArguments = defaultArgs;
        templateF.SourceModule = sourceModule;
        templateF.Valid = true;

        m_Templates[templateName].push_back(templateF);
    }

    FunctionInstance& FunctionCache::InstantiateOrReturn(const std::string& templateName, std::vector<Parameter> params, std::shared_ptr<Type> returnType, CodegenContext& context)
    {
        CLEAR_VERIFY(m_Templates.contains(templateName), "missing function template to instantiate");

        std::string mangledName = GetMangledName(templateName, params, returnType);

        if(m_Instances.contains(templateName)) 
            return GetInstance(templateName);

        if(m_Instances.contains(mangledName))
            return GetInstance(mangledName);

        FunctionTemplate& functionTemplate = GetTemplate(templateName, params);

        std::vector<Parameter> types;

        auto it1 = params.begin();
        auto it2 = functionTemplate.Parameters.begin();

        bool regenerateMangledName = false;

        llvm::SmallVector<std::pair<std::string, Parameter>> aliases;

        while(it1 != params.end() && it2 != functionTemplate.Parameters.end())
        {
            // in future transferring of types from generics to real will happen here.
            if(it2->IsVariadic) 
            {
                regenerateMangledName = true;
                break;
            }
            else if (it2->Type->IsGeneric())
            {
                regenerateMangledName = true;

                auto genericType = dyn_cast<GenericType>(it2->Type);

                auto it = std::find_if(aliases.begin(), aliases.end(), [&](const auto& alias)
                {
                    return alias.first == genericType->GetHash();
                });

                if(it == aliases.end())
                {
                    aliases.push_back(std::make_pair(genericType->GetHash(), *it1));
                    context.ClearModule->CreateAlias(genericType->GetHash(), it1->Type->GetHash());
                    types.push_back(*it1);
                }
                else 
                {
                    types.push_back(it->second);
                }
               
            }
            else if(it2->Type->IsTrait())
            {
                regenerateMangledName = true;
                types.push_back(*it1);
            }
            else 
            {
                types.push_back(*it2); 
            }


            it1++;
            it2++;
        }

        while(it1 != params.end())
        {              
            if(!it2->Type || it2->Type->IsTrait())
                types.push_back({ "", it1->Type } );
            else 
                types.push_back({ "", it2->Type } );

            it1++;
        }

        if(returnType && returnType->IsGeneric())
        {
            auto it = std::find_if(aliases.begin(), aliases.end(), [&](const auto& alias)
            {
                return alias.first == returnType->GetHash();
            });

            returnType = it->second.Type;
        }

        std::vector<llvm::Type*> parameterTypes;
        std::transform(types.begin(), types.end(), std::back_inserter(parameterTypes), [](auto& a) { return a.Type->Get(); });

		llvm::FunctionType* functionType = llvm::FunctionType::get(returnType ? returnType->Get() : llvm::FunctionType::getVoidTy(context.Context), parameterTypes, false);

        if(regenerateMangledName)
        {
            mangledName = GetMangledName(templateName, types, returnType);
            
            if(m_Instances.contains(mangledName))
                return GetInstance(mangledName);
        }

        CodegenContext ctx = functionTemplate.SourceModule->GetCodegenContext();

		FunctionInstance functionData;
        
        functionData.FunctionType = functionType;
        functionData.ReturnType   = returnType;
        functionData.Parameters   = types;
        functionData.MangledName  = mangledName;

		llvm::Function* function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, functionData.MangledName, ctx.Module);
        functionData.Function     = function;
        m_Instances[mangledName] = functionData;
        
        std::shared_ptr<ASTFunctionDefinition> definition = std::dynamic_pointer_cast<ASTFunctionDefinition>(functionTemplate.Root);
        CLEAR_VERIFY(definition, "invalid object");

        definition->SetName(mangledName);
        definition->Instantiate(functionData, ctx);

        if(ctx.ClearModule != context.ClearModule)
        {
            m_Instances[mangledName].Function = llvm::cast<llvm::Function>(context.Module.getOrInsertFunction(mangledName, functionType).getCallee());
        }

        while(!aliases.empty())
        {
            context.ClearModule->RemoveAlias(aliases.back().first);
            aliases.pop_back();
        }
        
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

    FunctionTemplate& FunctionCache::GetTemplate(const std::string& templateName, const std::vector<Parameter>& params)
    {
        static FunctionTemplate s_NullTemplate;

        if(!m_Templates.contains(templateName))
        {
            CLEAR_UNREACHABLE("cannot find template name ", templateName);
            return s_NullTemplate;
        }
        
        auto& candidates = m_Templates.at(templateName);
        
        auto ScoreTemplate = [&](const FunctionTemplate& functionTemplate)
        {
            size_t score = 0;

            if(params.size() > functionTemplate.Parameters.size() && !functionTemplate.IsVariadic) 
                return score;

            size_t paramSize = functionTemplate.IsVariadic ? functionTemplate.Parameters.size() - 1 : functionTemplate.Parameters.size();
            
            if(params.size() == paramSize)
            {
                score = 100;
            }  

            size_t i = 0;
            for(; i < params.size(); i++)
            {
                auto& param1 = params[i];
                auto& param2 = i < functionTemplate.Parameters.size() ? functionTemplate.Parameters[i] : functionTemplate.Parameters.back();


                if(!param2.Type)  // can be any type 
                {
                    score += 75;
                    continue;
                };

                if(param2.Type->IsTrait())
                {
                    auto baseTy1 = param1.Type;
                    auto baseTy2 = param2.Type;

                    while(baseTy1->IsPointer() && baseTy2->IsPointer())
                    {
                        std::shared_ptr<PointerType> pointerTy = std::dynamic_pointer_cast<PointerType>(baseTy1);
                        baseTy1 = pointerTy->GetBaseType();

                        pointerTy = std::dynamic_pointer_cast<PointerType>(baseTy2);
                        baseTy2 = pointerTy->GetBaseType();
                    }

                    CLEAR_VERIFY(baseTy1->IsClass(), "parameter must be a class!");
                    CLEAR_VERIFY(baseTy2->IsTrait(), "not a valid pointer to a trait!");

                    auto classTy = dyn_cast<ClassType>(baseTy1);
                    auto traitTy = dyn_cast<TraitType>(baseTy2);

                    if(!traitTy->DoesClassImplementTrait(classTy))
                        return size_t(0);

                    score += 100;
                    continue;
                }

                if(param1.Type == param2.Type || param2.Type->IsGeneric())
                {
                    score += 100;
                }
                else if (TypeCasting::CanBePromoted(param1.Type, param2.Type))
                {
                    score += 75;
                }
                else if (TypeCasting::CanBeCasted(param1.Type, param2.Type))
                {
                    llvm::Module& mod = *functionTemplate.SourceModule->GetModule();

                    // scoring system to decide best candidate to cast to
                    size_t min = std::min(param1.Type->GetSizeInBytes(mod), param2.Type->GetSizeInBytes(mod));
                    size_t max = std::max(param1.Type->GetSizeInBytes(mod), param2.Type->GetSizeInBytes(mod));

                    float similarity = (float)min / max;
                    float contribution = 10.0f * similarity + 25.0f * (param1.Type->GetFlags() == param2.Type->GetFlags());

                    if (contribution < 5.0f)
                        contribution = 5.0f;
                    
                    score += (size_t)contribution;
                }
                else 
                {
                    return size_t(0);
                }
            }

            for(; i < paramSize; i++)
            {
                if(!functionTemplate.DefaultArguments[i])
                {
                    break;
                }

                score += 10;
            }

            if(i < paramSize) 
                return size_t(0);

            return score;
        };

        struct Candidate
        {
            size_t Index;
            size_t Score;

            bool operator<(const Candidate& other) const 
            {
                return Score < other.Score; 
            }
        };

        std::priority_queue<Candidate> queue;

        for (size_t i = 0; i < candidates.size(); ++i)
        {
            const auto& candidate = candidates[i];
            size_t score = ScoreTemplate(candidate);
            if (score > 0)
                queue.push({ i, score });
        }

        CLEAR_VERIFY(!queue.empty(), "no viable function");
        return candidates[queue.top().Index];
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

    bool FunctionCache::HasTemplateMangled(const std::string& name)
    {
        std::string demangledName = DeMangleName(name);

        if(!m_Templates.contains(demangledName))
            return false;

        for(const auto& templateFn : m_Templates.at(demangledName))
        {
            if(templateFn.MangledName == name)
                return true;
        }

        return false;
    }

    void FunctionCache::RegisterTemplate(const std::string& templateName, const FunctionTemplate& functionTemplate)
    {
        m_Templates[templateName].push_back(functionTemplate);
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
        templateF.IsExternal = true;
        templateF.Root = nullptr; // declerations have no responsibility to instantiate functions

        m_Templates[decleration.MangledName].push_back(templateF);
    }

    std::string FunctionCache::GetMangledName(const std::string& templateName, const std::vector<Parameter>& params, std::shared_ptr<Type> returnType)
    {
        if(templateName == "main") return templateName;

        std::string mangledName = "_CLR";
        mangledName += templateName + "$";

        for(const auto& param : params)
        {
            if(param.IsVariadic)
            {
                mangledName += "va";
                break;
            }

            mangledName += param.Type->GetHash();
        }   

        mangledName += "%";

        if(returnType)
            mangledName += returnType->GetHash();

        return mangledName;
    }

    std::string FunctionCache::DeMangleName(const std::string& name)
    {
        CLEAR_VERIFY(name.substr(0, 4) == "_CLR", "invalid mangled name");
        size_t nameEnd = name.find_first_of('$');
        return name.substr(4, nameEnd - 4);
    }

    const auto& FunctionCache::GetTemplates(const std::string& name)
    {
        return m_Templates[name];
    }
}