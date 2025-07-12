#include "SymbolTable.h"

#include "AST/ASTNode.h"
#include "Core/Log.h"

#include "Core/Utils.h"

namespace clear 
{
    static std::string GuessTypeNameFromNumber(const std::string& number)
    {
        NumberInfo info = GetNumberInfoFromLiteral(number);

        if (!info.Valid)
        {
            CLEAR_LOG_ERROR("invalid number ", number);
            return "";
        } 

        if(info.IsFloatingPoint)
        {
            switch (info.BitsNeeded)
			{
				case 32: return "float32"; break;
				case 64: return "float64"; break;
				default:
					break;
			}
        }
		else if (info.IsSigned)
		{
			switch (info.BitsNeeded)
			{
				case 8:  return "int8";
				case 16: return "int16";
				case 32: return "int32";
				case 64: return "int64";
				default:
					break;
			}
		}
        else
		{
			switch (info.BitsNeeded)
			{
				case 8:  return "uint8";
				case 16: return "uint16";
				case 32: return "uint32";
				case 64: return "uint64";
				default:
					break;
			}
		}
        
        CLEAR_LOG_ERROR("unable to guess type for ", number);
        return "";
    }

    SymbolTable::SymbolTable()
    {
    }

    SymbolTable::SymbolTable(const std::shared_ptr<SymbolTable>& other)
    {
        m_Previous = other;
    }

    Allocation SymbolTable::RequestTemporary(const std::shared_ptr<Type>& type, llvm::IRBuilder<>& builder)
    {
        auto [it, inserted] = m_Temporaries.try_emplace(type->Get(), size_t{});

        if (!inserted)
           return m_Allocations[it->second];

        llvm::BasicBlock* insertBlock = builder.GetInsertBlock();
        
        CLEAR_VERIFY(insertBlock, "cannot create an alloca without function");  
	    auto ip = builder.saveIP(); 
	    llvm::Function* function = insertBlock->getParent();    
	    builder.SetInsertPoint(&function->getEntryBlock());

	    Allocation allocation;
        allocation.Alloca = builder.CreateAlloca(type->Get(), nullptr, std::format("{}.{}", "tmp", type->GetHash()));
        allocation.Type = type; 

	    builder.restoreIP(ip);  
        
        it->second = m_Allocations.size();
        m_Allocations.push_back(allocation);

        return allocation;
    }

    Allocation SymbolTable::CreateGlobal(const std::string& name, std::shared_ptr<Type> type, llvm::Module& module, llvm::Value* value)
    {
        Allocation alloca;

        llvm::Constant* init = nullptr;

        if (value)
        {
            init = llvm::cast<llvm::Constant>(value);
        }
        else
        {
            init = llvm::Constant::getNullValue(type->Get());
        }

        alloca.Alloca = new llvm::GlobalVariable(
            module,
            type->Get(),
            false, 
            llvm::GlobalValue::ExternalLinkage,
            init,
            name
        );

        alloca.Type = type;
        alloca.IsGlobal = true;

        m_Variables[name] = m_Allocations.size();
        m_Allocations.push_back(alloca);

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

        m_Variables[name] = m_Allocations.size();
        m_Allocations.push_back(allocation);

        return allocation;
    }

    void SymbolTable::TrackAllocation(const std::string& name, Allocation allocation)
    {
        m_TrackedAllocations[name] = allocation;
    }

    void SymbolTable::OwnAllocation(const std::string& name, Allocation allocation)
    {
        m_Variables[name] = m_Allocations.size();
        m_Allocations.push_back(allocation);
    }

    Allocation SymbolTable::GetAlloca(const std::string& name)
    {
        if(m_Variables.contains(name)) 
        {
            size_t pos = m_Variables.at(name);
            return m_Allocations[pos];
        }

        if(m_TrackedAllocations.contains(name))
        {
            return m_TrackedAllocations[name];
        }

        std::shared_ptr<SymbolTable> ptr = m_Previous;

        while(ptr)
        {
            if(ptr->m_Variables.contains(name)) 
            {
                size_t pos = ptr->m_Variables.at(name);
                return ptr->m_Allocations[pos];
            }

            if(ptr->m_TrackedAllocations.contains(name))
            {
                return ptr->m_TrackedAllocations[name];
            }
                
            ptr = ptr->m_Previous;
        }

        return {};
    }

    void SymbolTable::SetPrevious(const std::shared_ptr<SymbolTable>& previous)
    {
        m_Previous = previous;
    }

    void SymbolTable::FlushScope(CodegenContext& ctx)
    {
        while(!ctx.DeferredCalls.empty() && ctx.DeferredCalls.back())
        {
            auto call = ctx.DeferredCalls.back();
            call->Codegen(ctx);
            ctx.DeferredCalls.pop_back();
        }

        if(!ctx.DeferredCalls.empty()) // remove dummy node
            ctx.DeferredCalls.pop_back();

        for(int64_t i = (int64_t)m_Allocations.size() - 1; i >= 0; i--)
        {
            if(m_Allocations[i].Type->IsArray() || m_Allocations[i].Type->IsCompound())
            {
                RecursiveCallDestructors(m_Allocations[i].Alloca, m_Allocations[i].Type, ctx, m_Allocations[i].IsGlobal);
            }
        }
        
        for(int64_t i = (int64_t)m_VariadicArguments.size() - 1; i >= 0; i--)
        {
            if(m_VariadicArguments[i].Type->IsArray() || m_VariadicArguments[i].Type->IsCompound())
            {
                RecursiveCallDestructors(m_VariadicArguments[i].Alloca, m_VariadicArguments[i].Type, ctx, m_VariadicArguments[i].IsGlobal);
            }
        }
    }

    void SymbolTable::RecursiveCallDestructors(llvm::Value* value, std::shared_ptr<Type> type, CodegenContext& ctx, bool isGlobal)
    {
        if(type->IsArray())
		{
			auto arrayTy = dyn_cast<ArrayType>(type);
			auto baseTy  = arrayTy->GetBaseType();

			if(!baseTy->IsCompound())
				return;

			for(size_t i = 0; i < arrayTy->GetArraySize(); i++)
			{
				llvm::Value* gep = nullptr;

				std::vector<llvm::Value*> indices = {
				        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.Context), 0), 
				        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.Context), i) 
				};

				if (isGlobal)
				{
					CLEAR_VERIFY(llvm::cast<llvm::Constant>(value), "cannot have global that is not a constant");

				    gep = llvm::ConstantExpr::getGetElementPtr(
				        arrayTy->Get(),                           
				        llvm::cast<llvm::Constant>(value),         
				        indices
				    );
				}
				else 
				{
				    gep = ctx.Builder.CreateGEP(
				        arrayTy->Get(),  
				        value,            
				        indices             
				    );
				}

				RecursiveCallDestructors(gep, baseTy, ctx, isGlobal);
			}
			
			return;
		}


		std::shared_ptr<StructType> structTy = nullptr;
		std::string functionName;  

        CLEAR_VERIFY(type->IsCompound(), "compound type");

		if(type->IsClass())
		{
			auto classTy = dyn_cast<ClassType>(type);
			structTy = classTy->GetBaseType();

			functionName = classTy->ConvertFunctionToClassFunction("_CLR__destruct__$%");

			if(!HasTemplateMangled(functionName))
			{
				return;
			}
		}
		else 
		{
			structTy = dyn_cast<StructType>(type);
		}

		CLEAR_VERIFY(structTy, "not a valid type");

        if(type->IsClass())
		{
            auto classTy = dyn_cast<ClassType>(type);

			Parameter param;

			param.Name = "this";
			param.Type = ctx.TypeReg->GetPointerTo(classTy);

			std::string name = classTy->GetHash() + "." + "__destruct__";

			auto function = InstantiateOrReturn(name, { param }, nullptr, ctx);

            static thread_local int32_t s_Index = INT32_MAX;

			if(isGlobal)
			{
				CLEAR_UNREACHABLE("unimplemented");
                CLEAR_VERIFY(llvm::cast<llvm::Constant>(value), "value not a constant");
				llvm::appendToGlobalDtors(ctx.Module, function.Function, s_Index--, llvm::cast<llvm::Constant>(value));
			}
			else 
			{
				ctx.Builder.CreateCall(function.Function, { value });
			}

		}

		const auto& memberIndices = structTy->GetMemberIndices();
		const auto& memberTypes   = structTy->GetMemberTypes();

		for(const auto& [name, subType] : memberTypes)
		{
			if(!subType->IsCompound()) 
				continue;

			size_t index = memberIndices.at(name);
			llvm::Value* gep = nullptr;

			if (isGlobal)
			{
			    std::vector<llvm::Constant*> indices = {
			        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.Context), 0), 
			        llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx.Context), index) 
				};
				
				CLEAR_VERIFY(llvm::cast<llvm::Constant>(value), "cannot have global that is not a constant");

			    gep = llvm::ConstantExpr::getGetElementPtr(
			        structTy->Get(),                           
			        llvm::cast<llvm::Constant>(value),         
			        indices
			    );
			}
			else 
			{
			    gep = ctx.Builder.CreateStructGEP(
			        structTy->Get(),  
			        value,            
			        index             
			    );
			}

			RecursiveCallDestructors(gep, subType, ctx, isGlobal);
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
        if(m_Variables.contains(name)) 
        {
            return true;
        }

        if(m_TrackedAllocations.contains(name))
        {
            return true;
        }

        std::shared_ptr<SymbolTable> ptr = m_Previous;

        while(ptr)
        {
            if(ptr->m_Variables.contains(name)) 
            {
                return true;
            }

            if(ptr->m_TrackedAllocations.contains(name))
            {
                return true;
            }

            ptr = ptr->m_Previous;
        }

        return false;
    }

    void SymbolTable::CreateTemplate(const std::string& templateName, 
                                     std::shared_ptr<Type> returnType, 
                                     const std::vector<Parameter>& params, 
                                     bool isVariadic,  
                                     const std::vector<std::shared_ptr<ASTNodeBase>>& defaultArgs, 
                                     std::shared_ptr<ASTNodeBase> root, 
                                    std::shared_ptr<Module> sourceModule)
    {
        m_FunctionCache.CreateTemplate(templateName, returnType, params, isVariadic, defaultArgs, root, sourceModule);
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