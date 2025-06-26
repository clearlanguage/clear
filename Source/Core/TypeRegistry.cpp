#include "TypeRegistry.h"

#include "API/LLVM/LLVMInclude.h"
#include "Core/Log.h"
#include "Utils.h"

namespace clear 
{
    TypeRegistry::TypeRegistry(std::shared_ptr<llvm::LLVMContext> context)
        : m_Context(context)
    {
    }

    void TypeRegistry::RegisterBuiltinTypes()
    {
        TypeFlagSet signedIntegerFlags;
        signedIntegerFlags.set((size_t)TypeFlags::Integral);
        signedIntegerFlags.set((size_t)TypeFlags::Signed);

        m_Types["int8"]  = std::make_shared<PrimitiveType>(llvm::Type::getInt8Ty(*m_Context), signedIntegerFlags, "int8");
        m_Types["int16"] = std::make_shared<PrimitiveType>(llvm::Type::getInt16Ty(*m_Context), signedIntegerFlags, "int16");
        m_Types["int32"] = std::make_shared<PrimitiveType>(llvm::Type::getInt32Ty(*m_Context), signedIntegerFlags, "int32");
        m_Types["int64"] = std::make_shared<PrimitiveType>(llvm::Type::getInt64Ty(*m_Context), signedIntegerFlags, "int64");

        TypeFlagSet integerFlags;
        integerFlags.set((size_t)TypeFlags::Integral);

        m_Types["uint8"]  = std::make_shared<PrimitiveType>(llvm::Type::getInt8Ty(*m_Context),  integerFlags, "uint8");
        m_Types["uint16"] = std::make_shared<PrimitiveType>(llvm::Type::getInt16Ty(*m_Context), integerFlags, "uint16");
        m_Types["uint32"] = std::make_shared<PrimitiveType>(llvm::Type::getInt32Ty(*m_Context), integerFlags, "uint32");
        m_Types["uint64"] = std::make_shared<PrimitiveType>(llvm::Type::getInt64Ty(*m_Context), integerFlags, "uint64");
        m_Types["bool"]   = std::make_shared<PrimitiveType>(llvm::Type::getInt8Ty(*m_Context),  integerFlags, "bool");

        m_Types["int"]  = m_Types["int32"];
        m_Types["uint"] = m_Types["uint32"];
        m_Types["string"] = GetPointerTo(m_Types["int8"]); //TODO: gonna be a class soon

        TypeFlagSet floatingFlags;
        floatingFlags.set((size_t)TypeFlags::Floating);
        floatingFlags.set((size_t)TypeFlags::Signed);

        m_Types["float32"] = std::make_shared<PrimitiveType>(llvm::Type::getFloatTy(*m_Context),  floatingFlags, "float32");
        m_Types["float64"] = std::make_shared<PrimitiveType>(llvm::Type::getDoubleTy(*m_Context), floatingFlags, "float64");
        
        m_Types["float"] = m_Types["float32"];

        m_Types["opaque_ptr"] = std::make_shared<PointerType>(nullptr, *m_Context);

        m_Types["void"] = std::make_shared<PrimitiveType>(*m_Context);
    }

    void TypeRegistry::RegisterType(const std::string& name, std::shared_ptr<Type> type)
    {
        CLEAR_VERIFY(!m_Types.contains(name), "conflicting type name ", name);
        m_Types[name] = type;
    }

    std::shared_ptr<Type> TypeRegistry::GetType(const std::string& name) const
    {
        if(m_Types.contains(name)) 
            return m_Types.at(name);
        
        return nullptr;
    }

    std::shared_ptr<Type> TypeRegistry::GetPointerTo(std::shared_ptr<Type> base)
    {
        if(!base) 
            return GetPointerTo(m_Types["void"]);

        std::string hash = base->GetHash();
        hash += "*";

        if(m_Types.contains(hash)) 
            return m_Types.at(hash);

        std::shared_ptr<PointerType> ptr = std::make_shared<PointerType>(base, *m_Context);
        m_Types[hash] = ptr;

        return ptr;
    }

    std::shared_ptr<Type> TypeRegistry::GetArrayFrom(std::shared_ptr<Type> base, size_t count)
    {
        CLEAR_VERIFY(base, "invalid base");

        std::string hash = base->GetHash();
        hash += "[" + std::to_string(count) + "]";

        if(m_Types.contains(hash)) 
            return m_Types.at(hash);

        std::shared_ptr<ArrayType> ptr = std::make_shared<ArrayType>(base, count);
        m_Types[hash] = ptr;

        return ptr;
    }

    std::shared_ptr<Type> TypeRegistry::GetConstFrom(std::shared_ptr<Type> base)
    {
        CLEAR_VERIFY(base, "invalid base");

        std::string hash = "const" + base->GetHash();

        if(m_Types.contains(hash)) 
            return m_Types.at(hash);

        std::shared_ptr<ConstantType> ptr = std::make_shared<ConstantType>(base);
        m_Types[hash] = ptr;

        return ptr;
    }

    std::shared_ptr<Type> TypeRegistry::GetSignedType(std::shared_ptr<Type> type)
    {
        CLEAR_VERIFY(type->IsIntegral(), "only works on integral types!");

        std::string hash = type->GetHash();

        if(hash[0] == 'u') 
            return GetType(hash.substr(1, hash.size()));

        return type;
    }

    std::shared_ptr<Type> TypeRegistry::GetTypeFromToken(const Token& token)
    {
        if(token.GetData() == "null") return m_Types["opaque_ptr"];

        if(token.IsType(TokenType::String))
        {
            return GetPointerTo(GetType("int8"));
        }

        if(token.IsType(TokenType::Char))
        {
            return GetType("int8");
        }

        if(token.IsType(TokenType::Keyword) && (token.GetData() == "true" || token.GetData() == "false") )
        {
            return GetType("bool");
        }

        if(token.IsType(TokenType::Number))
        {
            return GetType(GuessTypeNameFromNumber(token.GetData()));
        }

        if(token.IsType(TokenType::Identifier))
        {
            return GetType(token.GetData());
        }

        return GetType(token.GetData());
    }

    std::shared_ptr<Type> TypeRegistry::CreateStruct(const std::string& name, const std::vector<std::pair<std::string, std::shared_ptr<Type>>>& members)
    {
        std::shared_ptr<StructType> structType = std::make_shared<StructType>(name, members);
        m_Types[name] = structType;
        return structType;
    }

    std::shared_ptr<Type> TypeRegistry::ResolveType(const TypeDescriptor& descriptor)
    {
        if(descriptor.Description.size() == 0) return nullptr;

        auto Resolve = [&](const TypeDescriptor& desc) 
        {
            std::shared_ptr<Type> baseType;
            size_t start = 1;

            if(desc.Description[0].GetData() == "const")
            {
                baseType = GetConstFrom(m_Types[desc.Description[1].GetData()]);
                start++;
            }
            else 
            {
                baseType = m_Types[desc.Description[0].GetData()];
            }

            for(size_t i = start; i < desc.Description.size(); i++)
            {
                Token token = desc.Description[i];

                if(token.IsType(TokenType::Star))
                {
                    baseType = GetPointerTo(baseType); 
                }
                //else if (token.TokenType == TokenType::StaticArrayDef)
                //{
                  //  baseType = GetArrayFrom(baseType, std::stoll(token.Data));
                //}
                else if (token.GetData() == "const")
                {
                    CLEAR_VERIFY(!baseType->IsConst(), "cannot have a const of a constant!");
                    baseType = GetConstFrom(baseType);
                }
                else 
                {
                    CLEAR_UNREACHABLE("unimplemented type ", token.GetData());
                }
            }

            return baseType;
        };

        size_t index = 0;

        if(descriptor.Description[0].GetData() == "const")
        {
            index++;
            CLEAR_VERIFY(descriptor.Description.size() >= 2, "cannot have type descriptor inferred const");
        }

        if(m_Types.contains(descriptor.Description[index].GetData())) // type we already have just have to retrieve
        {
            return Resolve(descriptor);
        }

        const auto& desc = descriptor.Description[index];
        
        if(desc.GetData() == "struct")
        {
            return ResolveStruct(descriptor);
        }
        else if (desc.GetData() == "class")
        {
            return ResolveClass(descriptor);
        }
        else 
        {
            CLEAR_UNREACHABLE("unimplemented compound type descriptor ", desc.GetData());
        }
    
        return nullptr;
    }

    std::string TypeRegistry::GuessTypeNameFromNumber(const std::string& number)
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

    std::shared_ptr<Type> TypeRegistry::ResolveStruct(const TypeDescriptor& descriptor)
    {
        size_t index = 0;

        if(descriptor.Description[0].GetData() == "const")
        {
            index++;
            CLEAR_VERIFY(descriptor.Description.size() >= 2, "cannot have type descriptor inferred const");
        }

        std::string structName = descriptor.Description[index].GetData();

        std::shared_ptr<StructType> structTy = std::make_shared<StructType>(structName, *m_Context);
        m_Types[structName] = structTy;

        std::vector<std::pair<std::string, std::shared_ptr<Type>>> members;

        for(auto& [typeName, subType] : descriptor.ChildTypes)
        {
            std::shared_ptr<Type> type = ResolveType(*subType);
            members.push_back({typeName, type});
        }

        structTy->SetBody(members);
        return structTy;
    }

    std::shared_ptr<Type> TypeRegistry::ResolveClass(const TypeDescriptor& descriptor)
    {
        size_t index = 0;

        if(descriptor.Description[0].GetData() == "const")
        {
            index++;
            CLEAR_VERIFY(descriptor.Description.size() >= 2, "cannot have type descriptor inferred const");
        }

        std::string className = descriptor.Description[index].GetData();

        std::shared_ptr<StructType> structTy = std::make_shared<StructType>(className, *m_Context);
        std::shared_ptr<ClassType>  classTy  = std::make_shared<ClassType>(structTy);

        m_Types[className] = classTy;

        std::vector<std::pair<std::string, std::shared_ptr<Type>>> members;

        for(auto& [typeName, subType] : descriptor.ChildTypes)
        {
            std::shared_ptr<Type> type = ResolveType(*subType);
            members.push_back({typeName, type});
        }

        structTy->SetBody(members);

        return classTy;
    }
}
