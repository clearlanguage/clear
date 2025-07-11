#pragma once 

#include "Type.h"
#include "Lexing/Token.h"
#include "API/LLVM/LLVMInclude.h"

#include "Lexing/Token.h"
#include "Core/Log.h"

namespace clear 
{
    class TypeRegistry 
    {
    public:
        TypeRegistry(std::shared_ptr<llvm::LLVMContext> context);
        ~TypeRegistry() = default;

        void RegisterBuiltinTypes();
        void RegisterType(const std::string& name, std::shared_ptr<Type> type);
        void RemoveType(const std::string& name);


        std::shared_ptr<Type> GetType(const std::string& name) const; 
        std::shared_ptr<Type> GetPointerTo(std::shared_ptr<Type> base);
        std::shared_ptr<Type> GetArrayFrom(std::shared_ptr<Type> base, size_t count);
        std::shared_ptr<Type> GetConstFrom(std::shared_ptr<Type> base);
        std::shared_ptr<Type> GetSignedType(std::shared_ptr<Type> type);
        std::shared_ptr<Type> GetTypeFromToken(const Token& token);
        std::shared_ptr<Type> CreateStruct(const std::string& name, const std::vector<std::pair<std::string, std::shared_ptr<Type>>>& members);

        const auto& GetTypeTable() { return m_Types; }

        template<typename T, typename ...Args>
        std::shared_ptr<T> CreateType(const std::string& name, Args&&... args)
        {
            auto [it, success] = m_Types.try_emplace(name, std::make_shared<T>(std::forward<Args>(args)...));
            CLEAR_VERIFY(success, "failed to emplace type ", name);
            return std::dynamic_pointer_cast<T>(it->second);
        }

    private:
        std::string GuessTypeNameFromNumber(const std::string& number);

    private:
        std::unordered_map<std::string, std::shared_ptr<Type>> m_Types;
        std::shared_ptr<llvm::LLVMContext> m_Context;
    };
}