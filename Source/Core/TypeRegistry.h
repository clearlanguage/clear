#pragma once 

#include "Type.h"
#include "Lexing/Tokens.h"
#include "API/LLVM/LLVMInclude.h"

namespace clear 
{
    class TypeRegistry 
    {
    public:
        TypeRegistry(std::shared_ptr<llvm::LLVMContext> context);
        ~TypeRegistry() = default;

        void RegisterBuiltinTypes();
        void RegisterType(const std::string& name, std::shared_ptr<Type> type);

        std::shared_ptr<Type> GetType(const std::string& name) const; 
        std::shared_ptr<Type> GetPointerTo(std::shared_ptr<Type> base);
        std::shared_ptr<Type> GetArrayFrom(std::shared_ptr<Type> base, size_t count);
        std::shared_ptr<Type> GetTypeFromToken(const Token& token);
        std::shared_ptr<Type> CreateStruct(const std::string& name, const std::vector<std::pair<std::string, std::shared_ptr<Type>>>& members);

        static std::string GetTypeNameFromTokenType(TokenType type);

        const auto& GetTypeTable() { return m_Types; }

    private:
        std::string GuessTypeNameFromNumber(const std::string& number);
        void CascadeType(std::shared_ptr<Type> type);

    private:
        std::unordered_map<std::string, std::shared_ptr<Type>> m_Types;
        std::shared_ptr<llvm::LLVMContext> m_Context;
    };
}