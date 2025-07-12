#pragma once 

#include "Symbols/Symbol.h"
#include "Type.h"
#include "Lexing/Token.h"

#include "Lexing/Token.h"
#include "Core/Log.h"
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallVector.h>
#include <memory>
#include <string>
#include <unordered_map>

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

        void CreateClassTemplate(std::string_view name, std::shared_ptr<ASTNodeBase> classNode, llvm::ArrayRef<std::string> generics);
        std::optional<ClassTemplate> GetClassTemplate(std::string_view name);

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
        std::unordered_map<std::string, ClassTemplate> m_ClassTemplates;
        std::shared_ptr<llvm::LLVMContext> m_Context;
    };
}