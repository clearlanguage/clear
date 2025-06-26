#pragma once 

#include "Type.h"
#include "Lexing/Token.h"
#include "API/LLVM/LLVMInclude.h"

#include "Lexing/Token.h"

namespace clear 
{
    struct TypeDescriptor
    {
        // could be int****, structName[10] etc...
        std::vector<Token> Description;

        // this is for compound types (structs/classes), that have multiple types
        std::vector<std::pair<std::string, std::shared_ptr<TypeDescriptor>>> ChildTypes;

        // function names that are defined in this type, used for class methods
        std::vector<std::string> Functions; 
    };

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
        std::shared_ptr<Type> GetConstFrom(std::shared_ptr<Type> base);
        std::shared_ptr<Type> GetSignedType(std::shared_ptr<Type> type);
        std::shared_ptr<Type> GetTypeFromToken(const Token& token);
        std::shared_ptr<Type> CreateStruct(const std::string& name, const std::vector<std::pair<std::string, std::shared_ptr<Type>>>& members);
        std::shared_ptr<Type> ResolveType(const TypeDescriptor& descriptor);

        const auto& GetTypeTable() { return m_Types; }

        template<typename T, typename ...Args>
        void CreateType(const std::string& name, Args&&... args)
        {
            m_Types[name] = std::make_shared<T>(std::forward<Args>(args)...);
        }

    private:
        std::string GuessTypeNameFromNumber(const std::string& number);

        std::shared_ptr<Type> ResolveStruct(const TypeDescriptor& descriptor);
        std::shared_ptr<Type> ResolveClass(const TypeDescriptor& descriptor);


    private:
        std::unordered_map<std::string, std::shared_ptr<Type>> m_Types;
        std::shared_ptr<llvm::LLVMContext> m_Context;
    };
}