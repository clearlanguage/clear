#include "Type.h"
#include "Lexing/Tokens.h"


namespace clear 
{
    class TypeRegistry 
    {
    public:
        TypeRegistry() = default;
        ~TypeRegistry() = default;

        void RegisterBuiltinTypes();

        std::shared_ptr<Type> GetType(const std::string& name) const; 
        std::shared_ptr<Type> GetPointerTo(std::shared_ptr<Type> base);
        std::shared_ptr<Type> GetTypeFromToken(const Token& token);
    
        static std::string GetTypeNameFromTokenType(TokenType type);

        static void InitGlobal();
        static std::shared_ptr<TypeRegistry> GetGlobal();

    private:
        std::string GuessTypeNameFromNumber(const std::string& number);

    private:
        std::unordered_map<std::string, std::shared_ptr<Type>> m_Types;

    };
}