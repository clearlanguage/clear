#include "Lexing/Lexer.h"
#include "AST/ASTNode.h"
#include "Core/Type.h"

namespace clear  
{
    class Parser 
    {
    public:
        Parser() = delete;
        Parser(const ProgramInfo& info);

        ~Parser() = default;

    private:
        Token Consume();
        Token Peak();

        bool Match(TokenType tokenType);
        bool MatchAny(TokenSet tokenSet);

        void Expect(TokenType tokenType);
        void ExpectAny(TokenSet tokenSet);

        void ParseStatement();
        Ref<ASTExpression> ParseExpression();

        void ParseFunctionDefinition();
        void ParseFunctionDecleration();
        void ParseFunctionCall();

        void ParseVariableDecleration();
        Ref<Type> ParseVariableType();

    private:    
        std::vector<Token> m_Tokens;
        size_t m_Position = 0;

        Ref<ASTNodeBase> m_Root;

        TokenSet m_VariableType;
        TokenSet m_AssignmentOperators;
        TokenSet m_Terminators;
    };
}