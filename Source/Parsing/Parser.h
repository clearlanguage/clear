#pragma once

#include "Lexing/Lexer.h"
#include "AST/ASTNode.h"
#include "Core/Type.h"

#include <memory>
#include <vector>

namespace clear  
{
    class Parser 
    {        
    public:
        Parser() = delete;
        Parser(const ProgramInfo& info);
        ~Parser() = default;

        std::shared_ptr<ASTNodeBase> GetResult();

    private:
        std::shared_ptr<ASTNodeBase> Root();

        Token Consume();
        Token Peak();

        bool Match(TokenType tokenType);
        bool MatchAny(TokenSet tokenSet);

        void Expect(TokenType tokenType);
        void ExpectAny(TokenSet tokenSet);

        void ParseStatement();
        void ParseGeneric();
        void ParseFunctionDefinition();
        void ParseFunctionDecleration();        
        void ParseVariableDecleration();
        void ParseStruct();

        std::shared_ptr<ASTNodeBase> ParseExpression(bool isValueReference = false);
        std::shared_ptr<ASTNodeBase> ParseVariableReference(bool isValueReference = false);
        std::shared_ptr<ASTNodeBase> ParseFunctionCall();
        std::shared_ptr<ASTNodeBase> ParseArrayInitializer(std::shared_ptr<ASTNodeBase> storage);
        std::shared_ptr<ASTNodeBase> ParseAssignment(const std::string& variableName);
        std::shared_ptr<ASTNodeBase> ParseAssignment(std::shared_ptr<ASTNodeBase> storage);
        std::shared_ptr<Type> ParseVariableType();

        void ParseIndentation();

        BinaryExpressionType GetBinaryExpressionFromTokenType(TokenType type);

    private:    
        std::vector<Token> m_Tokens;
        size_t m_Position = 0;

        std::vector<std::shared_ptr<ASTNodeBase>> m_RootStack;

        TokenSet m_VariableType;
        TokenSet m_AssignmentOperators;
        TokenSet m_Terminators;
        TokenSet m_UnaryExpression;
        TokenSet m_Literals;
        TokenSet m_IgnoredTokens;
        TokenSet m_TypeIndirection;
        TokenSet m_ValueReferences;
    };
}