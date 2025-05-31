#pragma once

#include "Lexing/Lexer.h"
#include "AST/ASTNode.h"
#include "Core/Type.h"

#include <memory>
#include <vector>
#include <set>

namespace clear  
{
    class Parser 
    {        
    public:
        Parser() = delete;
        Parser(const ProgramInfo& info, TypeRegistry& registry);
        ~Parser() = default;

        std::shared_ptr<ASTNodeBase> GetResult();

    private:
        std::shared_ptr<ASTNodeBase> Root();

        Token Consume();
        Token Peak();
        Token Next();
        Token Prev();

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
        void ParseImport();
        void ParseReturn();
        void ParseIf();
        void ParseElseIf();
        void ParseElse();
        void ParseWhile();

        std::shared_ptr<ASTNodeBase> ParseExpression();
        std::shared_ptr<ASTNodeBase> ParseVariableReference();
        std::shared_ptr<ASTNodeBase> ParseOperand();
        std::shared_ptr<ASTNodeBase> ParseFunctionCall();
        std::shared_ptr<ASTNodeBase> ParseArrayInitializer(std::shared_ptr<ASTNodeBase> storage);
        std::shared_ptr<ASTNodeBase> ParseAssignment(const std::string& variableName);
        std::shared_ptr<ASTNodeBase> ParseAssignment(std::shared_ptr<ASTNodeBase> storage);
        std::shared_ptr<Type> ParseVariableType();

        void ParseIndentation();

        BinaryExpressionType GetBinaryExpressionFromTokenType(TokenType type);
        UnaryExpressionType GetPreUnaryExpressionTypeFromTokenType(TokenType type);
        UnaryExpressionType GetPostUnaryExpressionTypeFromTokenType(TokenType type);
        AssignmentOperatorType GetAssignmentOperatorFromTokenType(TokenType type);

    private:    
        std::vector<Token> m_Tokens;
        std::set<std::string> m_Aliases;
        size_t m_Position = 0;
        TypeRegistry& m_TypeRegistry;

        std::vector<std::shared_ptr<ASTNodeBase>> m_RootStack;

        TokenSet m_VariableType;
        TokenSet m_AssignmentOperators;
        TokenSet m_Terminators;
        TokenSet m_PreUnaryExpression;
        TokenSet m_PostUnaryExpression;
        TokenSet m_Literals;
        TokenSet m_IgnoredTokens;
        TokenSet m_TypeIndirection;
        TokenSet m_ValueReferences;
    };
}