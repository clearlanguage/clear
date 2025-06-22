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
        Parser(const ProgramInfo& info);
        ~Parser() = default;

        std::shared_ptr<ASTNodeBase> GetResult();

    private:
        std::shared_ptr<ASTNodeBase> Root();

        Token Consume();
        Token Peak();
        Token Next();
        Token Prev();
        void Undo();

        bool Match(TokenType tokenType);
        bool MatchAny(TokenSet tokenSet);

        void Expect(TokenType tokenType);
        void ExpectAny(TokenSet tokenSet);

        void ParseStatement();
        void ParseGeneric();
        void ParseFunctionDefinition(const std::string& className = "");
        void ParseFunctionDeclaration();        
        void ParseVariableDecleration();
        void ParseLetDecleration();
        void ParseConstDecleration();
        void ParseStruct();
        void ParseImport();
        void ParseReturn();
        void ParseIf();
        void ParseElseIf();
        void ParseElse();
        void ParseWhile();
        void ParseFor();
        void ParseIndentation();
        void ParseClass();
        void ParseLoopControls();

        std::shared_ptr<ASTNodeBase> ParseExpression();
        std::shared_ptr<ASTNodeBase> ParseVariableReference();
        std::shared_ptr<ASTNodeBase> ParseOperand();
        std::shared_ptr<ASTNodeBase> ParseFunctionCall();
        std::shared_ptr<ASTNodeBase> ParseArrayInitializer(std::shared_ptr<ASTNodeBase> storage);
        std::shared_ptr<ASTNodeBase> ParseAssignment(const std::string& variableName, bool initialize = false);
        std::shared_ptr<ASTNodeBase> ParseAssignment(std::shared_ptr<ASTNodeBase> storage, bool initialize = false);

        std::vector<Token> ParseVariableTypeTokens();

        std::pair<std::string, std::shared_ptr<TypeDescriptor>> ParseVariableTypeDescriptor();

        BinaryExpressionType GetBinaryExpressionFromTokenType(TokenType type);
        UnaryExpressionType GetPreUnaryExpressionTypeFromTokenType(TokenType type);
        UnaryExpressionType GetPostUnaryExpressionTypeFromTokenType(TokenType type);
        AssignmentOperatorType GetAssignmentOperatorFromTokenType(TokenType type);

        void SavePosition();
        void RestorePosition();
        void SkipUntil(TokenType type);
        void SkipUntil(TokenSet set);

    private:    
        std::vector<Token> m_Tokens;
        std::set<std::string> m_Aliases;
        size_t m_Position = 0;

        std::vector<std::shared_ptr<ASTNodeBase>> m_RootStack;
        std::vector<size_t> m_RestorePoints;

        TokenSet m_VariableType;
        TokenSet m_AssignmentOperators;
        TokenSet m_Terminators;
        TokenSet m_PreUnaryExpression;
        TokenSet m_PostUnaryExpression;
        TokenSet m_Literals;
        TokenSet m_IgnoredTokens;
        TokenSet m_TypeIndirection;
        TokenSet m_ValueReferences;
        TokenSet m_VariableName;
    };
}