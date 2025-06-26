#pragma once

#include "Lexing/Lexer.h"
#include "AST/ASTNode.h"
#include "Core/Type.h"
#include "Core/Operator.h"

#include <memory>
#include <vector>
#include <set>
#include <bitset>

namespace clear  
{

    class Parser 
    {        
    public:
        Parser() = delete;
        Parser(const std::vector<Token>& tokens);
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
        bool Match(const std::string& data);

        bool MatchAny(TokenSet tokenSet);

        void Expect(TokenType tokenType);
        void Expect(const std::string& data);

        void ExpectAny(TokenSet tokenSet);

        void ParseStatement();
        void ParseGeneral();
        void ParseFunctionDefinition(const std::string& className = "");
        void ParseFunctionDeclaration();        
        void ParseVariableDecleration(bool defaultInitialize = false);
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
        void ParseTrait();
        void ParseTraitFunctionDefinition();
        void ParseRaise();
        void ParseTry();
        void ParseCatch();
        void ParseEnum();
        void ParseDefer();

        std::shared_ptr<ASTNodeBase> ParseExpression(uint64_t terminationIndex = UINT64_MAX);
        std::shared_ptr<ASTNodeBase> ParseVariableReference();
        std::shared_ptr<ASTNodeBase> ParseOperand();
        std::shared_ptr<ASTNodeBase> ParseFunctionCall();
        std::shared_ptr<ASTNodeBase> ParseArrayInitializer(std::shared_ptr<ASTNodeBase> storage, bool initialize = false);
        std::shared_ptr<ASTNodeBase> ParseAssignment(const std::string& variableName, bool initialize = false);
        std::shared_ptr<ASTNodeBase> ParseAssignment(std::shared_ptr<ASTNodeBase> storage, bool initialize = false);
        std::shared_ptr<ASTNodeBase> CreateDefaultInitializerFromName(const std::string& name);

        std::shared_ptr<ASTNodeBase> ParseVariableDeclerationN(bool initialize = false);

        std::shared_ptr<ASTNodeBase> ParseTypeResolver();

        std::vector<Token> ParseVariableTypeTokens();


        std::pair<std::string, std::shared_ptr<TypeDescriptor>> ParseVariableTypeDescriptor();

        AssignmentOperatorType GetAssignmentOperatorFromTokenType(TokenType type);

        void SavePosition();
        void RestorePosition();
        void SkipUntil(TokenType type);
        //void SkipUntil(TokenSet set);

        size_t FindLastOf(TokenType type); // relative to end line
        size_t GetLastBracket(TokenType openBracket, TokenType closeBracket);

    private:
        static constexpr size_t s_MaxMatchSize = 15;
        bool LookAheadMatches(const std::function<bool(const Token&)>& terminator, const std::array<TokenType, s_MaxMatchSize>& match);

    private:    
        std::vector<Token> m_Tokens;
        std::set<std::string> m_Aliases;
        size_t m_Position = 0;

        std::vector<std::shared_ptr<ASTNodeBase>> m_RootStack;
        std::vector<size_t> m_RestorePoints;

        TokenSet m_Terminators;
        TokenSet m_AssignmentOperators;
        TokenSet m_Literals;

       /*  TokenSet m_VariableType;
        TokenSet m_AssignmentOperators;
        TokenSet m_PreUnaryExpression;
        TokenSet m_PostUnaryExpression;
        TokenSet m_Literals;
        TokenSet m_IgnoredTokens;
        
        TokenSet m_ValueReferences;
        TokenSet m_VariableName; */
    };
}