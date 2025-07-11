#pragma once

#include "Lexing/Lexer.h"
#include "AST/ASTNode.h"
#include "Symbols/Type.h"
#include "Core/Operator.h"
#include "Diagnostics/DiagnosticsBuilder.h"
#include <memory>
#include <vector>
#include <set>
#include <bitset>

namespace clear  
{
    class Module;

    class Parser 
    {        
    public:
        Parser() = delete;
        Parser(const std::vector<Token>& tokens, std::shared_ptr<Module> root, DiagnosticsBuilder& builder);
        ~Parser() = default;

        std::shared_ptr<ASTNodeBase> GetResult();

    private:
        std::shared_ptr<ASTNodeBase> Root();
        std::shared_ptr<Module> RootModule();

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

        void ParseUntil(TokenType endToken);
        void ParseUntil(const std::string& endToken);
        void ParseUntilMatchIndentation(size_t rootLevel);

        void ParseStatement();
        void ParseGeneral();
        void ParseFunctionDefinition(const std::string& className = "", bool descriptionOnly = false);
        void ParseFunctionDeclaration(const std::string& declareKeyword = "declare");        
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
        void ParseEnum();
        void ParseDefer();
        void ParseBlock();
        void ParseModule();
        void ParseEndModule();
        void ParseSwitch();

        std::shared_ptr<ASTNodeBase> ParseExpression(uint64_t terminationIndex = UINT64_MAX);
        std::shared_ptr<ASTNodeBase> ParseOperand();
        std::shared_ptr<ASTNodeBase> ParseFunctionCall();
        std::shared_ptr<ASTNodeBase> ParseInitializer(std::shared_ptr<ASTNodeBase> storage, bool initialize = false);
        std::shared_ptr<ASTNodeBase> ParseAssignment(std::shared_ptr<ASTNodeBase> storage, bool initialize = false);
        std::shared_ptr<ASTNodeBase> ParseVariableDecleration();
        std::shared_ptr<ASTNodeBase> ParseTypeResolver();

        AssignmentOperatorType GetAssignmentOperatorFromTokenType(TokenType type);

        void SavePosition();
        void RestorePosition();
        void SkipUntil(TokenType type);

        size_t FindLastOf(TokenType type); // relative to end line
        size_t GetLastBracket(TokenType openBracket, TokenType closeBracket);

    private:
        static constexpr size_t s_MaxMatchSize = 15;
        bool LookAheadMatches(const std::function<bool(const Token&)>& terminator, const std::array<TokenType, s_MaxMatchSize>& match);

        bool IsDeclaration();

    private:    
        std::vector<Token> m_Tokens;
        std::set<std::string> m_Aliases;
        size_t m_Position = 0;

        std::vector<std::shared_ptr<ASTNodeBase>> m_RootStack;
        std::vector<size_t> m_RestorePoints;

        TokenSet m_Terminators;
        TokenSet m_AssignmentOperators;
        TokenSet m_Literals;

        std::vector<std::shared_ptr<Module>> m_Modules;
        DiagnosticsBuilder& m_DiagnosticsBuilder;
    };
}