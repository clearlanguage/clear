#pragma once

#include "Lexing/Lexer.h"
#include "AST/ASTNode.h"
#include "Symbols/Type.h"
#include "Core/Operator.h"
#include "Diagnostics/DiagnosticsBuilder.h"
#include <functional>
#include <llvm/IR/InlineAsm.h>
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

		std::shared_ptr<ASTNodeBase> ParsePrefixExpr();
		std::shared_ptr<ASTNodeBase> ParseInfixExpr(std::shared_ptr<ASTNodeBase> lhs);
		std::shared_ptr<ASTNodeBase> ParsePostfixExpr(std::shared_ptr<ASTNodeBase> lhs);
		std::shared_ptr<ASTNodeBase> ParseFunctionCallExpr(std::shared_ptr<ASTNodeBase> lhs);
		std::shared_ptr<ASTNodeBase> ParseSubscriptExpr(std::shared_ptr<ASTNodeBase> lhs);
		std::shared_ptr<ASTNodeBase> ParseStructInitializerExpr(std::shared_ptr<ASTNodeBase> lhs);
		std::shared_ptr<ASTNodeBase> ParseAssignment(std::shared_ptr<ASTNodeBase> lhs);
		std::shared_ptr<ASTNodeBase> ParseCastExpr(std::shared_ptr<ASTNodeBase> lhs);
		std::shared_ptr<ASTNodeBase> ParseIsExpr(std::shared_ptr<ASTNodeBase> lhs);
		std::shared_ptr<ASTNodeBase> ParseTernary();
		std::shared_ptr<ASTNodeBase> ParseLambda();
		std::shared_ptr<ASTNodeBase> ParseFunctionType();
		std::shared_ptr<ASTNodeBase> ParseListInitializerExpr();
		std::shared_ptr<ASTNodeBase> ParseArrayType();
		std::shared_ptr<ASTNodeBase> ParseSizeofExpr();

    private:
        std::shared_ptr<ASTBlock> Root();
        std::shared_ptr<Module> RootModule();

        Token Consume();
        Token ErrorLocation();
        bool ReportExpected(TokenType type, DiagnosticCode code); // a missing token; true: recovered (a keyword where a name goes)
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

		void SkipUntil(TokenType type);
		size_t GetLastBracket(TokenType openBracket, TokenType closeBracket);

		std::shared_ptr<ASTBlock>    ParseCodeBlock();
		std::shared_ptr<ASTNodeBase> ParseStatement();
		std::shared_ptr<ASTNodeBase> ParseGeneral();
		std::shared_ptr<ASTNodeBase> ParseTupleOrExpr();
		std::shared_ptr<ASTFunctionDefinition> ParseFunctionDefinition(bool descriptionOnly = false, bool isProperty = false);
		std::shared_ptr<ASTNodeBase> ParseFunctionOrGeneric();
		std::shared_ptr<ASTFunctionDeclaration> ParseFunctionDeclaration(const std::string& declareKeyword = "declare");        
		std::shared_ptr<ASTReturn> ParseReturn();
		struct Condition { std::shared_ptr<ASTVariableDeclaration> Declaration; std::shared_ptr<ASTNodeBase> Value; };
		Condition ParseCondition();
		std::shared_ptr<ASTNodeBase> ParseIf();
		std::shared_ptr<ASTNodeBase> ParseWhile();
		std::shared_ptr<ASTNodeBase> ParseFor();
		std::shared_ptr<ASTNodeBase> ParseSwitch();
		std::shared_ptr<ASTNodeBase> ParseEnum();
		std::shared_ptr<ASTNodeBase> ParseDefer();
		std::shared_ptr<ASTNodeBase> ParseConst();
		std::shared_ptr<ASTNodeBase> ParseMacro();
		std::shared_ptr<ASTNodeBase> ParseVariant();
		bool NameSpecialMethod(std::shared_ptr<ASTFunctionDefinition> method, const Token& nameToken, bool isOperator);
		std::shared_ptr<ASTNodeBase> ParseAsync();
		std::shared_ptr<ASTNodeBase> ParseYield();
		std::shared_ptr<ASTImport> ParseImport();
		std::shared_ptr<ASTNodeBase> ParseLoopControl();
		std::shared_ptr<ASTNodeBase> ParseAssert();
		std::shared_ptr<ASTNodeBase> ParseClass();
		std::shared_ptr<ASTGenericTemplate> ParseGenericArgs(std::shared_ptr<ASTNodeBase> templateNode);
		std::shared_ptr<ASTNodeBase> ParseLet();
		std::shared_ptr<ASTBlock> ParseBlock();

        struct VariableDecleration
        {
            std::shared_ptr<ASTVariableDeclaration> Node;
            bool HasBeenInitialized = false;
        };

		std::shared_ptr<ASTNodeBase> ParseExpr(int64_t minBindingPower = 0);
        std::shared_ptr<ASTNodeBase> ParseFunctionCall();
        VariableDecleration ParseVariableDecleration();
		std::shared_ptr<ASTVariableDeclaration> ParseSelf();

        AssignmentOperatorType GetAssignmentOperatorFromTokenType(TokenType type);

		OperatorType GetPrefixOperator(const Token& token);
		OperatorType GetBinaryOperator(const Token& token);
		OperatorType GetPostfixOperator(const Token& token);
		
    private:    
        std::vector<Token> m_Tokens;
        std::set<std::string> m_Aliases;
        size_t m_Position = 0;

        TokenSet m_Terminators;
        TokenSet m_AssignmentOperators;
        TokenSet m_Literals;

        DiagnosticsBuilder& m_DiagnosticsBuilder;
        std::shared_ptr<ASTGenericTemplate> m_PendingGeneric; // type parameters of the function being parsed
        bool m_ParsingDeclaration = false;
    };
}
