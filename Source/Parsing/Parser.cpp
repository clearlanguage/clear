#include "Parser.h"
#include "AST/ASTNode.h"
#include "Core/Log.h"
#include "Core/Operator.h"
#include "Diagnostics/Diagnostic.h"
#include "Diagnostics/DiagnosticCode.h"
#include "Lexing/TokenDefinitions.h"
#include "Lexing/Token.h"
#include "Symbols/Module.h"
#include "Symbols/Type.h"

#include <llvm/Analysis/InlineModelFeatureMaps.h>
#include <llvm/Support/CommandLine.h>
#include <memory>
#include <print>
#include <stack>

namespace clear 
{
    #define EXPECT_TOKEN(type, code) \
    if (!Match(type)) { \
    auto location = (m_Position > 0 ? Prev() : Peak()); \
    m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, location, code, GetExpectedLength(type)); \
    m_Tokens.insert(m_Tokens.begin() + m_Position, Token(type, "")); \
    }

    #define EXPECT_TOKEN_RETURN(type, code, returnValue) \
    if (!Match(type)) { \
    auto location = (m_Position > 0 ? Prev() : Peak()); \
    m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, location, code, GetExpectedLength(type)); \
    SkipUntil(TokenType::EndLine); \
    return returnValue; \
    }


    #define EXPECT_DATA(str, code) \
    if (!Match(str)) { \
    auto location = (m_Position > 0 ? Prev() : Peak()); \
    m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, location, code); \
    SkipUntil(TokenType::EndLine); \
    return; \
    }
	
	#define EXPECT_DATA_RETURN(str, code, returnValue) \
    if (!Match(str)) { \
    auto location = (m_Position > 0 ? Prev() : Peak()); \
    m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, location, code); \
    SkipUntil(TokenType::EndLine); \
    return returnValue; \
    }

    #define VERIFY(cond, code)                             \
    if (!(cond)) {                                               \
    auto location = (m_Position > 0 ? Prev() : Peak());        \
    m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, location, code); \
    SkipUntil(TokenType::EndLine);  \
    return;                                                      \
    }

    #define VERIFY_WITH_RETURN(cond, code, returnValue)                             \
    if (!(cond)) {                                               \
    auto location = (m_Position > 0 ? Prev() : Peak());        \
    m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, location, code); \
    SkipUntil(TokenType::EndLine);  \
    return returnValue;                                                      \
    }

	using NoArgFn = std::function<std::shared_ptr<ASTNodeBase>(Parser*)>; 
	using ArgFn = std::function<std::shared_ptr<ASTNodeBase>(Parser*, std::shared_ptr<ASTNodeBase>)>; 

	struct OperatorInfo 
	{
		int LeftBindingPower;
		int RightBindingPower;
		NoArgFn PrefixParse = [](Parser* p) { return p->ParsePrefixExpr(); }; 
		ArgFn InfixParse    = [](Parser* p, std::shared_ptr<ASTNodeBase> node) { return p->ParseInfixExpr(node); };
		ArgFn PostfixParse  = [](Parser* p, std::shared_ptr<ASTNodeBase> node) { return p->ParsePostfixExpr(node);};  
	};

	static std::map<OperatorType, OperatorInfo> g_OperatorTable = {
		{OperatorType::Index,			  {6, 7}},
		{OperatorType::Dot,				  {6, 7}},
		{OperatorType::Subscript,	      {6, 7, nullptr, nullptr, [](Parser* p, std::shared_ptr<ASTNodeBase> node) { return p->ParseSubscriptExpr(node); }}},
		{OperatorType::FunctionCall,	  {6, 7, nullptr, nullptr, [](Parser* p, std::shared_ptr<ASTNodeBase> node) { return p->ParseFunctionCallExpr(node); }}},
		{OperatorType::StructInitializer, {6, 7, nullptr, nullptr, [](Parser* p, std::shared_ptr<ASTNodeBase> node) { return p->ParseStructInitializerExpr(node); }}},
		{OperatorType::ListInitializer,   {6, 7, [](Parser* p) { return p->ParseListInitializerExpr(); }}},
		{OperatorType::ArrayType,		  {6, 7, [](Parser* p) { return p->ParseArrayType(); }}},

		{OperatorType::Negation,      {5, 6}},
		{OperatorType::Increment,     {5, 6}},
		{OperatorType::Decrement,     {5, 6}},
		{OperatorType::PostIncrement, {5, 6}},
		{OperatorType::PostDecrement, {5, 6}},
		{OperatorType::Cast,		  {5, 6, nullptr, [](Parser* p, std::shared_ptr<ASTNodeBase> node) { return p->ParseCastExpr(node); }}},
		{OperatorType::Sizeof,		  {5, 6, [](Parser* p) { return p->ParseSizeofExpr(); }}},

		{OperatorType::BitwiseNot,   {5, 6}},
		{OperatorType::Address,      {5, 6}},
		{OperatorType::Dereference,  {5, 6}},
		{OperatorType::Not,          {5, 6}},

		{OperatorType::Power,        {5, 4}},

		{OperatorType::Mul, {3, 4}},
		{OperatorType::Div, {3, 4}},
		{OperatorType::Mod, {3, 4}},

		{OperatorType::Add, {2, 3}},
		{OperatorType::Sub, {2, 3}},

		{OperatorType::LeftShift,  {1, 2}},
		{OperatorType::RightShift, {1, 2}},

		{OperatorType::BitwiseAnd, {1, 2}},
		{OperatorType::BitwiseXor, {1, 2}},
		{OperatorType::BitwiseOr,  {1, 2}},
		
		{OperatorType::Is,				{1, 2,	nullptr, [](Parser* p, std::shared_ptr<ASTNodeBase> node) { return p->ParseIsExpr(node); }}},
		{OperatorType::LessThan,        {1, 2}},
		{OperatorType::GreaterThan,     {1, 2}},
		{OperatorType::LessThanEqual,   {1, 2}},
		{OperatorType::GreaterThanEqual,{1, 2}},
		{OperatorType::IsEqual,         {1, 2}},
		{OperatorType::NotEqual,        {1, 2}},
		{OperatorType::Ellipsis,        {1, 2}},

		{OperatorType::And,     {1, 2}},
		{OperatorType::Or,      {1, 2}},
		{OperatorType::Ternary, {1, 2, [](Parser* p) { return p->ParseTernary();}}},
		
		{OperatorType::Assignment, {0, 1, nullptr, [](Parser* p, std::shared_ptr<ASTNodeBase> node) { return p->ParseAssignment(node); }}}
	};

    Parser::Parser(const std::vector<Token>& tokens, std::shared_ptr<Module> rootModule, DiagnosticsBuilder& builder)
        : m_Tokens(tokens), m_DiagnosticsBuilder(builder)
    {
        m_Terminators = CreateTokenSet({
            TokenType::EndLine, 
		    TokenType::EndScope, 
		    TokenType::Comma,  
		    TokenType::Equals, 
            TokenType::StarEquals, 
            TokenType::SlashEquals, 
            TokenType::PlusEquals, 
            TokenType::MinusEquals, 
            TokenType::PercentEquals, 
            TokenType::RightBrace,
            TokenType::EndOfFile, 
            TokenType::Semicolon
        });

         m_AssignmentOperators = CreateTokenSet({
            TokenType::Equals, 
            TokenType::StarEquals,
            TokenType::SlashEquals, 
            TokenType::PlusEquals,
            TokenType::MinusEquals,
            TokenType::PercentEquals
        });


        m_Literals = CreateTokenSet({
            TokenType::Number,
		    TokenType::String,
            TokenType::Char, 
            TokenType::Keyword
        });

        rootModule->GetRoot()->Children.push_back(ParseCodeBlock());
    }

    Token Parser::Consume()
    {
        if(m_Position >= m_Tokens.size())
            return m_Tokens.back();

        return m_Tokens[m_Position++];
    }

    Token Parser::Peak()
    {
        if(m_Position >= m_Tokens.size())
            return m_Tokens.back();

        return m_Tokens[m_Position];
    }

    Token Parser::Next()
    {
        if(m_Position + 1 >= m_Tokens.size())
            return m_Tokens.back();

        return m_Tokens[m_Position + 1];
    }

    Token Parser::Prev()
    {
        CLEAR_VERIFY(m_Position > 0, "what are you doing?");
        return m_Tokens[m_Position - 1];
    }

    void Parser::Undo()
    {
        m_Position--;
    }

    bool Parser::Match(TokenType token)
    {
        return Peak().IsType(token);
    }

    bool Parser::Match(const std::string& data)
    {
        return Peak().GetData() == data;
    }

    bool Parser::MatchAny(TokenSet tokenSet)
    {
        return tokenSet.test((size_t)Peak().GetType());
    }

    /* bool Parser::MatchAny(TokenSet tokenSet)
    {
        return tokenSet.test((size_t)Peak().TokenType);
    } */

    void Parser::Expect(TokenType tokenType)
    {
        if(Match(tokenType)) return;

        //CLEAR_UNREACHABLE("expected ", TokenToString(tokenType), " but got ", TokenToString(Peak().TokenType), " ", Peak().Data);
        CLEAR_UNREACHABLE("TODO: add errors here");
    }

    void Parser::Expect(const std::string& data)
    {
        if(Match(data)) return;

        CLEAR_UNREACHABLE("TODO: add errors here");
    }

    void Parser::ExpectAny(TokenSet tokenSet)
    {
        if(MatchAny(tokenSet)) return;

        CLEAR_UNREACHABLE("TODO: add errors here");
    }

	std::shared_ptr<ASTBlock> Parser::ParseCodeBlock()
	{
		auto block = std::make_shared<ASTBlock>();
		
		while(!Match(TokenType::EndOfFile) && !Match(TokenType::EndScope))
        {
			auto statement = ParseStatement();

			if (statement)
				block->Children.push_back(statement);
			else 
				break;
        }

		Consume();
		return block;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseStatement()
    {
        while(Match(TokenType::EndLine))
        {
            Consume();
        }

        if (Match("pass"))
        {
            Consume();
            return nullptr;
		}

		if (Match(TokenType::EndScope))
			return nullptr;

        static std::map<std::string, std::function<std::shared_ptr<ASTNodeBase>()>> s_MappedKeywordsToFunctions = {
            {"function",  [this]() { return ParseFunctionDefinition(); }},
            {"declare",   [this]() { return ParseFunctionDeclaration(); }}, 
            {"return",    [this]() { return ParseReturn(); }}, 
            {"if",        [this]() { return ParseIf(); }},
			{"while",	  [this]() { return ParseWhile(); }},
			{"class",     [this]() { return ParseClass(); }},
			{"let",		  [this]() { return ParseLet(); }},
			{"import",	  [this]() { return ParseImport(); }},
        };
        
        if(s_MappedKeywordsToFunctions.contains(Peak().GetData()))
        {
            return s_MappedKeywordsToFunctions.at(Peak().GetData())();
        }

		return ParseGeneral();
    }

	std::shared_ptr<ASTNodeBase> Parser::ParseGeneral()
    {
        if(Match(TokenType::Identifier) && Next().IsType(TokenType::Colon))
        {
            return ParseBlock();
        }

		return ParseExpr();
    }

  	std::shared_ptr<ASTReturn> Parser::ParseReturn()
    {
        EXPECT_DATA_RETURN("return",DiagnosticCode_None, nullptr);
        Consume();

        std::shared_ptr<ASTReturn> returnStatement = std::make_shared<ASTReturn>();
        returnStatement->ReturnValue = ParseExpr();

		return returnStatement;
    }

	std::shared_ptr<ASTIfExpression> Parser::ParseIf()
    {
        EXPECT_DATA_RETURN("if", DiagnosticCode_None, nullptr);
        Consume();	

		auto expr = ParseExpr();

        EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedIndentation, nullptr);
        Consume();

        std::shared_ptr<ASTIfExpression> ifExpr = std::make_shared<ASTIfExpression>();
		
		ifExpr->ConditionalBlocks.push_back({
			.Condition = expr,
			.CodeBlock = ParseCodeBlock() 
		});
		
		while (Match("elseif"))		
		{
			Consume();

			auto expr = ParseExpr();

			EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedIndentation, nullptr);
			Consume();

			ifExpr->ConditionalBlocks.push_back({
				.Condition = expr,
				.CodeBlock = ParseCodeBlock() 
			});
		}
		
		if (Match("else"))
		{
			Consume();
			EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedIndentation, nullptr);
			Consume();

			ifExpr->ElseBlock = ParseCodeBlock();
		}

		return ifExpr;
    }

	std::shared_ptr<ASTWhileExpression> Parser::ParseWhile()
    {
        EXPECT_DATA_RETURN("while", DiagnosticCode_None, nullptr);
        Consume();
        
        std::shared_ptr<ASTWhileExpression> whileExp = std::make_shared<ASTWhileExpression>();

		auto expr = ParseExpr();
		
        EXPECT_TOKEN(TokenType::Colon,DiagnosticCode_ExpectedIndentation)
        Consume();

		whileExp->WhileBlock = {
			.Condition = expr,
			.CodeBlock = ParseCodeBlock()
		};

		return whileExp;
    }

	std::shared_ptr<ASTImport> Parser::ParseImport()
	{
		EXPECT_DATA_RETURN("import", DiagnosticCode_None, nullptr);
		Consume();

		std::shared_ptr<ASTImport> importExpr = std::make_shared<ASTImport>();
		importExpr->Filepath = Consume().GetData();

		if (!Match("as"))
			return importExpr;
		
		Consume();
		importExpr->Namespace = Consume().GetData();

		return importExpr;
	}


	std::shared_ptr<ASTFunctionDefinition> Parser::ParseFunctionDefinition(bool descriptionOnly)
	{
		EXPECT_DATA_RETURN("function", DiagnosticCode_None, nullptr);
		Consume();

		EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);
		std::string name = Consume().GetData();

		auto funcNode = std::make_shared<ASTFunctionDefinition>(name);

		EXPECT_TOKEN_RETURN(TokenType::LeftParen, DiagnosticCode_ExpectedLeftParanFunctionDefinition, nullptr);
		Consume();

		while (!Match(TokenType::RightParen))
		{
			if (Match(TokenType::Star) || Match("const") || Match("self"))
				funcNode->Arguments.push_back(ParseSelf());
			else
				funcNode->Arguments.push_back(ParseVariableDecleration().Node);
				
			if (Match(TokenType::Comma))
				Consume();  
		}
	   
		Consume();

		if (Match(TokenType::Colon)) 
		{
			Consume();

			VERIFY_WITH_RETURN(!descriptionOnly, DiagnosticCode_None, nullptr);	
			funcNode->CodeBlock = ParseCodeBlock();

			return funcNode;
		}

		if (Match(TokenType::EndLine) && descriptionOnly)
		{
			Consume();
			return funcNode;
		}

		EXPECT_TOKEN_RETURN(TokenType::RightThinArrow, DiagnosticCode_ExpectedFunctionReturnType, nullptr);
		Consume();
		
		funcNode->ReturnType = ParseExpr();

		if (!descriptionOnly)
		{
			EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedColon, nullptr);
			Consume();

			funcNode->CodeBlock = ParseCodeBlock();
		}

		return funcNode;
	}

	std::shared_ptr<ASTBlock> Parser::ParseBlock()
    {
        EXPECT_TOKEN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier);
        Consume();

        EXPECT_TOKEN(TokenType::Colon, DiagnosticCode_ExpectedColon);
        Consume();

        EXPECT_TOKEN(TokenType::EndLine, DiagnosticCode_ExpectedNewlineAferIndentation);
        Consume();
		
		return ParseCodeBlock();
    }

	std::shared_ptr<ASTFunctionDeclaration> Parser::ParseFunctionDeclaration(const std::string& declareKeyword)
    {
        EXPECT_DATA_RETURN(declareKeyword,DiagnosticCode_None, nullptr);
        Consume();

        EXPECT_TOKEN_RETURN(TokenType::Identifier,DiagnosticCode_ExpectedIdentifier, nullptr);
        std::string functionName = Consume().GetData();

        EXPECT_TOKEN_RETURN(TokenType::LeftParen,DiagnosticCode_ExpectedLeftParanFunctionDefinition, nullptr);
        Consume();

        size_t terminationIndex = GetLastBracket(TokenType::LeftParen, TokenType::RightParen);
        auto decleration = std::make_shared<ASTFunctionDeclaration>(functionName);

        // params
        while(!MatchAny(m_Terminators) && m_Position < terminationIndex)
        {
			EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);
			auto param = std::make_shared<ASTTypeSpecifier>(Consume().GetData());	
			
			EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedColon, nullptr);
			Consume();

            if(Match(TokenType::Ellipses))
            {
                Consume();
                EXPECT_TOKEN_RETURN(TokenType::RightParen,DiagnosticCode_ExpectedEndOfFunction, nullptr);
                param->IsVariadic = true;

                decleration->Arguments.push_back(param);

                break;
            }

            param->TypeResolver = ParseExpr();

            if(Match(TokenType::Ellipses))
            {
                Consume();
                EXPECT_TOKEN_RETURN(TokenType::RightParen,DiagnosticCode_ExpectedEndOfFunction, nullptr);

                param->IsVariadic = true;

                decleration->Arguments.push_back(param);
                break;
            }

            if(!Match(TokenType::RightParen))
            {
                Expect(TokenType::Comma);
                Consume();
            }
			
			decleration->Arguments.push_back(param);
        }

        EXPECT_TOKEN_RETURN(TokenType::RightParen,DiagnosticCode_ExpectedEndOfFunction, nullptr);
        Consume();

        // return type
        if(Match(TokenType::RightThinArrow))
        {
            Consume();
            decleration->ReturnTypeNode = ParseExpr();
        }

		return decleration;
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseFunctionCall()
    {
        EXPECT_TOKEN_RETURN(TokenType::LeftParen, DiagnosticCode_ExpectedIdentifier, nullptr);
		
        Expect(TokenType::LeftParen);
        Consume();

        size_t terminationIndex = GetLastBracket(TokenType::LeftParen, TokenType::RightParen);

        auto call = std::make_shared<ASTFunctionCall>();

        while(!MatchAny(m_Terminators) && m_Position < terminationIndex)
        {
            call->Arguments.push_back(ParseExpr());

            if(m_Position < terminationIndex)
            {
                Expect(TokenType::Comma);
                Consume();
            }
        }

        Expect(TokenType::RightParen);
        Consume();

        return call;
    }


    Parser::VariableDecleration Parser::ParseVariableDecleration()
    {
		EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, {});
		auto variableName = Consume();
		
        auto variableDecleration = std::make_shared<ASTVariableDeclaration>(variableName);
		
		if (Match(TokenType::Colon))
		{
			Consume();		
			variableDecleration->TypeResolver = ParseExpr();
		}

        bool hasBeenInitialized = false;

        if(Match(TokenType::Equals))
        {
            Consume();
            variableDecleration->Initializer = ParseExpr();
            hasBeenInitialized = true;
        }

        return { variableDecleration, hasBeenInitialized };
    }

	std::shared_ptr<ASTVariableDeclaration> Parser::ParseSelf()
	{
		auto ty = ParseExpr();

		std::shared_ptr<ASTVariableDeclaration> decl = std::make_shared<ASTVariableDeclaration>(Prev());
		decl->TypeResolver = ty;

		return decl;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseExpr(int64_t minBindingPower)
	{
		Token token = Peak();
		std::shared_ptr<ASTNodeBase> lhs;

		switch (token.GetType())
		{
			case TokenType::Number:
			case TokenType::String:
			{
				lhs = std::make_shared<ASTNodeLiteral>(token);
				Consume();

				break;
			}
			case TokenType::Identifier:
			{
				lhs = std::make_shared<ASTVariable>(token);
				Consume();
					
				break;
			}
			case TokenType::LeftParen:
			{
				Consume(); // (
				lhs = ParseExpr();
				Consume(); // )

				break;
			}
			case TokenType::Keyword:
			default:
			{
				OperatorType op = GetPrefixOperator(token);

				if (op == OperatorType::None)
				{
					VERIFY_WITH_RETURN(token.IsType(TokenType::Keyword), DiagnosticCode_None, nullptr);
					lhs = std::make_shared<ASTVariable>(token);
					Consume();

					break;
				}

				VERIFY_WITH_RETURN(op != OperatorType::None, DiagnosticCode_InvalidOperator, lhs);
				
				OperatorInfo info = g_OperatorTable.at(op);
				lhs = info.PrefixParse(this);

				break;
			}
		}
		
		do {
			if (MatchAny(m_Terminators))
				break;
			
			if (OperatorType op = GetPostfixOperator(Peak()); op != OperatorType::None)
			{
				OperatorInfo info = g_OperatorTable.at(op);
				if (info.LeftBindingPower < minBindingPower)
					break;

				lhs = info.PostfixParse(this, lhs);
				continue;
			}
			
			if (OperatorType op = GetBinaryOperator(Peak()); op != OperatorType::None)
			{
				OperatorInfo info = g_OperatorTable.at(op);
					if (info.LeftBindingPower < minBindingPower)
					break;

				lhs = info.InfixParse(this, lhs);
				continue;
			}
			
			break;
		} while (true);

		return lhs;
	}
	
	std::shared_ptr<ASTNodeBase> Parser::ParsePrefixExpr()
	{
		Token token = Consume();

		OperatorType op = GetPrefixOperator(token);
		VERIFY_WITH_RETURN(op != OperatorType::None, DiagnosticCode_InvalidOperator, nullptr);
				
		OperatorInfo info = g_OperatorTable.at(op);

		std::shared_ptr<ASTUnaryExpression> unary = std::make_shared<ASTUnaryExpression>(op);
		unary->Operand = ParseExpr(info.RightBindingPower);
			
		VERIFY_WITH_RETURN(unary->Operand, DiagnosticCode_None, nullptr);
		return unary;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseInfixExpr(std::shared_ptr<ASTNodeBase> lhs)
	{
		Token token = Consume();
		OperatorType op = GetBinaryOperator(token);
		OperatorInfo info = g_OperatorTable.at(op);

		std::shared_ptr<ASTBinaryExpression> binaryExpr = std::make_shared<ASTBinaryExpression>(op);
		binaryExpr->LeftSide = lhs;
		binaryExpr->RightSide = ParseExpr(info.RightBindingPower);

		return binaryExpr;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParsePostfixExpr(std::shared_ptr<ASTNodeBase> lhs)
	{
		Token token = Consume();
		OperatorType op = GetPostfixOperator(token);

		std::shared_ptr<ASTUnaryExpression> unaryExpr = std::make_shared<ASTUnaryExpression>(op);
		unaryExpr->Operand = lhs;

		return unaryExpr;
	}
	
	std::shared_ptr<ASTNodeBase> Parser::ParseFunctionCallExpr(std::shared_ptr<ASTNodeBase> lhs)
	{
		EXPECT_TOKEN_RETURN(TokenType::LeftParen, DiagnosticCode_None, nullptr);
		Consume();

		std::shared_ptr<ASTFunctionCall> funcCall = std::make_shared<ASTFunctionCall>();
		funcCall->Callee = lhs;
		
		while (!Match(TokenType::RightParen))
		{
			funcCall->Arguments.push_back(ParseExpr());

			if (Match(TokenType::RightParen))
				break;
			
			EXPECT_TOKEN_RETURN(TokenType::Comma, DiagnosticCode_ExpectedComma, nullptr);
			Consume();
		}

		Consume();
		return funcCall;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseSubscriptExpr(std::shared_ptr<ASTNodeBase> lhs)
	{
		EXPECT_TOKEN_RETURN(TokenType::LeftBracket, DiagnosticCode_None, nullptr);
		Consume();

		std::shared_ptr<ASTSubscript> subscript = std::make_shared<ASTSubscript>();
		subscript->Target = lhs;
		
		while (!Match(TokenType::RightBracket))
		{
			subscript->SubscriptArgs.push_back(ParseExpr());

			if (Match(TokenType::RightBracket))
				break;
			
			EXPECT_TOKEN_RETURN(TokenType::Comma, DiagnosticCode_ExpectedComma, nullptr);
			Consume();
		}

		Consume();
		return subscript;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseStructInitializerExpr(std::shared_ptr<ASTNodeBase> lhs)
	{
		EXPECT_TOKEN_RETURN(TokenType::LeftBrace, DiagnosticCode_None, nullptr);
		Consume();

		std::shared_ptr<ASTStructExpr> expr = std::make_shared<ASTStructExpr>();
		expr->TargetType = lhs;

		while(!Match(TokenType::RightBrace))
		{
			while(Match(TokenType::EndLine) || Match(TokenType::EndScope))
				Consume();

			if(Match(TokenType::RightBrace))
				break;

			expr->Values.push_back(ParseExpr());

			if(Match(TokenType::Comma))
				Consume();
		}

		Consume();
		return expr;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseAssignment(std::shared_ptr<ASTNodeBase> lhs)
	{
		Token assignmentType = Consume();
		AssignmentOperatorType opType = AssignmentOperatorType::None;

		switch (assignmentType.GetType())
		{
			case TokenType::Equals:
			{
				opType = AssignmentOperatorType::Normal;
				break;
			}
			default:
			{
				break;
				
			}
		}
		
		VERIFY_WITH_RETURN(opType != AssignmentOperatorType::None, DiagnosticCode_UnexpectedToken, nullptr);

		std::shared_ptr<ASTAssignmentOperator> node = std::make_shared<ASTAssignmentOperator>(opType); 
		node->Storage = lhs;
		node->Value = ParseExpr();
		
		return node;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseCastExpr(std::shared_ptr<ASTNodeBase> lhs)
	{
		Consume();
		
		OperatorInfo info = g_OperatorTable.at(OperatorType::Cast);

		std::shared_ptr<ASTCastExpr> castExpr = std::make_shared<ASTCastExpr>();
		castExpr->Object = lhs;
		castExpr->TypeNode = ParseExpr(info.RightBindingPower);

		return castExpr;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseIsExpr(std::shared_ptr<ASTNodeBase> lhs)
	{
		Consume();
		
		OperatorInfo info = g_OperatorTable.at(OperatorType::Is);

		std::shared_ptr<ASTIsExpr> isExpr = std::make_shared<ASTIsExpr>();
		isExpr->Object = lhs;
		isExpr->TypeNode = ParseExpr(info.RightBindingPower);

		return isExpr;
	
	
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseTernary()
	{
		EXPECT_DATA_RETURN("when", DiagnosticCode_None, nullptr);
		Consume();
		
		std::shared_ptr<ASTTernaryExpression> ternaryExpr = std::make_shared<ASTTernaryExpression>();
		ternaryExpr->Condition = ParseExpr();

		EXPECT_DATA_RETURN("use", DiagnosticCode_UnexpectedToken, nullptr);	
		Consume();

		ternaryExpr->Truthy = ParseExpr();
		
		EXPECT_DATA_RETURN("otherwise", DiagnosticCode_UnexpectedToken, nullptr);
		Consume();

		ternaryExpr->Falsy = ParseExpr();
		return ternaryExpr;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseListInitializerExpr()
	{
		EXPECT_TOKEN_RETURN(TokenType::LeftBrace, DiagnosticCode_None, nullptr);
		Consume();

		std::shared_ptr<ASTListExpr> expr = std::make_shared<ASTListExpr>();

		while(!Match(TokenType::RightBrace))
		{
			while(Match(TokenType::EndLine) || Match(TokenType::EndScope))
				Consume();

			if(Match(TokenType::RightBrace))
				break;

			expr->Values.push_back(ParseExpr());

			if(Match(TokenType::Comma))
				Consume();
		}

		Consume();
		return expr;
	}


	std::shared_ptr<ASTNodeBase> Parser::ParseArrayType()
	{
		EXPECT_TOKEN_RETURN(TokenType::LeftBracket, DiagnosticCode_None, nullptr);
		Consume();

		std::shared_ptr<ASTArrayType> arrayType = std::make_shared<ASTArrayType>();
		arrayType->SizeNode = ParseExpr();
		
		EXPECT_TOKEN_RETURN(TokenType::Semicolon, DiagnosticCode_ExpectedColon, nullptr);
		Consume();
		
		arrayType->TypeNode = ParseExpr();
		EXPECT_TOKEN_RETURN(TokenType::RightBracket, DiagnosticCode_ExpectedColon, nullptr);
		Consume();

		return arrayType;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseSizeofExpr()
	{
		Consume();
	
		OperatorInfo info = g_OperatorTable.at(OperatorType::Sizeof);

		std::shared_ptr<ASTSizeofExpr> sizeofExpr = std::make_shared<ASTSizeofExpr>();
		sizeofExpr->Object = ParseExpr(info.RightBindingPower);
		
		return sizeofExpr;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseClass()
    {
        EXPECT_DATA_RETURN("class", DiagnosticCode_None, nullptr);
        Consume();

        EXPECT_TOKEN_RETURN(TokenType::Identifier,  DiagnosticCode_ExpectedIdentifier, nullptr);
        std::string className = Consume().GetData();

        std::shared_ptr<ASTClass> classNode = std::make_shared<ASTClass>(className);
		std::shared_ptr<ASTGenericTemplate> genericTemplate;

        if(Match(TokenType::LeftBracket))
        {
			genericTemplate = ParseGenericArgs(classNode);
        }

        EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedColon, nullptr);
        Consume();

        while(!Match(TokenType::EndScope))
        {
            while(Match(TokenType::EndLine))
                Consume();

			if (Match(TokenType::EndScope))
				break;

            if(Match("function"))
            {
				classNode->MemberFunctions.push_back(ParseFunctionDefinition());
                continue;
            }
			
			EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);
            auto typeSpec = std::make_shared<ASTTypeSpecifier>(Consume().GetData());
			
			EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedColon, nullptr);
			Consume();

            typeSpec->TypeResolver = ParseExpr();

            if(Match(TokenType::Equals))
            {
				Consume();
                classNode->DefaultValues.push_back(ParseExpr());
            }
            else 
            {
                classNode->DefaultValues.push_back(nullptr);
            }

            classNode->Members.push_back(typeSpec);

            EXPECT_TOKEN_RETURN(TokenType::EndLine,DiagnosticCode_ExpectedNewlineAferIndentation, nullptr);
            Consume();
        }
		
		Consume();

		if (genericTemplate)
			return genericTemplate;
		
		return classNode;
    }
	
	std::shared_ptr<ASTGenericTemplate> Parser::ParseGenericArgs(std::shared_ptr<ASTNodeBase> templateNode)
	{
		std::shared_ptr<ASTGenericTemplate> genericTemplate = std::make_shared<ASTGenericTemplate>();
		genericTemplate->TemplateNode = templateNode;
		
		EXPECT_TOKEN_RETURN(TokenType::LeftBracket, DiagnosticCode_None, nullptr);
		Consume();

		while (!Match(TokenType::RightBracket))
		{
			EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);
			genericTemplate->GenericTypeNames.push_back(Consume().GetData());
			
			if (Match(TokenType::RightBracket))
				break;

			EXPECT_TOKEN_RETURN(TokenType::Comma, DiagnosticCode_ExpectedComma, nullptr);
			Consume();
		}
		
		Consume();
		return genericTemplate;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseLet()
	{
		EXPECT_DATA_RETURN("let", DiagnosticCode_None, nullptr);
		Consume();

		auto decleration = ParseVariableDecleration();
		return decleration.Node;
	}

    AssignmentOperatorType Parser::GetAssignmentOperatorFromTokenType(TokenType type)
    {
        switch (type)
        {
            case TokenType::Equals:          return AssignmentOperatorType::Normal;
            case TokenType::PlusEquals:      return AssignmentOperatorType::Add;
            case TokenType::MinusEquals:     return AssignmentOperatorType::Sub;
            case TokenType::StarEquals:      return AssignmentOperatorType::Mul;
            case TokenType::SlashEquals:     return AssignmentOperatorType::Div;    
            case TokenType::PercentEquals:   return AssignmentOperatorType::Mod;    
            default:
                break;
        }

        CLEAR_UNREACHABLE("unimplemented");
        return {};
    }

    void Parser::SkipUntil(TokenType type)
    {
        while(!Match(type) && !Match(TokenType::EndOfFile))
        {
            Consume();
        }
    }

    size_t Parser::GetLastBracket(TokenType openBracket, TokenType closeBracket)
    {
        size_t terminationIndex = 0;

		size_t index = m_Position;
		
        int64_t bracketCount = 1;

        while(bracketCount)
        {
            if(Match(openBracket))  bracketCount++;
            if(Match(closeBracket)) bracketCount--;

            m_Position++;

            VERIFY_WITH_RETURN(m_Position < m_Tokens.size() && bracketCount >= 0, DiagnosticCode_UnmatchedBracket, m_Position);
        }

        terminationIndex = m_Position - 1;
		
		m_Position = index;

        return terminationIndex;
    }

	OperatorType Parser::GetPrefixOperator(const Token& current)
	{
		switch (current.GetType()) 
		{
			case TokenType::Bang:               return OperatorType::Not;
			case TokenType::Minus:				return OperatorType::Negation;
			case TokenType::Star:				return OperatorType::Dereference;
			case TokenType::Decrement:			return OperatorType::Decrement;
			case TokenType::Increment:			return OperatorType::Increment;
			case TokenType::LeftBrace:			return OperatorType::ListInitializer;
			case TokenType::LeftBracket:		return OperatorType::ArrayType;
			case TokenType::Ampersand:			return OperatorType::Address;
			default:
				break;
		}

		if (current.GetData() == "when")   return OperatorType::Ternary;
		if (current.GetData() == "sizeof") return OperatorType::Sizeof;

		return OperatorType::None;
	}

	OperatorType Parser::GetBinaryOperator(const Token& current)
	{
		switch (current.GetType()) 
		{
			case TokenType::Plus:               return OperatorType::Add;
			case TokenType::Minus:				return OperatorType::Sub;
			case TokenType::ForwardSlash:       return OperatorType::Div;
			case TokenType::Star:				return OperatorType::Mul;
			case TokenType::Percent:            return OperatorType::Mod;
							
			case TokenType::Pipe:               return OperatorType::BitwiseOr;
			case TokenType::Hat:                return OperatorType::BitwiseXor;
			case TokenType::LeftShift:          return OperatorType::LeftShift;
			case TokenType::RightShift:         return OperatorType::RightShift;
			case TokenType::Telda:              return OperatorType::BitwiseNot;
			
			case TokenType::LogicalAnd:         return OperatorType::And;
			case TokenType::LogicalOr:          return OperatorType::Or;
			case TokenType::Bang:               return OperatorType::Not;
			
			case TokenType::EqualsEquals:       return OperatorType::IsEqual;
			case TokenType::BangEquals:         return OperatorType::NotEqual;
			case TokenType::LessThan:           return OperatorType::LessThan;
			case TokenType::LessThanEquals:     return OperatorType::LessThanEqual;
			case TokenType::GreaterThan:        return OperatorType::GreaterThan;
			case TokenType::GreaterThanEquals:  return OperatorType::GreaterThanEqual;
			case TokenType::Dot:                return OperatorType::Dot;
			case TokenType::LeftBracket:        return OperatorType::Index;
			case TokenType::Ellipses:           return OperatorType::Ellipsis;
		
			case TokenType::Equals:				return OperatorType::Assignment;
		
			default:
				break;
		}

		if (current.GetData() == "and")     return OperatorType::And;
		if (current.GetData() == "or")      return OperatorType::Or;
		if (current.GetData() == "not")     return OperatorType::Not;
		if (current.GetData() == "as")		return OperatorType::Cast;
		if (current.GetData() == "is")		return OperatorType::Is;

		return OperatorType::None;
	}

	OperatorType Parser::GetPostfixOperator(const Token& current)
	{
		switch (current.GetType()) 
		{
			case TokenType::Decrement:			return OperatorType::PostDecrement;
			case TokenType::Increment:			return OperatorType::PostIncrement;
			case TokenType::LeftParen:			return OperatorType::FunctionCall;
			case TokenType::LeftBracket:		return OperatorType::Subscript;
			case TokenType::LeftBrace:			return OperatorType::StructInitializer;
			default:
				break;
		}

		return OperatorType::None;
	}
}
