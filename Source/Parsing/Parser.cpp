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
    auto location = ErrorLocation(); \
    m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, location, code, GetExpectedLength(type)); \
    m_Tokens.insert(m_Tokens.begin() + m_Position, Token(type, "")); \
    }

    #define EXPECT_TOKEN_RETURN(type, code, returnValue) \
    if (!Match(type)) { \
    auto location = ErrorLocation(); \
    m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, location, code, GetExpectedLength(type)); \
    SkipUntil(TokenType::EndLine); \
    return returnValue; \
    }


    #define EXPECT_DATA(str, code) \
    if (!Match(str)) { \
    auto location = ErrorLocation(); \
    m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, location, code); \
    SkipUntil(TokenType::EndLine); \
    return; \
    }
	
	#define EXPECT_DATA_RETURN(str, code, returnValue) \
    if (!Match(str)) { \
    auto location = ErrorLocation(); \
    m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, location, code); \
    SkipUntil(TokenType::EndLine); \
    return returnValue; \
    }

    #define VERIFY(cond, code)                             \
    if (!(cond)) {                                               \
    auto location = ErrorLocation();        \
    m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, location, code); \
    SkipUntil(TokenType::EndLine);  \
    return;                                                      \
    }

    #define VERIFY_WITH_RETURN(cond, code, returnValue)                             \
    if (!(cond)) {                                               \
    auto location = ErrorLocation();        \
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

	// Binding powers, loosest to tightest (left-associative operators use rbp = lbp + 1):
	//   assignment < or < and < not < comparisons < | < ^ < & < shifts < + - < * / % < as < unary < postfix
	static std::map<OperatorType, OperatorInfo> g_OperatorTable = {
		{OperatorType::Index,			  {30, 31}},
		{OperatorType::Dot,				  {30, 31}},
		{OperatorType::Subscript,	      {30, 31, nullptr, nullptr, [](Parser* p, std::shared_ptr<ASTNodeBase> node) { return p->ParseSubscriptExpr(node); }}},
		{OperatorType::FunctionCall,	  {30, 31, nullptr, nullptr, [](Parser* p, std::shared_ptr<ASTNodeBase> node) { return p->ParseFunctionCallExpr(node); }}},
		{OperatorType::StructInitializer, {30, 31, nullptr, nullptr, [](Parser* p, std::shared_ptr<ASTNodeBase> node) { return p->ParseStructInitializerExpr(node); }}},
		{OperatorType::PostIncrement,     {30, 31}},
		{OperatorType::PostDecrement,     {30, 31}},
		{OperatorType::ListInitializer,   {30, 31, [](Parser* p) { return p->ParseListInitializerExpr(); }}},
		{OperatorType::ArrayType,		  {30, 31, [](Parser* p) { return p->ParseArrayType(); }}},

		{OperatorType::Negation,      {0, 26}},
		{OperatorType::Increment,     {0, 26}},
		{OperatorType::Decrement,     {0, 26}},
		{OperatorType::Sizeof,		  {0, 26, [](Parser* p) { return p->ParseSizeofExpr(); }}},
		{OperatorType::BitwiseNot,    {0, 26}},
		{OperatorType::Address,       {0, 26}},
		{OperatorType::Dereference,   {0, 26}},
		{OperatorType::Optional,      {0, 26}},

		{OperatorType::Cast,		  {24, 25, nullptr, [](Parser* p, std::shared_ptr<ASTNodeBase> node) { return p->ParseCastExpr(node); }}},
		{OperatorType::Power,         {23, 22}},

		{OperatorType::Mul, {20, 21}},
		{OperatorType::Div, {20, 21}},
		{OperatorType::Mod, {20, 21}},

		{OperatorType::Add, {18, 19}},
		{OperatorType::Sub, {18, 19}},

		{OperatorType::LeftShift,  {16, 17}},
		{OperatorType::RightShift, {16, 17}},

		{OperatorType::BitwiseAnd, {14, 15}},
		{OperatorType::BitwiseXor, {12, 13}},
		{OperatorType::BitwiseOr,  {10, 11}},
		
		{OperatorType::Is,				{8, 9,	nullptr, [](Parser* p, std::shared_ptr<ASTNodeBase> node) { return p->ParseIsExpr(node); }}},
		{OperatorType::LessThan,        {8, 9}},
		{OperatorType::GreaterThan,     {8, 9}},
		{OperatorType::LessThanEqual,   {8, 9}},
		{OperatorType::GreaterThanEqual,{8, 9}},
		{OperatorType::IsEqual,         {8, 9}},
		{OperatorType::NotEqual,        {8, 9}},
		{OperatorType::Ellipsis,        {30, 31}},
		{OperatorType::In,              {8, 9}},
		{OperatorType::NotIn,           {8, 9}},

		{OperatorType::Not,     {0, 7}},
		{OperatorType::And,     {4, 5}},
		{OperatorType::Or,      {2, 3}},
		{OperatorType::Ternary, {0, 1, [](Parser* p) { return p->ParseTernary();}}},
		{OperatorType::Lambda,  {0, 1, [](Parser* p) { return p->ParseLambda();}}},
		{OperatorType::FunctionType, {0, 1, [](Parser* p) { return p->ParseFunctionType();}}},
		
		{OperatorType::Assignment, {1, 1, nullptr, [](Parser* p, std::shared_ptr<ASTNodeBase> node) { return p->ParseAssignment(node); }}}
	};

    Parser::Parser(const std::vector<Token>& tokens, std::shared_ptr<Module> rootModule, DiagnosticsBuilder& builder)
        : m_Tokens(tokens), m_DiagnosticsBuilder(builder)
    {
        m_Terminators = CreateTokenSet({
            TokenType::EndLine, 
		    TokenType::EndScope, 
		    TokenType::Comma,  
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
            TokenType::PercentEquals,
            TokenType::AmpersandEquals,
            TokenType::PipeEquals,
            TokenType::HatEquals,
            TokenType::LeftShiftEquals,
            TokenType::RightShiftEquals
        });


        m_Literals = CreateTokenSet({
            TokenType::Number,
		    TokenType::String,
            TokenType::Char, 
            TokenType::Keyword
        });

        rootModule->GetRoot()->Children.push_back(ParseCodeBlock());
    }

    Token Parser::ErrorLocation()
    {
        Token current = Peak();
        bool atLineEnd = current.IsType(TokenType::EndLine) || current.IsType(TokenType::EndScope) || current.IsType(TokenType::EndOfFile);

        if (!atLineEnd || m_Position == 0)
            return current;

        // something is missing at the end of a line, point just past the last real token
        Token previous = Prev();
        Token location(TokenType::None, " ", previous.GetSourceFile(), previous.LineNumber, previous.ColumnNumber + previous.GetData().size());
        return location;
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
			size_t start = m_Position;
			auto statement = ParseStatement();

			if (statement)
				block->Children.push_back(statement);

			// a simple statement must end with its line; leftovers (like `a, b = b, a`) are an error, never ignored
			bool endsWithBlock = statement && (statement->GetType() == ASTNodeType::IfExpression || statement->GetType() == ASTNodeType::WhileLoop ||
											   statement->GetType() == ASTNodeType::ForLoop || statement->GetType() == ASTNodeType::FunctionDefinition ||
											   statement->GetType() == ASTNodeType::Class || statement->GetType() == ASTNodeType::Switch ||
											   statement->GetType() == ASTNodeType::Enum || statement->GetType() == ASTNodeType::GenericTemplate ||
											   statement->GetType() == ASTNodeType::Block || statement->GetType() == ASTNodeType::Macro);

			if (statement && !endsWithBlock && !Match(TokenType::EndLine) && !Match(TokenType::EndScope) && !Match(TokenType::EndOfFile))
			{
				m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, Peak(), DiagnosticCode_UnexpectedToken);
				SkipUntil(TokenType::EndLine);
			}

			// error recovery (or `pass`) produced nothing, make sure we never get stuck on the same token
			if (m_Position == start)
				Consume();
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

        // the handlers take the parser as an argument: a static table capturing `this` would stay bound to the first parser
        static const std::map<std::string, std::shared_ptr<ASTNodeBase>(*)(Parser*)> s_MappedKeywordsToFunctions = {
            {"function",  [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseFunctionOrGeneric(); }},
            {"declare",   [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseFunctionDeclaration(); }}, 
            {"return",    [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseReturn(); }}, 
            {"if",        [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseIf(); }},
			{"while",	  [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseWhile(); }},
			{"for",		  [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseFor(); }},
			{"switch",	  [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseSwitch(); }},
			{"enum",	  [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseEnum(); }},
			{"variant",	  [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseVariant(); }},
			{"defer",	  [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseDefer(); }},
			{"const",	  [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseConst(); }},
			{"class",     [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseClass(); }},
			{"union",     [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseClass(); }},
			{"trait",     [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseClass(); }},
			{"macro",     [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseMacro(); }},
			{"async",     [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseAsync(); }},
			{"yield",     [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseYield(); }},
			{"let",		  [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseLet(); }},
			{"import",	  [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseImport(); }},
			{"break",	  [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseLoopControl(); }},
			{"assert",	  [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseAssert(); }},
			{"continue",  [](Parser* p) -> std::shared_ptr<ASTNodeBase> { return p->ParseLoopControl(); }},
        };
        
        if(s_MappedKeywordsToFunctions.contains(Peak().GetData()))
        {
            return s_MappedKeywordsToFunctions.at(Peak().GetData())(this);
        }

		return ParseGeneral();
    }

	std::shared_ptr<ASTNodeBase> Parser::ParseGeneral()
    {
        if(Match(TokenType::Identifier) && Next().IsType(TokenType::Colon))
        {
            return ParseBlock();
        }

		auto first = ParseExpr();

		// a, b = b, a
		if (first && Match(TokenType::Comma))
		{
			auto destructure = std::make_shared<ASTDestructure>();
			destructure->Location = GetNodeLocation(first);
			destructure->Targets.push_back(first);

			while (Match(TokenType::Comma))
			{
				Consume();
				auto target = ParseExpr(2); // stop before `=`

				if (!target)
					break;

				destructure->Targets.push_back(target);
			}

			EXPECT_TOKEN_RETURN(TokenType::Equals, DiagnosticCode_ExpectedAssignment, nullptr);
			Consume();

			destructure->Value = ParseTupleOrExpr();
			return destructure;
		}

		return first;
    }

	std::shared_ptr<ASTNodeBase> Parser::ParseTupleOrExpr()
	{
		// a, b without parentheses (in return and on the right of a destructuring assignment)
		auto first = ParseExpr();

		if (!first || !Match(TokenType::Comma))
			return first;

		auto tuple = std::make_shared<ASTTupleExpr>();
		tuple->Location = GetNodeLocation(first);
		tuple->Values.push_back(first);

		while (Match(TokenType::Comma))
		{
			Consume();
			auto element = ParseExpr();

			if (!element)
				break;

			tuple->Values.push_back(element);
		}

		return tuple;
	}

  	std::shared_ptr<ASTReturn> Parser::ParseReturn()
    {
        EXPECT_DATA_RETURN("return",DiagnosticCode_None, nullptr);
        Token keyword = Consume();

        std::shared_ptr<ASTReturn> returnStatement = std::make_shared<ASTReturn>();
        returnStatement->Location = keyword;
        returnStatement->ReturnValue = ParseTupleOrExpr(); // return a, b

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
		
		// `elseif` and `else if` mean the same
		while (Match("elseif") || (Match("else") && Next().GetData() == "if"))		
		{
			if (Consume().GetData() == "else")
				Consume(); // if

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

	std::shared_ptr<ASTNodeBase> Parser::ParseAssert()
	{
		Token keyword = Consume(); // assert

		auto assertNode = std::make_shared<ASTAssert>();
		assertNode->Location = keyword;
		assertNode->Condition = ParseExpr();

		if (!assertNode->Condition)
		{
			m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, ErrorLocation(), DiagnosticCode_UnexpectedToken);
			SkipUntil(TokenType::EndLine);
			return nullptr;
		}

		// assert condition, "message"
		if (Match(TokenType::Comma))
		{
			Consume();
			assertNode->Message = ParseExpr();
		}

		return assertNode;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseLoopControl()
	{
		Token keyword = Consume();
		return std::make_shared<ASTLoopControlFlow>(keyword.GetData(), keyword);
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseFor()
	{
		Token keyword = Consume(); // for

		auto forExpr = std::make_shared<ASTForExpression>();
		forExpr->Location = keyword;

		EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);
		forExpr->VariableName = Consume();

		EXPECT_DATA_RETURN("in", DiagnosticCode_InvalidForLoop, nullptr);
		Consume();

		auto first = ParseExpr();

		if (!first)
		{
			m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, ErrorLocation(), DiagnosticCode_InvalidForLoop);
			SkipUntil(TokenType::EndLine);
			return nullptr;
		}

		if (Match(TokenType::DotDot) || Match(TokenType::DotDotEquals))
		{
			forExpr->Inclusive = Consume().IsType(TokenType::DotDotEquals);
			forExpr->Start = first;
			forExpr->End = ParseExpr();

			if (!forExpr->End)
			{
				m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, ErrorLocation(), DiagnosticCode_InvalidForLoop);
				SkipUntil(TokenType::EndLine);
				return nullptr;
			}
		}
		else
		{
			forExpr->Iterable = first;
		}

		EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedColon, nullptr);
		Consume();

		forExpr->CodeBlock = ParseCodeBlock();
		return forExpr;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseSwitch()
	{
		Token keyword = Consume(); // switch

		auto switchNode = std::make_shared<ASTSwitch>();
		switchNode->Location = keyword;
		switchNode->Value = ParseExpr();

		if (!switchNode->Value)
		{
			m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, ErrorLocation(), DiagnosticCode_UnexpectedToken);
			SkipUntil(TokenType::EndLine);
			return nullptr;
		}

		EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedColon, nullptr);
		Consume();

		while (true)
		{
			while (Match(TokenType::EndLine))
				Consume();

			if (Match(TokenType::EndOfFile))
				break;

			if (Match(TokenType::EndScope))
			{
				Consume();
				break;
			}

			if (Match("case"))
			{
				Consume();

				SwitchCase switchCase;

				do 
				{
					if (Match(TokenType::Comma))
						Consume();

					auto value = ParseExpr();

					if (!value)
					{
						m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, ErrorLocation(), DiagnosticCode_UnexpectedToken);
						SkipUntil(TokenType::EndLine);
						return nullptr;
					}

					switchCase.Values.push_back(value);
				} while (Match(TokenType::Comma));

				EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedColon, nullptr);
				Consume();

				switchCase.CodeBlock = ParseCodeBlock();
				switchNode->Cases.push_back(switchCase);
				continue;
			}

			if (Match("default"))
			{
				Token defaultToken = Consume();

				if (switchNode->DefaultCaseCodeBlock)
					m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, defaultToken, DiagnosticCode_DuplicateCase);

				EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedColon, nullptr);
				Consume();

				switchNode->DefaultCaseCodeBlock = ParseCodeBlock();
				continue;
			}

			m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, ErrorLocation(), DiagnosticCode_ExpectedCase);
			SkipUntil(TokenType::EndLine);
		}

		return switchNode;
	}

	// variant Number:          a value of one of these types, remembering which
	//     int
	//     float64
	std::shared_ptr<ASTNodeBase> Parser::ParseVariant()
	{
		Token keyword = Consume(); // variant

		auto enumNode = std::make_shared<ASTEnum>();
		enumNode->Location = keyword;
		enumNode->IsTypeVariant = true;

		EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);
		enumNode->Name = Consume();

		EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedColon, nullptr);
		Consume();

		while (true)
		{
			while (Match(TokenType::EndLine) || Match(TokenType::Comma))
				Consume();

			if (Match(TokenType::EndOfFile))
				break;

			if (Match(TokenType::EndScope))
			{
				Consume();
				break;
			}

			if (Match("function"))
			{
				auto method = ParseFunctionDefinition();

				if (method)
					enumNode->Methods.push_back(method);

				continue;
			}

			Token start = Peak();
			auto type = ParseExpr();

			if (!type)
				return nullptr;

			auto field = std::make_shared<ASTVariableDeclaration>(Token(TokenType::Identifier, "value", start.GetSourceFile(), start.LineNumber, start.ColumnNumber));
			field->TypeResolver = type;

			enumNode->Members.push_back({ start, nullptr });
			enumNode->Payloads.push_back({ field });
		}

		return enumNode;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseEnum()
	{
		Token keyword = Consume(); // enum

		auto enumNode = std::make_shared<ASTEnum>();
		enumNode->Location = keyword;

		EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);
		enumNode->Name = Consume();

		EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedColon, nullptr);
		Consume();

		while (true)
		{
			while (Match(TokenType::EndLine) || Match(TokenType::Comma))
				Consume();

			if (Match(TokenType::EndOfFile))
				break;

			if (Match(TokenType::EndScope))
			{
				Consume();
				break;
			}

			// methods make this a rich enum
			if (Match("function"))
			{
				auto method = ParseFunctionDefinition();

				if (method)
					enumNode->Methods.push_back(method);

				continue;
			}

			EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);
			Token name = Consume();

			std::shared_ptr<ASTNodeBase> value;
			std::vector<std::shared_ptr<ASTVariableDeclaration>> payload;

			// Circle(radius: float64): a case carrying data
			if (Match(TokenType::LeftParen))
			{
				Consume();

				while (!Match(TokenType::RightParen))
				{
					auto field = ParseVariableDecleration().Node;

					if (!field)
						return nullptr;

					payload.push_back(field);

					if (Match(TokenType::Comma))
					{
						Consume();
						continue;
					}

					EXPECT_TOKEN_RETURN(TokenType::RightParen, DiagnosticCode_UnmatchedBracket, nullptr);
				}

				Consume(); // )
				enumNode->HasPayloads = true;
			}

			if (Match(TokenType::Equals))
			{
				Consume();
				value = ParseExpr();
			}

			enumNode->Members.push_back({ name, value });
			enumNode->Payloads.push_back(payload);
		}

		return enumNode;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseDefer()
	{
		Token keyword = Consume(); // defer

		auto deferNode = std::make_shared<ASTDefer>();
		deferNode->Location = keyword;
		deferNode->Expr = ParseExpr();

		if (!deferNode->Expr)
		{
			m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, ErrorLocation(), DiagnosticCode_UnexpectedToken);
			SkipUntil(TokenType::EndLine);
			return nullptr;
		}

		return deferNode;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseConst()
	{
		Token keyword = Consume(); // const

		auto declaration = ParseVariableDecleration();

		if (!declaration.Node)
			return nullptr;

		declaration.Node->IsConst = true;
		declaration.Node->Location = keyword;

		if (!declaration.HasBeenInitialized)
		{
			m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, declaration.Node->GetName(), DiagnosticCode_ConstNeedsValue);
			return nullptr;
		}

		return declaration.Node;
	}

	std::shared_ptr<ASTImport> Parser::ParseImport()
	{
		EXPECT_DATA_RETURN("import", DiagnosticCode_None, nullptr);
		Consume();

		std::shared_ptr<ASTImport> importExpr = std::make_shared<ASTImport>();
		importExpr->Location = Peak();

		EXPECT_TOKEN_RETURN(TokenType::String, DiagnosticCode_ExpectedModuleName, nullptr);
		importExpr->Filepath = Consume().GetData();

		if (!Match("as"))
			return importExpr;
		
		Consume();
		importExpr->Namespace = Consume().GetData();

		return importExpr;
	}


	std::shared_ptr<ASTNodeBase> Parser::ParseFunctionOrGeneric()
	{
		auto function = ParseFunctionDefinition();

		if (function && m_PendingGeneric)
		{
			auto generic = m_PendingGeneric;
			m_PendingGeneric = nullptr;
			generic->TemplateNode = function;
			return generic;
		}

		m_PendingGeneric = nullptr;
		return function;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseAsync()
	{
		Token keyword = Consume(); // async

		if (!Match("function"))
		{
			m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, Peak(), DiagnosticCode_UnexpectedToken);
			return nullptr;
		}

		auto node = ParseFunctionOrGeneric();

		if (auto function = std::dynamic_pointer_cast<ASTFunctionDefinition>(node))
			function->IsAsync = true;
		else if (auto generic = std::dynamic_pointer_cast<ASTGenericTemplate>(node))
		{
			if (auto function = std::dynamic_pointer_cast<ASTFunctionDefinition>(generic->TemplateNode))
				function->IsAsync = true;
		}

		return node;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseYield()
	{
		auto yield = std::make_shared<ASTYield>();
		yield->Location = Consume();
		yield->Value = ParseExpr();
		return yield;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseMacro()
	{
		EXPECT_DATA_RETURN("macro", DiagnosticCode_None, nullptr);
		Consume();

		EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);
		auto macro = std::make_shared<ASTMacro>();
		macro->Name = Consume();
		macro->Location = macro->Name;

		EXPECT_TOKEN_RETURN(TokenType::LeftParen, DiagnosticCode_ExpectedLeftParanFunctionDefinition, nullptr);
		Consume();

		while (!Match(TokenType::RightParen) && !Match(TokenType::EndOfFile))
		{
			EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);
			macro->Parameters.push_back(Consume().GetData());

			if (Match(TokenType::Comma))
			{
				Consume();
				continue;
			}

			EXPECT_TOKEN_RETURN(TokenType::RightParen, DiagnosticCode_ExpectedEndOfFunction, nullptr);
		}

		Consume();

		EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedColon, nullptr);
		Consume();

		macro->Body = ParseCodeBlock();
		return macro;
	}

	std::shared_ptr<ASTFunctionDefinition> Parser::ParseFunctionDefinition(bool descriptionOnly, bool isProperty)
	{
		if (!isProperty)
		{
			EXPECT_DATA_RETURN("function", DiagnosticCode_None, nullptr);
		}

		Consume();

		EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);
		Token nameToken = Consume();

		auto funcNode = std::make_shared<ASTFunctionDefinition>(nameToken.GetData());
		funcNode->SetNameToken(nameToken);
		funcNode->Location = nameToken;

		// function max[T](a: T, b: T) -> T
		if (Match(TokenType::LeftBracket))
		{
			m_PendingGeneric = ParseGenericArgs(funcNode);

			if (!m_PendingGeneric)
				return nullptr;
		}

		EXPECT_TOKEN_RETURN(TokenType::LeftParen, DiagnosticCode_ExpectedLeftParanFunctionDefinition, nullptr);
		Consume();

		while (!Match(TokenType::RightParen))
		{
			std::shared_ptr<ASTVariableDeclaration> param;

			// `*self` / `self` shorthand for the receiver, `self: *Vec` is an ordinary parameter
			if (Match(TokenType::Star) || Match("const") || (Match("self") && !Next().IsType(TokenType::Colon)))
				param = ParseSelf();
			else
				param = ParseVariableDecleration().Node;

			if (!param)
				return nullptr; // already reported
			
			funcNode->Arguments.push_back(param);
				
			if (Match(TokenType::Comma))
			{
				Consume();
				continue;
			}

			EXPECT_TOKEN_RETURN(TokenType::RightParen, DiagnosticCode_ExpectedEndOfFunction, nullptr);
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

		if (descriptionOnly && Match(TokenType::EndLine))
			Consume();

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

        // in a declaration `...` marks C varargs, it is not the unpack operator
        m_ParsingDeclaration = true;
        struct ResetFlag { bool& Flag; ~ResetFlag() { Flag = false; } } resetFlag { m_ParsingDeclaration };

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
			variableDecleration->TypeResolver = ParseExpr(2);
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
		// a bare `self` is a pointer to the object (*Self), like `*self`; `self: T` takes a copy
		if (Match("self") && !Next().IsType(TokenType::Colon))
		{
			Token selfToken = Consume();
			auto pointer = std::make_shared<ASTUnaryExpression>(OperatorType::Dereference);
			pointer->Operand = std::make_shared<ASTVariable>(selfToken);

			std::shared_ptr<ASTVariableDeclaration> decl = std::make_shared<ASTVariableDeclaration>(selfToken);
			decl->TypeResolver = pointer;
			return decl;
		}

		auto ty = ParseExpr();

		std::shared_ptr<ASTVariableDeclaration> decl = std::make_shared<ASTVariableDeclaration>(Prev());
		decl->TypeResolver = ty;

		return decl;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseExpr(int64_t minBindingPower)
	{
		if (MatchAny(m_Terminators))
			return nullptr;
		
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
				// await task  /  await pause()
				if (token.GetData() == "await" && m_Position + 1 < m_Tokens.size() && 
					(m_Tokens[m_Position + 1].IsType(TokenType::Identifier) || m_Tokens[m_Position + 1].IsType(TokenType::LeftParen)))
				{
					Consume();
					auto await = std::make_shared<ASTAwait>();
					await->Location = token;

					if (Match("pause") && m_Position + 2 < m_Tokens.size() && m_Tokens[m_Position + 1].IsType(TokenType::LeftParen) && m_Tokens[m_Position + 2].IsType(TokenType::RightParen))
					{
						Consume(); Consume(); Consume();
						await->IsPause = true;
					}
					else
					{
						await->Operand = ParseExpr(g_OperatorTable.at(OperatorType::Dereference).RightBindingPower);

						if (!await->Operand)
							return nullptr;
					}

					lhs = await;
					break;
				}

				// move lambda: ...  the lambda takes over the values it uses instead of borrowing them
				if (token.GetData() == "move" && m_Position + 1 < m_Tokens.size() && m_Tokens[m_Position + 1].GetData() == "lambda")
				{
					Consume(); // move
					auto lambda = std::dynamic_pointer_cast<ASTLambda>(ParseLambda());

					if (!lambda)
						return nullptr;

					lambda->MovesCaptures = true;
					lhs = lambda;
					break;
				}

				// name!(a, b): a macro use
				if (m_Position + 2 < m_Tokens.size() && m_Tokens[m_Position + 1].IsType(TokenType::Bang) && m_Tokens[m_Position + 2].IsType(TokenType::LeftParen))
				{
					auto call = std::make_shared<ASTMacroCall>();
					call->Name = Consume();
					call->Location = call->Name;
					Consume(); // !
					Consume(); // (

					while (!Match(TokenType::RightParen) && !Match(TokenType::EndOfFile))
					{
						auto argument = ParseExpr();

						if (!argument)
							return nullptr;

						call->Arguments.push_back(argument);

						if (Match(TokenType::Comma))
						{
							Consume();
							continue;
						}

						EXPECT_TOKEN_RETURN(TokenType::RightParen, DiagnosticCode_ExpectedEndOfFunction, nullptr);
					}

					Consume();
					lhs = call;
					break;
				}

				lhs = std::make_shared<ASTVariable>(token);
				Consume();
					
				break;
			}
			case TokenType::LeftParen:
			{
				Token open = Consume(); // (
				lhs = ParseExpr();

				// (a, b) is a tuple, (a,) a tuple with one element
				if (Match(TokenType::Comma))
				{
					auto tuple = std::make_shared<ASTTupleExpr>();
					tuple->Location = open;
					tuple->Values.push_back(lhs);

					while (Match(TokenType::Comma))
					{
						Consume();

						if (Match(TokenType::RightParen))
							break;

						auto element = ParseExpr();

						if (!element)
						{
							EXPECT_TOKEN_RETURN(TokenType::RightParen, DiagnosticCode_UnmatchedBracket, nullptr);
							break;
						}

						tuple->Values.push_back(element);
					}

					lhs = tuple;
				}

				EXPECT_TOKEN_RETURN(TokenType::RightParen, DiagnosticCode_UnmatchedBracket, nullptr);
				Consume(); // )

				break;
			}
			case TokenType::Char:
			{
				lhs = std::make_shared<ASTNodeLiteral>(token);
				Consume();

				break;
			}
			case TokenType::Keyword:
			default:
			{
				if (token.GetData() == "true" || token.GetData() == "false" || token.GetData() == "null" || token.GetData() == "none")
				{
					lhs = std::make_shared<ASTNodeLiteral>(token);
					Consume();

					break;
				}

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
		
		if (lhs && lhs->Location.GetSourceFile().empty())
			lhs->Location = token;

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

			// `x not in items`
			if (Match("not") && Next().GetData() == "in")
			{
				OperatorInfo info = g_OperatorTable.at(OperatorType::NotIn);
				if (info.LeftBindingPower < minBindingPower)
					break;

				Token notToken = Consume(); // not
				Consume();                  // in

				auto binaryExpr = std::make_shared<ASTBinaryExpression>(OperatorType::NotIn);
				binaryExpr->Location = notToken;
				binaryExpr->LeftSide = lhs;
				binaryExpr->RightSide = ParseExpr(info.RightBindingPower);
				lhs = binaryExpr;
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
		binaryExpr->Location = token;
		binaryExpr->LeftSide = lhs;
		binaryExpr->RightSide = ParseExpr(info.RightBindingPower);

		return binaryExpr;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParsePostfixExpr(std::shared_ptr<ASTNodeBase> lhs)
	{
		Token token = Consume();
		OperatorType op = GetPostfixOperator(token);

		std::shared_ptr<ASTUnaryExpression> unaryExpr = std::make_shared<ASTUnaryExpression>(op);
		unaryExpr->Location = token;
		unaryExpr->Operand = lhs;

		return unaryExpr;
	}
	
	std::shared_ptr<ASTNodeBase> Parser::ParseFunctionCallExpr(std::shared_ptr<ASTNodeBase> lhs)
	{
		EXPECT_TOKEN_RETURN(TokenType::LeftParen, DiagnosticCode_None, nullptr);
		Consume();

		std::shared_ptr<ASTFunctionCall> funcCall = std::make_shared<ASTFunctionCall>();
		funcCall->Location = Prev();
		funcCall->Callee = lhs;
		
		while (!Match(TokenType::RightParen))
		{
			auto argument = ParseExpr();

			if (!argument)
			{
				EXPECT_TOKEN_RETURN(TokenType::RightParen, DiagnosticCode_UnmatchedBracket, nullptr);
				break;
			}

			funcCall->Arguments.push_back(argument);

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
			auto argument = ParseExpr();

			if (!argument)
			{
				EXPECT_TOKEN_RETURN(TokenType::RightBracket, DiagnosticCode_UnmatchedBracket, nullptr);
				break;
			}

			subscript->SubscriptArgs.push_back(argument);

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

		while(!Match(TokenType::RightBrace) && !Match(TokenType::EndOfFile))
		{
			while(Match(TokenType::EndLine) || Match(TokenType::EndScope))
				Consume();

			if(Match(TokenType::RightBrace))
				break;

			auto value = ParseExpr();

			if (!value)
			{
				EXPECT_TOKEN_RETURN(TokenType::RightBrace, DiagnosticCode_UnexpectedToken, nullptr);
				break;
			}

			expr->Values.push_back(value);

			if(Match(TokenType::Comma))
				Consume();
		}

		EXPECT_TOKEN_RETURN(TokenType::RightBrace, DiagnosticCode_UnmatchedBracket, nullptr);
		Consume();
		return expr;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseAssignment(std::shared_ptr<ASTNodeBase> lhs)
	{
		Token assignmentType = Consume();
		AssignmentOperatorType opType = AssignmentOperatorType::None;

		switch (assignmentType.GetType())
		{
			case TokenType::MinusEquals:
			{
				opType = AssignmentOperatorType::Sub;
				break;
			}
			case TokenType::PlusEquals:
			{
				opType = AssignmentOperatorType::Add;
				break;
			}
			case TokenType::StarEquals:
			{
				opType = AssignmentOperatorType::Mul;
				break;
			}
			case TokenType::SlashEquals:
			{
				opType = AssignmentOperatorType::Div;
				break;
			}
			case TokenType::PercentEquals:
			{
				opType = AssignmentOperatorType::Mod;
				break;
			}
			case TokenType::Equals:
			{
				opType = AssignmentOperatorType::Normal;
				break;
			}
			case TokenType::AmpersandEquals:  opType = AssignmentOperatorType::BitAnd; break;
			case TokenType::PipeEquals:       opType = AssignmentOperatorType::BitOr;  break;
			case TokenType::HatEquals:        opType = AssignmentOperatorType::BitXor; break;
			case TokenType::LeftShiftEquals:  opType = AssignmentOperatorType::Shl;    break;
			case TokenType::RightShiftEquals: opType = AssignmentOperatorType::Shr;    break;
			default:
			{
				break;
				
			}
		}

		if (std::shared_ptr<ASTVariableDeclaration> decl = std::dynamic_pointer_cast<ASTVariableDeclaration>(lhs))
		{
			VERIFY_WITH_RETURN(opType == AssignmentOperatorType::Normal, DiagnosticCode_InvalidOperator, nullptr);

			decl->Initializer = ParseExpr();
			return decl;
		}
		
		VERIFY_WITH_RETURN(opType != AssignmentOperatorType::None, DiagnosticCode_UnexpectedToken, nullptr);

		std::shared_ptr<ASTAssignmentOperator> node = std::make_shared<ASTAssignmentOperator>(opType); 
		node->Location = assignmentType;
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
		isExpr->Location = Prev();
		isExpr->Object = lhs;

		// x is not none
		if (Match("not"))
		{
			Consume();
			isExpr->Negate = true;
		}

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

	std::shared_ptr<ASTNodeBase> Parser::ParseLambda()
	{
		Token keyword = Consume(); // lambda

		auto lambda = std::make_shared<ASTLambda>();
		lambda->Location = keyword;

		if (Match(TokenType::LeftParen))
		{
			// lambda (x: int, y: int) -> int: body
			Consume();

			while (!Match(TokenType::RightParen))
			{
				auto parameter = ParseVariableDecleration().Node;

				if (!parameter)
					return nullptr;

				lambda->Parameters.push_back(parameter);

				if (Match(TokenType::Comma))
				{
					Consume();
					continue;
				}

				EXPECT_TOKEN_RETURN(TokenType::RightParen, DiagnosticCode_UnmatchedBracket, nullptr);
			}

			Consume(); // )

			if (Match(TokenType::RightThinArrow))
			{
				Consume();
				lambda->ReturnType = ParseExpr(2);
			}
		}
		else
		{
			// lambda x, y: body (types come from where the lambda is used)
			while (Match(TokenType::Identifier))
			{
				lambda->Parameters.push_back(std::make_shared<ASTVariableDeclaration>(Consume()));

				if (!Match(TokenType::Comma))
					break;

				Consume();
			}
		}

		EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedColon, nullptr);
		Consume();

		lambda->Body = ParseExpr();

		if (!lambda->Body)
		{
			m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, ErrorLocation(), DiagnosticCode_UnexpectedToken);
			return nullptr;
		}

		return lambda;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseFunctionType()
	{
		// function(int, int) -> int
		Token keyword = Consume();

		auto type = std::make_shared<ASTFunctionTypeExpr>();
		type->Location = keyword;

		EXPECT_TOKEN_RETURN(TokenType::LeftParen, DiagnosticCode_ExpectedLeftParanFunctionDefinition, nullptr);
		Consume();

		while (!Match(TokenType::RightParen))
		{
			auto parameter = ParseExpr(2);

			if (!parameter)
			{
				EXPECT_TOKEN_RETURN(TokenType::RightParen, DiagnosticCode_UnmatchedBracket, nullptr);
				break;
			}

			type->Parameters.push_back(parameter);

			if (Match(TokenType::Comma))
			{
				Consume();
				continue;
			}

			EXPECT_TOKEN_RETURN(TokenType::RightParen, DiagnosticCode_UnmatchedBracket, nullptr);
		}

		Consume(); // )

		if (Match(TokenType::RightThinArrow))
		{
			Consume();
			type->ReturnType = ParseExpr(2);
		}

		return type;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseListInitializerExpr()
	{
		EXPECT_TOKEN_RETURN(TokenType::LeftBrace, DiagnosticCode_None, nullptr);
		Consume();

		std::shared_ptr<ASTListExpr> expr = std::make_shared<ASTListExpr>();

		while(!Match(TokenType::RightBrace) && !Match(TokenType::EndOfFile))
		{
			while(Match(TokenType::EndLine) || Match(TokenType::EndScope))
				Consume();

			if(Match(TokenType::RightBrace))
				break;

			auto value = ParseExpr();

			if (!value)
			{
				EXPECT_TOKEN_RETURN(TokenType::RightBrace, DiagnosticCode_UnexpectedToken, nullptr);
				break;
			}

			expr->Values.push_back(value);

			if(Match(TokenType::Comma))
				Consume();
		}

		EXPECT_TOKEN_RETURN(TokenType::RightBrace, DiagnosticCode_UnmatchedBracket, nullptr);
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

	// operator names, and the hook the compiler looks for under each
	static const std::vector<std::pair<std::string, std::string>> s_OperatorNames = {
		{ "add", "__add__" }, { "subtract", "__sub__" }, { "multiply", "__mul__" }, { "divide", "__div__" },
		{ "modulo", "__mod__" }, { "power", "__pow__" },
		{ "equals", "__eq__" }, { "not_equals", "__ne__" }, { "less", "__lt__" }, { "less_equal", "__le__" },
		{ "greater", "__gt__" }, { "greater_equal", "__ge__" },
		{ "get", "__getitem__" }, { "set", "__setitem__" }, { "len", "__len__" }, { "contains", "__contains__" },
		{ "iterate", "__iter__" }, { "call", "__call__" }, { "str", "__str__" }, { "hash", "__hash__" },
		{ "destruct", "__destruct__" },
	};

	bool Parser::NameSpecialMethod(std::shared_ptr<ASTFunctionDefinition> method, const Token& nameToken, bool isOperator)
	{
		const std::string name = method->GetName();

		if (isOperator)
		{
			auto it = std::find_if(s_OperatorNames.begin(), s_OperatorNames.end(), [&](auto& entry) { return entry.first == name; });

			if (it == s_OperatorNames.end())
			{
				std::string known;
				for (auto& [operatorName, hook] : s_OperatorNames)
					known += (known.empty() ? "" : ", ") + operatorName;

				Token where = nameToken;
				where.SetData(std::format("{}’. The operators are: {}", name, known));
				m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, where, DiagnosticCode_UnknownOperator, name.size());
				return false;
			}

			method->SetName(it->second);
			return true;
		}

		// the constructor is `function init`
		if (name == "init")
		{
			method->SetName("__init__");
			return true;
		}

		// Python-style __add__: point at the Clear spelling
		if (name.size() > 4 && name.starts_with("__") && name.ends_with("__"))
		{
			std::string suggestion = name == "__init__" ? "function init(self, ...)" : "operator <name>(self, ...)";

			for (auto& [operatorName, hook] : s_OperatorNames)
			{
				if (hook == name)
					suggestion = std::format("operator {}(self, ...)", operatorName);
			}

			Token where = nameToken;
			where.SetData(std::format("{}’ is written ‘{}", name, suggestion));
			m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, where, DiagnosticCode_UseOperatorSyntax, name.size());
			return false;
		}

		return true;
	}

	std::shared_ptr<ASTNodeBase> Parser::ParseClass()
    {
        // `union Name:` is parsed like a class whose fields share storage, `trait Name:` holds only method signatures
        bool isUnion = Match("union");
        bool isTrait = Match("trait");

        if (!isUnion && !isTrait)
        {
            EXPECT_DATA_RETURN("class", DiagnosticCode_None, nullptr);
        }

        Consume();

        EXPECT_TOKEN_RETURN(TokenType::Identifier,  DiagnosticCode_ExpectedIdentifier, nullptr);
        Token nameToken = Consume();
        std::string className = nameToken.GetData();

        std::shared_ptr<ASTClass> classNode = std::make_shared<ASTClass>(className);
        classNode->IsUnion = isUnion;
        classNode->IsTrait = isTrait;
        classNode->Location = nameToken;
		std::shared_ptr<ASTGenericTemplate> genericTemplate;

        if(Match(TokenType::LeftBracket))
        {
			genericTemplate = ParseGenericArgs(classNode);
        }

        // class Dog(Animal, Named): a base class and/or traits
        if (Match(TokenType::LeftParen))
        {
            Consume();

            while (!Match(TokenType::RightParen) && !Match(TokenType::EndOfFile))
            {
                auto base = ParseExpr(); // the parser stops at `,` and `)`

                if (!base)
                    return nullptr;

                classNode->Bases.push_back(base);

                if (Match(TokenType::Comma))
                {
                    Consume();
                    continue;
                }

                EXPECT_TOKEN_RETURN(TokenType::RightParen, DiagnosticCode_ExpectedEndOfFunction, nullptr);
            }

            Consume();
        }

        EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedColon, nullptr);
        Consume();

        while(!Match(TokenType::EndScope) && !Match(TokenType::EndOfFile))
        {
            while(Match(TokenType::EndLine))
                Consume();

			if (Match(TokenType::EndScope))
				break;

            // virtual function speak(self): dispatched through the vtable
            // property area(self) -> float: read as obj.area;  property area(self, value: float): obj.area = value
            // operator add(self, other: Vec2) -> Vec2: what `a + b` calls
            bool isVirtual = Match("virtual") && Next().GetData() == "function";
            bool isProperty = Match("property") && Next().IsType(TokenType::Identifier);
            bool isOperator = Match("operator") && Next().IsType(TokenType::Identifier);

            // methods of classes that inherit (or are inherited from) dispatch automatically
            if (isVirtual)
            {
                m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, Peak(), DiagnosticCode_VirtualNotNeeded);
                Consume();
            }

            if(Match("function") || isProperty || isOperator)
            {
				Token methodToken = Next();
				auto method = ParseFunctionDefinition(isTrait, isProperty || isOperator);

				if (method)
				{
					method->IsVirtual = isVirtual;
					method->IsProperty = isProperty;

					// the setter lives next to the getter under its own name
					if (isProperty && method->Arguments.size() == 2)
						method->SetName("__set_" + method->GetName());

					if (!NameSpecialMethod(method, methodToken, isOperator))
						method = nullptr;
				}

				if (m_PendingGeneric)
				{
					m_PendingGeneric = nullptr;
					m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, methodToken, DiagnosticCode_GenericMethodUnsupported);
				}

				if (method)
					classNode->MemberFunctions.push_back(method);

                continue;
            }
			
			EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);
            auto typeSpec = std::make_shared<ASTTypeSpecifier>(Consume().GetData());
			
			EXPECT_TOKEN_RETURN(TokenType::Colon, DiagnosticCode_ExpectedColon, nullptr);
			Consume();

            // stop before `=` so `x: int = 5` is a type followed by a default, not an assignment
            typeSpec->TypeResolver = ParseExpr(2);

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

		while (!Match(TokenType::RightBracket) && !Match(TokenType::EndOfFile))
		{
			EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);
			genericTemplate->GenericTypeNames.push_back(Consume().GetData());
			genericTemplate->Constraints.emplace_back();

			// [T: Shape]: T must be a class that satisfies the trait Shape
			if (Match(TokenType::Colon))
			{
				Consume();
				EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);
				genericTemplate->Constraints.back() = Consume();
			}
			
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
		Token keyword = Consume();

		// let q, r = divmod(7, 2)   /   let (q, r) = ...
		bool parenthesized = Match(TokenType::LeftParen) && Next().IsType(TokenType::Identifier);
		size_t afterParen = parenthesized ? m_Position + 1 : m_Position;
		bool isDestructure = afterParen + 1 < m_Tokens.size() && m_Tokens[afterParen].IsType(TokenType::Identifier) && m_Tokens[afterParen + 1].IsType(TokenType::Comma);

		if (isDestructure)
		{
			if (parenthesized)
				Consume();

			auto destructure = std::make_shared<ASTDestructure>();
			destructure->Location = keyword;
			destructure->IsDeclaration = true;

			do
			{
				if (Match(TokenType::Comma))
					Consume();

				EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);
				destructure->Targets.push_back(std::make_shared<ASTVariable>(Consume()));
			} while (Match(TokenType::Comma));

			if (parenthesized)
			{
				EXPECT_TOKEN_RETURN(TokenType::RightParen, DiagnosticCode_UnmatchedBracket, nullptr);
				Consume();
			}

			EXPECT_TOKEN_RETURN(TokenType::Equals, DiagnosticCode_ExpectedAssignment, nullptr);
			Consume();

			destructure->Value = ParseTupleOrExpr();
			return destructure;
		}

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
            case TokenType::AmpersandEquals:  return AssignmentOperatorType::BitAnd;
            case TokenType::PipeEquals:       return AssignmentOperatorType::BitOr;
            case TokenType::HatEquals:        return AssignmentOperatorType::BitXor;
            case TokenType::LeftShiftEquals:  return AssignmentOperatorType::Shl;
            case TokenType::RightShiftEquals: return AssignmentOperatorType::Shr;
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
			case TokenType::Telda:				return OperatorType::BitwiseNot;
			case TokenType::QuestionMark:		return OperatorType::Optional;
			default:
				break;
		}

		if (current.GetData() == "not")    return OperatorType::Not;
		if (current.GetData() == "when")   return OperatorType::Ternary;
		if (current.GetData() == "lambda") return OperatorType::Lambda;
		if (current.GetData() == "function") return OperatorType::FunctionType;
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
			case TokenType::StarStar:			return OperatorType::Power;
			case TokenType::Percent:            return OperatorType::Mod;
							
			case TokenType::Ampersand:          return OperatorType::BitwiseAnd;
			case TokenType::Pipe:               return OperatorType::BitwiseOr;
			case TokenType::Hat:                return OperatorType::BitwiseXor;
			case TokenType::LeftShift:          return OperatorType::LeftShift;
			case TokenType::RightShift:         return OperatorType::RightShift;
			
			case TokenType::LogicalAnd:         return OperatorType::And;
			case TokenType::LogicalOr:          return OperatorType::Or;
			
			case TokenType::EqualsEquals:       return OperatorType::IsEqual;
			case TokenType::BangEquals:         return OperatorType::NotEqual;
			case TokenType::LessThan:           return OperatorType::LessThan;
			case TokenType::LessThanEquals:     return OperatorType::LessThanEqual;
			case TokenType::GreaterThan:        return OperatorType::GreaterThan;
			case TokenType::GreaterThanEquals:  return OperatorType::GreaterThanEqual;
			case TokenType::Dot:                return OperatorType::Dot;
			case TokenType::LeftBracket:        return OperatorType::Index;
			
			case TokenType::StarEquals:
			case TokenType::SlashEquals:
			case TokenType::MinusEquals:
			case TokenType::PlusEquals:
			case TokenType::PercentEquals:
			case TokenType::AmpersandEquals:
			case TokenType::PipeEquals:
			case TokenType::HatEquals:
			case TokenType::LeftShiftEquals:
			case TokenType::RightShiftEquals:
			case TokenType::Equals:				return OperatorType::Assignment;
		
			default:
				break;
		}

		if (current.GetData() == "and")     return OperatorType::And;
		if (current.GetData() == "or")      return OperatorType::Or;
		if (current.GetData() == "as")		return OperatorType::Cast;
		if (current.GetData() == "in")		return OperatorType::In;
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
			case TokenType::Ellipses:			return m_ParsingDeclaration ? OperatorType::None : OperatorType::Ellipsis; // f(values...)
			default:
				break;
		}

		return OperatorType::None;
	}
}
