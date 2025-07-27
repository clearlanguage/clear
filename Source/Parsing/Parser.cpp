#include "Parser.h"
#include "AST/ASTNode.h"
#include "Core/Log.h"
#include "Core/Operator.h"
#include "Diagnostics/DiagnosticCode.h"
#include "Lexing/TokenDefinitions.h"
#include "Lexing/Token.h"
#include "Symbols/Module.h"
#include "Symbols/SymbolTable.h"

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


    Parser::Parser(const std::vector<Token>& tokens, std::shared_ptr<Module> rootModule,DiagnosticsBuilder& builder)
        : m_Tokens(tokens), m_DiagnosticsBuilder(builder)
    {
        m_Modules.push_back(rootModule);
        //m_RootStack.push_back(rootModule->GetRoot());

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

        while(!Match(TokenType::EndOfFile))
        {
            ParseStatement();
        }
    }

    std::shared_ptr<ASTNodeBase> Parser::GetResult()
    {
        return m_RootStack[0];
    }

    std::shared_ptr<ASTBlock> Parser::Root()
    {
        return m_RootStack.back();
    }

    std::shared_ptr<Module> Parser::RootModule()
    {
        return m_Modules.back();
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

   /*  void Parser::ExpectAny(TokenSet tokenSet)
    {
        if(MatchAny(tokenSet)) return;
        
        CLEAR_LOG_ERROR("missing expected token from token set");    
        CLEAR_UNREACHABLE("TODO");
    } */

    void Parser::ParseStatement()
    {
        if (Match(TokenType::EndLine))
        {
            Consume();
            return;
        }

        if (Match("pass"))
        {
            Consume();
            return;
        }

        static std::map<std::string, std::function<void()>> s_MappedKeywordsToFunctions = {
            {"function",  [this]() { ParseFunctionDefinition(); }},
            {"while",     [this]() { ParseWhile(); }},
            {"for",       [this]() { ParseFor(); }},
            {"declare",   [this]() { ParseFunctionDeclaration(); }}, 
            {"struct",    [this]() { ParseStruct(); }}, 
            {"return",    [this]() { ParseReturn(); }}, 
            {"if",        [this]() { ParseIf(); }},
            {"else",      [this]() { ParseElse(); }},
            {"elseif",    [this]() { ParseElseIf(); }},
            {"defer",     [this]() { ParseDefer(); }},
            {"class",     [this]() { ParseClass(); }},
            {"enum",      [this]() { ParseEnum(); }},
            {"trait",     [this]() { ParseTrait(); }},
            {"module",    [this]() { ParseModule(); }},
            {"endmodule", [this]() { ParseEndModule(); }},
            {"break",     [this]() { ParseLoopControls();}},
            {"continue",  [this]() { ParseLoopControls();}},
            {"switch",    [this]() { ParseSwitch();}}, 
            {"pass",      [this]() { Consume(); }}
        };
        
        static std::map<TokenType, std::function<void()>> s_MappedTokenTypeToFunctions = {
            {TokenType::EndScope,   [this]() { ParseIndentation(); }}
        };  

        if(s_MappedKeywordsToFunctions.contains(Peak().GetData()))
        {
            s_MappedKeywordsToFunctions.at(Peak().GetData())();
        }
        else if (s_MappedTokenTypeToFunctions.contains(Peak().GetType()))
        {
            s_MappedTokenTypeToFunctions.at(Peak().GetType())();
        }
        else 
        {
            ParseGeneral();
        }
    }

    void Parser::ParseGeneral()
    {
        if(Match(TokenType::Identifier) && Next().IsType(TokenType::Colon))
        {
            ParseBlock();
            return;
        }

        bool isDeclaration = IsDeclaration();

        // parse either expression or declerations, allowing for multiple on the same line seperated by commas
        if(!isDeclaration)
        {
            while(!(Match(TokenType::EndLine) || Match(TokenType::EndScope) || Match(TokenType::EndOfFile)))
            {
                auto expr = ParseExpression();

                if(MatchAny(m_AssignmentOperators))
                {
                    Root()->Children.push_back(ParseAssignment(expr));
                    return;
                }
                else 
                {
                    Root()->Children.push_back(expr);
                }

                if(Match(TokenType::Comma))
                    Consume();
            }

            return;
        }

        while(isDeclaration)
        {
            auto decleration = ParseVariableDecleration();

            if(decleration.HasBeenInitialized)
            {
                Root()->Children.push_back(decleration.Node);
            }
            else 
            {
                auto initializer = std::make_shared<ASTDefaultInitializer>();
                initializer->Storage = decleration.Node;

                Root()->Children.push_back(initializer);
            }

            if(Match(TokenType::Comma))
                Consume();

            isDeclaration = IsDeclaration();
        }
    }

    void Parser::ParseLoopControls() 
    {
        auto node = std::make_shared<ASTLoopControlFlow>(Peak().GetData());
        Consume();
        Root()->Children.push_back(node);
    }

    void Parser::ParseTrait()
    { 
        EXPECT_DATA("trait",DiagnosticCode_None);
        Consume();

        std::string traitName = Consume().GetData();

        EXPECT_TOKEN(TokenType::Colon,  DiagnosticCode_ExpectedIndentation);
        Consume();

        EXPECT_TOKEN(TokenType::EndLine, DiagnosticCode_ExpectedNewlineAferIndentation);
        Consume();
    
        std::shared_ptr<ASTTrait> trait = std::make_shared<ASTTrait>(traitName);

        while (Match("function") || Match(TokenType::Keyword) || Match(TokenType::Identifier))
        {
			m_RootStack.push_back(std::make_shared<ASTBlock>());

            if(Match("function")) // parse as function decleration
            {
                ParseFunctionDeclaration("function");
				trait->FunctionDeclarations.push_back(std::dynamic_pointer_cast<ASTFunctionDeclaration>(m_RootStack.back()->Children[0]));
            }
            else 
            {
                ParseVariableDecleration();
				trait->VariableDeclarations.push_back(std::dynamic_pointer_cast<ASTVariableDeclaration>(m_RootStack.back()->Children[0]));
            }

			m_RootStack.pop_back();

            while(Match(TokenType::EndLine))
                Consume();
        }

        m_RootStack.pop_back();
        Root()->Children.push_back(trait);

        if(Match(TokenType::EndScope))
            Consume(); 
    }


    void Parser::ParseStruct()
    {
        EXPECT_DATA("struct",DiagnosticCode_None);
        Consume();

        EXPECT_TOKEN(TokenType::Identifier,DiagnosticCode_ExpectedIdentifier);
        std::string structName = Consume().GetData();

        EXPECT_TOKEN(TokenType::Colon,DiagnosticCode_ExpectedIndentation);
        Consume();

        EXPECT_TOKEN(TokenType::EndLine,DiagnosticCode_ExpectedNewlineAferIndentation);
        Consume();

        auto struct_ = std::make_shared<ASTStruct>(structName);

        std::vector<std::shared_ptr<ASTTypeSpecifier>> typeSpecifiers;
        std::vector<std::shared_ptr<ASTNodeBase>> defaultArgs;

        auto Flush = [&]()
        {
            for(const auto& type : typeSpecifiers)
            {
                struct_->Members.push_back(type);
            }

            for(const auto& defaultArg : defaultArgs)
            {
                struct_->DefaultValues.push_back(defaultArg);
            }

            Root()->Children.push_back(struct_);
            Consume();
        };

        auto ConstructType = [&](const std::shared_ptr<ASTNodeBase>& type)
        {   
            EXPECT_TOKEN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier);
            std::string memberName = Consume().GetData();

            auto member = std::make_shared<ASTTypeSpecifier>(memberName);
            member->TypeResolver = type;

            typeSpecifiers.push_back(member);

            if(Match(TokenType::Equals))
            {
                Consume();
                defaultArgs.push_back(ParseExpression());
            }
            else 
            {
                defaultArgs.push_back(nullptr);
            }
        };

        while(!Match(TokenType::EndScope))
        {
            auto type = ParseTypeResolver();
            ConstructType(type);

            while(Match(TokenType::Comma))
            {
                Consume();
                ConstructType(type);
            }
            
            while(Match(TokenType::EndLine))
                Consume();
        }

        Flush();
    }

    void Parser::ParseImport()
    {
        CLEAR_UNREACHABLE("unimplemented");
       /*  Expect(TokenType::Import);

        Consume();

        Expect(TokenType::RValueString);

        std::string path  = Consume().Data;
        std::string alias = "";

        if(Match(TokenType::As))
        {
            Consume();

            Expect(TokenType::RValueString);
            alias = Consume().Data;
            
            auto aliases = Split(alias, '.');            
            m_Aliases.insert(aliases.begin(), aliases.end());
        }

        std::shared_ptr<ASTImport> import = std::make_shared<ASTImport>(path, alias);

        Expect(TokenType::EndLine);
        Root()->Children.push_back(import); */
    }

    void Parser::ParseReturn()
    {
        EXPECT_DATA("return",DiagnosticCode_None);
        Consume();

        std::shared_ptr<ASTReturn> returnStatement = std::make_shared<ASTReturn>();
        returnStatement->ReturnValue = ParseExpression();

        Root()->Children.push_back(returnStatement); 
    }

    void Parser::ParseIf()
    {
        EXPECT_DATA("if",DiagnosticCode_None);
        Consume();

		std::shared_ptr<ASTBlock> codeBlock = std::make_shared<ASTBlock>();

        std::shared_ptr<ASTIfExpression> ifExpr = std::make_shared<ASTIfExpression>();
		
		ifExpr->ConditionalBlocks.push_back({
			.Condition = ParseExpression(GetLastBracket(TokenType::QuestionMark, TokenType::Colon)),
			.CodeBlock = codeBlock 
		});

        Root()->Children.push_back(ifExpr);
        m_RootStack.push_back(codeBlock); 

        EXPECT_TOKEN(TokenType::Colon, DiagnosticCode_ExpectedIndentation)
        Consume();
    }

    void Parser::ParseElse()
    {
        EXPECT_DATA("else",DiagnosticCode_None);
        Consume();

        auto& last = Root()->Children.back();
        std::shared_ptr<ASTIfExpression> ifExpr = std::dynamic_pointer_cast<ASTIfExpression>(last);
        VERIFY(ifExpr, DiagnosticCode_ElseNotInIfBlock);
        
        std::shared_ptr<ASTBlock> codeBlock = std::make_shared<ASTBlock>();
        ifExpr->ElseBlock = codeBlock;
            
        m_RootStack.push_back(codeBlock); 

        EXPECT_TOKEN(TokenType::Colon, DiagnosticCode_ExpectedIndentation)
        Consume();
    }

    void Parser::ParseWhile()
    {
        EXPECT_DATA("while",DiagnosticCode_None);
        Consume();
        
		std::shared_ptr<ASTBlock> codeBlock = std::make_shared<ASTBlock>();

        std::shared_ptr<ASTWhileExpression> whileExp = std::make_shared<ASTWhileExpression>();

		whileExp->WhileBlock = {
			.Condition = ParseExpression(GetLastBracket(TokenType::QuestionMark,TokenType::Colon)),
			.CodeBlock = codeBlock
		};


        Root()->Children.push_back(whileExp);
        m_RootStack.push_back(codeBlock);

        EXPECT_TOKEN(TokenType::Colon,DiagnosticCode_ExpectedIndentation)
        Consume();
    }

    void Parser::ParseFor()
    {
        EXPECT_DATA("for",DiagnosticCode_None);
        Consume();

        EXPECT_TOKEN(TokenType::Identifier,DiagnosticCode_ExpectedIdentifier);
        std::string name = Consume().GetData();

        EXPECT_DATA("in", DiagnosticCode_InvalidForLoop);
        Consume();

        // TODO: add more comprehensive parseIter function here. for now only variadic arguments are supported

        EXPECT_TOKEN(TokenType::Identifier,DiagnosticCode_ExpectedIdentifier);
        auto var = std::make_shared<ASTVariable>(Consume().GetData());

        auto forLoop = std::make_shared<ASTForExpression>(name);
        forLoop->Iterator = var;

        Root()->Children.push_back(forLoop);

        auto body = std::make_shared<ASTBlock>();
        forLoop->CodeBlock = body;

        m_RootStack.push_back(body);

        EXPECT_TOKEN(TokenType::Colon,DiagnosticCode_ExpectedIndentation)
        Consume();
    }

    void Parser::ParseElseIf()
    {
        EXPECT_DATA("elseif",DiagnosticCode_None);
        Consume();

        auto& last = Root()->Children.back();

        std::shared_ptr<ASTIfExpression> ifExpr = std::dynamic_pointer_cast<ASTIfExpression>(last);
        VERIFY(ifExpr, DiagnosticCode_ElseNotInIfBlock);

        std::shared_ptr<ASTBlock> codeBlock = std::make_shared<ASTBlock>();

        ifExpr->ConditionalBlocks.push_back(ConditionalBlock {
			.Condition = ParseExpression(GetLastBracket(TokenType::QuestionMark, TokenType::Colon)),
			.CodeBlock = codeBlock
		});

        m_RootStack.push_back(codeBlock); 

        EXPECT_TOKEN(TokenType::Colon, DiagnosticCode_ExpectedIndentation);
        Consume();
    }

    void Parser::ParseFunctionDefinition(bool descriptionOnly)
    {
        EXPECT_DATA("function", DiagnosticCode_None);
        Consume();

        EXPECT_TOKEN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier);
        std::string name = Consume().GetData();

        auto funcNode = std::make_shared<ASTFunctionDefinition>(name);

        if(Match(TokenType::LeftBracket))
        {
            while(!Match(TokenType::RightBracket))
            {
                Consume();

                EXPECT_TOKEN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier);
                funcNode->AddGeneric(Consume().GetData());
            }  

            Consume();
        }


        EXPECT_TOKEN(TokenType::LeftParen, DiagnosticCode_ExpectedLeftParanFunctionDefinition);

        Consume();

        size_t i = 0;

        std::vector<std::shared_ptr<ASTTypeSpecifier>> params;
        std::vector<std::shared_ptr<ASTDefaultArgument>> defaultArgs;
        std::shared_ptr<ASTTypeResolver> returnType;

        auto Flush = [&]()
        {
            for(const auto& param : params)
            {
                funcNode->Arguments.push_back(param);
            }

            funcNode->ReturnType = returnType;

            for(const auto& arg : defaultArgs)
            {
                funcNode->DefaultArguments.push_back(arg);
            }

            Root()->Children.push_back(funcNode);
			

            if(!descriptionOnly)
			{
				auto block = std::make_shared<ASTBlock>();
				funcNode->CodeBlock = block;
                m_RootStack.push_back(block);
			}

            Consume();
        };

        while (!Match(TokenType::RightParen))
        {
            if (Match(TokenType::Identifier) && Next().IsType(TokenType::Ellipses))
            {
                auto x = std::make_shared<ASTTypeSpecifier>(Consume().GetData());
                x->IsVariadic = true;

                params.push_back(x);
                Consume();
            
                EXPECT_TOKEN(TokenType::RightParen,DiagnosticCode_ExpectedEndOfFunction)
                break;
            }
           
            auto type = ParseTypeResolver();
            EXPECT_TOKEN(TokenType::Identifier,DiagnosticCode_ExpectedIdentifier);
            std::string name = Consume().GetData();
           
            if(Match(TokenType::Equals)) 
            {
                Consume();
            
                std::shared_ptr<ASTDefaultArgument> arg = std::make_shared<ASTDefaultArgument>(i);
                arg->Value = ParseExpression();
            
                defaultArgs.push_back(arg);
            }
           
            if(Match(TokenType::Ellipses))
            {
                auto x = std::make_shared<ASTTypeSpecifier>(name);
                x->IsVariadic = true;
                x->TypeResolver = type;

                params.push_back(x);

                Consume();
                EXPECT_TOKEN(TokenType::RightParen,DiagnosticCode_ExpectedEndOfFunction);

                break;
            }
           
            if(!Match(TokenType::RightParen))
            {
                EXPECT_TOKEN(TokenType::Comma,DiagnosticCode_ExpectedCommaBetweenFunctionParams)
                Consume();
            }
           
            auto x = std::make_shared<ASTTypeSpecifier>(name);
            x->TypeResolver = type;

            params.push_back(x);

            i++;
        }
       
        Consume();

        if (Match(TokenType::Colon)) 
        {
            Flush();
            return;
        }

        if(Match(TokenType::EndLine) && descriptionOnly)
        {
            Flush();
            return;
        }

        EXPECT_TOKEN(TokenType::RightThinArrow, DiagnosticCode_ExpectedFunctionReturnType);
        Consume();
        
        returnType = ParseTypeResolver();
        Flush();
    }

    void Parser::ParseEnum()
    {
        EXPECT_DATA("enum",DiagnosticCode_None);
        Consume();

        std::string enumName = Consume().GetData();

        EXPECT_TOKEN(TokenType::Colon, DiagnosticCode_ExpectedColon);
        Consume();

        EXPECT_TOKEN(TokenType::EndLine, DiagnosticCode_ExpectedNewlineAferIndentation)
        Consume();

        std::vector<std::string> names;

        std::shared_ptr<ASTEnum> enum_ = std::make_shared<ASTEnum>(enumName);

        EXPECT_TOKEN(TokenType::Identifier,DiagnosticCode_ExpectedIdentifier);
        enum_->AddEnumName(Consume().GetData());

        Token zero(TokenType::Number, "0");

        if(Match(TokenType::Comma))
        {
            enum_->EnumValues.push_back(std::make_shared<ASTNodeLiteral>(zero));
        }
        else 
        {
            Consume();
            enum_->EnumValues.push_back(ParseExpression());
        }

        if(Match(TokenType::Comma)) 
            Consume();
        
        if(Match(TokenType::EndLine))
            Consume();

        while(Match(TokenType::Identifier))
        {
            enum_->AddEnumName(Consume().GetData());

            if(Match(TokenType::Comma))
            {
                Consume();
                enum_->EnumValues.push_back(nullptr); // nullptr indicates use 1 + previous
                
                if(Match(TokenType::EndLine))
                    Consume();
                
                continue;
            }

            if(Match(TokenType::EndLine))
            {
                enum_->EnumValues.push_back(nullptr); 
                Consume();
                break;
            }

            EXPECT_TOKEN(TokenType::Equals,DiagnosticCode_ExpectedAssignment)
            Consume();

            enum_->EnumValues.push_back(ParseExpression());
            
            if(Match(TokenType::Comma))
                Consume();

            if(Match(TokenType::EndLine))
                Consume();
        }

        while(Match(TokenType::EndLine))
            Consume();

        EXPECT_TOKEN(TokenType::EndScope,DiagnosticCode_ExpectedEndOfScope)
        Consume();

        Root()->Children.push_back(enum_); 
    }

    void Parser::ParseDefer()
    {
        EXPECT_DATA("defer", DiagnosticCode_None);
        Consume();

        auto defer = std::make_shared<ASTDefer>();
		auto block = std::make_shared<ASTBlock>();

        m_RootStack.push_back(block);
        ParseGeneral();
        m_RootStack.pop_back();
		
		defer->Expr = block;
        Root()->Children.push_back(defer);
    }

    void Parser::ParseBlock()
    {
        EXPECT_TOKEN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier);
        Consume();

        EXPECT_TOKEN(TokenType::Colon, DiagnosticCode_ExpectedColon);
        Consume();

        EXPECT_TOKEN(TokenType::EndLine, DiagnosticCode_ExpectedNewlineAferIndentation);
        Consume();

        auto block = std::make_shared<ASTBlock>();

        Root()->Children.push_back(block);
        m_RootStack.push_back(block);
    }

    void Parser::ParseModule()
    {
        EXPECT_DATA("module", DiagnosticCode_None);
        Consume();

        EXPECT_TOKEN(TokenType::String, DiagnosticCode_ExpectedModuleName)
        std::string modules = Consume().GetData(); // TODO: indexing into modules using .

        auto mod = RootModule()->EmplaceOrReturn(modules);

        m_Modules.push_back(mod);
        //m_RootStack.push_back(mod->GetRoot());
    }

    void Parser::ParseEndModule()
    {
        EXPECT_DATA("endmodule",DiagnosticCode_None);
        Consume();

        m_Modules.pop_back();
        //m_RootStack.pop_back();
    }

    void Parser::ParseFunctionDeclaration(const std::string& declareKeyword)
    {
        EXPECT_DATA(declareKeyword,DiagnosticCode_None);
        Consume();

        EXPECT_TOKEN(TokenType::Identifier,DiagnosticCode_ExpectedIdentifier);
        std::string functionName = Consume().GetData();

        EXPECT_TOKEN(TokenType::LeftParen,DiagnosticCode_ExpectedLeftParanFunctionDefinition);
        Consume();

        size_t terminationIndex = GetLastBracket(TokenType::LeftParen, TokenType::RightParen);
        auto decleration = std::make_shared<ASTFunctionDeclaration>(functionName);

        // params
        while(!MatchAny(m_Terminators) && m_Position < terminationIndex)
        {
            if(Match(TokenType::Ellipses))
            {
                Consume();
                EXPECT_TOKEN(TokenType::RightParen,DiagnosticCode_ExpectedEndOfFunction)

                auto param = std::make_shared<ASTTypeSpecifier>("");
                param->IsVariadic = true;

                decleration->Arguments.push_back(param);

                break;
            }

            auto type = ParseTypeResolver();

            if(Match(TokenType::Identifier)) 
                Consume();
            
            if(!Match(TokenType::RightParen))
            {
                Expect(TokenType::Comma);
                Consume();
            }

            auto param = std::make_shared<ASTTypeSpecifier>("");
            param->TypeResolver = type;

            decleration->Arguments.push_back(param);
        }

        EXPECT_TOKEN(TokenType::RightParen,DiagnosticCode_ExpectedEndOfFunction)
        VERIFY(m_Position == terminationIndex, DiagnosticCode_None);

        Consume();

        // return type
        if(Match(TokenType::RightThinArrow))
        {
            Consume();
            decleration->ReturnType = ParseTypeResolver();
        }

        Root()->Children.push_back(decleration);
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseFunctionCall()
    {
        EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);

        std::string functionName = Consume().GetData();

        Expect(TokenType::LeftParen);
        Consume();

        size_t terminationIndex = GetLastBracket(TokenType::LeftParen, TokenType::RightParen);

        auto call = std::make_shared<ASTFunctionCall>(functionName);

        while(!MatchAny(m_Terminators) && m_Position < terminationIndex)
        {
            call->Arguments.push_back(ParseExpression(terminationIndex));

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


    std::shared_ptr<ASTNodeBase> Parser::ParseOperand()
    {
        if(MatchAny(m_Literals)) 
            return std::make_shared<ASTNodeLiteral>(Consume());

        if(Match(TokenType::Identifier))
        {
            SavePosition();

            auto ty = ParseTypeResolver();

            if(Match(TokenType::LeftBrace))
            {
                return ParseList<ASTStructExpr>(ty);
            }

            if(Match(TokenType::Dot))
            {
                return ty;
            }

            RestorePosition();

            if(Next().IsType(TokenType::LeftParen))
            {
                return ParseFunctionCall();
            }

            if(Prev().IsType(TokenType::Dot))
            {
                return std::make_shared<ASTMember>(Consume().GetData());
            }

            return std::make_shared<ASTVariable>(Consume().GetData());
        }

        if(Match(TokenType::LeftBrace))
        {
            return ParseList<ASTListExpr>();
        }


        CLEAR_UNREACHABLE("unimplemented");
        return {};
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseAssignment(std::shared_ptr<ASTNodeBase> storage, bool initialize)
    {
        ExpectAny(m_AssignmentOperators);

        Token assignmentToken = Consume();
        auto assignType = GetAssignmentOperatorFromTokenType(assignmentToken.GetType());
   
        std::shared_ptr<ASTAssignmentOperator> assign;

        if(initialize)
        {
            CLEAR_VERIFY(assignType == AssignmentOperatorType::Normal, "not a valid initializer");
            assign = std::make_shared<ASTAssignmentOperator>(AssignmentOperatorType::Initialize);
        }
        else 
        {
            assign = std::make_shared<ASTAssignmentOperator>(assignType);
        }

        assign->Storage = storage;            
        assign->Value = ParseExpression(); 

        return assign;
    }

    Parser::VariableDecleration Parser::ParseVariableDecleration()
    {
        auto type = ParseTypeResolver();

        Expect(TokenType::Identifier);
        
        auto variableDecleration = std::make_shared<ASTVariableDeclaration>(Consume().GetData());
        variableDecleration->TypeResolver = type;

        bool hasBeenInitialized = false;

        if(Match(TokenType::Equals))
        {
            Consume();
            variableDecleration->Initializer = ParseExpression();
            hasBeenInitialized = true;
        }

        return { variableDecleration, hasBeenInitialized };
    }

    void Parser::ParseSwitch() 
    { 
        EXPECT_DATA("switch", DiagnosticCode_None); 
        Consume(); 

        auto switchStatement = std::make_shared<ASTSwitch>(); 
        switchStatement->Value = ParseExpression(); 

        EXPECT_TOKEN(TokenType::Colon, DiagnosticCode_ExpectedColon);

        Consume(); 

        EXPECT_TOKEN(TokenType::EndLine, DiagnosticCode_None);

        Consume();

        bool hasDefault = false; 

        while(Match("case")) 
        { 
            Consume();

            while(Match(TokenType::EndLine)) 
            { 
                Consume(); 
            } 
            
			SwitchCase switchCase;
			
            switchCase.Values.push_back(ParseExpression()); 

            while(!Match(TokenType::Colon)) 
            { 
                EXPECT_TOKEN(TokenType::Comma, DiagnosticCode_ExpectedComma); 
                Consume(); 
                switchCase.Values.push_back(ParseExpression()); 
            } 

            Consume();

            auto block = std::make_shared<ASTBlock>(); 
            switchCase.CodeBlock = block; 

            size_t rootLevel = m_RootStack.size();

            m_RootStack.push_back(block); 

            while(rootLevel < m_RootStack.size())
            {
                ParseStatement();
            }

			switchStatement->Cases.push_back(switchCase);

            if(Match("default")) 
            { 
                hasDefault = true; 

                Consume(); 
                EXPECT_TOKEN(TokenType::Colon, DiagnosticCode_ExpectedColon); 
                Consume(); 

                block = std::make_shared<ASTBlock>(); 

                switchStatement->DefaultCaseCodeBlock = block; 

                size_t rootLevel = m_RootStack.size();

                m_RootStack.push_back(block); 

                while(rootLevel < m_RootStack.size())
                {
                    ParseStatement();
                }

                break; 
            } 
        } 

        if(!hasDefault) 
            switchStatement->DefaultCaseCodeBlock = std::make_shared<ASTBlock>(); 

        Root()->Children.push_back(switchStatement);
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseExpression(uint64_t terminationIndex) // infix to RPN and creates nodes
    {
        std::vector<std::shared_ptr<ASTNodeBase>> expression;
        std::stack<Operator> operators;

        auto PopOperatorsUntil = [&](auto condition) 
        {
            while (!operators.empty() && !condition(operators.top())) 
            {
                const auto& currentOperator = operators.top();
				
                if (currentOperator.IsBinary)
                    expression.push_back(std::make_shared<ASTBinaryExpression>(currentOperator.OperatorExpr));
                else if (currentOperator.IsUnary)
                    expression.push_back(std::make_shared<ASTUnaryExpression>(currentOperator.OperatorExpr));
                else
                    expression.push_back(std::make_shared<ASTTernaryExpression>());
                
                operators.pop();
            }
        };

        auto IsTokenOperand = [&](const Token& token)
        {
            bool isBasicType = token.IsType(TokenType::Identifier) ||
                               token.IsType(TokenType::Number) ||
                               token.IsType(TokenType::String) || 
                               token.IsType(TokenType::LeftBrace);
        
            bool isSpecialKeyword =  token.GetData() == "true" ||
                                     token.GetData() == "false" ||
                                     token.GetData() == "null";
        
            return isBasicType || isSpecialKeyword;
        };
        
        auto IsOperand = [&]()
        {
            return IsTokenOperand(Peak());
        };

        auto HandleOpenBracket = [&]() 
        {
            operators.push({ .IsOpenBracket=true });
            Consume();
        };

        auto HandleCloseBracket = [&]() 
        {
            PopOperatorsUntil([](const Operator& op) { return op.IsOpenBracket; });
            
            if (!operators.empty()) 
                operators.pop(); // remove the open bracket

            Consume();
        };

        auto GetOperatorTypeFromContext = [&]()
        {
            Token current = Peak();

            // unambiguous cases 
            switch (current.GetType()) 
            {
                case TokenType::Plus:               return OperatorType::Add;
                case TokenType::ForwardSlash:       return OperatorType::Div;
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
            
                default:
                    break;
            }

            // ambiguous cases
            bool isPrevOperandOrBracket = IsTokenOperand(Prev()) || 
                                          Prev().IsType(TokenType::RightParen) || 
                                          Prev().IsType(TokenType::RightBracket);

            switch (current.GetType())
            {
                case TokenType::Star:
                {
                    if (isPrevOperandOrBracket) 
                        return OperatorType::Mul;
                
                    return OperatorType::Dereference;
                }
            
                case TokenType::Ampersand:
                {
                    if (isPrevOperandOrBracket) 
                        return OperatorType::BitwiseAnd;
                
                    return OperatorType::Address;
                }
            
                case TokenType::Minus:
                {
                    if (isPrevOperandOrBracket)
                        return OperatorType::Sub;
                
                    return OperatorType::Negation;
                }
            
                case TokenType::Decrement:
                {
                    bool prev = IsTokenOperand(Prev());
                    bool next = IsTokenOperand(Next());
                
                    if (prev || next)
                    {
                        CLEAR_VERIFY(!(prev && next), "ambiguous decrement");

                        if(next) 
                            return OperatorType::Decrement;

                        return OperatorType::PostDecrement;
                    }


                    CLEAR_UNREACHABLE("invalid decrement");
                    return OperatorType::None;
                }

                case TokenType::Increment:
                {
                    bool prev = IsTokenOperand(Prev());
                    bool next = IsTokenOperand(Next());
                
                    if (prev || next)
                    {
                        CLEAR_VERIFY(!(prev && next), "ambiguous increment");

                        if(next) 
                            return OperatorType::Increment;

                        return OperatorType::PostIncrement;
                    }
                    
                    CLEAR_UNREACHABLE("invalid increment");
                    return OperatorType::None;
                }

                default: 
                {
                    break;
                }
            }

            return OperatorType::None;
        };

        auto HandleBinaryOperator = [&](OperatorType operatorType) 
        {
            if(operatorType == OperatorType::None)
            {
                Consume();
                return;
            }

            int precedence = g_Precedence.at(operatorType);

            PopOperatorsUntil([&](const Operator& op) 
            {
                if(op.IsRightAssociative())
                    return op.IsOpenBracket || precedence >= op.Precedence;
                
                return op.IsOpenBracket || precedence > op.Precedence;
            });


            operators.push({
                .OperatorExpr = operatorType,
                .IsBinary = true,
                .Precedence = precedence
            });
        };

        auto HandlePreUnaryOperators = [&]() 
        {
            OperatorType operatorType = GetOperatorTypeFromContext();

            while(g_PreUnaryOperators.test((size_t)operatorType))
            {
                Consume();

                int precedence = g_Precedence.at(operatorType);

                PopOperatorsUntil([&](const Operator& op) 
                {
                    if(op.IsRightAssociative())
                        return op.IsOpenBracket || precedence >= op.Precedence;
                    
                    return op.IsOpenBracket || precedence > op.Precedence;
                });

                operators.push({
                    .OperatorExpr = operatorType, 
                    .IsUnary = true,
                    .Precedence = precedence
                });

                operatorType = GetOperatorTypeFromContext();
            }
        };

        auto HandlePostUnaryOperators = [&]() 
        {
            OperatorType operatorType = GetOperatorTypeFromContext();

            while(g_PostUnaryOperators.test((size_t)operatorType))
            {
                Consume();

                int precedence = g_Precedence.at(operatorType);

                PopOperatorsUntil([&](const Operator& op) 
                { 
                     if(op.IsRightAssociative())
                        return op.IsOpenBracket || precedence >= op.Precedence;

                    return op.IsOpenBracket || precedence > op.Precedence;
                });

                operators.push({
                    .OperatorExpr = operatorType, 
                    .IsUnary = true,
                    .Precedence = precedence
                });

                operatorType = GetOperatorTypeFromContext();
            }
        };

        auto DebugPrintExpression = [&]()
        {
            for(const auto& expr : expression)
            {
                expr->Print();
            }  
            
            std::println();
        };


        while (!MatchAny(m_Terminators) && m_Position < terminationIndex) 
        {
            HandlePreUnaryOperators();

            OperatorType operatorType = GetOperatorTypeFromContext();

            if (IsOperand()) 
            {
                expression.push_back(ParseOperand());
            }
            else if (Match(TokenType::LeftParen)) 
            {
                HandleOpenBracket();
            }
            else if (Match(TokenType::RightParen) || Match(TokenType::RightBracket)) 
            {
                HandleCloseBracket();
            }
            else if (operatorType != OperatorType::None)
            {
                HandleBinaryOperator(operatorType);

                if(operatorType == OperatorType::Index) 
                {
                    operators.push({ .IsOpenBracket=true });
                }

                Consume();
            }
            else if (Match("?")) 
            {
                PopOperatorsUntil([](const Operator& op) { return op.OperatorExpr == OperatorType::Ternary || op.IsOpenBracket; });

                operators.push({ .IsBeginTernary = true });
                Consume();
            }
            else if (Match(":"))
            {
                PopOperatorsUntil([](const Operator& op)
                {
                    return op.IsBeginTernary;
                });

                if (!operators.empty()) 
                    operators.pop(); // remove the start ternary

                Consume();

                operators.push({
                    .OperatorExpr = OperatorType::Ternary, 
                    .Precedence = g_Precedence.at(OperatorType::Ternary)
                });
            }
            
            HandlePostUnaryOperators();
        }

        PopOperatorsUntil([](const Operator&) { return false; });

        //DebugPrintExpression();

        return ASTExpression::AssembleFromRPN(expression);
    }

    std::shared_ptr<ASTTypeResolver> Parser::ParseTypeResolver()
    {
        std::shared_ptr<ASTTypeResolver> resolver = std::make_shared<ASTTypeResolver>();

        if(Match("let"))
        {
            resolver->PushToken(Consume());
            EXPECT_TOKEN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier);

            return resolver;
        }

        if(Match("const"))
        {
            resolver->PushToken(Consume());

            if(Peak().IsType(TokenType::Identifier) && Next().IsType(TokenType::Equals))
            {
                return resolver;
            }
        }

        while(Match("const") || Match(TokenType::Star) || Match(TokenType::LeftBracket))
        {
            resolver->PushToken(Consume());

            if(Prev().IsType(TokenType::LeftBracket))
            {                
                resolver->Children.push_back(ParseExpression());

                EXPECT_TOKEN_RETURN(TokenType::Semicolon, DiagnosticCode_None, resolver);
                Consume();

                resolver->Children.push_back(ParseTypeResolver());
                EXPECT_TOKEN_RETURN(TokenType::RightBracket, DiagnosticCode_None, resolver);
               
                resolver->PushToken(Consume()); // can't be anything after ]
                return resolver;
            }
        }

        if(!(Match(TokenType::Keyword) || Match(TokenType::Identifier)))
            return resolver;

        resolver->PushToken(Consume());

        while(Match(TokenType::Dot))
        {
            resolver->PushToken(Consume());

            EXPECT_TOKEN_RETURN(TokenType::Identifier, DiagnosticCode_ExpectedIdentifier, nullptr);

            resolver->PushToken(Consume());
        }

        if(Match(TokenType::LeftBracket))
        {
            Consume();
             // hack to allow the ASTTypeResolver to differentiate between an array and a template
            resolver->PushToken(Token(TokenType::LessThan, "<"));

            size_t prev = m_Position;

            while(!Match(TokenType::RightBracket))
            {
                resolver->Children.push_back(ParseTypeResolver());

                if(Match(TokenType::Comma))
                {
                    Consume();
                }

                if(m_Position == prev)
                    return resolver;

                prev = m_Position;
            }

            Consume();
            resolver->PushToken(Token(TokenType::GreaterThan, ">"));
        }

        return resolver;
    }


    void Parser::ParseIndentation()
    {
        m_RootStack.pop_back();
        Consume();
    }

    void Parser::ParseClass()
    {
        EXPECT_DATA("class", DiagnosticCode_None);
        Consume();

        EXPECT_TOKEN(TokenType::Identifier,  DiagnosticCode_ExpectedIdentifier);
        std::string className = Consume().GetData();

		std::shared_ptr<ASTStruct> structNode = std::make_shared<ASTStruct>(className);
        std::shared_ptr<ASTClass> classNode = std::make_shared<ASTClass>(className);
		classNode->Struct = structNode;

        Root()->Children.push_back(classNode);

        if(Match(TokenType::LeftBracket))
        {
            while(!Match(TokenType::RightBracket))
            {   
                Consume();

                std::string genericName = Consume().GetData();
                classNode->AddGeneric(genericName);
            }

            Consume();
        }

        EXPECT_TOKEN(TokenType::Colon, DiagnosticCode_ExpectedColon);
        Consume();

        std::vector<std::shared_ptr<ASTTypeSpecifier>> types;
        std::vector<std::shared_ptr<ASTNodeBase>> defaultValues;

        auto Flush = [&]()
        {
            for(const auto& defaultValue : defaultValues)
            {
                structNode->DefaultValues.push_back(defaultValue);
            }

            for(const auto& type : types)
            {
                structNode->Members.push_back(type);
            }

            Consume(); 
        };

        while(!Match(TokenType::EndScope))
        {
            while(Match(TokenType::EndLine))
                Consume();

            if(Match("function"))
            {
                size_t rootLevel = m_RootStack.size();
				
				m_RootStack.push_back(std::make_shared<ASTBlock>());

                ParseFunctionDefinition();

                // continue parsing as normal until end of function definition
                while(rootLevel < m_RootStack.size())
                {
                    ParseStatement();
                }

				classNode->MemberFunctions.push_back(m_RootStack.back()->Children[0]);
				m_RootStack.pop_back();

                continue;
            }

            auto type = ParseTypeResolver();

            EXPECT_TOKEN(TokenType::Identifier,DiagnosticCode_ExpectedIdentifier);

            auto typeSpec = std::make_shared<ASTTypeSpecifier>(Consume().GetData());
            typeSpec->TypeResolver = type;

            if(Match(TokenType::Equals))
            {
                Consume();
                defaultValues.push_back(ParseExpression());
            }
            else 
            {
                defaultValues.push_back(nullptr);
            }

            types.push_back(typeSpec);

            if(Match(TokenType::Comma))
                Consume();
            
            EXPECT_TOKEN(TokenType::EndLine,DiagnosticCode_ExpectedNewlineAferIndentation);

            Consume();
        }

        Flush();
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

    void Parser::SavePosition()
    {
        m_RestorePoints.push_back(m_Position);
    }

    void Parser::RestorePosition()
    {
        CLEAR_VERIFY(!m_RestorePoints.empty(), " cannot restore position when there aren't any saved!");
        m_Position = m_RestorePoints.back();
        m_RestorePoints.pop_back();
    }

    void Parser::SkipUntil(TokenType type)
    {
        while(!Match(type) && !Match(TokenType::EndOfFile))
        {
            Consume();
        }
    }

    size_t Parser::FindLastOf(TokenType type)
    {
        size_t current = m_Position;

        SkipUntil(TokenType::EndLine);

        while((size_t)m_Position-- > current)
        {
            if(Match(type))
            {
                size_t result = m_Position;
                m_Position = current;
                return result;
            }
        }

        CLEAR_UNREACHABLE("couldn't find expected token");
        
        m_Position = current;
        return current; 
    }

    size_t Parser::GetLastBracket(TokenType openBracket, TokenType closeBracket)
    {
        size_t terminationIndex = 0;

        SavePosition();

        int64_t bracketCount = 1;

        while(bracketCount)
        {
            if(Match(openBracket))  bracketCount++;
            if(Match(closeBracket)) bracketCount--;

            m_Position++;

            VERIFY_WITH_RETURN(m_Position < m_Tokens.size() && bracketCount >= 0, DiagnosticCode_UnmatchedBracket, m_Position);
        }

        terminationIndex = m_Position - 1;

        RestorePosition();

        return terminationIndex;
    }
    
    bool Parser::IsDeclaration()
    {
        if (Match(TokenType::EndLine))
            return false;

        SavePosition();

        ParseTypeResolver();

        bool isDecl = Match(TokenType::Identifier); 

        RestorePosition(); 

        return isDecl;
    }

}
