#include "Parser.h"
#include "AST/ASTNode.h"
#include "Core/Log.h"
#include "Core/TypeRegistry.h"
#include "Core/Utils.h"
#include "Lexing/Token.h"

#include <stack>
#include <print>


namespace clear 
{
    Parser::Parser(const std::vector<Token>& tokens)
        : m_Tokens(tokens)
    {
        std::shared_ptr<ASTNodeBase> root = std::make_shared<ASTNodeBase>(); 
        root->CreateSymbolTable();

        m_RootStack.push_back(root);

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
            TokenType::Colon,
            TokenType::RightBrace,
            TokenType::EndOfFile
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

    std::shared_ptr<ASTNodeBase> Parser::Root()
    {
        return m_RootStack.back();
    }

    Token Parser::Consume()
    {
        CLEAR_VERIFY(m_Position + 1 < m_Tokens.size(), "what are you doing?");
        return m_Tokens[m_Position++];
    }

    Token Parser::Peak()
    {
        CLEAR_VERIFY(m_Position < m_Tokens.size(), "what are you doing?");
        return m_Tokens[m_Position];
    }

    Token Parser::Next()
    {
        CLEAR_VERIFY(m_Position + 1 < m_Tokens.size(), "what are you doing?");
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
        if(Match(TokenType::EndLine))
        {
            Consume();
            return;
        }

        if(Match("pass"))
        {
            Consume();
            return;
        }

        static std::map<std::string, std::function<void()>> s_MappedKeywordsToFunctions = {
            {"function",  [this]() { ParseFunctionDefinition(); }},
            {"while",     [this]() { ParseWhile(); }},
            {"for",       [this]() { ParseFor(); }},
            {"let",       [this]() { ParseLetDecleration(); }},
            {"declare",   [this]() { ParseFunctionDeclaration(); }}, 
            {"const",     [this]() { ParseConstDecleration(); }}, 
            {"struct",    [this]() { ParseStruct(); }}, 
            {"return",    [this]() { ParseReturn(); }}, 
            {"if",        [this]() { ParseIf(); }},
            {"else",      [this]() { ParseElse(); }},
            {"elseif",    [this]() { ParseElseIf(); }},
            {"defer",     [this]() { ParseDefer(); }},
            {"class",     [this]() { ParseClass(); }},
            {"enum",      [this]() { ParseEnum(); }},
            {"trait",     [this]() { ParseTrait(); }},
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
        // first we need to determine how to parse
        auto terminator = [](const Token& token)
        {
            return token.IsType(TokenType::EndOfFile)  || 
                   token.IsType(TokenType::EndLine)    || 
                   token.IsType(TokenType::Equals)     ||
                   token.IsType(TokenType::PlusEquals) ||
                   token.IsType(TokenType::LeftParen)  || 
                   token.IsType(TokenType::Comma)      || 
                   token.IsType(TokenType::Dot);
        };


        // int var
        static constexpr std::array<TokenType, s_MaxMatchSize> s_KeywordIdentifier = 
                {TokenType::Keyword, TokenType::Identifier, TokenType::None};
        
        // Foo var
        static constexpr std::array<TokenType, s_MaxMatchSize> s_IdentifierIdentifier = 
                {TokenType::Identifier, TokenType::Identifier, TokenType::None};

        bool isDecleration = LookAheadMatches(terminator, s_KeywordIdentifier) || LookAheadMatches(terminator, s_IdentifierIdentifier);

        // parse either expression or declerations, allowing for multiple on the same line seperated by commas

        if(!isDecleration)
        {
            while(!(Match(TokenType::EndLine) || Match(TokenType::EndScope) || Match(TokenType::EndOfFile)))
            {
                auto expr = ParseExpression();

                if(MatchAny(m_AssignmentOperators))
                {
                    Root()->Push(ParseAssignment(expr));
                    return;
                }
                else 
                {
                    Root()->Push(expr);
                }

                if(Match(TokenType::Comma))
                    Consume();
            }
        }

        while(isDecleration)
        {
            auto decleration = ParseVariableDecleration();

            if(MatchAny(m_AssignmentOperators))
            {
                Root()->Push(ParseAssignment(decleration, true));
            }
            else 
            {
                auto initializer = std::make_shared<ASTDefaultInitializer>();
                initializer->Push(decleration);

                Root()->Push(initializer);
            }

            if(Match(TokenType::Comma))
                Consume();

            isDecleration = LookAheadMatches(terminator, s_KeywordIdentifier) || LookAheadMatches(terminator, s_IdentifierIdentifier);
        }
    }

    void Parser::ParseLoopControls() 
    {
        CLEAR_UNREACHABLE("unimplemented");
        /* auto node = std::make_shared<ASTLoopControlFlow>(Peak().TokenType);
        Consume();
        Root()->Push(node);  */
    }

    void Parser::ParseTrait()
    { 
        Expect("trait");
        Consume();

        std::string traitName = Consume().GetData();

        Expect(TokenType::Colon);
        Consume();

        Expect(TokenType::EndLine);
        Consume();
    
        std::shared_ptr<ASTTrait> trait = std::make_shared<ASTTrait>(traitName);

        m_RootStack.push_back(trait);

        while (Match("function") || Match(TokenType::Keyword) || Match(TokenType::Identifier))
        {
            if(Match("function")) // parse function decleration
            {
                ParseFunctionDeclaration("function");
            }
            else 
            {
                ParseVariableDecleration();
            }

            while(Match(TokenType::EndLine))
                Consume();
        }

        m_RootStack.pop_back();
        Root()->Push(trait);

        if(Match(TokenType::EndScope))
            Consume(); 
    }

    void Parser::ParseLetDecleration()
    {
        Expect("let");
        Consume();

        Expect(TokenType::Identifier);

        while(Match(TokenType::Identifier))
        {
            std::string variableName = Consume().GetData();

            Expect(TokenType::Equals);
            Consume();

            auto inferredType = std::make_shared<ASTInferredDecleration>(variableName);
            inferredType->Push(ParseExpression());

            Root()->Push(inferredType);

            if(Match(TokenType::Comma))
            {
                Consume();
                Expect(TokenType::Identifier);
            }
        }
     }

    void Parser::ParseConstDecleration()
    {       
        Expect("const");
        Consume();

        if(!Match(TokenType::Identifier))
        {
            Undo();
            Root()->Push(ParseVariableDecleration());

            return;
        }

        while(Match(TokenType::Identifier))
        {
            std::string variableName = Consume().GetData();
        
            Expect(TokenType::Equals);
            Consume();
        
            auto inferredType = std::make_shared<ASTInferredDecleration>(variableName, true);
            inferredType->Push(ParseExpression());
        
            Root()->Push(inferredType);
        
            if(Match(TokenType::Comma))
            {
                Consume();
                Expect(TokenType::Identifier);
            }
        }
    }

    void Parser::ParseStruct()
    {
        Expect("struct");
        Consume();

        Expect(TokenType::Identifier);
        std::string structName = Consume().GetData();

        Expect(TokenType::Colon);
        Consume();

        Expect(TokenType::EndLine);
        Consume();

        auto struct_ = std::make_shared<ASTStruct>(structName);

        std::vector<std::shared_ptr<ASTTypeSpecifier>> typeSpecifiers;
        std::vector<std::shared_ptr<ASTNodeBase>> defaultArgs;

        auto Flush = [&]()
        {
            for(const auto& type : typeSpecifiers)
            {
                struct_->Push(type);
            }

            for(const auto& defaultArg : defaultArgs)
            {
                struct_->Push(defaultArg);
            }

            Root()->Push(struct_);
            Consume();
        };

        auto ConstructType = [&](const std::shared_ptr<ASTNodeBase>& type)
        {   
            Expect(TokenType::Identifier);
            std::string memberName = Consume().GetData();

            auto member = std::make_shared<ASTTypeSpecifier>(memberName);
            member->Push(type);

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
        Root()->Push(import); */
    }

    void Parser::ParseReturn()
    {
        Expect("return");
        Consume();

        std::shared_ptr<ASTReturn> returnStatement = std::make_shared<ASTReturn>();
        returnStatement->Push(ParseExpression());

        Root()->Push(returnStatement); 
    }

    void Parser::ParseIf()
    {
        Expect("if");
        Consume();

        std::shared_ptr<ASTIfExpression> ifExpr = std::make_shared<ASTIfExpression>();
        ifExpr->Push(ParseExpression());

        std::shared_ptr<ASTNodeBase> base = std::make_shared<ASTNodeBase>();
        base->CreateSymbolTable();
        ifExpr->Push(base);

        Root()->Push(ifExpr);
        m_RootStack.push_back(base); 

        Expect(TokenType::Colon);
        Consume();
    }

    void Parser::ParseElse()
    {
        Expect("else");
        Consume();

        auto& last = Root()->GetChildren().back();
        std::shared_ptr<ASTIfExpression> ifExpr = std::dynamic_pointer_cast<ASTIfExpression>(last);
        CLEAR_VERIFY(ifExpr, "invalid node");
        
        std::shared_ptr<ASTNodeBase> base = std::make_shared<ASTNodeBase>();
        base->CreateSymbolTable();
        ifExpr->Push(base);
            
        m_RootStack.push_back(base); 

        Expect(TokenType::Colon);
        Consume();
    }

    void Parser::ParseWhile()
    {
        Expect("while");
        Consume();

        std::shared_ptr<ASTWhileExpression> whileExp = std::make_shared<ASTWhileExpression>();
        whileExp->Push(ParseExpression());

        std::shared_ptr<ASTNodeBase> base = std::make_shared<ASTNodeBase>();
        whileExp->Push(base);

        Root()->Push(whileExp);
        m_RootStack.push_back(base);

        Expect(TokenType::Colon);
        Consume();
    }

    void Parser::ParseFor()
    {
        Expect("for");
        Consume();

        Expect(TokenType::Identifier);
        std::string name = Consume().GetData();

        Expect("in");
        Consume();

        // TODO: add more comprehensive parseIter function here. for now only variadic arguments are supported

        Expect(TokenType::Identifier);
        auto var = std::make_shared<ASTVariable>(Consume().GetData());

        auto forLoop = std::make_shared<ASTForExpression>(name);
        forLoop->Push(var);

        Root()->Push(forLoop);

        auto body = std::make_shared<ASTNodeBase>();
        body->CreateSymbolTable();
        
        forLoop->Push(body);

        m_RootStack.push_back(body);

        Expect(TokenType::Colon);
        Consume();
    }

    void Parser::ParseElseIf()
    {
        Expect("elseif");
        Consume();

        auto& last = Root()->GetChildren().back();
        std::shared_ptr<ASTIfExpression> ifExpr = std::dynamic_pointer_cast<ASTIfExpression>(last);
        CLEAR_VERIFY(ifExpr, "invalid node");

        ifExpr->Push(ParseExpression());

        std::shared_ptr<ASTNodeBase> base = std::make_shared<ASTNodeBase>();
        base->CreateSymbolTable();

        ifExpr->Push(base);

        m_RootStack.push_back(base); 

        Expect(TokenType::Colon);
        Consume();
    }

    void Parser::ParseFunctionDefinition(const std::string& className,  bool descriptionOnly)
    {
        Expect("function");
        Consume();

        Expect(TokenType::Identifier);
        std::string name = Consume().GetData();

        if(!className.empty())
        {
            name = className + "." + name;
        }

        auto funcNode = std::make_shared<ASTFunctionDefinition>(name);

        Expect(TokenType::LeftParen);

        Consume();

        size_t i = 0;

        std::vector<std::shared_ptr<ASTTypeSpecifier>> params;
        std::vector<std::shared_ptr<ASTDefaultArgument>> defaultArgs;
        std::shared_ptr<ASTNodeBase> returnType;


        if(!className.empty())
        {
            Token classNameToken(TokenType::Identifier, className);
            Token pointerToken(TokenType::Star, "*");

            auto classPointer = std::make_shared<ASTTypeSpecifier>("this");
            auto typeResolver = std::make_shared<ASTTypeResolver>();

            // className*
            typeResolver->PushToken(classNameToken);
            typeResolver->PushToken(pointerToken);

            classPointer->Push(typeResolver);

            params.push_back(classPointer);
        }

        auto Flush = [&]()
        {
            for(const auto& param : params)
            {
                funcNode->Push(param);
            }

            funcNode->Push(returnType);

            for(const auto& arg : defaultArgs)
            {
                funcNode->Push(arg);
            }

            Root()->Push(funcNode);

            if(!descriptionOnly)
                m_RootStack.push_back(funcNode);

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
            
                Expect(TokenType::RightParen);
            
                break;
            }
           
            auto type = ParseTypeResolver();
            Expect(TokenType::Identifier);
            std::string name = Consume().GetData();
           
            if(Match(TokenType::Equals)) 
            {
                Consume();
            
                std::shared_ptr<ASTDefaultArgument> arg = std::make_shared<ASTDefaultArgument>(i);
                arg->Push(type);
                arg->Push(ParseExpression());
            
                defaultArgs.push_back(arg);
            }
           
            if(Match(TokenType::Ellipses))
            {
                auto x = std::make_shared<ASTTypeSpecifier>(name);
                x->IsVariadic = true;
                x->Push(type);

                params.push_back(x);

                Consume();
                Expect(TokenType::RightParen);

                break;
            }
           
            if(!Match(TokenType::RightParen))
            {
                Expect(TokenType::Comma);
                Consume();
            }
           
            auto x = std::make_shared<ASTTypeSpecifier>(name);
            x->Push(type);

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

        Expect(TokenType::RightThinArrow);
        Consume();
        
        returnType = ParseTypeResolver();

        Flush();
    }

    void Parser::ParseEnum()
    {
        Expect("enum");
        Consume();

        std::string enumName = Consume().GetData();

        Expect(TokenType::Colon);
        Consume();

        Expect(TokenType::EndLine);
        Consume();

        std::vector<std::string> names;

        std::shared_ptr<ASTEnum> enum_ = std::make_shared<ASTEnum>(enumName);

        Expect(TokenType::Identifier);
        enum_->AddEnumName(Consume().GetData());

        Token zero(TokenType::Number, "0");

        if(Match(TokenType::Comma))
        {
            enum_->Push(std::make_shared<ASTNodeLiteral>(zero));
        }
        else 
        {
            Consume();
            enum_->Push(ParseExpression());
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
                enum_->Push(nullptr); // nullptr indicates use 1 + previous
                
                if(Match(TokenType::EndLine))
                    Consume();
                
                continue;
            }

            if(Match(TokenType::EndLine))
            {
                enum_->Push(nullptr); 
                Consume();
                break;
            }

            Expect(TokenType::Equals);
            Consume();

            enum_->Push(ParseExpression());
            
            if(Match(TokenType::Comma))
                Consume();

            if(Match(TokenType::EndLine))
                Consume();
        }

        while(Match(TokenType::EndLine))
            Consume();

        Expect(TokenType::EndScope);
        Consume();

        Root()->Push(enum_); 
    }

    void Parser::ParseDefer()
    {
        Expect("defer");
        Consume();

        auto defer = std::make_shared<ASTDefer>();

        m_RootStack.push_back(defer);
        ParseGeneral();
        m_RootStack.pop_back();

        Root()->Push(defer);
    }

    void Parser::ParseFunctionDeclaration(const std::string& declareKeyword)
    {
        Expect(declareKeyword);
        Consume();

        Expect(TokenType::Identifier);
        std::string functionName = Consume().GetData();

        Expect(TokenType::LeftParen);
        Consume();

        size_t terminationIndex = GetLastBracket(TokenType::LeftParen, TokenType::RightParen);
        auto decleration = std::make_shared<ASTFunctionDecleration>(functionName);

        // params
        while(!MatchAny(m_Terminators) && m_Position < terminationIndex)
        {
            if(Match(TokenType::Ellipses))
            {
                Consume();
                Expect(TokenType::RightParen);

                auto param = std::make_shared<ASTTypeSpecifier>("");
                param->IsVariadic = true;

                decleration->Push(param);

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
            param->Push(type);

            decleration->Push(param);
        }

        Expect(TokenType::RightParen);
        CLEAR_VERIFY(m_Position == terminationIndex, "invalid function decleration");

        Consume();

        // return type
        if(Match(TokenType::RightThinArrow))
        {
            Consume();
            decleration->Push(ParseTypeResolver());
        }

        Root()->Push(decleration);
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseFunctionCall()
    {
        Expect(TokenType::Identifier);

        std::string functionName = Consume().GetData();

        Expect(TokenType::LeftParen);
        Consume();

        size_t terminationIndex = GetLastBracket(TokenType::LeftParen, TokenType::RightParen);

        auto call = std::make_shared<ASTFunctionCall>(functionName);

        while(!MatchAny(m_Terminators) && m_Position < terminationIndex)
        {

            call->Push(ParseExpression(terminationIndex));

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


        CLEAR_UNREACHABLE("unimplemented");
        return {};
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseInitializer(std::shared_ptr<ASTNodeBase> storageOrType, bool initialize)
    {
        Expect(TokenType::LeftBrace);

        std::vector<std::vector<size_t>> indices;
        std::vector<size_t> currentIndex = { 0 };

        std::shared_ptr<ASTInitializerList> initializer = std::make_shared<ASTInitializerList>(initialize);
        initializer->Push(storageOrType);

        while(!Match(TokenType::EndLine))
        {
            if(Match(TokenType::LeftBrace))
            {
                currentIndex.push_back(0);
                Consume();
            }
            else if (Match(TokenType::RightBrace))
            {
                currentIndex.pop_back();
                currentIndex.back()++;

                Consume();
            }
            else if (Match(TokenType::Comma))
            {
                Consume();
            }
            else 
            {
                initializer->Push(ParseExpression());
                indices.push_back(currentIndex);

                currentIndex.back()++;
            }
        }

        initializer->SetIndices(indices);
        Consume();

        return initializer;

    }

    std::shared_ptr<ASTNodeBase> Parser::ParseAssignment(std::shared_ptr<ASTNodeBase> storage, bool initialize)
    {
        ExpectAny(m_AssignmentOperators);

        Token assignmentToken = Consume();
        auto assignType = GetAssignmentOperatorFromTokenType(assignmentToken.GetType());

        if(Match(TokenType::LeftBrace))
        {
            CLEAR_VERIFY(assignType == AssignmentOperatorType::Normal, "not a valid assignment");
            return ParseInitializer(storage, initialize);
        }
        
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

        assign->Push(storage);            
        assign->Push(ParseExpression()); 

        return assign;
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseVariableDecleration()
    {
        auto type = ParseTypeResolver();

        Expect(TokenType::Identifier);
        
        auto variableDecleration = std::make_shared<ASTVariableDeclaration>(Consume().GetData());
        variableDecleration->Push(type);

        return variableDecleration;
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseExpression(uint64_t terminationIndex) // infix to RPN and creates nodes
    {
        std::shared_ptr<ASTExpression> expression = std::make_shared<ASTExpression>();
        std::stack<Operator> operators;

        auto PopOperatorsUntil = [&](auto condition) 
        {
            while (!operators.empty() && !condition(operators.top())) 
            {
                const auto& currentOperator = operators.top();

                if (currentOperator.IsBinary)
                    expression->Push(std::make_shared<ASTBinaryExpression>(currentOperator.OperatorExpr));
                else 
                    expression->Push(std::make_shared<ASTUnaryExpression>(currentOperator.OperatorExpr));
                
                operators.pop();
            }
        };

        auto IsTokenOperand = [&](const Token& token)
        {
            bool isBasicType = token.IsType(TokenType::Identifier) ||
                               token.IsType(TokenType::Number) ||
                               token.IsType(TokenType::String);
        
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

        auto HandleOperator = [&](OperatorType operatorType) 
        {
            if(operatorType == OperatorType::None)
            {
                Consume();
                return;
            }

            int precedence = g_Precedence.at(operatorType);

            PopOperatorsUntil([&](const Operator& op) 
            {
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
                    return op.IsOpenBracket || precedence >= op.Precedence;
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
                    if(op.OperatorExpr == OperatorType::Power)
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
            for(const auto& expr : expression->GetChildren())
            {
                if(auto e = std::dynamic_pointer_cast<ASTBinaryExpression>(expr))
                    std::print("binary_op ");
                else if(auto e = std::dynamic_pointer_cast<ASTUnaryExpression>(expr))
                    std::print("unary_op ");
                else 
                    std::print("operand ");
            }  
            
            std::println();
        };


        while (!MatchAny(m_Terminators) && m_Position < terminationIndex) 
        {
            HandlePreUnaryOperators();

            OperatorType operatorType = GetOperatorTypeFromContext();

            if (IsOperand()) 
            {
                expression->Push(ParseOperand());
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
                HandleOperator(operatorType);

                if(operatorType == OperatorType::Index) 
                {
                    operators.push({ .IsOpenBracket=true });
                }

                Consume();
            }
            
            HandlePostUnaryOperators();
        }

        PopOperatorsUntil([](const Operator&) { return false; });

        //DebugPrintExpression();

        return expression;
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseTypeResolver()
    {
        std::shared_ptr<ASTTypeResolver> resolver = std::make_shared<ASTTypeResolver>();

        if(Match("const"))
            resolver->PushToken(Consume());
        
        resolver->PushToken(Consume());

        while(Match("const") || Match(TokenType::Star) || Match(TokenType::LeftBracket))
        {
            resolver->PushToken(Consume());

            if(Prev().IsType(TokenType::LeftBracket))
            {
                size_t terminationIndex = GetLastBracket(TokenType::LeftBracket, TokenType::RightBracket);
                
                resolver->Push(ParseExpression(terminationIndex));
                resolver->PushToken(Consume());
            }
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
        Expect("class");
        Consume();

        Expect(TokenType::Identifier);
        std::string className = Consume().GetData();

        Expect(TokenType::Colon);
        Consume();

        std::shared_ptr<ASTClass> classNode = std::make_shared<ASTClass>(className);
        Root()->Push(classNode);

        m_RootStack.push_back(classNode);

        std::vector<std::shared_ptr<ASTTypeSpecifier>> types;
        std::vector<std::shared_ptr<ASTNodeBase>> defaultValues;

        auto Flush = [&]()
        {
            for(const auto& defaultValue : defaultValues)
            {
                classNode->Push(defaultValue);
            }

            for(const auto& type : types)
            {
                classNode->Push(type);
            }

            m_RootStack.pop_back();
            Consume(); 
        };

        while(!Match(TokenType::EndScope))
        {
            if(Match("function"))
            {
                size_t rootLevel = m_RootStack.size();

                ParseFunctionDefinition(className);

                // continue parsing as normal until end of function definition
                while(rootLevel < m_RootStack.size())
                {
                    ParseStatement();
                }

                continue;
            }

            if(Match(TokenType::Keyword) || Match(TokenType::Identifier))
            {
                auto type = ParseTypeResolver();

                Expect(TokenType::Identifier);

                auto typeSpec = std::make_shared<ASTTypeSpecifier>(Consume().GetData());
                typeSpec->Push(type);

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

                Expect(TokenType::EndLine);
                Consume();
                continue;
            }           

            CLEAR_LOG_WARNING("ignoring token ", Consume().GetData(), " in class ", className);
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

        size_t bracketCount = 1;

        while(bracketCount)
        {
            if(Match(openBracket))  bracketCount++;
            if(Match(closeBracket)) bracketCount--;

            m_Position++;

            CLEAR_VERIFY(m_Position < m_Tokens.size(), "mismatched brackets");
        }

        terminationIndex = m_Position - 1;

        RestorePosition();

        return terminationIndex;
    }

    bool Parser::LookAheadMatches(const std::function<bool(const Token&)>& terminator, const std::array<TokenType, s_MaxMatchSize>& match)
    {
        size_t pos = m_Position; 
        size_t k = 0;

        while (pos < m_Tokens.size() && !terminator(m_Tokens[pos]) && k < s_MaxMatchSize && match[k] != TokenType::None)
        {
            if (m_Tokens[pos].IsType(match[k]))
                k++; 

            pos++;
        }

        // if we matched all tokens in match (until None), return true
        return k == s_MaxMatchSize || match[k] == TokenType::None;
    }

}
