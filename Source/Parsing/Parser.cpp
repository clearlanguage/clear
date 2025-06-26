#include "Parser.h"
#include "AST/ASTNode.h"
#include "Core/Log.h"
#include "Core/TypeRegistry.h"
#include "Core/Utils.h"
#include "Lexing/Token.h"

#include <stack>


namespace clear 
{
    Parser::Parser(const std::vector<Token>& tokens)
        : m_Tokens(tokens)
    {
        std::shared_ptr<ASTNodeBase> root = std::make_shared<ASTNodeBase>(); //TODO: change back to this once function definitions are implemented
        std::shared_ptr<ASTFunctionDefinition> main = std::make_shared<ASTFunctionDefinition>("main");

        root->CreateSymbolTable();
        root->Push(main);

        m_RootStack.push_back(root);
        m_RootStack.push_back(main);

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

        /* m_VariableType = CreateTokenSet({
            TokenType::Int8Type, 
            TokenType::Int16Type, 
            TokenType::Int32Type, 
            TokenType::Int64Type, 
            TokenType::UInt8Type,
            TokenType::UInt16Type,
            TokenType::UInt32Type,
            TokenType::UInt64Type,
            TokenType::Float32Type,
            TokenType::Float64Type,
            TokenType::Bool,
            TokenType::StructName,
            TokenType::StringType, 
            TokenType::CharType, 
            TokenType::TypeIdentifier, 
            TokenType::Const
        });

        m_AssignmentOperators = CreateTokenSet({
            TokenType::Assignment, 
            TokenType::MultiplyAssign,
            TokenType::DivideAssign, 
            TokenType::PlusAssign,
            TokenType::MinusAssign,
            TokenType::ModuloAssign
        });

        m_PreUnaryExpression = CreateTokenSet({
            TokenType::Increment,    
            TokenType::Decrement,    
            TokenType::BitwiseNot,   
            TokenType::AddressOp,	  
            TokenType::DereferenceOp,
            TokenType::Negation, 
            TokenType::Not,
            TokenType::Ellipsis
        });

        m_PostUnaryExpression = CreateTokenSet({
            TokenType::Increment,    
            TokenType::Decrement
        });

        m_Literals = CreateTokenSet({
            TokenType::RValueNumber,
		    TokenType::RValueString,
		    TokenType::BooleanData,
		    TokenType::Null,
            TokenType::RValueChar
        });

        m_TypeIndirection = CreateTokenSet({
            TokenType::PointerDef,
            TokenType::StaticArrayDef,
            TokenType::Const
        });

        m_ValueReferences = CreateTokenSet({
            TokenType::VariableReference, 
            TokenType::DereferenceOp,
        });

        m_VariableName = CreateTokenSet({
            TokenType::VariableName,
            TokenType::VariableReference,
            TokenType::MemberName
        }); */

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
            {"function",  [this]() { ParseFunctionDefinition(); }}
        };
        
        static std::map<TokenType, std::function<void()>> s_MappedTokenTypeToFunctions = {
            {TokenType::EndScope,   [this]() { ParseIndentation(); }}, 
            {TokenType::Keyword,    [this]() { ParseGeneral(); }}
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
            CLEAR_UNREACHABLE("unimplemented");
        }
        

       /*  

        static std::map<TokenType, std::function<void()>> s_MappedFunctions = {
            {TokenType::Import,        [this]() { ParseImport(); }},
            {TokenType::Struct,        [this]() { ParseStruct(); }},
            {TokenType::Class ,        [this]() { ParseClass(); }},
            {TokenType::Declaration,   [this]() { ParseFunctionDeclaration(); }},
            {TokenType::Function,      [this]() { ParseFunctionDefinition(); }},
            {TokenType::ConditionalIf, [this]() { ParseIf(); }},
            {TokenType::ElseIf,        [this]() { ParseElseIf(); }},
            {TokenType::Else,          [this]() { ParseElse(); }},
            {TokenType::EndIndentation,[this]() { ParseIndentation(); }},
            {TokenType::Return,        [this]() { ParseReturn(); }},
            {TokenType::While,         [this]() { ParseWhile(); }},
            {TokenType::For,           [this]() { ParseFor(); }},
            {TokenType::Let,           [this]() { ParseLetDecleration(); }},
            {TokenType::Const,         [this]() { ParseConstDecleration(); }},
            {TokenType::Continue,      [this]() { ParseLoopControls(); }},
            {TokenType::Break,         [this]() { ParseLoopControls(); }},
            {TokenType::Trait,         [this]() { ParseTrait(); }}, 
            {TokenType::Enum,          [this]() { ParseEnum(); }}, 
            {TokenType::Defer,         [this]() { ParseDefer(); }}

        };

        if(MatchAny(m_VariableType) && !Match(TokenType::Const))
        {
            ParseVariableDecleration(true);
            return;
        }

        if(s_MappedFunctions.contains(Peak().TokenType))
        {
            s_MappedFunctions.at(Peak().TokenType)();
        }
        else 
        {
            ParseGeneral();
        } */
    }

    void Parser::ParseGeneral()
    {
        // first we need to determine how to parse

        auto terminator = [&](const Token& token)
        {
            return token.IsType(TokenType::EndOfFile) || 
                   token.IsType(TokenType::EndLine)   || 
                   token.IsType(TokenType::EqualsEquals);
        };


        // int var
        static constexpr std::array<TokenType, s_MaxMatchSize> s_KeywordIdentifier = 
                {TokenType::Keyword, TokenType::Identifier, TokenType::None};
        
        // Foo var
        static constexpr std::array<TokenType, s_MaxMatchSize> s_IdentifierIdentifier = 
                {TokenType::Identifier, TokenType::Identifier, TokenType::None};

        
        if(LookAheadMatches(terminator, s_KeywordIdentifier) || LookAheadMatches(terminator, s_IdentifierIdentifier))
        {
            auto decleration = ParseVariableDeclerationN(true);

            if(MatchAny(m_AssignmentOperators))
            {
                Root()->Push(ParseAssignment(decleration, true));
                return;
            }
            
            Root()->Push(decleration);
            
            return;
        }
        else 
        {
            CLEAR_UNREACHABLE("unimplemented match");
        }

        Root()->Push(ParseExpression());

        /* std::shared_ptr<ASTNodeBase> expression = ParseExpression();

        if(MatchAny(m_AssignmentOperators))
        {
            Root()->Push(ParseAssignment(expression));
            return;
        }

        Root()->Push(expression); */
    }

    void Parser::ParseVariableDecleration(bool defaultInitialize)
    {
        auto type = ParseTypeResolver();

        Expect(TokenType::Identifier);
        
        auto variableDecleration = std::make_shared<ASTVariableDeclaration>(Consume().GetData());
        variableDecleration->Push(type);

        Root()->Push(variableDecleration);

      /*   TypeDescriptor variableType = { ParseVariableTypeTokens() };

        //ExpectAny(m_VariableName);
        ExpectAny(m_VariableName);

        std::string variableName = Consume().Data;

        std::vector<std::shared_ptr<ASTVariableDeclaration>> variableDeclerations;
        std::vector<std::shared_ptr<ASTNodeBase>> assignmentOperators;

        variableDeclerations.push_back(std::make_shared<ASTVariableDeclaration>(variableName, variableType));

        auto Flush = [&]()
        {   
            for(auto dec : variableDeclerations)
            {
                Root()->Push(dec);
            }

            for(auto assignment : assignmentOperators)
            {
                Root()->Push(assignment);
            }
        };

        if(Match(TokenType::EndLine))
        {  
            if(defaultInitialize)
            {
                assignmentOperators.push_back(CreateDefaultInitializerFromName(variableDeclerations.back()->GetName()));
            }

            Flush();
            Consume();
            return; 
        }

        bool assigned = false;

        while(Match(TokenType::Comma) || MatchAny(m_AssignmentOperators))
        {
            if(MatchAny(m_AssignmentOperators))
            {
                assignmentOperators.push_back(ParseAssignment(variableDeclerations.back()->GetName(), true));
                assigned = true;
                continue;
            }

            Consume();
            ExpectAny(m_VariableName);
            variableDeclerations.push_back(std::make_shared<ASTVariableDeclaration>(Consume().Data, variableType));

            if(!assigned && defaultInitialize)
            {
                assignmentOperators.push_back(CreateDefaultInitializerFromName(variableDeclerations.back()->GetName()));
            }

            assigned = false;
        }

        Flush(); */
    }

    void Parser::ParseLoopControls() 
    {
        CLEAR_UNREACHABLE("unimplemented");
       /*  auto node = std::make_shared<ASTLoopControlFlow>(Peak().TokenType);
        Consume();
        Root()->Push(node); */
    }

    void Parser::ParseTrait()
    {
        CLEAR_UNREACHABLE("unimplemented");
        /* 
        Expect(TokenType::Trait);
        Consume();

        std::string traitName = Consume().Data;

        Expect(TokenType::EndLine);
        Consume();

        Expect(TokenType::StartIndentation);
        Consume();
    
        std::shared_ptr<ASTTrait> trait = std::make_shared<ASTTrait>(traitName);

        m_RootStack.push_back(trait);

        while (Match(TokenType::Function) || MatchAny(m_VariableType))
        {
            if(Match(TokenType::Function)) // parse function decleration
            {
                ParseTraitFunctionDefinition();
            }
            else if (MatchAny(m_VariableType))
            {
                ParseVariableDecleration();
            }
            else 
            {
                CLEAR_LOG_WARNING("Ignored token ", TokenToString(Peak().TokenType));
            }

            Consume();
        }

        m_RootStack.pop_back();
        Root()->Push(trait);

        if(Match(TokenType::EndIndentation))
            Consume(); */
    }

    void Parser::ParseLetDecleration()
    {
        CLEAR_UNREACHABLE("unimplemented");

/*         Expect(TokenType::Let);
        Consume();

        ExpectAny(m_VariableName);

        while(MatchAny(m_VariableName))
        {
            std::string variableName = Consume().Data;

            Expect(TokenType::Assignment);
            Consume();

            auto inferredType = std::make_shared<ASTInferredDecleration>(variableName);
            inferredType->Push(ParseExpression());
            
            Root()->Push(inferredType);
            
            if(Match(TokenType::Comma))
            {
                Consume();
                ExpectAny(m_VariableName);
            }
        }
 */    }

    void Parser::ParseConstDecleration()
    {
        CLEAR_UNREACHABLE("unimplemented");
       /*  Expect(TokenType::Const);
        SavePosition();

        TypeDescriptor variableType = { ParseVariableTypeTokens() };

        if(variableType.Description.size() >= 2 && variableType.Description.back().TokenType != TokenType::VariableReference)
        {
            RestorePosition();
            ParseVariableDecleration(true);
            return;
        }

        variableType.Description.pop_back(); // remove the variable reference
        Undo();

        ExpectAny(m_VariableName);

        while(MatchAny(m_VariableName))
        {
            std::string variableName = Consume().Data;

            Expect(TokenType::Assignment);
            Consume();

            auto inferredType = std::make_shared<ASTInferredDecleration>(variableName, true);
            inferredType->Push(ParseExpression());
            
            Root()->Push(inferredType);
            
            if(Match(TokenType::Comma))
            {
                Consume();
                ExpectAny(m_VariableName);
            }
        } */
    }

    void Parser::ParseStruct()
    {
        CLEAR_UNREACHABLE("unimplemented");

    /*     Expect(TokenType::Struct);

        Consume();

        Expect(TokenType::StructName);

        std::string structName = Consume().Data;

        Expect(TokenType::EndLine);
        Consume();

        Expect(TokenType::StartIndentation);
        Consume();

        Token token;
        token.TokenType = TokenType::StructName;
        token.Data = structName;

        TypeDescriptor structTyDesc;
        structTyDesc.Description = { token };

        auto struct_ = std::make_shared<ASTStruct>();

        while(!Match(TokenType::EndIndentation))
        {
            std::shared_ptr<TypeDescriptor> subType = std::make_shared<TypeDescriptor>();

            subType->Description = ParseVariableTypeTokens(); 
            
            ExpectAny(m_VariableName);

            std::string name = Consume().Data;
            structTyDesc.ChildTypes.push_back({name, subType});

            if(Match(TokenType::Assignment))
            {
                Consume();
                struct_->Push(ParseExpression());
            }
            else 
            {
                struct_->Push(nullptr);
            }

            if(Match(TokenType::Comma))
                Consume();

            Expect(TokenType::EndLine);
            Consume();
        }

        struct_->SetTypeDesc(structTyDesc);
        Root()->Push(struct_);

        Consume(); */
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
        CLEAR_UNREACHABLE("unimplemented");
        /* Expect(TokenType::Return);

        Consume();

        std::shared_ptr<ASTReturn> returnStatement = std::make_shared<ASTReturn>();
        returnStatement->Push(ParseExpression());

        Root()->Push(returnStatement); */
    }

    void Parser::ParseIf()
    {
        CLEAR_UNREACHABLE("unimplemented");

        /* Expect(TokenType::ConditionalIf);
        Consume();

        std::shared_ptr<ASTIfExpression> ifExpr = std::make_shared<ASTIfExpression>();
        ifExpr->Push(ParseExpression());

        std::shared_ptr<ASTNodeBase> base = std::make_shared<ASTNodeBase>();
        base->CreateSymbolTable();
        ifExpr->Push(base);

        Root()->Push(ifExpr);
        m_RootStack.push_back(base); */
    }

    void Parser::ParseElse()
    {
        CLEAR_UNREACHABLE("unimplemented");

       /*  Expect(TokenType::Else);
        Consume();

        auto& last = Root()->GetChildren().back();
        std::shared_ptr<ASTIfExpression> ifExpr = std::dynamic_pointer_cast<ASTIfExpression>(last);
        CLEAR_VERIFY(ifExpr, "invalid node");
        
        std::shared_ptr<ASTNodeBase> base = std::make_shared<ASTNodeBase>();
        base->CreateSymbolTable();
        ifExpr->Push(base);
            
        m_RootStack.push_back(base); */
    }

    void Parser::ParseWhile()
    {
        CLEAR_UNREACHABLE("unimplemented");

       /*  Expect(TokenType::While);
        Consume();

        std::shared_ptr<ASTWhileExpression> whileExp = std::make_shared<ASTWhileExpression>();
        whileExp->Push(ParseExpression());

        std::shared_ptr<ASTNodeBase> base = std::make_shared<ASTNodeBase>();
        whileExp->Push(base);

        Root()->Push(whileExp);
        m_RootStack.push_back(base); */
    }

    void Parser::ParseFor()
    {
        CLEAR_UNREACHABLE("unimplemented");

       /*  Expect(TokenType::For);
        Consume();

        ExpectAny(m_VariableName);
        std::string name = Consume().Data;

        Expect(TokenType::In);
        Consume();

        // TODO: add more comprehensive parseIter function here. for now only variadic arguments are supported

        ExpectAny(m_VariableName);
        auto var = std::make_shared<ASTVariable>(Consume().Data);

        auto forLoop = std::make_shared<ASTForExpression>(name);
        forLoop->Push(var);

        Root()->Push(forLoop);

        auto body = std::make_shared<ASTNodeBase>();
        body->CreateSymbolTable();
        
        forLoop->Push(body);

        m_RootStack.push_back(body); */
    }

    void Parser::ParseElseIf()
    {
        CLEAR_UNREACHABLE("unimplemented");

        /* Expect(TokenType::ElseIf);
        Consume();

        auto& last = Root()->GetChildren().back();
        std::shared_ptr<ASTIfExpression> ifExpr = std::dynamic_pointer_cast<ASTIfExpression>(last);
        CLEAR_VERIFY(ifExpr, "invalid node");

        ifExpr->Push(ParseExpression());

        std::shared_ptr<ASTNodeBase> base = std::make_shared<ASTNodeBase>();
        base->CreateSymbolTable();

        ifExpr->Push(base);

        m_RootStack.push_back(base); */
    }

    void Parser::ParseFunctionDefinition(const std::string& className)
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


        if(!className.empty())
        {
            // TODO: Add this argument
        }


       while (!Match(TokenType::RightParen))
       {
           if (Match(TokenType::Identifier))
           {
               auto x = std::make_shared<ASTFunctionParameter>(Consume().GetData());
               Expect(TokenType::Ellipses);
               x->IsVariadic = true;
               funcNode->Push(x);
               Consume();
               Expect(TokenType::RightParen);
               break;
           }
           auto type = ParseTypeResolver();
           Expect(TokenType::Identifier);
           std::string name = Consume().GetData();

           if(Match(TokenType::Equals)) {
               Consume();
               std::shared_ptr<ASTDefaultArgument> arg = std::make_shared<ASTDefaultArgument>(i);
               arg->Push(type);
               arg->Push(ParseExpression());
               funcNode->Push(arg);
           }
           if(Match(TokenType::Ellipses))
           {
               auto x = std::make_shared<ASTFunctionParameter>(name);
               x->IsVariadic = true;
               x->Push(type);
               funcNode->Push(x);
               Consume();
               Expect(TokenType::RightParen);
               break;
           }

           if(!Match(TokenType::RightParen))
           {
               Expect(TokenType::Comma);
               Consume();
           }
           auto x = std::make_shared<ASTFunctionParameter>(name);
           x->Push(type);
           i++;

       }
        Consume();
        if (Match(TokenType::Colon)) {
            Root()->Push(funcNode);
            m_RootStack.push_back(funcNode);
            Consume();
            return;
        }

        Expect(TokenType::RightThinArrow);
        Consume();
        auto returnType = ParseTypeResolver();
        funcNode->Push(returnType);

    }

    void Parser::ParseTraitFunctionDefinition() 
    {
        CLEAR_UNREACHABLE("unimplemented");

       /*  Expect(TokenType::Function);

        Consume();
        Expect(TokenType::FunctionName);
        std::string functionName = Consume().Data;

        Expect(TokenType::StartFunctionParameters);

        Consume();

        std::vector<UnresolvedParameter> params;

        while(!MatchAny(m_Terminators))
        {
            UnresolvedParameter param;

            if(Match(TokenType::Ellipsis))
            {
                Consume();
                Expect(TokenType::EndFunctionParameters);
                params.push_back(param);

                break;
            }

            param.Type = { ParseVariableTypeTokens() };

            if(MatchAny(m_VariableName))
                Consume();

            if(!Match(TokenType::EndFunctionParameters))
            {
                Expect(TokenType::Comma);
                Consume();
            }

            params.push_back(param);
        }

        Expect(TokenType::EndFunctionParameters);

        Consume();

        TypeDescriptor returnType;

        if(Match(TokenType::RightArrow))
        {
            Consume();

            Expect(TokenType::FunctionType);

            Consume();

            returnType = { ParseVariableTypeTokens() };
        }

        Root()->Push(std::make_shared<ASTFunctionDecleration>(functionName, returnType, params)); */
    }

    void Parser::ParseRaise()
    {
        CLEAR_UNREACHABLE("unimplemented");

        /* Expect(TokenType::Raise);

        std::shared_ptr<ASTRaise> raise = std::make_shared<ASTRaise>();
        raise->Push(ParseExpression());

        Root()->Push(raise); */
    }

    void Parser::ParseTry()
    {
        CLEAR_UNREACHABLE("unimplemented");

       /*  Expect(TokenType::Try);
        Consume();

        std::shared_ptr<ASTTryCatch> tryCatch = std::make_shared<ASTTryCatch>();
        std::shared_ptr<ASTNodeBase> tryBlock = std::make_shared<ASTNodeBase>();

        tryCatch->Push(tryBlock);

        Root()->Push(tryCatch);
        m_RootStack.push_back(tryBlock); */
    }

    void Parser::ParseCatch()
    {
        CLEAR_UNREACHABLE("unimplemented");

        /* Expect(TokenType::Catch);
        Consume();

        auto& last = Root()->GetChildren().back();
        auto tryCatch = std::dynamic_pointer_cast<ASTTryCatch>(last);
        CLEAR_VERIFY(tryCatch, "invalid node");

        m_RootStack.push_back(tryCatch);
        ParseVariableDecleration();
        m_RootStack.pop_back();

        std::shared_ptr<ASTNodeBase> base = std::make_shared<ASTNodeBase>();
        tryCatch->Push(base);

        m_RootStack.push_back(base); */
    }

    void Parser::ParseEnum()
    {
        CLEAR_UNREACHABLE("unimplemented");


       /*  Expect(TokenType::Enum);
        Consume();

        std::string enumName = Consume().Data;

        Expect(TokenType::EndLine);
        Consume();

        Expect(TokenType::StartIndentation);
        Consume();

        std::vector<std::string> names;

        std::shared_ptr<ASTEnum> enum_ = std::make_shared<ASTEnum>(enumName);

        Expect(TokenType::VariableReference);
        enum_->AddEnumName(Consume().Data);

        Token zero;
        zero.Data = "0";
        zero.TokenType = TokenType::RValueNumber;

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

        while(Match(TokenType::VariableReference))
        {
            enum_->AddEnumName(Consume().Data);

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

            Expect(TokenType::Assignment);
            Consume();

            enum_->Push(ParseExpression());
            
            if(Match(TokenType::Comma))
                Consume();

            if(Match(TokenType::EndLine))
                Consume();
        }

        Expect(TokenType::EndIndentation);
        Consume();

        Root()->Push(enum_); */
    }

    void Parser::ParseDefer()
    {
        CLEAR_UNREACHABLE("unimplemented");


        /* Expect(TokenType::Defer);
        Consume();

        auto defer = std::make_shared<ASTDefer>();

        m_RootStack.push_back(defer);
        ParseGeneral();
        m_RootStack.pop_back();

        Root()->Push(defer); */
    }

    void Parser::ParseFunctionDeclaration()
    {
        CLEAR_UNREACHABLE("unimplemented");
/* 
        Expect(TokenType::Declaration);

        Consume();

        ExpectAny(m_VariableName);

        Consume();

        Expect(TokenType::FunctionCall);

        std::string functionName = Consume().Data;

        Expect(TokenType::OpenBracket);

        Consume();

        std::vector<UnresolvedParameter> params;

        while(!MatchAny(m_Terminators))
        {
            UnresolvedParameter param;
            
            if(Match(TokenType::Ellipsis))
            {
                Consume();
                Expect(TokenType::EndFunctionArguments);
                params.push_back(param);

                break;
            }
            
            param.Type = { ParseVariableTypeTokens() };

            if(MatchAny(m_VariableName)) 
                Consume();
            
            if(!Match(TokenType::EndFunctionArguments))
            {
                Expect(TokenType::Comma);
                Consume();
            }

            params.push_back(param);
        }

        Expect(TokenType::EndFunctionArguments);

        Consume();

        TypeDescriptor returnType;

        if(Match(TokenType::RightArrow))
        {
            Consume();

            Expect(TokenType::FunctionType);

            Consume();

            returnType = { ParseVariableTypeTokens() };
        }

        Root()->Push(std::make_shared<ASTFunctionDecleration>(functionName, returnType, params)); */
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseFunctionCall()
    {
        CLEAR_UNREACHABLE("unimplemented");

       /*  ExpectAny(m_VariableName);

        std::string functionName = Consume().Data;

        Expect(TokenType::FunctionCall);    
        Consume();

        Expect(TokenType::OpenBracket);

        Consume();

        std::shared_ptr<ASTFunctionCall> call = std::make_shared<ASTFunctionCall>(functionName);

        while(!MatchAny(m_Terminators))
        {
            call->Push(ParseExpression());

            if(!Match(TokenType::EndFunctionArguments)) 
            {
                Expect(TokenType::Comma);
                Consume();
            }
        }

        Expect(TokenType::EndFunctionArguments);
        Consume();

        return call; */

        return {};
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseVariableReference()
    {
        CLEAR_UNREACHABLE("unimplemented");
      /*   
        ExpectAny(m_VariableName);

        if(Next().TokenType == TokenType::FunctionCall) 
            return ParseFunctionCall();
        
        if(m_Aliases.contains(Peak().Data))
        {
            return ParseFunctionCall();
        }
        
        std::string name = Consume().Data;
        return std::make_shared<ASTVariable>(name); */

        return {};
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseOperand()
    {
        if(MatchAny(m_Literals)) 
            return std::make_shared<ASTNodeLiteral>(Consume());

        if(Match(TokenType::Identifier))
            return std::make_shared<ASTVariable>(Consume().GetData());

        CLEAR_UNREACHABLE("unimplemented");
        return {};
/*  
        if(MatchAny(m_Literals)) 
            return std::make_shared<ASTNodeLiteral>(Consume());

        if(Match(TokenType::MemberName))
        {
            if(Next().TokenType == TokenType::FunctionCall) 
                return ParseFunctionCall();
            
            return std::make_shared<ASTMember>(Consume().Data);
        }

        if(Match(TokenType::FunctionCall)) 
            Undo();

        std::shared_ptr<ASTNodeBase> variableReference;

        ExpectAny(m_VariableName);

        variableReference = ParseVariableReference();

        return variableReference; */

    }

    std::shared_ptr<ASTNodeBase> Parser::ParseArrayInitializer(std::shared_ptr<ASTNodeBase> storage, bool initialize)
    {
        CLEAR_UNREACHABLE("unimplemented");
        return {};
/*  
        Expect(TokenType::StartArray);

        std::vector<std::vector<size_t>> indices;
        std::vector<size_t> currentIndex = { 0 };

        std::shared_ptr<ASTInitializerList> initializer = std::make_shared<ASTInitializerList>(initialize);
        initializer->Push(storage);

        while(!Match(TokenType::EndLine))
        {
            if(Match(TokenType::StartArray))
            {
                currentIndex.push_back(0);
                Consume();
            }
            else if (Match(TokenType::EndArray))
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

        return initializer; */

    }

    std::shared_ptr<ASTNodeBase> Parser::ParseAssignment(const std::string& variableName, bool initialize)
    {
        CLEAR_UNREACHABLE("unimplemented");
        return {};

        //return ParseAssignment(std::make_shared<ASTVariable>(variableName), initialize);
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseAssignment(std::shared_ptr<ASTNodeBase> storage, bool initialize)
    {
        ExpectAny(m_AssignmentOperators);

        Token assignmentToken = Consume();

        // TODO: array initializer 

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

        assign->Push(storage);            
        assign->Push(ParseExpression()); 

        return assign;

       /*  ExpectAny(m_AssignmentOperators);

        Token assignmentToken = Consume();

        if(Match(TokenType::StartArray))
        {
            CLEAR_VERIFY(assignmentToken.TokenType == TokenType::Assignment, "invalid assignment");

            auto initializer = ParseArrayInitializer(storage);
            return initializer;
        }

        auto assignType = GetAssignmentOperatorFromTokenType(assignmentToken.TokenType);

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

        return assign; */
    }

    std::shared_ptr<ASTNodeBase> Parser::CreateDefaultInitializerFromName(const std::string& name)
    {
        CLEAR_UNREACHABLE("unimplemented");
/* 
        auto variable = std::make_shared<ASTVariable>(name);
        auto defaultInit = std::make_shared<ASTDefaultInitializer>();
        defaultInit->Push(variable);

        return defaultInit; */
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseVariableDeclerationN(bool initialize)
    {
        auto type = ParseTypeResolver();

        Expect(TokenType::Identifier);
        
        auto variableDecleration = std::make_shared<ASTVariableDeclaration>(Consume().GetData());
        variableDecleration->Push(type);

        return variableDecleration;
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseExpression() // infix to RPN and creates nodes
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
            return  token.IsType(TokenType::Identifier) || token.IsType(TokenType::Number) || token.IsType(TokenType::String) || 
                   (token.IsType(TokenType::Keyword) && (token.GetData() == "true" || token.GetData() == "false"));
        };

        auto IsTokenBoundary = [&](const Token& token)
        {
            return GetBinaryExpressionFromTokenType(token.GetType()) != BinaryExpressionType::None ||
                   token.IsType(TokenType::RightBracket) ||
                   m_Terminators.test((size_t)token.GetType());
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
            
                default:
                    break;
            }

            // ambiguous cases
            switch (current.GetType())
            {
                case TokenType::Star:
                {
                    if (IsTokenOperand(Prev())) 
                        return OperatorType::Mul;
                
                    return OperatorType::Dereference;
                }
            
                case TokenType::Ampersand:
                {
                    if (IsTokenOperand(Prev())) 
                        return OperatorType::BitwiseAnd;
                
                    return OperatorType::Address;
                }
            
                case TokenType::Minus:
                {
                    if (IsTokenOperand(Prev()))
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
                
                    return OperatorType::None; // -- double negate does nothing so we can ignore
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

        auto HandleOperator = [&]() 
        {
            OperatorType operatorType = GetOperatorTypeFromContext();

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

            Consume();
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

        while (!MatchAny(m_Terminators)) 
        {
            HandlePreUnaryOperators();

            OperatorType operatorType = GetOperatorTypeFromContext();

            if (IsOperand()) 
            {
                expression->Push(ParseOperand());
            }
            else if (Match(TokenType::LeftBracket)) 
            {
                HandleOpenBracket();
            }
            else if (Match(TokenType::RightBracket)) 
            {
                HandleCloseBracket();
            }
            else if (operatorType != OperatorType::None) 
            {
                HandleOperator();
            }
            else 
            {
                CLEAR_LOG_WARNING("ignore token ", Consume().GetData());
            }

            HandlePostUnaryOperators();
        }

        PopOperatorsUntil([](const Operator&) { return false; });
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
                resolver->Push(ParseExpression());
        }

        return resolver;
    }

    std::vector<Token> Parser::ParseVariableTypeTokens()
    {
         
        std::vector<Token> tokens;

        /* if(Match("const"))
            tokens.push_back(Consume());
        
        tokens.push_back(Consume());

        while(MatchAny(m_TypeIndirection))
        {
            tokens.push_back(Consume());
        } */

        return tokens; 
    }

    std::pair<std::string, std::shared_ptr<TypeDescriptor>> Parser::ParseVariableTypeDescriptor()
    {
        CLEAR_UNREACHABLE("unimplemented");
        return {};
        /* 
        std::shared_ptr<TypeDescriptor> subType = std::make_shared<TypeDescriptor>();

        subType->Description = ParseVariableTypeTokens(); 
        
        ExpectAny(m_VariableName);

        std::string name = Consume().Data;

        return {name, subType}; */
    }

    void Parser::ParseIndentation()
    {
        m_RootStack.pop_back();
        Consume();
    }

    void Parser::ParseClass()
    {
        CLEAR_UNREACHABLE("unimplemented");
/* 
        Expect(TokenType::Class);
        Consume();

        Expect(TokenType::ClassName);
        std::string className = Consume().Data;

        SkipUntil(TokenType::StartIndentation);
        Consume();

        Token token;
        token.TokenType = TokenType::ClassName;
        token.Data = className;

        TypeDescriptor classTy;
        classTy.Description = { token };

        std::shared_ptr<ASTClass> classNode = std::make_shared<ASTClass>();
        Root()->Push(classNode);

        m_RootStack.push_back(classNode);

        while(!Match(TokenType::EndIndentation))
        {
            if(MatchAny(m_VariableType))
            {
                classTy.ChildTypes.push_back(ParseVariableTypeDescriptor());

                if(Match(TokenType::Assignment))
                {
                    Consume();
                    classNode->Push(ParseExpression());
                }
                else 
                {
                    classNode->Push(nullptr);
                }

                if(Match(TokenType::Comma))
                    Consume();

                Expect(TokenType::EndLine);
                Consume();
                
                continue;
            }

            if(Match(TokenType::Function))
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

            CLEAR_LOG_INFO("ignoring token ", TokenToString(Consume().TokenType), " in class ", className);
        }

        classNode->SetTypeDescriptor(classTy);
        m_RootStack.pop_back();

        Consume(); */
    }

    BinaryExpressionType Parser::GetBinaryExpressionFromTokenType(TokenType type)
    {
        CLEAR_UNREACHABLE("unimplemented");
        return BinaryExpressionType::None;

/* 
        switch (type)
	    {
			case TokenType::Assignment:			return BinaryExpressionType::Assignment;
			case TokenType::MultiplyAssign:
			case TokenType::MulOp:				return BinaryExpressionType::Mul;
			case TokenType::PlusAssign:
			case TokenType::AddOp:				return BinaryExpressionType::Add;
			case TokenType::DivideAssign:
			case TokenType::DivOp:				return BinaryExpressionType::Div;
			case TokenType::MinusAssign:
			case TokenType::SubOp:				return BinaryExpressionType::Sub;
			case TokenType::ModuloAssign:
			case TokenType::ModOp:				return BinaryExpressionType::Mod;
			case TokenType::IsEqual:			return BinaryExpressionType::Eq;
			case TokenType::NotEqual:			return BinaryExpressionType::NotEq;
			case TokenType::GreaterThan:		return BinaryExpressionType::Greater;
			case TokenType::LessThan:			return BinaryExpressionType::Less;
			case TokenType::LessThanEqual:		return BinaryExpressionType::LessEq;
			case TokenType::GreaterThanEqual:	return BinaryExpressionType::GreaterEq;
			case TokenType::BitwiseNot:			return BinaryExpressionType::BitwiseNot;
			case TokenType::LeftShift:			return BinaryExpressionType::BitwiseLeftShift;
			case TokenType::RightShift:			return BinaryExpressionType::BitwiseRightShift;
			case TokenType::BitwiseOr:			return BinaryExpressionType::BitwiseOr;
			case TokenType::BitwiseXor:			return BinaryExpressionType::BitwiseXor;
			case TokenType::BitwiseAnd:			return BinaryExpressionType::BitwiseAnd;
			case TokenType::IndexOperator:		return BinaryExpressionType::Index;	
            case TokenType::Power:              return BinaryExpressionType::Pow;
            case TokenType::And:                return BinaryExpressionType::And;
            case TokenType::Or:                 return BinaryExpressionType::Or;
            case TokenType::DotOp:              return BinaryExpressionType::MemberAccess;

			default:
				break;
		}

		return BinaryExpressionType::None; */
    }

    UnaryExpressionType Parser::GetPreUnaryExpressionTypeFromTokenType(TokenType type)
    {
        CLEAR_UNREACHABLE("unimplemented");
        return UnaryExpressionType::None;
/*  
        switch (type)
        {
			case TokenType::Increment:      return UnaryExpressionType::PreIncrement;
			case TokenType::Decrement:      return UnaryExpressionType::PreDecrement;
			case TokenType::BitwiseNot:     return UnaryExpressionType::BitwiseNot;
			case TokenType::AddressOp:	    return UnaryExpressionType::Reference;
			case TokenType::DereferenceOp:	return UnaryExpressionType::Dereference;
			case TokenType::Negation:       return UnaryExpressionType::Negation; 
			case TokenType::Not:            return UnaryExpressionType::Not;
            case TokenType::Ellipsis:       return UnaryExpressionType::Unpack;

			default:
				break;
		}


		return UnaryExpressionType::None; */
    }

    UnaryExpressionType Parser::GetPostUnaryExpressionTypeFromTokenType(TokenType type)
    {
        CLEAR_UNREACHABLE("unimplemented");
        return UnaryExpressionType::None;
/* 
        switch (type)
		{
			case TokenType::Increment:  return UnaryExpressionType::PostIncrement;
			case TokenType::Decrement:  return UnaryExpressionType::PostDecrement;
			default:
				break;
		}

		return UnaryExpressionType::None; */
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

   /*  void Parser::SkipUntil(TokenSet set)
    {
        while(!MatchAny(set) && !Match(TokenType::Eof))
        {
            Consume();
        }
    } */
}
