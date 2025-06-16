#include "Parser.h"
#include "AST/ASTNode.h"
#include "Core/Log.h"
#include "Core/TypeRegistry.h"
#include "Core/Utils.h"
#include "Lexing/Tokens.h"

#include <stack>


namespace clear 
{
    static std::map<TokenType, int32_t> s_Precedence = {
        {TokenType::IndexOperator,      7}, // x[y]
        
        {TokenType::Negation,           6}, // -x
        {TokenType::Increment,          6}, // ++x
        {TokenType::Decrement,          6}, // --x
        {TokenType::BitwiseNot,         6}, // ~x
        {TokenType::AddressOp,          6}, // &x
        {TokenType::DereferenceOp,      6}, // *x
        {TokenType::Not,                6}, // not x

        {TokenType::Power,              5}, // x ** y

        {TokenType::MulOp,              4}, // x * y
        {TokenType::DivOp,              4}, // x / y
        {TokenType::ModOp,              4}, // x % y

        {TokenType::AddOp,              3}, // x + y
        {TokenType::SubOp,              3}, // x - y

        {TokenType::LeftShift,          2}, // x << y
        {TokenType::RightShift,         2}, // x >> y

        {TokenType::BitwiseAnd,         1}, // x & y
        {TokenType::BitwiseXor,         1}, // x ^ y
        {TokenType::BitwiseOr,          1}, // x | y

        {TokenType::LessThan,           0},
        {TokenType::GreaterThan,        0},
        {TokenType::LessThanEqual,      0},
        {TokenType::GreaterThanEqual,   0},
        {TokenType::IsEqual,            0},
        {TokenType::NotEqual,           0},

        {TokenType::And,               -1}, // x and y
        {TokenType::Or,                -2}, // x or y
    };

    Parser::Parser(const ProgramInfo& info)
        : m_Tokens(info.Tokens)
    {
        std::shared_ptr<ASTNodeBase> root = std::make_shared<ASTNodeBase>();
        root->CreateSymbolTable();
        m_RootStack.push_back(root);

        m_Tokens.push_back({TokenType::Eof});

        m_VariableType = CreateTokenSet({
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

        m_Terminators = CreateTokenSet({
            TokenType::EndLine, 
		    TokenType::EndIndentation, 
		    TokenType::Comma,  
		    TokenType::EndFunctionArguments, 
		    TokenType::EndArray,
		    TokenType::Assignment, 
            TokenType::MultiplyAssign, 
            TokenType::DivideAssign, 
            TokenType::PlusAssign, 
            TokenType::MinusAssign, 
            TokenType::ModuloAssign, 
            TokenType::StartIndentation,
            TokenType::Eof
        });

        m_PreUnaryExpression = CreateTokenSet({
            TokenType::Increment,    
            TokenType::Decrement,    
            TokenType::BitwiseNot,   
            TokenType::AddressOp,	  
            TokenType::DereferenceOp,
            TokenType::Negation, 
            TokenType::Not
        });

        m_PostUnaryExpression = CreateTokenSet({
            TokenType::Increment,    
            TokenType::Decrement
        });

        m_Literals = CreateTokenSet({
            TokenType::RValueNumber,
		    TokenType::RValueString,
		    TokenType::BooleanData,
		    TokenType::Null
        });

        m_IgnoredTokens = CreateTokenSet({
            TokenType::StartIndentation,
            TokenType::EndLine, 
            TokenType::Comma
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
            TokenType::VariableReference
        });

        while(!Match(TokenType::Eof))
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
        return m_Tokens[m_Position++];
    }

    Token Parser::Peak()
    {
        return m_Tokens[m_Position];
    }

    Token Parser::Next()
    {
        return m_Tokens[m_Position + 1];
    }

    Token Parser::Prev()
    {
        return m_Tokens[m_Position - 1];
    }

    void Parser::Undo()
    {
        m_Position--;
    }

    bool Parser::Match(TokenType token)
    {
        return Peak().TokenType == token;
    }

    bool Parser::MatchAny(TokenSet tokenSet)
    {
        return tokenSet.test((size_t)Peak().TokenType);
    }

    void Parser::Expect(TokenType tokenType)
    {
        if(Match(tokenType)) return;
        
        CLEAR_UNREACHABLE("expected ", TokenToString(tokenType), " but got ", TokenToString(Peak().TokenType), " ", Peak().Data);
    }

    void Parser::ExpectAny(TokenSet tokenSet)
    {
        if(MatchAny(tokenSet)) return;
        
        CLEAR_LOG_ERROR("missing expected token from token set");    
        CLEAR_UNREACHABLE("TODO");
    }

    void Parser::ParseStatement()
    {
        if(MatchAny(m_IgnoredTokens))
        {
            CLEAR_LOG_WARNING("ignoring token ", TokenToString(Consume().TokenType));
            return;
        }

        static std::map<TokenType, std::function<void()>> s_MappedFunctions = {
            {TokenType::Import,        [this]() { ParseImport(); }},
            {TokenType::Struct,        [this]() { ParseStruct(); }},
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
            {TokenType::Continue,      [this]()  {ParseLoopControls();}},
        {TokenType::Break,      [this]()  {ParseLoopControls();}}

        };

        if(MatchAny(m_VariableType) && !Match(TokenType::Const))
        {
            ParseVariableDecleration();
            return;
        }

        if(s_MappedFunctions.contains(Peak().TokenType))
        {
            s_MappedFunctions.at(Peak().TokenType)();
        }
        else 
        {
            ParseGeneric();
        }
    }

    void Parser::ParseGeneric()
    {
        std::shared_ptr<ASTNodeBase> expression = ParseExpression();

        if(MatchAny(m_AssignmentOperators))
        {
            Root()->Push(ParseAssignment(expression));
            return;
        }

        Root()->Push(expression);
    }

    void Parser::ParseVariableDecleration()
    {
        TypeDescriptor variableType = { ParseVariableTypeTokens() };

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
           Flush();
           Consume();
           return; 
        }

        while(Match(TokenType::Comma) || MatchAny(m_AssignmentOperators))
        {
            if(MatchAny(m_AssignmentOperators))
            {
                assignmentOperators.push_back(ParseAssignment(variableDeclerations.back()->GetName(), true));
                continue;
            }

            Consume();
            ExpectAny(m_VariableName);
            variableDeclerations.push_back(std::make_shared<ASTVariableDeclaration>(Consume().Data, variableType));
        }

        Flush();
    }

    void Parser::ParseLoopControls() {
        auto node = std::make_shared<ASTLoopControlFlow>(Peak().TokenType);
        Consume();
        Root()->Push(node);

    }


    void Parser::ParseLetDecleration()
    {
        Expect(TokenType::Let);
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
    }

    void Parser::ParseConstDecleration()
    {
        Expect(TokenType::Const);
        SavePosition();

        TypeDescriptor variableType = { ParseVariableTypeTokens() };

        if(variableType.Description.size() == 2 && variableType.Description.back().TokenType != TokenType::VariableReference)
        {
            RestorePosition();
            ParseVariableDecleration();
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
        }
    }

    void Parser::ParseStruct()
    {
        Expect(TokenType::Struct);

        Consume();

        Expect(TokenType::StructName);

        std::string structName = Consume().Data;

        Expect(TokenType::EndLine);
        Consume();

        Expect(TokenType::StartIndentation);
        Consume();

        Token token;
        token.TokenType = TokenType::TypeIdentifier;
        token.Data = structName;

        TypeDescriptor structTyDesc;
        structTyDesc.Description = { token };

        while(!Match(TokenType::EndIndentation))
        {
            std::shared_ptr<TypeDescriptor> subType = std::make_shared<TypeDescriptor>();

            subType->Description = ParseVariableTypeTokens(); 
            
            ExpectAny(m_VariableName);

            std::string name = Consume().Data;
            structTyDesc.ChildTypes.push_back({name, subType});

            Expect(TokenType::EndLine);
            Consume();
        }

        //m_TypeRegistry.ResolveType(structTyDesc);
        Root()->Push(std::make_shared<ASTStruct>(structTyDesc));
        Consume();
    }

    void Parser::ParseImport()
    {
        Expect(TokenType::Import);

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
        Root()->Push(import);
    }

    void Parser::ParseReturn()
    {
        Expect(TokenType::Return);

        Consume();

        std::shared_ptr<ASTReturn> returnStatement = std::make_shared<ASTReturn>();
        returnStatement->Push(ParseExpression());

        Root()->Push(returnStatement);
    }

    void Parser::ParseIf()
    {
        Expect(TokenType::ConditionalIf);
        Consume();

        std::shared_ptr<ASTIfExpression> ifExpr = std::make_shared<ASTIfExpression>();
        ifExpr->Push(ParseExpression());

        std::shared_ptr<ASTNodeBase> base = std::make_shared<ASTNodeBase>();
        ifExpr->Push(base);

        Root()->Push(ifExpr);
        m_RootStack.push_back(base);
    }

    void Parser::ParseElse()
    {
        Expect(TokenType::Else);
        Consume();

        auto& last = Root()->GetChildren().back();
        std::shared_ptr<ASTIfExpression> ifExpr = std::dynamic_pointer_cast<ASTIfExpression>(last);
        CLEAR_VERIFY(ifExpr, "invalid node");
        
        std::shared_ptr<ASTNodeBase> base = std::make_shared<ASTNodeBase>();
        ifExpr->Push(base);
            
        m_RootStack.push_back(base);
    }

    void Parser::ParseWhile()
    {
        Expect(TokenType::While);
        Consume();

        std::shared_ptr<ASTWhileExpression> whileExp = std::make_shared<ASTWhileExpression>();
        whileExp->Push(ParseExpression());

        std::shared_ptr<ASTNodeBase> base = std::make_shared<ASTNodeBase>();
        whileExp->Push(base);

        Root()->Push(whileExp);
        m_RootStack.push_back(base);
    }

    void Parser::ParseFor()
    {
        Expect(TokenType::For);
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
        forLoop->Push(body);

        m_RootStack.push_back(body);
    }

    void Parser::ParseElseIf()
    {
        Expect(TokenType::ElseIf);
        Consume();

        auto& last = Root()->GetChildren().back();
        std::shared_ptr<ASTIfExpression> ifExpr = std::dynamic_pointer_cast<ASTIfExpression>(last);
        CLEAR_VERIFY(ifExpr, "invalid node");

        ifExpr->Push(ParseExpression());

        std::shared_ptr<ASTNodeBase> base = std::make_shared<ASTNodeBase>();
        ifExpr->Push(base);

        m_RootStack.push_back(base);
    }

    void Parser::ParseFunctionDefinition()
    {
        Expect(TokenType::Function);

        Consume();

        Expect(TokenType::FunctionName);

        std::string name = Consume().Data;
        TypeDescriptor returnType;
        std::vector<UnresolvedParameter> params;

        Expect(TokenType::StartFunctionParameters);

        Consume();

        while(!Match(TokenType::EndFunctionParameters)) 
        {
            UnresolvedParameter param;

            if(MatchAny(m_VariableName))
            {
                param.IsVariadic = true;
                param.Name = Consume().Data;
                Expect(TokenType::Ellipsis);

                params.push_back(param);

                Consume();
                Expect(TokenType::EndFunctionParameters);

                break;
            }

            param.Type = { ParseVariableTypeTokens() };  
            
            ExpectAny(m_VariableName);

            param.Name = Consume().Data;

            if(Match(TokenType::Ellipsis))
            {
                param.IsVariadic = true;
                params.push_back(param);

                Consume();

                Expect(TokenType::EndFunctionParameters);
                break;
            }

            params.push_back(param);

            if(!Match(TokenType::EndFunctionParameters)) 
            {
                Expect(TokenType::Comma);
                Consume();
            }
        }

        Consume();

        if(Match(TokenType::EndLine) || Match(TokenType::StartIndentation)) 
        {
            std::shared_ptr<ASTFunctionDefinition> func = std::make_shared<ASTFunctionDefinition>(name, returnType, params);
            Root()->Push(func);
            m_RootStack.push_back(func);

            Consume();

            return;
        }

        Expect(TokenType::RightArrow);

        Consume();

        Expect(TokenType::FunctionType);

        Consume();

        ExpectAny(m_VariableType);

        returnType = { ParseVariableTypeTokens() };

        std::shared_ptr<ASTFunctionDefinition> func = std::make_shared<ASTFunctionDefinition>(name, returnType, params);
        Root()->Push(func);
        m_RootStack.push_back(func);

        Expect(TokenType::EndLine);

        Consume();
    }

    void Parser::ParseFunctionDeclaration()
    {
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

        Root()->Push(std::make_shared<ASTFunctionDecleration>(functionName, returnType, params));
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseFunctionCall()
    {
        ExpectAny(m_VariableName);

        std::string functionName = Consume().Data;

        while(Match(TokenType::DotOp))
        {
            functionName += ".";
            Consume();
            Expect(TokenType::MemberName);
            functionName += Consume().Data;
        }

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

        return call;
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseVariableReference()
    {
        ExpectAny(m_VariableName);

        if(Next().TokenType == TokenType::FunctionCall) 
            return ParseFunctionCall();
        
        if(m_Aliases.contains(Peak().Data))
        {
            return ParseFunctionCall();
        }
        
        std::string name = Consume().Data;

        return std::make_shared<ASTVariable>(name);
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseOperand()
    {
        if(MatchAny(m_Literals)) 
            return std::make_shared<ASTNodeLiteral>(Consume());

        if(Match(TokenType::FunctionCall)) Undo();

        std::shared_ptr<ASTNodeBase> variableReference;

        ExpectAny(m_VariableName);

        variableReference = ParseVariableReference();

        if(!Match(TokenType::DotOp)) 
            return variableReference;

        std::shared_ptr<ASTMemberAccess> memberAccess = std::make_shared<ASTMemberAccess>();
        memberAccess->Push(variableReference);

        Consume();

        while(Match(TokenType::MemberName))
        {
            memberAccess->Push(std::make_shared<ASTMember>(Consume().Data));

            if(Match(TokenType::DotOp)) 
                Consume();
        }

        return memberAccess;
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseArrayInitializer(std::shared_ptr<ASTNodeBase> storage)
    {
        Expect(TokenType::StartArray);

        std::vector<std::vector<size_t>> indices;
        std::vector<size_t> currentIndex = { 0 };

        std::shared_ptr<ASTInitializerList> initializer = std::make_shared<ASTInitializerList>();
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

        return initializer;
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseAssignment(const std::string& variableName, bool initialize)
    {
        return ParseAssignment(std::make_shared<ASTVariable>(variableName), initialize);
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseAssignment(std::shared_ptr<ASTNodeBase> storage, bool initialize)
    {
        ExpectAny(m_AssignmentOperators);

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

        return assign;
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseExpression()
    {
        struct Operator
        {
            BinaryExpressionType BinaryExpression;
            UnaryExpressionType UnaryExpression;
            bool IsOpenBracket = false;
            int32_t Precedence = 0;
        };

        std::shared_ptr<ASTExpression> expression = std::make_shared<ASTExpression>();
        std::stack<Operator> operators;

        auto PopOperatorsUntil = [&](auto condition) 
        {
            while (!operators.empty() && !condition(operators.top())) 
            {
                const auto& currentOperator = operators.top();

                if (currentOperator.BinaryExpression != BinaryExpressionType::None)
                    expression->Push(std::make_shared<ASTBinaryExpression>(currentOperator.BinaryExpression));
                else 
                    expression->Push(std::make_shared<ASTUnaryExpression>(currentOperator.UnaryExpression));
                
                operators.pop();
            }
        };

        auto IsTokenOperand = [&](Token token)
        {
            return token.TokenType == TokenType::VariableReference || token.TokenType == TokenType::VariableName ||
                   m_Literals.test((size_t)token.TokenType);
        };

        auto IsTokenBoundary = [&](Token token)
        {
            return GetBinaryExpressionFromTokenType(token.TokenType) != BinaryExpressionType::None ||
                   token.TokenType == TokenType::CloseBracket ||
                   m_Terminators.test((size_t)token.TokenType);
        };

        auto IsOperand = [&]()
        {
            return MatchAny(m_VariableName) || Match(TokenType::FunctionCall) ||
                   MatchAny(m_Literals);
        };

        auto HandleOpenBracket = [&]() 
        {
            operators.push({ BinaryExpressionType::None, UnaryExpressionType::None, true, 0 });
            Consume();
        };

        auto HandleCloseBracket = [&]() 
        {
            PopOperatorsUntil([](const Operator& op) { return op.IsOpenBracket; });
            
            if (!operators.empty()) 
                operators.pop(); // remove the open bracket

            Consume();
        };

        auto HandleOperator = [&]() 
        {
            TokenType tokenType = Peak().TokenType;
            int precedence = s_Precedence.at(tokenType);

            PopOperatorsUntil([&](const Operator& op) 
            {
                return op.IsOpenBracket || precedence > op.Precedence;
            });

            operators.push({
                GetBinaryExpressionFromTokenType(tokenType),
                UnaryExpressionType::None,
                false,
                precedence
            });

            Consume();
        };

        auto ShouldHandleAsPost = [&]()
        {
            if(!(Match(TokenType::Increment) || Match(TokenType::Decrement)))
                return false;
            
            bool left  = IsTokenOperand(Prev());
            bool right = IsTokenBoundary(Next());
            
            return left && right;
        };      

        auto HandlePreUnaryOperators = [&]() 
        {
            while(MatchAny(m_PreUnaryExpression) && !ShouldHandleAsPost())
            {
                Token token = Consume();
                int precedence = s_Precedence.at(token.TokenType);

                PopOperatorsUntil([&](const Operator& op) 
                {
                    return op.IsOpenBracket || precedence >= op.Precedence;
                });

                operators.push({
                    BinaryExpressionType::None,
                    GetPreUnaryExpressionTypeFromTokenType(token.TokenType),
                    false,
                    precedence
                });
            }
        };

        auto HandlePostUnaryOperators = [&]() 
        {
            while(MatchAny(m_PostUnaryExpression) && ShouldHandleAsPost())
            {
                Token token = Consume();

                int precedence = s_Precedence.at(token.TokenType);

                PopOperatorsUntil([&](const Operator& op) 
                {
                    return op.IsOpenBracket || precedence > op.Precedence;
                });

                operators.push({
                    BinaryExpressionType::None,
                    GetPostUnaryExpressionTypeFromTokenType(token.TokenType),
                    false,
                    precedence
                });
            }
        };

        while (!MatchAny(m_Terminators)) 
        {
            HandlePreUnaryOperators();

            if (IsOperand()) 
            {
                expression->Push(ParseOperand());
            }
            else if (Match(TokenType::OpenBracket)) 
            {
                HandleOpenBracket();
            }
            else if (Match(TokenType::CloseBracket)) 
            {
                HandleCloseBracket();
            }
            else if (s_Precedence.contains(Peak().TokenType)) 
            {
                HandleOperator();
            }

            HandlePostUnaryOperators();
        }

        PopOperatorsUntil([](const Operator&) { return false; });

        return expression;
    }

    std::vector<Token> Parser::ParseVariableTypeTokens()
    {
        std::vector<Token> tokens;

        if(Match(TokenType::Const))
            tokens.push_back(Consume());
        
        tokens.push_back(Consume());

        while(MatchAny(m_TypeIndirection))
        {
            tokens.push_back(Consume());
        }

        return tokens;
    }


    void Parser::ParseIndentation()
    {
        while(Match(TokenType::EndIndentation))
        {
            m_RootStack.pop_back();
            Consume();
        }
    }

    void Parser::ParseClass()
    {
        Expect(TokenType::Class);
        Consume();

        Expect(TokenType::ClassName);
        std::string className = Consume().Data;

        SkipUntil(TokenType::StartIndentation);
        Consume();
    }

    BinaryExpressionType Parser::GetBinaryExpressionFromTokenType(TokenType type)
    {
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
            

			default:
				break;
		}

		return BinaryExpressionType::None;
    }

    UnaryExpressionType Parser::GetPreUnaryExpressionTypeFromTokenType(TokenType type)
    {
        switch (type)
        {
			case TokenType::Increment:      return UnaryExpressionType::PreIncrement;
			case TokenType::Decrement:      return UnaryExpressionType::PreDecrement;
			case TokenType::BitwiseNot:     return UnaryExpressionType::BitwiseNot;
			case TokenType::AddressOp:	    return UnaryExpressionType::Reference;
			case TokenType::DereferenceOp:	return UnaryExpressionType::Dereference;
			case TokenType::Negation:       return UnaryExpressionType::Negation; 
			case TokenType::Not:            return UnaryExpressionType::Not; 
			default:
				break;
		}


		return UnaryExpressionType::None;
    }

    UnaryExpressionType Parser::GetPostUnaryExpressionTypeFromTokenType(TokenType type)
    {
        switch (type)
		{
			case TokenType::Increment:  return UnaryExpressionType::PostIncrement;
			case TokenType::Decrement:  return UnaryExpressionType::PostDecrement;
			default:
				break;
		}

		return UnaryExpressionType::None;
    }

    AssignmentOperatorType Parser::GetAssignmentOperatorFromTokenType(TokenType type)
    {
        switch (type)
        {
            case TokenType::Assignment:      return AssignmentOperatorType::Normal;
            case TokenType::PlusAssign:      return AssignmentOperatorType::Add;
            case TokenType::MinusAssign:     return AssignmentOperatorType::Sub;
            case TokenType::MultiplyAssign:  return AssignmentOperatorType::Mul;
            case TokenType::DivideAssign:    return AssignmentOperatorType::Div;    
            case TokenType::ModuloAssign:    return AssignmentOperatorType::Mod;    
            default:
                break;
        }

        CLEAR_UNREACHABLE("invalid token type for assignment");

        return AssignmentOperatorType();
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
        while(!Match(type) && !Match(TokenType::Eof))
        {
            Consume();
        }
    }

    void Parser::SkipUntil(TokenSet set)
    {
        while(!MatchAny(set) && !Match(TokenType::Eof))
        {
            Consume();
        }
    }
}
