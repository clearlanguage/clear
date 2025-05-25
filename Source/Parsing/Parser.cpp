#include "Parser.h"
#include "AST/ASTNodeN.h"
#include "Core/Log.h"

#include <stack>

namespace clear 
{
    static std::map<TokenType, int32_t> s_Precedence = {
	    	{TokenType::DotOp,			    5},
	        {TokenType::IndexOperator,      5}, 
	        {TokenType::Power,              4}, 
	        {TokenType::Negation,           4},
	        {TokenType::Increment,          4},
	        {TokenType::Decrement,          4},
	        {TokenType::BitwiseNot,         4},
	        {TokenType::DereferenceOp,      4},
	        {TokenType::AddressOp,          4},
	        {TokenType::DivOp,              3},
	        {TokenType::MulOp,              3},
	        {TokenType::LeftShift,          3},
	        {TokenType::RightShift,         3},
	        {TokenType::AddOp,              2},
	        {TokenType::SubOp,              2},
	        {TokenType::BitwiseAnd,         1}, 
	        {TokenType::BitwiseXor,         1},
	        {TokenType::BitwiseOr,          1},
	        {TokenType::IsEqual,            0}, 
	        {TokenType::NotEqual,           0},
	        {TokenType::LessThan,           0},
	        {TokenType::GreaterThan,        0},
	        {TokenType::LessThanEqual,      0},
	        {TokenType::GreaterThanEqual,   0},
	        {TokenType::OpenBracket,       -1}  
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
            TokenType::CharType
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
		    TokenType::StartIndentation, 
            TokenType::Eof
        });

        m_UnaryExpression = CreateTokenSet({
            TokenType::Increment,    
            TokenType::Decrement,    
            TokenType::BitwiseNot,   
            TokenType::AddressOp,	  
            TokenType::DereferenceOp,
            TokenType::Negation     
        });

        m_Literals = CreateTokenSet({
            TokenType::RValueNumber,
		    TokenType::RValueString,
		    TokenType::BooleanData,
		    TokenType::Null
        });

        m_IgnoredTokens = CreateTokenSet({
            TokenType::StartIndentation,
            TokenType::EndLine
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
        
        CLEAR_LOG_ERROR("missing expected token");
        CLEAR_UNREACHABLE("TODO");
    }

    void Parser::ExpectAny(TokenSet tokenSet)
    {
        if(MatchAny(tokenSet)) return;
        
        CLEAR_LOG_ERROR("missing expected token");    
        CLEAR_UNREACHABLE("TODO");
    }

    void Parser::ParseStatement()
    {
        if(MatchAny(m_IgnoredTokens))
        {
            CLEAR_LOG_WARNING("ignoring token ", TokenToString(Consume().TokenType));
            return;
        }

        if(MatchAny(m_VariableType))
        {
            ParseVariableDecleration();
        }
        else if (Match(TokenType::Declaration))
        {
            ParseFunctionDecleration();
        }
        else if (Match(TokenType::Function))
        {
            ParseFunctionDefinition();
        }
        else if (Match(TokenType::EndIndentation))
        {
            ParseIndentation();
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
            Consume();

            std::shared_ptr<ASTAssignmentOperator> assign = std::make_shared<ASTAssignmentOperator>(AssignmentOperatorType::Normal);
            assign->Push(expression);
            assign->Push(ParseExpression()); 

            Root()->Push(assign);

            return;
        }

        Root()->Push(expression);
    }

    void Parser::ParseVariableDecleration()
    {
        std::shared_ptr<Type> variableType = ParseVariableType();

        Expect(TokenType::VariableName);
        std::string variableName = Consume().Data;

        //if(match(TokenType::Comma)) deal with this later

        Root()->Push(std::make_shared<ASTVariableDeclaration>(variableName, variableType));

        if(Match(TokenType::EndLine))
        {
           Consume();
           return; 
        }

        ExpectAny(m_AssignmentOperators);

        Token assignmentToken = Consume();

        std::shared_ptr<ASTAssignmentOperator> assign = std::make_shared<ASTAssignmentOperator>(AssignmentOperatorType::Normal);
        assign->Push(std::make_shared<ASTVariableReference>(variableName));
        assign->Push(ParseExpression()); 

        Root()->Push(assign);
    }

    void Parser::ParseFunctionDefinition()
    {
        Expect(TokenType::Function);

        Consume();

        Expect(TokenType::FunctionName);

        std::string name = Consume().Data;
        std::shared_ptr<Type> returnType;
        std::vector<Parameter> params;

        Expect(TokenType::StartFunctionParameters);

        Consume();

        while(!Match(TokenType::EndFunctionParameters)) 
        {
            Parameter param;

            if(Match(TokenType::Ellipsis))
            {
                param.IsVariadic = true;
                params.push_back(param);

                Consume();

                Expect(TokenType::EndFunctionParameters);
                break;
            }

            param.Type = ParseVariableType();
            
            Expect(TokenType::VariableReference);

            param.Name = Consume().Data;
            params.push_back(param);

            if(!Match(TokenType::EndFunctionParameters)) 
            {
                Expect(TokenType::Comma);
                Consume();
            }
        }

        Consume();

        if(Match(TokenType::EndLine))
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

        returnType = ParseVariableType();

        std::shared_ptr<ASTFunctionDefinition> func = std::make_shared<ASTFunctionDefinition>(name, returnType, params);
        Root()->Push(func);
        m_RootStack.push_back(func);

        Expect(TokenType::EndLine);

        Consume();
    }

    void Parser::ParseFunctionDecleration()
    {
        Expect(TokenType::Declaration);

        Consume();

        Expect(TokenType::VariableReference);

        Consume();

        Expect(TokenType::FunctionCall);

        std::string functionName = Consume().Data;

        Expect(TokenType::OpenBracket);

        Consume();

        std::vector<Parameter> params;

        while(!MatchAny(m_Terminators))
        {
            Parameter param;
            
            if(Match(TokenType::Ellipsis))
            {
                param.IsVariadic = true;
                Consume();
                Expect(TokenType::EndFunctionArguments);
                params.push_back(param);

                break;
            }
            
            param.Type = ParseVariableType();

            if(Match(TokenType::VariableReference)) 
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

        std::shared_ptr<Type> returnType;

        if(Match(TokenType::RightArrow))
        {
            Consume();

            Expect(TokenType::FunctionType);

            Consume();

            returnType = ParseVariableType();
        }

        Root()->Push(std::make_shared<ASTFunctionDecleration>(functionName, returnType, params));
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseFunctionCall()
    {
        Expect(TokenType::FunctionCall);

        std::string functionName = Consume().Data;

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
        if(Match(TokenType::AddressOp))
        {
            Consume();
            Expect(TokenType::VariableReference);
            return std::make_shared<ASTVariableReference>(Consume().Data);
        }

        Expect(TokenType::VariableReference);
        
        std::string name = Consume().Data;

        if(Match(TokenType::FunctionCall)) 
            return ParseFunctionCall();

        return std::make_shared<ASTVariableExpression>(name);
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

                //TODO: unary expressions go here
                
                operators.pop();
            }
        };

        auto IsOperand = [&]()
        {
            return Match(TokenType::VariableReference) || 
                   Match(TokenType::AddressOp) || 
                   MatchAny(m_Literals);
        };

        auto HandleOperand = [&]() 
        {
            if (Match(TokenType::VariableReference) || Match(TokenType::AddressOp)) 
            {
                expression->Push(ParseVariableReference());
            }
            else if (MatchAny(m_Literals)) 
            {
                expression->Push(std::make_shared<ASTNodeLiteral>(Consume()));
            }
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
                Type::GetBinaryExpressionTypeFromToken(tokenType),
                UnaryExpressionType::None,
                false,
                precedence
            });

            Consume();
        };

        while (!MatchAny(m_Terminators)) 
        {
            if (IsOperand()) 
            {
                HandleOperand();
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
            else 
            {
                CLEAR_UNREACHABLE("unimplemented token");
                break;
            }
        }

        PopOperatorsUntil([](const Operator&) { return false; });

        return expression;
    }

    std::shared_ptr<Type> Parser::ParseVariableType()
    {
        std::shared_ptr<Type> type = std::make_shared<Type>(Consume());

        while(Match(TokenType::PointerDef))
        {
            type = std::make_shared<Type>(type); 
            Consume();
        }

        return type;
    }

    void Parser::ParseIndentation()
    {
        while(Match(TokenType::EndIndentation))
        {
            m_RootStack.pop_back();
            Consume();
        }
    }
}
