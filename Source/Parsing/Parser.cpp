/* #include "Parser.h"

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
        : m_Tokens(info.Tokens), m_Root(Ref<ASTNodeBase>::Create())
    {
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

        while(!Match(TokenType::Eof))
        {
            ParseStatement();
        }
    }

    Ref<ASTNodeBase> Parser::GetResult()
    {
        return m_Root;
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
        else if (Match(TokenType::FunctionCall))
        {
            ParseFunctionCall();
        }
        else 
        {
            ParseExpression();
        }
    }

    void Parser::ParseVariableDecleration()
    {
        Ref<Type> variableType = ParseVariableType();

        Expect(TokenType::VariableName);
        std::string variableName = Consume().Data;

        //if(match(TokenType::Comma)) deal with this later

        m_Root->PushChild(Ref<ASTVariableDeclaration>::Create(variableName, variableType));

        if(Match(TokenType::EndLine))
        {
           Consume();
           return; 
        }

        ExpectAny(m_AssignmentOperators);
        Ref<ASTNodeBase> expression = ParseExpression();
    }

    Ref<ASTNodeBase> Parser::ParseVariableReference()
    {
        return Ref<ASTVariableExpression>::Create(Consume().Data); 
    }

    void Parser::ParseFunctionDefinition()
    {

    }

    void Parser::ParseFunctionDecleration()
    {
    }

    void Parser::ParseFunctionCall()
    {
    }
    
    Ref<ASTNodeBase> Parser::ParseExpression()
    {
        struct Operator
	    {
	    	BinaryExpressionType BinaryExpression;
	    	UnaryExpressionType  UnaryExpression;
	    	bool IsOpenBracket = false;
	    	int32_t Precedence = 0;
	    };

        std::stack<Operator> operators;

        while(!MatchAny(m_Terminators))
        {
            while(MatchAny(m_UnaryExpression))
            {

            }
        }


        return Ref<ASTNodeBase>();
    }

    Ref<Type> Parser::ParseVariableType()
    {
        Ref<Type> type = Ref<Type>::Create(Consume());

        while(Match(TokenType::PointerDef))
        {
            type = Ref<Type>::Create(type); 
            Consume();
        }

        return type;
    }
}
 */