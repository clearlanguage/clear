#include "Parser.h"

#include "Core/Log.h"

namespace clear 
{
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

        while(Peak().TokenType != TokenType::Eof)
        {
            ParseStatement();
        }
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
        else // ??
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

        m_Root->PushChild(Ref<ASTVariableDeclaration>::Create(variableType, variableName));

        if(Match(TokenType::EndLine))
        {
           Consume();
           return; 
        }

        ExpectAny(m_AssignmentOperators);
        
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
    
    void Parser::ParseExpression()
    {
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
