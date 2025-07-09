#include "Token.h"
#include "Lexing/TokenDefinitions.h"

#include <Core/Log.h>


namespace clear
{
    std::string_view Token::GetTypeAsString() const
    {
        switch (m_Type)
        {
            case TokenType::None:               return "None";
        
            case TokenType::Identifier:         return "Identifier";
            case TokenType::Keyword:            return "Keyword";
            case TokenType::Number:             return "Number";
            case TokenType::String:             return "String";
        
            case TokenType::Colon:              return "Colon";
            case TokenType::Semicolon:          return "Semicolon";
            case TokenType::Comma:              return "Comma";
            case TokenType::Dot:                return "Dot";
            case TokenType::Equals:             return "Equals";
            case TokenType::Plus:               return "Plus";
            case TokenType::Minus:              return "Minus";
            case TokenType::Star:               return "Star";
            case TokenType::Ampersand:          return "Ampersand";
            case TokenType::Pipe:               return "Pipe";
            case TokenType::ForwardSlash:       return "ForwardSlash";
            case TokenType::Percent:            return "Percent";
            case TokenType::Hat:                return "Hat";
            case TokenType::Telda:              return "Telda";
        
            case TokenType::LeftParen:          return "LeftParen";
            case TokenType::RightParen:         return "RightParen";
            case TokenType::LeftBrace:          return "LeftBrace";
            case TokenType::RightBrace:         return "RightBrace";
            case TokenType::LeftBracket:        return "LeftBracket";
            case TokenType::RightBracket:       return "RightBracket";
            case TokenType::LessThan:           return "LessThan";
            case TokenType::GreaterThan:        return "GreaterThan";
            case TokenType::Bang:               return "Bang";
            case TokenType::RightThinArrow:     return "RightThinArrow";
            case TokenType::LeftThinArrow:      return "LeftThinArrow";
            case TokenType::PlusEquals:         return "PlusEquals";
            case TokenType::MinusEquals:        return "MinusEquals";
            case TokenType::StarEquals:         return "StarEquals";
            case TokenType::SlashEquals:        return "SlashEquals";
            case TokenType::PercentEquals:      return "PercentEquals";
            case TokenType::EqualsEquals:       return "EqualsEquals";
            case TokenType::BangEquals:         return "BangEquals";
            case TokenType::LessThanEquals:     return "LessThanEquals";
            case TokenType::GreaterThanEquals:  return "GreaterThanEquals";
            case TokenType::AmpersandEquals:    return "AmpersandEquals";
            case TokenType::PipeEquals:         return "PipeEquals";
            case TokenType::HatEquals:          return "HatEquals";
            case TokenType::LeftShift:          return "LeftShift";
            case TokenType::RightShift:         return "RightShift";
            case TokenType::LeftShiftEquals:    return "LeftShiftEquals";
            case TokenType::RightShiftEquals:   return "RightShiftEquals";
            case TokenType::LogicalAnd:         return "LogicalAnd";
            case TokenType::LogicalOr:          return "LogicalOr";
            case TokenType::Increment:          return "Increment";
            case TokenType::Decrement:          return "Decrement";
            case TokenType::Ellipses:           return "Ellipses";
            
            case TokenType::EndLine:            return "EndLine";
            case TokenType::EndScope:           return "EndScope";
            case TokenType::EndOfFile:          return "EndOfFile";
            default:                            return "Unknown";
        }
    }

    size_t GetExpectedLength(TokenType type) 
    {
        switch (type) 
        {
            case TokenType::EndScope:
            case TokenType::EndLine:
            case TokenType::Char:       return 1;
            case TokenType::Number:     
            case TokenType::String:     
            case TokenType::Identifier: return 5;
            default:    
                return 1;
        }
    }
}
