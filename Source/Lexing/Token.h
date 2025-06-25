#pragma once 

#include "TokenDefinitions.h"

#include <inttypes.h>
#include <bitset>
#include <string>
#include <string_view>
#include <set>

namespace clear 
{
    class Token 
    {
    public:
        Token() = default;
        Token(TokenType type, const std::string& data) : m_Type(type), m_Data(data) {}
        
        ~Token() = default;

        int64_t   AsInt()    const { return std::stoll(m_Data); }
        uint64_t  AsUInt()   const { return std::stoull(m_Data); }
        bool      AsBool()   const { return m_Data == "true"; }
        double    AsFloat()  const { return std::stod(m_Data); }
        char      AsChar()   const { return m_Data.empty() ? '\0' : m_Data[0]; }

        bool IsType(TokenType type)  const { return m_Type == type; }
        const std::string& GetData() const { return m_Data; }
        TokenType GetType()          const { return m_Type; }
        
        std::string GetTypeAsString() const
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
                case TokenType::ThinArrow:          return "ThinArrow";
                case TokenType::FatArrow:           return "FatArrow";
            
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
                case TokenType::HatEquals:           return "HatEquals";
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
    private:
        TokenType m_Type = TokenType::None;
        std::string m_Data;
    };
}