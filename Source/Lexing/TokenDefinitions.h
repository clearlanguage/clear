#pragma once 

#include <unordered_set>
#include <unordered_map>
#include <string>

namespace clear 
{
    enum class TokenType
    {
        None = 0,

        Identifier,
        Keyword,
        Number,
        String,

        Colon,           // :
        Semicolon,       // ;
        Comma,           // ,
        Dot,             // .
        Equals,          // =
        Plus,            // +
        Minus,           // -
        Star,            // *
        Ampersand,       // &
        Pipe,            // |
        ForwardSlash,    // /
        Percent,         // %
        Hat,             // ^
        Telda,           // ~

        LeftParen,       // (
        RightParen,      // )
        LeftBrace,       // {
        RightBrace,      // }
        LeftBracket,     // [
        RightBracket,    // ]
        LessThan,        // <
        GreaterThan,     // >
        Bang,            // !
        ThinArrow,       // ->
        FatArrow,        // =>

        PlusEquals,          // +=
        MinusEquals,         // -=
        StarEquals,          // *=
        SlashEquals,         // /=
        PercentEquals,       // %=
        EqualsEquals,        // ==
        BangEquals,          // !=
        LessThanEquals,      // <=
        GreaterThanEquals,   // >=
        AmpersandEquals,     // &=
        PipeEquals,          // |=
        HatEquals,           // ^=
        LeftShift,           // <<
        RightShift,          // >>
        LeftShiftEquals,     // <<=
        RightShiftEquals,    // >>=
        LogicalAnd,          // &&
        LogicalOr,           // ||
        Increment,           // ++
        Decrement,           // --
        Ellipses,            // ...
        
        EndLine,
        EndScope,
        EndOfFile
    };

    template<typename T>
    using set = std::unordered_set<T>;

    template<typename Key, typename Value>
    using map = std::unordered_map<Key, Value>;

    inline set<std::string> g_Keywords = {
        "bool",

        "uint8", "uint16", "uint32", "uint64",
        "uint",

        "int8", "int16", "int32", "int64",
        "int",

        "float32", "float64",
        "float",

        "if", "else", "while", "for", "return", "break", "continue",
        "true", "false", "null", "in", "and", "or",

        "struct", "function", "const", "class", "restriction", 
        "trait", "property", "declare", "block", "enum"
    };

    inline set<std::string> g_Punctuators = {
        ";", ",", ":", "(", ")", 
        "{", "}", "[", "]", "->"
    };

    inline set<std::string> g_Operators = {
        "+", "-", "*", "/", "%",
        "=", "<", ">", 
        "!", "&", "|",
        "^", "~",
        "."
    };

    inline map<std::string, TokenType> g_OperatorMappings = {
            {"+",    TokenType::Plus},
            {"-",    TokenType::Minus},
            {"*",    TokenType::Star},
            {"/",    TokenType::ForwardSlash},
            {"%",    TokenType::Percent},
            {"=",    TokenType::Equals},
            {"<",    TokenType::LessThan},
            {">",    TokenType::GreaterThan},
            {"!",    TokenType::Bang},
            {"&",    TokenType::Ampersand},
            {"|",    TokenType::Pipe},
            {"^",    TokenType::Hat},
            {"~",    TokenType::Telda},
            {".",    TokenType::Dot},
            {"...",  TokenType::Ellipses},

            {"++",   TokenType::Increment},
            {"--",   TokenType::Decrement},
            {"+=",   TokenType::PlusEquals},
            {"-=",   TokenType::MinusEquals},
            {"*=",   TokenType::StarEquals},
            {"/=",   TokenType::SlashEquals},
            {"%=",   TokenType::PercentEquals},
            {"==",   TokenType::EqualsEquals},
            {"!=",   TokenType::BangEquals},
            {"<=",   TokenType::LessThanEquals},
            {">=",   TokenType::GreaterThanEquals},
            {"&=",   TokenType::AmpersandEquals},
            {"|=",   TokenType::PipeEquals},
            {"^=",   TokenType::HatEquals},
            {"<<",   TokenType::LeftShift},
            {">>",   TokenType::RightShift},
            {"<<=",  TokenType::LeftShiftEquals},
            {">>=",  TokenType::RightShiftEquals},

            {"->",   TokenType::ThinArrow},
            {"=>",   TokenType::FatArrow}
    };

    inline const size_t g_MaxOperatorSize = 3;

    inline map<std::string, TokenType> g_PunctuatorMappings = {
            {":",  TokenType::Colon},
            {";",  TokenType::Semicolon},
            {",",  TokenType::Comma},
            {"(",  TokenType::LeftParen},
            {")",  TokenType::RightParen},
            {"{",  TokenType::LeftBrace},
            {"}",  TokenType::RightBrace},
            {"[",  TokenType::LeftBracket},
            {"]",  TokenType::RightBracket}
    };

    
}