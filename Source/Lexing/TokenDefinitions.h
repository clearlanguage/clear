#pragma once

#include <unordered_set>
#include <unordered_map>
#include <string>
#include <bitset>

namespace clear
{
    enum class TokenType
    {
        None = 0,

        Identifier,
        Keyword,
        Number,
        String,
        Char,

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
        RightThinArrow,       // ->
        LeftThinArrow,        // <-

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
        EndOfFile,

        Count
    };

    using TokenSet = std::bitset<(size_t)TokenType::Count>;

    inline constexpr TokenSet CreateTokenSet(std::initializer_list<TokenType> tokenTypes)
    {
        TokenSet tokenSet;

        for(auto type : tokenTypes)
            tokenSet.set((size_t)type);

        return tokenSet;
    }

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
        "true", "false", "null", "in", "and", "or", "elseif", "defer",
        "switch", "case", "default", "import", "as", "module", "endmodule",

        "struct", "function", "class", "restriction",
        "trait", "property", "declare", "block", "enum",

        "not","and","or",

        "let","const",
    };

    inline set<std::string> g_Punctuators = {
        ";", ",", ":", "(", ")",
        "{", "}", "[", "]",
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

            {"->",   TokenType::RightThinArrow},
            {"<-",   TokenType::LeftThinArrow},
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

    inline std::unordered_map<TokenType, size_t> g_TokenLengths = {
        {TokenType::Plus,              1},
        {TokenType::Minus,             1},
        {TokenType::Star,              1},
        {TokenType::ForwardSlash,      1},
        {TokenType::Percent,           1},
        {TokenType::Equals,            1},
        {TokenType::LessThan,          1},
        {TokenType::GreaterThan,       1},
        {TokenType::Bang,              1},
        {TokenType::Ampersand,         1},
        {TokenType::Pipe,              1},
        {TokenType::Hat,               1},
        {TokenType::Telda,             1},
        {TokenType::Dot,               1},
        {TokenType::Ellipses,          3},

        {TokenType::Increment,         2},
        {TokenType::Decrement,         2},
        {TokenType::PlusEquals,        2},
        {TokenType::MinusEquals,       2},
        {TokenType::StarEquals,        2},
        {TokenType::SlashEquals,       2},
        {TokenType::PercentEquals,     2},
        {TokenType::EqualsEquals,      2},
        {TokenType::BangEquals,        2},
        {TokenType::LessThanEquals,    2},
        {TokenType::GreaterThanEquals, 2},
        {TokenType::AmpersandEquals,   2},
        {TokenType::PipeEquals,        2},
        {TokenType::HatEquals,         2},
        {TokenType::LeftShift,         2},
        {TokenType::RightShift,        2},
        {TokenType::LeftShiftEquals,   3},
        {TokenType::RightShiftEquals,  3},
        {TokenType::RightThinArrow,    2},
        {TokenType::LeftThinArrow,     2},
        {TokenType::Colon,         1},
        {TokenType::Semicolon,     1},
        {TokenType::Comma,         1},
        {TokenType::LeftParen,     1},
        {TokenType::RightParen,    1},
        {TokenType::LeftBrace,     1},
        {TokenType::RightBrace,    1},
        {TokenType::LeftBracket,   1},
        {TokenType::RightBracket,  1}
    };


}