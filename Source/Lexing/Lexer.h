#pragma once 

#include "Token.h"

#include <filesystem>
#include <vector>
#include <string>
#include <unordered_set>
#include <unordered_map>

namespace clear 
{
    class Lexer
    {
    public:
        Lexer(const std::filesystem::path& path);
        ~Lexer() = default;
    
        const auto& GetTokens() { return m_Tokens; }
    
    private:
        void Lex();
        void Eat();

        void EatWord();
        void EatOperator();
        void EatPunctuator();
        void EatString();
        void EatNumber();
        void FlushScopes();
        void EatHex();
        void EatBin();

        bool IsLineOnlyWhitespace();
        bool IsAllowedCharacter(char character);

        std::pair<double, bool> GetNumber(const std::string& string);

        std::string Peak();
        std::string Prev();

    private:
        template<typename T>
        std::string GetWord(T shouldContinueFn)
        {
            size_t start = m_Position;

            while (shouldContinueFn())
                m_Position++;

            return m_Contents.substr(start, m_Position - start);
        }

    private:
        std::vector<Token> m_Tokens;
        std::string m_Contents;
        size_t m_Position = 0;
        uint32_t m_Indents = 0;
    };
}

