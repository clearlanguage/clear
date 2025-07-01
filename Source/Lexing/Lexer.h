#pragma once 

#include "Token.h"

#include "Diagnostics/DiagnosticsBuilder.h"

#include <filesystem>
#include <vector>
#include <string>

namespace clear 
{
    static DiagnosticsBuilder s_NullBuilder;

    class Lexer
    {
    public:
        Lexer(const std::filesystem::path& path, DiagnosticsBuilder& builder = s_NullBuilder);
        ~Lexer() = default;
    
        const auto& GetTokens() { return m_Tokens; }
    
    private:
        void Lex();
        void Eat();

        void EatWord();
        void EatOperator();
        void EatPunctuator();
        void EatString();
        void EatComment();
        void EatChar();
        void EatNumber();
        void EatMultiLineComment();
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
                Increment();

            return m_Contents.substr(start, m_Position - start);
        }

        void Increment();
        void Increment(size_t n);

        void Report(char character, DiagnosticCode code, Severity severity);
        void Report(const std::string& str, DiagnosticCode code, Severity severity);
        void Report(const Token& token, DiagnosticCode code, Severity severity);

        void AbortCurrent();

        void EmplaceBack(TokenType type, const std::string& data);

    private:
        std::vector<Token> m_Tokens;
        std::string m_Contents;
        std::filesystem::path m_File;

        size_t m_Position = 0;
        uint32_t m_Indents = 0;

        size_t m_LineNumber   = 0;
        size_t m_ColumnNumber = 0;

        DiagnosticsBuilder& m_DiagBuilder;
    };
}

