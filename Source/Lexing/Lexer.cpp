#include "Lexer.h"

#include "Core/Log.h"

#include <fstream>
#include <iostream>
#include <map>
#include <charconv>
#include <math.h>
#include <sstream>
#include <fast_float/fast_float.h>



namespace clear 
{
    Lexer::Lexer(const std::filesystem::path& path, DiagnosticsBuilder& builder)
        : m_File(path), m_DiagBuilder(builder)
    {
        std::fstream file(path);
        CLEAR_VERIFY(file.is_open(), "failed to open file ", path);

        std::stringstream stream;
        stream << file.rdbuf();
        m_Contents = stream.str();

        Lex();
    }

    void Lexer::Lex()
    {
        while(m_Position < m_Contents.size())
        {
            Eat();
        }

        while(m_Indents-- != 0) 
            EmplaceBack(TokenType::EndScope, "EndScope");

        EmplaceBack(TokenType::EndOfFile, "EOF");
    }

    void Lexer::Eat()
    {
        if(Prev() == "\n")
        {
            EmplaceBack(TokenType::EndLine, " ");
        }

        if(Prev() == "\n" && !IsLineOnlyWhitespace()) 
        {
            FlushScopes();
        }

        if(m_Position >= m_Contents.size())
            return;

        std::string top(1, m_Contents[m_Position]);

        if (std::isdigit(m_Contents[m_Position]))
        {
            EatNumber();
        }
        else if(IsAllowedCharacter(m_Contents[m_Position]))
        {
            EatWord();
        }
        else if(top == "/")
        {
            if (m_Contents[m_Position+1] == '/')
            {
                EatComment();
            }
            else if (m_Contents[m_Position+1] == '*')
            {
                EatMultiLineComment();
            }
            else
            {
                EatOperator();
            }

        }
        else if (g_Operators.contains(top))
        {
            EatOperator();
        }
        else if (g_Punctuators.contains(top))
        {
            EatPunctuator();
        }
        else if (top == "\"")
        {
            EatString();
        }
        else if (top == "'")
        {
            EatChar();
        }
        else if (std::isspace(m_Contents[m_Position]))
        {
            Increment();
        }
        else 
        {
            CLEAR_UNREACHABLE("unexpected token ", m_Contents[m_Position]);
        }
    }

    void Lexer::EatComment() 
    {
        auto IsCommentContinue = [&]()
        {
            return m_Position < m_Contents.size() &&
                   m_Contents[m_Position] != '\n';
        };

        GetWord(IsCommentContinue);
    }

    void Lexer::EatMultiLineComment()
    {
        Increment(2);

        bool valid = false;

        while (m_Position < m_Contents.size())
        {
            if (m_Contents[m_Position] == '*' && m_Contents[m_Position + 1] == '/')
            {
                Increment(2);

                valid = true;

                break;
            }

            Increment();
        }

        if (m_Position >= m_Contents.size() && !valid)
        {
            Report(m_Contents.back(), DiagnosticCode_UnterminatedComment, Severity::High);
        }

    }

    void Lexer::EatWord()
    {
        auto ShouldContinue = [&]()
        {
            return m_Position < m_Contents.size() && 
                   IsAllowedCharacter(m_Contents[m_Position]);
        };

        std::string word = GetWord(ShouldContinue);

        if(g_Keywords.contains(word)) 
        {
            EmplaceBack(TokenType::Keyword, word);
        }
        else 
        {
            EmplaceBack(TokenType::Identifier, word);
        }
    }

    void Lexer::EatOperator()
    {
        auto ShouldContinue = [&]()
        {
            std::string top(1, m_Contents[m_Position]);
            return m_Position < m_Contents.size() && 
                   !std::isspace(m_Contents[m_Position]) && 
                   g_Operators.contains(top);
        };

        std::string word = GetWord(ShouldContinue);

        for(size_t i = 0; i < word.size(); )
        {
            std::string operator_ = word.substr(i, g_MaxOperatorSize);

            while(!g_OperatorMappings.contains(operator_) && !operator_.empty())
            {
                operator_.pop_back();
            }

            if(operator_.empty())
            {
                Report(operator_, DiagnosticCode_InvalidOperator, Severity::High);
                AbortCurrent();

                return;
            }
            
            CLEAR_VERIFY(!operator_.empty(), "not a valid operator ", word);

            EmplaceBack(g_OperatorMappings.at(operator_), operator_);
            i += operator_.size();
        }
    }

    void Lexer::EatPunctuator()
    {
        // punctuators guarenteed to be one size only for now

        std::string word(1, m_Contents[m_Position]);
        Increment();

        if(g_PunctuatorMappings.contains(word))
        {
            EmplaceBack(g_PunctuatorMappings.at(word), word);
        }
        else 
        {
            Report(word, DiagnosticCode_InvalidSyntax, Severity::High);
            AbortCurrent();
        }   
    }

    void Lexer::EatString()
    {
        size_t start = m_Position;
        Increment();

        std::string lexedString;

        while (m_Position < m_Contents.size()) 
        {
            char c = m_Contents[m_Position];
            Increment();
        
            if (c == '"') 
                break;
        
            if (c == '\\') 
            {
                if (m_Position >= m_Contents.size())
                    break;
            
                char next = m_Contents[m_Position];
                Increment();

                switch (next) 
                {
                    case 'n':  lexedString += '\n'; break;
                    case 't':  lexedString += '\t'; break;
                    case 'r':  lexedString += '\r'; break;
                    case 'a':  lexedString += '\a'; break;
                    case 'v':  lexedString += '\v'; break;
                    case 'f':  lexedString += '\f'; break;
                    case '\\': lexedString += '\\'; break;
                    case '"':  lexedString += '\"'; break;
                    case '\'': lexedString += '\''; break;
                    case '0':  lexedString += '\0'; break;
                    default:   lexedString += next; break;
                }

                continue;
            }
        
            lexedString += c;
        }

        if(m_Position == m_Contents.size())
        {
            Report(m_Tokens.back(), DiagnosticCode_UnterminatedString, Severity::High);
            return;
        }

        EmplaceBack(TokenType::String, lexedString);
    }

    void Lexer::EatNumber()
    {
        std::string prefix = m_Contents.substr(m_Position, 2);

        if(prefix == "0x" || prefix == "0X")
        {
            EatHex();
            return;
        }

        if(prefix == "0b" || prefix == "0B")
        {
            EatBin();
            return;
        }

        auto ShouldContinue = [&]() 
        {
            return m_Position < m_Contents.size() &&
                   (std::isdigit(m_Contents[m_Position]) ||
                    m_Contents[m_Position] == '.' ||
                    m_Contents[m_Position] == 'e' ||
                    m_Contents[m_Position] == 'E' ||
                    m_Contents[m_Position] == '+' ||
                    m_Contents[m_Position] == '-');
        };

        std::string word = GetWord(ShouldContinue);

        auto [value, isNumber] = GetNumber(word);

        if (!isNumber) 
        {
            Report(word, DiagnosticCode_InvalidNumberLiteral, Severity::High);
            AbortCurrent();

            return;
        }

        std::ostringstream oss;
        oss << std::setprecision(17) << value;

        EmplaceBack(TokenType::Number, oss.str());
    }
        
    void Lexer::FlushScopes()
    {
        size_t tabWidth = 4;

		bool indenting = true;
		size_t totalSpaces = 0;

		while (indenting)
		{
			if (Peak() == "\t")
			{
				totalSpaces += tabWidth;
                Increment();
			}
			else if (Peak() == " ")
			{
				totalSpaces++;
                Increment();
			}
			else
			{
				indenting = false;
			}
		}

		size_t localIndents = totalSpaces / 4;

        while (m_Indents > localIndents)
		{
            EmplaceBack(TokenType::EndScope, "");
			m_Indents--;
		}
        
        m_Indents = localIndents;
    }

    void Lexer::EatHex()
    {
        Increment(2);

        auto ShouldContinue = [&]()
        {
            bool isDigit = std::isdigit(m_Contents[m_Position]);
            bool isValidCap   = m_Contents[m_Position] >= 'A' && m_Contents[m_Position] <= 'F';
            bool isValidLower = m_Contents[m_Position] >= 'a' && m_Contents[m_Position] <= 'f';

            return  m_Position < m_Contents.size() && (isDigit || isValidCap || isValidLower);
        };

        std::string word = GetWord(ShouldContinue);

        size_t k = 0;
        uint64_t num = 0;

        for (auto it = word.rbegin(); it != word.rend(); it++) 
        {
            uint64_t digit = 0;

            if(std::isdigit(*it)) 
            {
                digit = *it - '0';
            }
            else 
            {
                digit = std::tolower(*it) - 'a' + 10;
            }

            num += digit * std::pow(16, k++);
        }

        EmplaceBack(TokenType::Number, std::to_string(num));
    }

    void Lexer::EatBin()
    {
        Increment(2);

        auto ShouldContinue = [&]()
        {
            return  m_Position < m_Contents.size() && (m_Contents[m_Position] == '0' || m_Contents[m_Position] == '1');
        };

        std::string word = GetWord(ShouldContinue);

        size_t k = 0;
        uint64_t num = 0;

        for (auto it = word.rbegin(); it != word.rend(); it++) 
        {
            uint64_t digit = *it - '0';
            num += digit * std::pow(2, k++);
        }

        EmplaceBack(TokenType::Number, std::to_string(num));
    }

    void Lexer::EatChar()
    {
        size_t start = m_Position;
        Increment();

        if (m_Position >= m_Contents.size()) 
        {
            CLEAR_LOG_ERROR("char error");
            return;
        }

        char value;

        if (m_Contents[m_Position] == '\\') 
        {
            Increment();

            if (m_Position >= m_Contents.size()) 
            {
                Report(m_Contents.back(), DiagnosticCode_UnterminatedString, Severity::High);
                return;
            }

            char esc = m_Contents[m_Position];
            switch (esc) 
            {
                case 'n':  value = '\n'; break;
                case 't':  value = '\t'; break;
                case 'r':  value = '\r'; break;
                case 'a':  value = '\a'; break;
                case 'v':  value = '\v'; break;
                case 'f':  value = '\f'; break;
                case '\\': value = '\\'; break;
                case '\'': value = '\''; break;
                case '0':  value = '\0'; break;
                default:
                    value = esc;
                break;
            }

        } 
        else 
        {
            value = m_Contents[m_Position];
        }

        Increment();

        if (m_Position >= m_Contents.size() || m_Contents[m_Position] != '\'') 
        {
            Report(m_Tokens.back(), DiagnosticCode_InvalidCharLiteral, Severity::High);
            AbortCurrent();

            return;
        }

        Increment();

        EmplaceBack(TokenType::Char, std::string(1, value));
    }

    bool Lexer::IsLineOnlyWhitespace()
    {
        size_t position = m_Position;

        while(Peak() != "\n")
        {
            std::string top = Peak();

            if (!std::isspace(top.back())) 
            {
                m_Position = position;
                return false;   
            }

            Increment();
        }

        m_Position = position;
        return true;
    }

    bool Lexer::IsAllowedCharacter(char character)
    {
        return std::isalnum(character) || character == '_';
    }

    std::pair<double, bool> Lexer::GetNumber(const std::string& string)
    {
        double value;
        auto [ptr, ec] = fast_float::from_chars(string.data(), string.data() + string.size(), value);
        bool valid = ec == std::errc() && ptr == string.data() + string.size();

        return std::make_pair(value, valid);
    }

    std::string Lexer::Peak()
    {
        if(m_Position >= m_Contents.size()) 
            return std::string(1, '\0');
        
        return std::string(1, m_Contents[m_Position]);
    }

    std::string Lexer::Prev()
    {
         if(m_Position == 0) 
            return std::string(1, '\0');
        
        return std::string(1, m_Contents[m_Position - 1]);
    }

    void Lexer::Increment()
    {
        if(m_Position == m_Contents.size())
            return;
        
        if(m_Contents[m_Position] == '\n')
        {
            m_LineNumber++;
            m_ColumnNumber = 0;
        }
        else 
        {
            m_ColumnNumber++;
        }

        m_Position++;
    }

    void Lexer::Increment(size_t n)
    {
        for(size_t i = 0; i < n; i++)
            Increment();
    }

    void Lexer::Report(char character, DiagnosticCode code, Severity severity)
    {
        Token token(TokenType::None, std::string(1, character), m_File, m_LineNumber, m_ColumnNumber);
        m_DiagBuilder.Report(Stage::Lexing, severity, token, code);
    }

     void Lexer::Report(const std::string& str, DiagnosticCode code, Severity severity)
    {
        Token token(TokenType::None, str, m_File, m_LineNumber, m_ColumnNumber);
        m_DiagBuilder.Report(Stage::Lexing, severity, token, code);
    }

    void Lexer::Report(const Token& token, DiagnosticCode code, Severity severity)
    {
        m_DiagBuilder.Report(Stage::Lexing, severity, token, code);
    }

    void Lexer::AbortCurrent()
    {
        while(m_Position < m_Contents.size() && m_Contents[m_Position] != '\n')
        {
            Increment();
        }
    }

    void Lexer::EmplaceBack(TokenType type, const std::string& data)
    {
        m_Tokens.emplace_back(type, data, m_File, m_LineNumber, m_ColumnNumber);
    }
}
