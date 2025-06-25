#include "Lexer.h"

#include <fstream>
#include <iostream>
#include <map>
#include <charconv>
#include <exception>
#include <math.h>

namespace clear 
{
    Lexer::Lexer(const std::filesystem::path& path)
    {
        std::fstream file(path);

        if(!file.is_open())
        {
            // HALT
            throw std::exception();
        }

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
            m_Tokens.push_back(Token(TokenType::EndScope, ""));
    }

    void Lexer::Eat()
    {
        if(Prev() == "\n")
        {
            m_Tokens.emplace_back(TokenType::EndLine, " ");
        }

        if(Prev() == "\n" && !IsLineOnlyWhitespace()) 
        {
            FlushScopes();
        }

        std::string top(1, m_Contents[m_Position]);

        if (std::isdigit(m_Contents[m_Position]))
        {
            EatNumber();
        }
        else if(IsAllowedCharacter(m_Contents[m_Position]))
        {
            EatWord();
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
        else if (std::isspace(m_Contents[m_Position]))
        {
            m_Position++;
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
            m_Tokens.emplace_back(TokenType::Keyword, word);
        }
        else 
        {
            m_Tokens.emplace_back(TokenType::Identifier, word);
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
            
            /* VERIFY !operator_.empty() */

            m_Tokens.emplace_back(g_OperatorMappings.at(operator_), operator_);
            i += operator_.size();
        }
    }

    void Lexer::EatPunctuator()
    {
        // punctuators guarenteed to be one size only for now

        std::string word(1, m_Contents[m_Position++]);

        if(g_PunctuatorMappings.contains(word))
        {
            m_Tokens.emplace_back(g_PunctuatorMappings.at(word), word);
        }
        else 
        {
            // UNREACHABLE("invalid punctuator", word)
        }   
    }

    void Lexer::EatString()
    {
        size_t start = m_Position;
        m_Position++;

        while(m_Position < m_Contents.size() && m_Contents[m_Position] != '"') 
            m_Position++;
        
        m_Tokens.emplace_back(TokenType::String, m_Contents.substr(start+1, m_Position - start - 1));
        m_Position++;
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
                   !std::isspace(m_Contents[m_Position]) && 
                   (std::isdigit(m_Contents[m_Position]) || 
                    m_Contents[m_Position] == '.');
        };

        std::string word = GetWord(ShouldContinue);

        auto [number, isNumber] = GetNumber(word);

        /* VERIFY isNumber lexer error*/
        m_Tokens.emplace_back(TokenType::Number, word);
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
				m_Position++;
			}
			else if (Peak() == " ")
			{
				totalSpaces++;
                m_Position++;
			}
			else
			{
				indenting = false;
			}
		}

		size_t localIndents = totalSpaces / 4;

        while (m_Indents > localIndents)
		{
            m_Tokens.push_back(Token(TokenType::EndScope, ""));
			m_Indents--;
		}

        m_Indents = localIndents;
    }

    void Lexer::EatHex()
    {
        m_Position += 2; // skip the 0x

        auto ShouldContinue = [&]()
        {
            bool isDigit = std::isdigit(m_Contents[m_Position]);
            bool isValidCap   = m_Contents[m_Position] >= 'A' && m_Contents[m_Position] <= 'F';
            bool isValidLower = m_Contents[m_Position] >= 'a' && m_Contents[m_Position] <= 'f';

            return  m_Position < m_Contents.size() && (isDigit || isValidCap || isValidLower);
        };

        std::string word = GetWord(ShouldContinue);

        size_t k = 0;
        size_t num = 0;

        for (auto it = word.rbegin(); it != word.rend(); it++) 
        {
            size_t digit = 0;

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

        m_Tokens.emplace_back(TokenType::Number, std::to_string(num));
    }

    void Lexer::EatBin()
    {
        m_Position += 2; // skip the 0b

        auto ShouldContinue = [&]()
        {
            return  m_Position < m_Contents.size() && (m_Contents[m_Position] == '0' || m_Contents[m_Position] == '1');
        };

        std::string word = GetWord(ShouldContinue);

        size_t k = 0;
        size_t num = 0;

        for (auto it = word.rbegin(); it != word.rend(); it++) 
        {
            size_t digit = *it - '0';
            num += digit * std::pow(2, k++);
        }

        m_Tokens.emplace_back(TokenType::Number, std::to_string(num));
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

            m_Position++;
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
        auto [ptr, ec] = std::from_chars(string.data(), string.data() + string.size(), value);
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
}
