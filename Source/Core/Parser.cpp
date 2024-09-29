#include "Parser.h"
#include <sstream>
#include <functional>
#include <algorithm>
#include <map>
#include <fstream>
#include <iostream>
#include <iosfwd>
#include <Core/Log.h>
#include <Core/Utils.h>


namespace clear
{
	Parser::Parser()
	{
		m_StateMap[CurrentParserState::Default]      = [this]() { _DefaultState(); };
		m_StateMap[CurrentParserState::VariableName] = [this]() { _VariableNameState(); };
		m_StateMap[CurrentParserState::RValue]       = [this]() { _ParsingRValueState(); };
		m_StateMap[CurrentParserState::Operator]     = [this]() { _OperatorState(); };
		m_StateMap[CurrentParserState::Indentation]  = [this]() { _IndentationState(); };
	}

	char Parser::_GetNextChar()
	{
		if(m_Buffer.length() > m_CurrentTokenIndex)
		{
			auto c = m_Buffer[m_CurrentTokenIndex++];
			return c;
		}

		return 0;
	}

	void Parser::_Backtrack()
	{
		m_CurrentTokenIndex--;
	}

	const bool Parser::_IsEndOfFile()
	{
		return m_CurrentTokenIndex == m_Buffer.length();
	}

	ProgramInfo Parser::CreateTokensFromFile(const std::filesystem::path& path)
	{
		m_ProgramInfo.Tokens.clear();
		m_CurrentTokenIndex = 0;
		m_Indents = 0;
		m_CurrentIndentLevel = 0;
		m_CurrentIndentationLevel = 0;
		m_LineStarted = false;
		m_CurrentState = CurrentParserState::Default;
		m_Buffer.clear();
		m_CurrentString.clear();

		m_File.open(path);

		if (!m_File.is_open())
		{
			std::cout << "failed to open file " << path << std::endl;
			return m_ProgramInfo;
		}

		std::stringstream stream;
		stream << m_File.rdbuf();

		m_Buffer = stream.str();

		while (m_CurrentTokenIndex < m_Buffer.length())
		{
			m_StateMap.at(m_CurrentState)();
		}

		while (m_Indents > 0)
		{
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::EndIndentation });
			m_Indents--;
		}

		return m_ProgramInfo;
	}


	void Parser::_DefaultState()
	{
		char current = _GetNextChar();
		
		if (current == ')')
		{
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::CloseBracket, .Data = ")"});
			return;
		}
		if (current == '\n' || current == ' ')
		{
			m_CurrentState = CurrentParserState::Indentation;
			m_CurrentString.clear();
			return;
		}
		else if (current == '\n')
		{
			//TODO: TEMPORARY
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::EndLine });
		}

		

		m_CurrentString += current;

		if (s_KeyWordMap.contains(m_CurrentString))
		{
			auto& value = s_KeyWordMap.at(m_CurrentString);


			m_CurrentState = value.NextState;

			if (value.TokenToPush != TokenType::None)
				m_ProgramInfo.Tokens.push_back({ .TokenType = value.TokenToPush, .Data = m_CurrentString });

			m_CurrentString.clear();
			return;
		}

		if (m_CurrentString.size() == 1 && s_OperatorMap.contains(str(current)))
		{
			auto& value = s_OperatorMap.at(str(current));

			m_CurrentState = CurrentParserState::Operator;			
			m_CurrentString.clear();

			return;
		}
	}

	void Parser::_ParsingRValueState()
	{
		char current = _GetNextChar();

		//want to ignore all spaces in between = and actual variable
		while (isspace(current))
			current = _GetNextChar();

		m_CurrentString.clear();

		//brackets
		if (current == '(')
		{
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::OpenBracket, .Data = "(" });
			m_CurrentState = CurrentParserState::RValue;
			return;
		}
		else if (current == ')')
		{
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::CloseBracket, .Data = ")" });
			m_CurrentState = CurrentParserState::RValue;
			return;
		}
		else if (current == '"') //strings
		{
			_ParseString();
		}
		else if (std::isdigit(current) || current == '-') // postive/negative numbers
		{
			m_CurrentString.push_back(current);
			_ParseNumber();
		}
		else if(true) //TODO: implement this later
		{
			//could be a variable reference, class/struct reference etc...
			m_CurrentString.push_back(current);
			_Backtrack();
			_ParseOther();
		}

		m_CurrentState = CurrentParserState::Default;
	}

	void Parser::_VariableNameState()
	{
		char current = _GetNextChar();

		//want to ignore all spaces in between type and variable
		while (isspace(current))
			current = _GetNextChar();

		m_CurrentString.clear();

		//allow _ and any character from alphabet
		while (std::isalpha(current) || current == '_') 
		{
			m_CurrentString += current;
			current = _GetNextChar();
		}

		m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::VariableName, .Data = m_CurrentString });
		m_CurrentString.clear();

		m_CurrentState = CurrentParserState::Default;
	}

	void Parser::_OperatorState() 
	{
		_Backtrack();
		std::string before = str(_GetNextChar());
		std::string h = before;
		char current  = before[0];
		while (s_OperatorMap.contains(str(current))) 
		{
			current = _GetNextChar();

			if (!s_OperatorMap.contains(str(current))) 
				break;

			h+=current;
		}

		ParserMapValue value;
		std::string data;
		_Backtrack();

		if (s_OperatorMap.contains(h))
		{
			value = s_OperatorMap.at(h);
			data = h;
		}
		else 
		{
			value = s_OperatorMap.at(before);
			data = before;
		}
		if (value.TokenToPush != TokenType::None)
			m_ProgramInfo.Tokens.push_back({ .TokenType = value.TokenToPush, .Data = data });

		m_CurrentState = value.NextState;
	}

	void Parser::_IndentationState()
	{
		char next = _GetNextChar();

		bool indenting = true;
		size_t localIndents = 0;

		while (indenting)
		{
			if (next == '\t')
			{
				localIndents++;
				continue;
			}
			
			size_t spaceCounter = 0;

			if (next == ' ')
			{
				spaceCounter = 1;

				while (next == ' ' && spaceCounter < 4)
				{
					next = _GetNextChar();
					spaceCounter++;
				}
			}
			
			if (spaceCounter == 4)
				localIndents++;
			else
				indenting = false;
		}

		if (localIndents > m_Indents)
		{
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::StartIndentation });
			m_Indents = localIndents;
		}

		while (m_Indents > localIndents)
		{
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::EndIndentation });
			m_Indents--;
		}

		m_CurrentState = CurrentParserState::Default;
		_Backtrack();
	}

	void Parser::_ParseNumber()
	{
		char current = _GetNextChar();

		if (current == '\0')
		{
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::RValueNumber, .Data = m_CurrentString });
			m_CurrentString.clear();
			return;
		}

		bool usedDecimal = false;

		while (true)
		{
			if (std::isdigit(current))
			{
				m_CurrentString.push_back(current);
			}
			else if (current == '.' && usedDecimal) // need to throw some type of error again TODO
			{
				std::cout << "cannot have two decimal points" << std::endl;
				break;
			}
			else if (current == '.')
			{
				m_CurrentString.push_back(current);
				usedDecimal = true;
			}
			else
			{
				break;
			}

			current = _GetNextChar();
		}

		m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::RValueNumber, .Data = m_CurrentString });
		m_CurrentString.clear();
		_Backtrack();
	}

	void Parser::_ParseString()
	{
		char current = _GetNextChar();

		while (current != '"' && current != '\0')
		{
			//may want to add raw strings to allow these
			if (current == '\n')
			{
				current = _GetNextChar();
				continue;
			}

			m_CurrentString += current;
			current = _GetNextChar();
		}

		m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::RValueString, .Data = m_CurrentString });
		m_CurrentString.clear();
	}

	void Parser::_ParseOther()
	{
		char current = _GetNextChar();
		m_CurrentString.clear();
		m_CurrentString += current;
		
		while ((std::isalnum(current) || current == '_' || current == '.') && current)
		{
			current = _GetNextChar();
			if (current == '\n' || current == '\0' || isspace(current))
				break;

			m_CurrentString += current;
		}

		if (s_KeyWordMap.contains(m_CurrentString))
		{
			auto& value = s_KeyWordMap.at(m_CurrentString);
			m_ProgramInfo.Tokens.push_back({ .TokenType = value.TokenToPush, .Data = m_CurrentString });
		}

		m_CurrentString.clear();
		m_CurrentState = CurrentParserState::Default;
	}
}