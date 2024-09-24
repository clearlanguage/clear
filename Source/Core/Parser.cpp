#include "Parser.h"

#include <functional>
#include <algorithm>
#include <map>
#include <fstream>
#include <iostream>

namespace clear {

	std::string_view TokenToString(TokenType token)
	{
		switch (token)
		{
			case TokenType::None:			    return "None";
			case TokenType::IntType:		    return "IntType";
			case TokenType::StringType:			return "StringType";
			case TokenType::CharType:		    return "CharType";
			case TokenType::FloatType:		    return "FloatType";
			case TokenType::VariableName:		return "VariableName";
			case TokenType::EndOfStatement:		return "EndOfStatement";
			case TokenType::Assignment:			return "Assignment";
			case TokenType::RValueNumber:		return "RValueNumber";
			case TokenType::RValueString:		return "RValueString";
			default:
				break;
		}

		return "";
	}

	Parser::Parser()
	{
		m_StateMap[CurrentParserState::Default]      = [this]() { _DefaultState(); };
		m_StateMap[CurrentParserState::VariableName] = [this]() { _VariableNameState(); };
		m_StateMap[CurrentParserState::RValue]       = [this]() { _ParsingRValueState(); };

		m_OperatorMap['=']  = { .NextState = CurrentParserState::RValue, .TokenToPush = TokenType::Assignment};
		m_OperatorMap['"']  = { .NextState = CurrentParserState::String, .TokenToPush = TokenType::None }; //token will be pushed by string state

		m_KeyWordMap["int"]    = { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::IntType };
		m_KeyWordMap["string"] = { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::StringType };
	}

	char Parser::_GetNextChar()
	{
		return m_Buffer[m_CurrentTokenIndex++];
	}

	ProgramInfo Parser::CreateTokensFromFile(const std::filesystem::path& path)
	{
		m_ProgramInfo.Tokens.clear();
		m_CurrentTokenIndex = 0;
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

		return m_ProgramInfo;
	}


	void Parser::_DefaultState()
	{
		char current = _GetNextChar();

		if (std::isspace(current))
			return;

		m_CurrentString += current;

		if (m_CurrentString.size() == 1 && m_OperatorMap.contains(current))
		{
			auto& value = m_OperatorMap.at(current);

			m_CurrentState = value.NextState;

			if (value.TokenToPush != TokenType::None)
				m_ProgramInfo.Tokens.push_back({ .TokenType = value.TokenToPush, .Data = m_CurrentString });
			
			m_CurrentString.clear();
			return;
		}

		if (m_KeyWordMap.contains(m_CurrentString))
		{
			auto& value = m_KeyWordMap.at(m_CurrentString);


			m_CurrentState = value.NextState;

			if (value.TokenToPush != TokenType::None)
				m_ProgramInfo.Tokens.push_back({ .TokenType = value.TokenToPush, .Data = m_CurrentString });

			m_CurrentString.clear();
			return;
		}


	}

	void Parser::_ParsingRValueState()
	{
		char current = _GetNextChar();

		//want to ignore all spaces in between = and actual variable
		while (std::isspace(current))
			current = _GetNextChar();

		m_CurrentString.clear();

		if (current == '"')
		{
			_ParseString();
		}
		else if (std::isdigit(current) || current == '-') // OR negative numbers
		{
			m_CurrentString.push_back(current);
			_ParseNumber();
		}
		else if(false) //TODO: implement this later
		{
			//could be a variable reference, class/struct reference etc...
			m_CurrentString.push_back(current);
			_ParseOther();
		}

		m_CurrentState = CurrentParserState::Default;
	}

	void Parser::_VariableNameState()
	{
		char current = _GetNextChar();
		std::cout << current << std::endl;

		//want to ignore all spaces in between type and variable
		while (std::isspace(current)) 
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

	void Parser::_ParseNumber()
	{
		char current = _GetNextChar();

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
	}

	void Parser::_ParseString()
	{
		char current = _GetNextChar();

		while (current != '"')
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
	}
}