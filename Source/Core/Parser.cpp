#include "Parser.h"
#include <sstream>
#include <functional>
#include <algorithm>
#include <map>
#include <fstream>
#include <iostream>
#include <iosfwd>


std::string str(char hello){
	return std::string(1,hello);
}

template <typename... Args>
void print(Args &&...args)
{
	(std::cout << ... << std::forward<Args>(args));
	std::cout << std::endl;
}
	namespace clear
{

	std::string_view TokenToString(TokenType token)
	{
		switch (token)
		{
			case TokenType::None:			return "None";
			case TokenType::Int8Type:		return "Int8Type";
			case TokenType::Int16Type:		return "Int16Type";
			case TokenType::Int32Type:		return "Int32Type";
			case TokenType::Int64Type:		return "Int64Type";
			case TokenType::UInt8Type:		return "UInt8Type";
			case TokenType::UInt16Type:		return "UInt16Type";
			case TokenType::UInt32Type:		return "UInt32Type";
			case TokenType::UInt64Type:		return "UInt64Type";
			case TokenType::Float32Type:	return "Float32Type";
			case TokenType::Float64Type:	return "Float64Type";
			case TokenType::StringType:		return "StringType";
			case TokenType::VariableName:	return "VariableName";
			case TokenType::Assignment:		return "Assignment";
			case TokenType::RValueNumber:	return "RValueNumber";
			case TokenType::RValueString:	return "RValueString";
			case TokenType::MulOp:			return "MulOp";
			case TokenType::AddOp:			return "AddOp";
			case TokenType::DivOp:			return "DivOp";
			case TokenType::SubOp:			return "SubOp";
			case TokenType::Bool:			return "Bool";
			case TokenType::CloseBracket:	return "CloseBracket";
			case TokenType::OpenBracket:	return "OpenBracket";
			case TokenType::BooleanData:	return "BooleanData";
			case TokenType::ConditionalIf:  return "ConditionalIf";
			case TokenType::IsEqual:		return "IsEqual";
			case TokenType::Null:           return "Null";
			case TokenType::NotEqual:       return "NotEqual";
			case TokenType::GreaterThan:	return "GreaterThan";
			case TokenType::GreaterThanEqual:return "GreaterThanEqual";
			case TokenType::LessThanEqual:	return "LessThanEqual";
			case TokenType::LessThan:		return "LessThan";
			case TokenType::Not:			return "Not";
			case TokenType::Ellipsis:		return "Ellipsis";
			case TokenType::DotOp: 			return "DotOp";

			default: break;
		}

		return "";
	}

	Parser::Parser()
	{
		m_StateMap[CurrentParserState::Default]      = [this]() { _DefaultState(); };
		m_StateMap[CurrentParserState::VariableName] = [this]() { _VariableNameState(); };
		m_StateMap[CurrentParserState::RValue]       = [this]() { _ParsingRValueState(); };
		m_StateMap[CurrentParserState::Operator] = [this](){ _OperatorState(); };

		m_OperatorMap["="] = { .NextState = CurrentParserState::Operator,  .TokenToPush = TokenType::Assignment};
		m_OperatorMap["*"] = {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::MulOp};
		m_OperatorMap["+"] = {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::AddOp};
		m_OperatorMap["/"] = {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::DivOp};
		m_OperatorMap["-"] = {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::SubOp};
		m_OperatorMap["%"] = {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::ModOp};
		m_OperatorMap["=="] = {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::IsEqual};
		m_OperatorMap["<"] = {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::LessThan};
		m_OperatorMap[">"] = {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::GreaterThan};
		m_OperatorMap["!="] = {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::NotEqual};
		m_OperatorMap["<="] = {.NextState = CurrentParserState::Operator,.TokenToPush = TokenType::LessThanEqual};
		m_OperatorMap[">="] = {.NextState =CurrentParserState::Operator,.TokenToPush = TokenType::GreaterThanEqual};
		m_OperatorMap["..."] = {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::Ellipsis};
		m_OperatorMap["."] = {.NextState = CurrentParserState::Operator,.TokenToPush = TokenType::DotOp};

		m_OperatorMap["!"] = {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::Not};

		m_KeyWordMap["int8"]	= { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::Int8Type };
		m_KeyWordMap["int16"]	= { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::Int16Type };
		m_KeyWordMap["int32"]	= { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::Int32Type };
		m_KeyWordMap["int64"]	= { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::Int64Type };
		m_KeyWordMap["int"] = m_KeyWordMap["int32"];

		m_KeyWordMap["uint8"]	= { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::UInt8Type };
		m_KeyWordMap["uint16"]	= { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::UInt16Type };
		m_KeyWordMap["uint32"]	= { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::UInt32Type };
		m_KeyWordMap["uint64"]	= { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::UInt64Type };
		m_KeyWordMap["uint"] = m_KeyWordMap["uint32"];

		m_KeyWordMap["string"]	= { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::StringType };
		m_KeyWordMap["bool"]	= { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::Bool };

		m_KeyWordMap["float32"] = { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::Float32Type };
		m_KeyWordMap["float64"] = { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::Float64Type };

		m_KeyWordMap["false"] = {.NextState = CurrentParserState::Default, .TokenToPush = TokenType::BooleanData};
		m_KeyWordMap["true"] =  {.NextState = CurrentParserState::Default, .TokenToPush = TokenType::BooleanData};
		m_KeyWordMap["null"] = {.NextState = CurrentParserState::Default, .TokenToPush = TokenType::Null};

		m_KeyWordMap["if"] = {.NextState = CurrentParserState::RValue, .TokenToPush = TokenType::ConditionalIf};
	}

	char Parser::_GetNextChar()
	{
		if(m_Buffer.length() > m_CurrentTokenIndex)
			return m_Buffer[m_CurrentTokenIndex++];

		return 0;
	}

	void Parser::_Backtrack()
	{
		m_CurrentTokenIndex--;
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

		if (current == ')')
		{
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::CloseBracket, .Data = ")"});
			return;
		}

		m_CurrentString += current;

		if (m_KeyWordMap.contains(m_CurrentString))
		{
			auto& value = m_KeyWordMap.at(m_CurrentString);


			m_CurrentState = value.NextState;

			if (value.TokenToPush != TokenType::None)
				m_ProgramInfo.Tokens.push_back({ .TokenType = value.TokenToPush, .Data = m_CurrentString });

			m_CurrentString.clear();
			return;
		}

		if (m_CurrentString.size() == 1 && m_OperatorMap.contains(str(current)))
		{
			auto& value = m_OperatorMap.at(str(current));

			m_CurrentState = value.NextState;

			// if (value.TokenToPush != TokenType::None)
			// 	m_ProgramInfo.Tokens.push_back({ .TokenType = value.TokenToPush, .Data = m_CurrentString });
			
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

	void Parser::_OperatorState() {
		_Backtrack();
		std::string before = str(_GetNextChar());
		std::string h = before;
		char current  = before[0];
		while (m_OperatorMap.contains(str(current))) {
			current = _GetNextChar();
			if (!m_OperatorMap.contains(str(current))) {
				break;
			};
			h+=current;
		}
		print(h);
		ParserMapValue value;
		std::string data;
		_Backtrack();
		if (m_OperatorMap.contains(h)){
			value = m_OperatorMap.at(h);
			data = h;
		}else {
			value = m_OperatorMap.at(before);
			data = before;
		}
		if (value.TokenToPush != TokenType::None)
			m_ProgramInfo.Tokens.push_back({ .TokenType = value.TokenToPush, .Data = data });
		// m_CurrentState = value.NextState;
		m_CurrentState = CurrentParserState::Default;
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
			if (current == '\n' || std::isspace(current))
				break;

			m_CurrentString += current;
		}

		if (m_KeyWordMap.contains(m_CurrentString))
		{
			auto& value = m_KeyWordMap.at(m_CurrentString);
			m_ProgramInfo.Tokens.push_back({ .TokenType = value.TokenToPush, .Data = m_CurrentString });
		}

		m_CurrentString.clear();
		m_CurrentState = CurrentParserState::Default;
	}
}