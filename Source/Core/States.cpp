#include "States.h"

#include <string>
#include <string_view>
#include <iostream>
#include <map>

namespace clear
{

	static const std::map<std::string_view, TokenType> s_TokenMap = {
		{"int",    TokenType::IntType}, 
		{"string", TokenType::StringType}
	};

	static bool s_ParsingString = false;

	void States::NoneState(std::string& current, ProgramInfo& info, Tokenizer::CurrentState& state)
	{
		if (s_TokenMap.contains(current))
		{
			TokenType token = s_TokenMap.at(current);
			info.Tokens.push_back({ .TokenType = token });

			_SwitchState(state, token);
			current.clear();
		}
	}

	void States::VariableNameState(std::string& current, ProgramInfo& info, Tokenizer::CurrentState& state)
	{
		info.Tokens.push_back({ .TokenType = TokenType::VariableName, .Data = current });

		current.clear();
		state = Tokenizer::CurrentState::None;
	}

	void States::RValueState(std::string& current, ProgramInfo& info, Tokenizer::CurrentState& state)
	{
		size_t tokenIndex = info.Tokens.size() - 3;
		if(info.Tokens[tokenIndex].TokenType == TokenType::StringType)
			info.Tokens.push_back({ .TokenType = TokenType::RValueString, .Data = current });
		else 
			info.Tokens.push_back({ .TokenType = TokenType::RValueNumber, .Data = current });

		current.clear();
		state = Tokenizer::CurrentState::None;
	}

	void States::ParsingArguments(std::string& current, ProgramInfo& info, Tokenizer::CurrentState& state)
	{
	}

	void States::AssignmentState(std::string& current, ProgramInfo& info, Tokenizer::CurrentState& state)
	{
		info.Tokens.push_back({ .TokenType = TokenType::Assignment });

		current.clear();
		state = Tokenizer::CurrentState::ParsingRValue;
	}

	void States::_SwitchState(Tokenizer::CurrentState& state, TokenType token)
	{
		switch (token)
		{
			case TokenType::StringType:	  state = Tokenizer::CurrentState::ParsingVariable;	break;
			case TokenType::IntType:	  state = Tokenizer::CurrentState::ParsingVariable;	break;
			case TokenType::VariableName: state = Tokenizer::CurrentState::ParsingRValue;   break;
			case TokenType::RValueString:
			case TokenType::RValueNumber:
			case TokenType::None:
			default:
				state = Tokenizer::CurrentState::None;
				break;
		}
	}

}
