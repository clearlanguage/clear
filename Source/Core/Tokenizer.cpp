#include "Tokenizer.h"

#include "States.h"

#include <functional>
#include <algorithm>
#include <map>

namespace alkhat {

	using StateMapType    = std::map<Tokenizer::CurrentState, std::function<void(std::string&, ProgramInfo&, Tokenizer::CurrentState&)>>;
	using OperatorMapType = std::map<char, Tokenizer::CurrentState>;

	static const StateMapType s_StateMap = {
		{Tokenizer::CurrentState::None,					  States::NoneState},
		{Tokenizer::CurrentState::ParsingRValue,		  States::RValueState},
		{Tokenizer::CurrentState::ParsingVariable,		  States::VariableNameState},
		{Tokenizer::CurrentState::ParsingArgumentsState,  States::ParsingArguments}, 
		{Tokenizer::CurrentState::ParsingAssignment,	  States::AssignmentState}
	};

	static const OperatorMapType s_OperatorMap = {
		{'=', Tokenizer::CurrentState::ParsingAssignment}
	};

	static Tokenizer::CurrentState s_CurrentState = Tokenizer::CurrentState::None;

	ProgramInfo Tokenizer::Tokenize(const std::vector<std::string>& words)
	{
		ProgramInfo info{};

		std::string current = "";

		for (const auto& line : words)
		{
			for (size_t i = 0; i < line.length(); i++)
			{
				if (std::isspace(line[i]) && s_CurrentState != Tokenizer::CurrentState::ParsingRValue)
				{
					continue;
				}
				else if (std::isalnum(line[i]))
				{
					current.clear();

					while (i < line.length() && std::isalnum(line[i]))
					{
						current += line[i];
						i++;
					}
				}
				else if (s_OperatorMap.contains(line[i]))
				{
					current.clear();
					current += line[i];
					s_CurrentState = s_OperatorMap.at(line[i]);
				}
				else
				{
					continue;
				}
				
				s_StateMap.at(s_CurrentState)(current, info, s_CurrentState);
			}


			info.Tokens.push_back({.TokenType = TokenType::EndOfStatement });
		}
		
		return info;
	}

	std::string_view Tokenizer::TokenToString(TokenType token)
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

}