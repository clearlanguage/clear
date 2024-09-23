#pragma once

#include <vector>
#include <string>
#include <string_view>
#include <optional>

namespace alkhat
{
	enum class TokenType
	{
		None = 0, IntType, CharType, FloatType,  RValueNumber, RValueString,
		VariableName, StringType, EndOfStatement, Assignment
	};

	struct Token
	{
		TokenType TokenType = TokenType::None;
		std::optional<std::string> Data = std::nullopt;
	};

	struct ProgramInfo
	{
		std::vector<Token> Tokens;
	};

	class Tokenizer
	{
	public:
		enum class CurrentState
		{
			None = 0,
			ParsingVariable, 
			ParsingRValue, 
			ParsingArgumentsState, 
			ParsingAssignment
		};

	public:
		static ProgramInfo Tokenize(const std::vector<std::string>& words);
		static std::string_view TokenToString(TokenType token);
	};

}