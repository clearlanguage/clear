#pragma once

#include <vector>
#include <string>
#include <string_view>
#include <fstream>
#include <filesystem>
#include <map>
#include <functional>

namespace clear
{
	enum class TokenType
	{
		None = 0, IntType, CharType, FloatType,  RValueNumber, RValueString,
		VariableName, StringType, EndOfStatement, Assignment
	};

	struct Token
	{
		TokenType TokenType = TokenType::None;
		std::string Data = "";
	};

	struct ProgramInfo
	{
		std::vector<Token> Tokens;
	};

	enum class CurrentParserState
	{
		Default = 0, 
		RValue,
		VariableName, 
		Number, 
		String,
		Function
	};

	std::string_view TokenToString(TokenType token);

	struct ParserMapValue
	{
		CurrentParserState NextState = CurrentParserState::Default;
		TokenType TokenToPush = TokenType::None;
	};

	class Parser
	{
	public:
		using StateMapType    = std::map<CurrentParserState, std::function<void()>>;
		using OperatorMapType = std::map<char, ParserMapValue>;
		using KeyWordMapType  = std::map<std::string, ParserMapValue>;

	public:
		Parser();
		~Parser() = default;

		ProgramInfo CreateTokensFromFile(const std::filesystem::path& path);

	private:
		void _DefaultState();
		void _VariableNameState();

		void _ParsingRValueState();
		void _ParseNumber();
		void _ParseString();
		void _ParseOther();

		char _GetNextChar();


	private:
		size_t m_CurrentTokenIndex = 0;
		std::ifstream m_File;
		ProgramInfo m_ProgramInfo;

		StateMapType m_StateMap;
		OperatorMapType m_OperatorMap;
		KeyWordMapType m_KeyWordMap;

		CurrentParserState m_CurrentState = CurrentParserState::Default;

		std::string m_Buffer;
		std::string m_CurrentString;
	};

}