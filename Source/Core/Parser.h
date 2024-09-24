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
		None = 0, Int8Type, Int16Type, Int32Type, Int64Type, UInt8Type, UInt16Type, UInt32Type, UInt64Type, 
		Bool, Float32Type, Float64Type, RValueNumber, RValueString, VariableName, StringType, 
		Assignment, MulOp, AddOp, DivOp, SubOp, ModOp, OpenBracket, CloseBracket
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
		void _Backtrack();

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