#pragma once

#include <vector>
#include <string>
#include <string_view>
#include <fstream>
#include <filesystem>
#include <map>
#include <functional>
#include "Tokens.h"

namespace clear
{
	struct ProgramInfo
	{
		std::vector<Token> Tokens;
	};

	class Parser
	{
	public:
		using StateMapType    = std::map<CurrentParserState, std::function<void()>>;

	public:
		Parser();
		~Parser() = default;

		ProgramInfo CreateTokensFromFile(const std::filesystem::path& path);

	private:
		void _DefaultState();
		void _VariableNameState();
		void _OperatorState();
		void _IndentationState();
		void _FunctionNameState();
		void _FunctionArgumentState();
		void _ArrowState();
		void _FunctionTypeState();

		void _ParsingRValueState();
		void _ParseNumber();
		void _ParseString();
		void _ParseOther();


		char _GetNextChar();
		void _Backtrack();

		const bool _IsEndOfFile();

	private:
		size_t m_CurrentTokenIndex = 0;
		size_t m_CurrentIndentLevel = 0;
		size_t m_CurrentIndentationLevel = 0;
		size_t m_Indents = 0;
		bool m_LineStarted = false;
		std::ifstream m_File;
		ProgramInfo m_ProgramInfo;

		StateMapType m_StateMap;

		CurrentParserState m_CurrentState = CurrentParserState::Default;

		std::string m_Buffer;
		std::string m_CurrentString;
	};

}