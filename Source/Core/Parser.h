#pragma once

#include <vector>
#include <string>
#include <string_view>
#include <fstream>
#include <filesystem>
#include <map>
#include <functional>
#include "Tokens.h"
#include <queue>

namespace clear
{
	struct ProgramInfo
	{
		std::vector<Token> Tokens;
	};

	class Parser
	{
	public:
		using StateMapType    = std::map<ParserState, std::function<void()>>;

	public:
		Parser();
		~Parser() = default;

		ProgramInfo CreateTokensFromFile(const std::filesystem::path& path);
		void InitParser();
		ProgramInfo ParseProgram();

	private:
		void _DefaultState();
		void _VariableNameState();
		void _OperatorState();
		void _IndentationState();
		void _FunctionNameState();
		void _FunctionParameterState();
		void _ArrowState();
		void _FunctionTypeState();
		void _CommentState();
		void _StructNameState();
		void _FunctionParamterState();
		void _MultiLineCommentState();
		void _IndexOperatorState();



		void _ParsingRValueState();
		void _ParseNumber();
		void _ParseString();
		void _ParseOther();
		void _ParseChar();

		void _PushToken(const TokenType tok, const std::string& data);
		bool _IsLineClosed() const;

		char _GetNextChar();
		void _Backtrack();
		void _EndLine();

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

		ParserState m_CurrentState = ParserState::Default;

		std::string m_Buffer;
		std::string m_CurrentString;
		std::vector<char> m_BracketStack;

	};

}