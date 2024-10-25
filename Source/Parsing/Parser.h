#pragma once

#include "Tokens.h"
#include "Errors.h"

#include <vector>
#include <string>
#include <string_view>
#include <fstream>
#include <filesystem>
#include <map>
#include <functional>
#include <queue>

namespace clear
{
	struct ProgramInfo
	{
		std::vector<Token> Tokens;
		std::vector<Error> Errors;

	};

	struct ArrayDeclarationReturn 
	{
		std::vector<Token> Tokens;
		bool error = false;
		std::string errormsg;
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
		bool IsSubParser = false;

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
		void _FunctionParamaterState();
		void _MultiLineCommentState();
		void _IndexOperatorState();
		void _AsterisksState();


		Token _GetLastToken();
		Token _CreateToken(const TokenType tok, const std::string& data);

		void _VerifyCondition(bool condition,std::string Error, std::string Advice,std::string ErrorType,std::string Cause);
		void _RaiseError(Error& err);
		Error _CreateError(std::string& Error, std::string& Advice,std::string& ErrorType,std::string& Cause);

		ProgramInfo _SubParse(std::string arg);

		void _ParsingRValueState();
		void _ParseNumber();
		void _ParseString();
		void _ParseOther();
		void _ParseChar();
		void _ParseBinaryLiteral();
		void _ParseHexLiteral();
		void _ParseList();
		std::vector<std::string> _ParseBrackets(char end,bool commas);

		void _ParseArrayDecleration(ArrayDeclarationReturn& output);
		int _ParsePointerDecleration();

		void _PushToken(const TokenType tok, const std::string& data);
		bool _IsLineClosed();
		char _SkipSpaces();

		char _GetNextChar();
		void _Backtrack();
		void _EndLine();


	private:
		size_t m_CurrentTokenIndex = 0;
		size_t m_CurrentIndentLevel = 0;
		size_t m_CurrentIndentationLevel = 0;
		size_t m_Indents = 0;
		size_t m_TokenIndexStart = 0;
		size_t m_CurrentLine = 1;
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