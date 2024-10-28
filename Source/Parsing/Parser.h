#pragma once

#include "Tokens.h"
#include "Errors.h"

#include <vector>
#include <string>
#include <string_view>
#include <fstream>
#include <filesystem>
#include <map>
#include <concepts>
#include <functional>
#include <queue>

namespace clear
{
	template<typename... Args>
	concept RestrictToString = ((std::convertible_to<Args, std::string> || std::convertible_to<Args, const char*> || std::convertible_to<Args, char>) && ...);

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
		std::string advice;
		int lastIndex = -1;
	};

	struct TypeScope 
	{
		std::set<std::string> TypeDeclarations;
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
		void _FunctionArgumentState();
		void _MultiLineCommentState();
		void _IndexOperatorState();
		void _AsterisksState();


		Token _GetLastToken();
		Token _CreateToken(const TokenType tok, const std::string& data);



		template<typename ...Args>
		void _VerifyCondition(bool condition, int ErrorNumber, int startIndex, int endIndex, Args&&... args) requires RestrictToString<Args...>
		{
			ErrorNumber--;
			ErrorReference err = g_ErrorsReference.at(ErrorNumber);
			std::string msg = std::vformat(err.ErrorMessage, std::make_format_args(args...));
			_VerifyCondition(condition, msg, err.Advice, err.ErrorType, startIndex, endIndex);
		}

		template<typename ...Args>
		void _VerifyCondition(bool condition, int ErrorNumber, int startIndex, Args&&... args) requires RestrictToString<Args...>
		{
			ErrorNumber--;
			ErrorReference err = g_ErrorsReference.at(ErrorNumber);
			std::string msg = std::vformat(err.ErrorMessage, std::make_format_args(args...));
			_VerifyCondition(condition, msg, err.Advice, err.ErrorType, startIndex);
		}

		template<typename ...Args>
		void _VerifyCondition(bool condition, int ErrorNumber, Args&&... args) requires RestrictToString<Args...>
		{
			ErrorNumber--;
			ErrorReference err = g_ErrorsReference.at(ErrorNumber);
			std::string msg = std::vformat(err.ErrorMessage, std::make_format_args(args...));
			_VerifyCondition(condition, msg, err.Advice, err.ErrorType);
		}

		void _VerifyCondition(bool condition,std::string Error, std::string Advice,std::string ErrorType);
		void _VerifyCondition(bool condition,std::string Error, std::string Advice,std::string ErrorType,int startIndex,int endIndex);
		void _VerifyCondition(bool condition,std::string Error, std::string Advice,std::string ErrorType,int startIndex);


		void _RaiseError(Error& err);
		Error _CreateError(std::string& Error, std::string& Advice,std::string& ErrorType);

		ProgramInfo _SubParse(std::string arg);

		bool _IsTypeDeclared(const std::string& type);

		void _ParsingRValueState();
		void _ParseNumber();
		void _ParseString();
		void _ParseOther();
		void _ParseChar();
		void _ParseBinaryLiteral();
		void _ParseHexLiteral();
		void _ParseList();
		std::vector<std::string> _ParseBrackets(char end,bool commas);

		void _ParseArrayDeclaration(ArrayDeclarationReturn& output);
		int _ParsePointerDeclaration();

		void _PushToken(const TokenType tok, const std::string& data);
		bool _IsLineClosed();
		bool _IsEndOfLine();
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
		std::vector<TypeScope> m_ScopeStack;

	};

}