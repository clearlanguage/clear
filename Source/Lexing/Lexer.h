#pragma once

#include "Tokens.h"
#include "Errors.h"

#include <vector>
#include <string>
#include <fstream>
#include <filesystem>
#include <map>
#include <concepts>
#include <functional>
#include <format>

namespace clear
{
	template<typename... Args>
	concept RestrictToString = ((std::convertible_to<Args, std::string> || std::convertible_to<Args, const char*> || std::same_as<Args, char>) && ...);

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

	struct BracketParsingReturn 
	{
		std::vector<std::string> tokens;
		std::vector<int> indexes;
	};

	struct TypeScope
	{
		std::set<std::string> TypeDeclarations;
		std::set<std::string> RestrictionDeclarations;
	};

	class Lexer
	{
	public:
		using StateMapType    = std::map<LexerState, std::function<void()>>;

	public:
		Lexer();
		~Lexer() = default;

		ProgramInfo CreateTokensFromFile(const std::filesystem::path& path);
		void InitLexer();
		ProgramInfo ParseProgram();
		std::string m_CurrentErrorState;
		bool IsSubLexer = false;

	private:
		void DefaultState();
		void VariableNameState();
		void OperatorState();
		void IndentationState();
		void FunctionNameState();
		void FunctionParameterState();
		void ArrowState();
		void FunctionTypeState();
		void CommentState();
		void StructNameState();
		void FunctionArgumentState();
		void MultiLineCommentState();
		void IndexOperatorState();
		void AsterisksState();
		void MinusOperator();
		void DeclarationState();
		void IncrementOperator();
		void RestrictionState();
		void DotOpState();


		Token GetLastToken();
		Token GetLastToken(size_t x);
		Token CreateToken(const TokenType tok, const std::string& data);



		template<typename ...Args>
		void VerifyCondition(bool condition, int ErrorNumber, int startIndex, int endIndex, Args&&... args) requires RestrictToString<Args...>
		{
			if (condition) return;
			ErrorNumber--;
			ErrorReference err = g_ErrorsReference.at(ErrorNumber);
			err.ErrorMessage = std::vformat(err.ErrorMessage, std::make_format_args(args...));
			err.Advice =  std::vformat(err.Advice, std::make_format_args(args...));
			err.ErrorType = std::vformat(err.ErrorType, std::make_format_args(args...));
			VerifyCondition(condition, err.ErrorMessage, err.Advice, err.ErrorType, startIndex, endIndex);
		}

		template<typename ...Args>
		void VerifyCondition(bool condition, int ErrorNumber, int startIndex, Args&&... args) requires RestrictToString<Args...>
		{
			if (condition) return;
			ErrorNumber--;
			ErrorReference err = g_ErrorsReference.at(ErrorNumber);
			err.ErrorMessage = std::vformat(err.ErrorMessage, std::make_format_args(args...));
			err.Advice =  std::vformat(err.Advice, std::make_format_args(args...));
			err.ErrorType = std::vformat(err.ErrorType, std::make_format_args(args...));
			VerifyCondition(condition, err.ErrorMessage, err.Advice, err.ErrorType, startIndex);
		}

		template<typename ...Args>
		void VerifyCondition(bool condition, int ErrorNumber, Args&&... args) requires RestrictToString<Args...>
		{
			if (condition) return;
			ErrorNumber--;
			ErrorReference err = g_ErrorsReference.at(ErrorNumber);
			err.ErrorMessage = std::vformat(err.ErrorMessage, std::make_format_args(args...));
			err.Advice =  std::vformat(err.Advice, std::make_format_args(args...));
			err.ErrorType = std::vformat(err.ErrorType, std::make_format_args(args...));
			VerifyCondition(condition, err.ErrorMessage, err.Advice, err.ErrorType);
		}

		void VerifyCondition(bool condition,std::string Error, std::string Advice,std::string ErrorType);
		void VerifyCondition(bool condition,std::string Error, std::string Advice,std::string ErrorType,int startIndex,int endIndex) ;
		void VerifyCondition(bool condition,std::string Error, std::string Advice,std::string ErrorType,int startIndex);


		void RaiseError(Error& err);
		Error CreateError(std::string& Error, std::string& Advice,std::string& ErrorType);

		ProgramInfo SubParse(std::string arg);
		ProgramInfo SubParse(std::string arg,bool allowvarname);


		bool IsTypeDeclared(const std::string& type);
		bool IsRestrictionDeclared(const std::string& type);

		void ParsingRValueState();
		void ParseNumber();
		void ParseString();
		void ParseOther();
		void ParseChar();
		void AmpersandState();
		void ParseBinaryLiteral();
		void ParseHexLiteral();
		void ParseExponentNumber(std::string x);
		void ParseList();
		std::string CleanBrackets(std::string x);
		BracketParsingReturn ParseBrackets(char end,bool commas);

		void ParseArrayDeclaration();
		void ParsePointerDeclaration();
		void ParseGenericDeclaration();

		void PushToken(const TokenType tok, const std::string& data);
		void PushToken(Token tok);
		bool IsEndOfLine();
		char SkipSpaces();
		void ResetSecondState();

		std::string GetCurrentErrorContext(std::string ErrorRef);

		char GetNextChar();
		void Backtrack();
		void EndLine();

		void PushVariableReference(const std::string& x);
		void ClassNameState();


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

		LexerState m_CurrentState = LexerState::Default;
		LexerSecondaryState m_SecondState = LexerSecondaryState::None;

		std::string m_Buffer;
		std::string m_CurrentString;
		std::vector<char> m_BracketStack;
		std::vector<TypeScope> m_ScopeStack;
		bool m_subLexerError = false;
		bool m_NoVariableNames = false;

	};

}