#include "Parser.h"
#include "Errors.h"
#include <sstream>
#include <functional>
#include <algorithm>
#include <map>
#include <fstream>
#include <iostream>
#include <iosfwd>
#include <stack>
#include <Core/Log.h>
#include <Core/Utils.h>
#include <llvm/ADT/StringExtras.h>


namespace clear
{
	Parser::Parser()
	{
		m_StateMap[ParserState::Default]      = [this]() { _DefaultState(); };
		m_StateMap[ParserState::VariableName] = [this]() { _VariableNameState(); };
		m_StateMap[ParserState::RValue]       = [this]() { _ParsingRValueState(); };
		m_StateMap[ParserState::Operator]     = [this]() { _OperatorState(); };
		m_StateMap[ParserState::Indentation]  = [this]() { _IndentationState(); };
		m_StateMap[ParserState::FunctionName] = [this]() {_FunctionNameState();};
		m_StateMap[ParserState::FunctionParameters] = [this]() { _FunctionParameterState(); };
		m_StateMap[ParserState::ArrowState] = [this](){_ArrowState();};
		m_StateMap[ParserState::FunctionTypeState] = [this]() {_FunctionTypeState();};
		m_StateMap[ParserState::StructName] = [this]() { _StructNameState(); };
		m_StateMap[ParserState::FunctionParamaters] = [this]()  {_FunctionArgumentState(); };
		m_StateMap[ParserState::Comment] = [this]() { _CommentState(); };
		m_StateMap[ParserState::MultilineComment] = [this]() { _MultiLineCommentState(); };
		m_StateMap[ParserState::IndexOperator] = [this]() { _IndexOperatorState(); };
		m_StateMap[ParserState::AsterisksOperator] = [this]() {_AsterisksState();};
		m_StateMap[ParserState::AmpersandOperator] = [this]() {_AmpersandState();};

	}

	void Parser::_PushToken(Token tok) {
		_PushToken(tok.TokenType,tok.Data);
	}


	void Parser::_PushToken(const TokenType tok, const std::string &data)
	{
		TokenLocation location;
		location.from = m_TokenIndexStart;
		location.to = m_CurrentTokenIndex;
		location.line = m_CurrentLine;
		m_ProgramInfo.Tokens.push_back({ .TokenType = tok, .Data = data ,.Location = location});
	}

	Token Parser::_GetLastToken() {
		if (m_ProgramInfo.Tokens.empty())
			return Token{.TokenType = TokenType::EndLine,.Data = ""};

		return m_ProgramInfo.Tokens.at(m_ProgramInfo.Tokens.size()-1);
	}


	char Parser::_GetNextChar()
	{
		if(m_Buffer.length() > m_CurrentTokenIndex)
		{
			auto c = m_Buffer[m_CurrentTokenIndex++];
			return c;
		}

		return 0;
	}

	void Parser::_Backtrack()
	{
		m_CurrentTokenIndex--;
	}

	// const bool Parser::_IsEndOfFile()
	// {
	// 	return m_CurrentTokenIndex == m_Buffer.length();
	// }

	void Parser::_EndLine()
	{
		if ( _GetLastToken().TokenType != TokenType::EndLine) {
			_PushToken({ .TokenType = TokenType::EndLine });
		}
		m_CurrentLine++;
	}

	ProgramInfo Parser::ParseProgram()
	{
		m_ScopeStack.emplace_back();
		while (m_CurrentTokenIndex < m_Buffer.length())
		{
			m_TokenIndexStart = m_CurrentTokenIndex;
			m_StateMap.at(m_CurrentState)();
		}

		while (m_Indents > 0)
		{
			_PushToken({ .TokenType = TokenType::EndIndentation });
			m_Indents--;
		}
		_VerifyCondition(m_BracketStack.empty(),1);

		return m_ProgramInfo;
	}

	void Parser::InitParser()
	{
		m_ProgramInfo.Tokens.clear();
		m_ScopeStack.clear();
		m_CurrentTokenIndex = 0;
		m_CurrentErrorState.clear();
		m_Indents = 0;
		m_CurrentIndentLevel = 0;
		m_CurrentIndentationLevel = 0;
		m_LineStarted = false;
		m_CurrentState = ParserState::Default;
		m_Buffer.clear();
		m_CurrentString.clear();
	}


	ProgramInfo Parser::CreateTokensFromFile(const std::filesystem::path& path)
	{
		InitParser();
		m_File.open(path);

		if (!m_File.is_open())
		{
			std::cout << "failed to open file " << path << std::endl;
			return m_ProgramInfo;
		}

		std::stringstream stream;
		stream << m_File.rdbuf();

		m_Buffer = stream.str();
		m_Buffer+='\n';
		return ParseProgram();

	}

	 char Parser::_SkipSpaces() {
		_Backtrack();
		char current = _GetNextChar();
		while (IsSpace(current))
			current = _GetNextChar();

		return current;
	 }

	void Parser::_FunctionArgumentState() {
		char current = _GetNextChar();

		current = _SkipSpaces();
		m_CurrentString.clear();
		CLEAR_PARSER_VERIFY(current == '(', "149.FAS");

		m_CurrentErrorState = "function arguments";
		auto bracketsInfo = _ParseBrackets(')',true);
		m_CurrentState = ParserState::Default;
		int i = 0;
		for (const std::string& arg : bracketsInfo.tokens) {
			m_TokenIndexStart = bracketsInfo.indexes.at(i);
			ProgramInfo info = _SubParse(arg);
			for (const Token& tok :info.Tokens) {
				_PushToken(tok);
			}
			_PushToken(TokenType::Comma, "");
			i++;
		}
		if (_GetLastToken().TokenType == TokenType::Comma) {
			m_ProgramInfo.Tokens.pop_back();
		}

		_PushToken(TokenType::EndFunctionParameters, ")"); //TODO: change me to end function args
		current = _GetNextChar();
		_SkipSpaces();
		if (current != ')')
			_Backtrack();
	}


	void Parser::_MultiLineCommentState() {
		char current = _GetNextChar();
		m_TokenIndexStart = m_CurrentTokenIndex-3;
		while (current!= '\0') {
			current = _GetNextChar();
			if (current == '*') {
				current = _GetNextChar();
				if (current == '\\') {
					m_CurrentState = ParserState::Default;
					return;
				}else {
					_Backtrack();
				}
			}
		}
		int j = m_TokenIndexStart;
		while (j < m_Buffer.length() && m_Buffer[j] != '\n' && m_Buffer[j] != ';') {
			j++;
		}

		_VerifyCondition(false,17,m_TokenIndexStart,j);
	}

	void Parser::_CommentState() {
		char current = _GetNextChar();
		while (current != '\n' && current != '\0') {
			current = _GetNextChar();
		}
		m_CurrentState = ParserState::Default;
		if (current == '\n')
			_Backtrack();
	}
	bool Parser::_IsEndOfLine() {
		if (m_ProgramInfo.Tokens.empty())
			return true;
		TokenType tok = _GetLastToken().TokenType;
		return (tok == TokenType::EndLine);
	}

	bool Parser::_IsLineClosed() {

		if (m_ProgramInfo.Tokens.empty())
			return true;
		TokenType tok = _GetLastToken().TokenType;
		return !(tok == TokenType::CloseBracket);
	}

	std::string Parser::_GetCurrentErrorContext(std::string ErrorRef) {
		CLEAR_PARSER_VERIFY(!m_CurrentErrorState.empty(),ErrorRef)
		if (IsSubParser) {
			return m_CurrentErrorState;
		}
		std::string ret = m_CurrentErrorState;
		m_CurrentErrorState.clear();
		return ret;
	}



	BracketParsingReturn Parser::_ParseBrackets(char end, bool commas) {
		char start = g_CloserToOpeners.at(end);
		char current = start;
		std::vector<char> stack;
		stack.push_back(start);
		bool detectedEnd = false;
		bool ExpectingValue = false;
		BracketParsingReturn ret;

		std::string ErrorReference = _GetCurrentErrorContext("25410");

		ret.indexes.push_back(m_CurrentTokenIndex);
		while (!stack.empty() && current != '\0') {
			current = _GetNextChar();
			if ((current == '\'' || current == '"') && !(stack.back() == '\'' || stack.back() == '"')) {
				stack.push_back(current);
			}else {

				if (!(stack.back() == '\'' || stack.back() == '"')) {
					if (g_Openers.contains(current)) {
						stack.push_back(current);
					}
					if (g_CloserToOpeners.contains(current)) {
						_VerifyCondition(g_CloserToOpeners.at(current) == stack.back(),18,Str(g_CloserToOpeners.at(current)) ,Str(stack.back()));
						stack.pop_back();
					}
				}else {
					if ((current == '\'' || current == '"')) {
						if (m_Buffer[m_CurrentTokenIndex-2] != '\\') {
							// std::string type;
							// if (current == '"') {
							// 	type = "string";
							// }else {
							// 	type = "char";
							// }
							// _VerifyCondition(current == stack.back(),19,type,Str(stack.back()),Str(current));
							if (current == stack.back())
								stack.pop_back();
						}
					}
				}

			}
			if ( (current == end && stack.empty()) || (current == ',' && stack.size() == 1) || current == '\0')
			{
				if (!((current == ',' && commas) || current!= ',')) {
					_VerifyCondition(false,16,ErrorReference);
				}
				_VerifyCondition(!(current == ',' && ExpectingValue),38,ErrorReference);
				if (current ==',') {
					ExpectingValue = true;
				}
				if (current == end) {
					_VerifyCondition(!ExpectingValue,32,ret.indexes.back()-2);
					detectedEnd = true;
				}

				if (!m_CurrentString.empty()) {
					ret.tokens.push_back(m_CurrentString);
					ExpectingValue = false;
				}
				else {
					_VerifyCondition(!ExpectingValue,32,ret.indexes.back()-2);
				}

				m_CurrentString.clear();
				m_CurrentTokenIndex++;
				_SkipSpaces();
				_Backtrack();
				ret.indexes.push_back(m_CurrentTokenIndex);


			}
			else
			{
				if(!(std::isspace(current) && m_CurrentString.empty()))
					m_CurrentString += current;
			}


		}



		_VerifyCondition(detectedEnd, 27,ErrorReference, Str(end) );


		return ret;

	}


	void Parser::_IndexOperatorState() {
		char current = _GetNextChar();
		CLEAR_PARSER_VERIFY(current == '[', "318.IOS");

		m_CurrentErrorState = "array index";
		auto parsed= _ParseBrackets(']',false);
		_VerifyCondition(!parsed.tokens.empty(),24);

		ProgramInfo info = _SubParse( parsed.tokens[0]);
		for (const Token& tok :info.Tokens) {
			_PushToken(tok);
		}

		m_CurrentString.clear();
		m_CurrentState = ParserState::Default;

		_PushToken(TokenType::CloseBracket,"]");
		// m_CurrentString+= "INDEX_OP";

	}

	void Parser::_DefaultState()
	{
		char current = _GetNextChar();

		if (current == '(')
		{
			if (!m_CurrentString.empty() || !_IsLineClosed())
			{
				if (!m_CurrentString.empty())
				{
					_PushToken(TokenType::VariableReference, m_CurrentString);
				}

				_PushToken(TokenType::FunctionCall, m_CurrentString);
				m_CurrentState = ParserState::FunctionParamaters;
				_Backtrack();

			}else {
				m_BracketStack.push_back('(');
			}

			_PushToken({ .TokenType = TokenType::OpenBracket, .Data = "(" });


			return;
		}
		_VerifyCondition(current!=']',25,"index operator");
		if (current == '"' ) {
			_VerifyCondition(m_CurrentString.empty(), 25,"string");
			_ParseString();
		}
		if (current == '{') {
			_VerifyCondition(m_CurrentString.empty(), 25,"list");
			_ParseList();
		}
		if (current == '\'') {
			_VerifyCondition(m_CurrentString.empty(), 25,"char");
			_ParseChar();
		}

		if (std::isdigit(current) && m_CurrentString.empty())
		{
				m_CurrentString += current;
				_ParseNumber();
				return;
		}

		if (IsVarNameChar(current))
			m_CurrentString += current;


		if (!m_CurrentString.empty() && !IsVarNameChar(current))
		{
			if (g_KeyWordMap.contains(m_CurrentString) ) {
				auto& value = g_KeyWordMap.at(m_CurrentString);

				m_CurrentState = value.NextState;

				if (value.TokenToPush != TokenType::None)
					_PushToken({ .TokenType = value.TokenToPush, .Data = m_CurrentString });

				m_CurrentString.clear();
				if (!IsSpace(current))
					_Backtrack();
				return;

			}
			if ((((!g_OperatorMap.contains(Str(current)) && current != '\n' && current != ')') || (( current == '*' || current == '&'))) && _IsTypeDeclared(m_CurrentString))) {
				_PushToken(TokenType::VariableReference, m_CurrentString);
				m_CurrentState = ParserState::VariableName;
				m_CurrentString.clear();


				if (!IsSpace(current))
					_Backtrack();

				return;
			}
			else
			{
				_VerifyCondition(!_IsTypeDeclared(m_CurrentString),34);
				_PushToken(TokenType::VariableReference, m_CurrentString);
				m_CurrentString.clear();

			}
		}

		if (current == ':' || current == '\n')
		{
			m_CurrentState = ParserState::Indentation;
			m_CurrentString.clear();
			if (current == '\n' && m_BracketStack.empty())
				_EndLine();

			return;
		}

		if (g_OperatorMap.contains(Str(current)))
		{
			m_CurrentState = ParserState::Operator;
			m_CurrentString.clear();
		}

		if (current == '[') {
			m_CurrentState = ParserState::IndexOperator;
			_PushToken(TokenType::IndexOperator,"");
			_PushToken(TokenType::OpenBracket,"[");
			_Backtrack();

		}
		if (current == ')')
		{
			_VerifyCondition(!m_BracketStack.empty() && m_BracketStack.back(),25,"Brackets");

			m_BracketStack.pop_back();
			_PushToken({ .TokenType = TokenType::CloseBracket, .Data = ")"});

			return;
		}


	}
	void Parser::_ArrowState()
	{
		if (m_ProgramInfo.Tokens.size() > 1 &&
			m_ProgramInfo.Tokens.at(m_ProgramInfo.Tokens.size()-2).TokenType == TokenType::EndFunctionParameters)
		{
			m_CurrentState = ParserState::FunctionTypeState;
			return;
		}

		m_CurrentState = ParserState::Default;

	}
	void Parser::_FunctionTypeState()
	{
		char current = _GetNextChar();

		//want to ignore all spaces in between type and variable
		current = _SkipSpaces();
		m_CurrentString.clear();

		//allow _ and any character from alphabet
		while (current != '\n' && current != '\0' && current != ':')
		{
			m_CurrentString += current;
			current = _GetNextChar();
		}

		_PushToken({ .TokenType = TokenType::FunctionType, .Data = m_CurrentString });

		ProgramInfo info = _SubParse(m_CurrentString+" functype ");
		if (info.Tokens.back().TokenType ==  TokenType::VariableName && info.Tokens.back().Data == "functype") {
			info.Tokens.pop_back();
		}
		for (const Token& tok :info.Tokens) {
			_PushToken(tok);
		}

		_Backtrack();
		m_CurrentString.clear();
		m_CurrentState = ParserState::Default;

	}

	bool Parser::_IsTypeDeclared(const std::string& type) {
		for (TypeScope& arg : m_ScopeStack) {
			if (arg.TypeDeclarations.contains(type)) {
				return true;
			}
		}
		return false;
	}


	void Parser::_StructNameState() {
		char current = _GetNextChar();

		current = _SkipSpaces();
		_VerifyCondition((current != ':' && current != '\n' &&current != '\0'),3);
		m_TokenIndexStart = m_CurrentTokenIndex-1;
		m_CurrentString.clear();
		bool expectingEnd = false;
		while (current!= '\n' && current != '\0' && current != ':')
		{
			_VerifyCondition(!(expectingEnd&&IsSpace(current)),37,-1,m_CurrentTokenIndex-2);
			if (IsSpace(current)) {
				expectingEnd = true;
			}else {

			_VerifyCondition(IsVarNameChar(current),36,Str(current));
			}
			m_CurrentString += current;
			current = _GetNextChar();
		}
		_VerifyCondition(!(std::isdigit(m_CurrentString[0])),35);
		_VerifyCondition(!_IsTypeDeclared(m_CurrentString), 4,m_CurrentString);
		m_ScopeStack.back().TypeDeclarations.insert(m_CurrentString);


		_PushToken(TokenType::StructName, m_CurrentString);
		m_CurrentString.clear();
		_Backtrack();
		m_CurrentState = ParserState::Default;
	}

	Token Parser::_CreateToken(const TokenType tok, const std::string &data) {
		return Token{ .TokenType = tok, .Data = data };
	}

	void Parser::_VerifyCondition(bool condition, std::string Error, std::string Advice, std::string ErrorType, int startIndex, int endIndex) {
		if ((!condition) && !IsSubParser) {

		if (startIndex!= -1) {
			m_TokenIndexStart = startIndex;
		}
		if (endIndex != -1) {
			m_CurrentTokenIndex = endIndex;
			}
		}
		_VerifyCondition(condition, Error, Advice, ErrorType);
	}

	void Parser::_VerifyCondition(bool condition, std::string Error, std::string Advice, std::string ErrorType, int startIndex) {
		if (!condition && !IsSubParser) {
			if (startIndex!= -1) {
				m_TokenIndexStart = startIndex;
				}
			}

		_VerifyCondition(condition, Error, Advice, ErrorType);

	}



	void Parser::_ParsingRValueState()
	{
		char current = _GetNextChar();

		//want to ignore all spaces in between = and actual variable
		current = _SkipSpaces();
		m_CurrentString.clear();
		if (m_BracketStack.empty())
			_VerifyCondition(current != '\n' && current != '\0' && !g_CloserToOpeners.contains(current),5,m_TokenIndexStart-1);
		if (current == '\n') {
			return;
		}
		//brackets
		if (g_OperatorMap.contains(Str(current))) {
			m_CurrentState = ParserState::Operator;
			return;
		}
		if (current == '(')
		{
			m_BracketStack.push_back('(');
			_PushToken({ .TokenType = TokenType::OpenBracket, .Data = "(" });
			m_CurrentState = ParserState::RValue;
			return;
		}
		if (current == ')')
		{
			_PushToken({ .TokenType = TokenType::CloseBracket, .Data = ")" });
			m_CurrentState = ParserState::RValue;

			_VerifyCondition(!m_BracketStack.empty() && m_BracketStack.back() == '(',1);
			m_BracketStack.pop_back();

			return;
		}
		if (current == '"') //strings
		{
			_ParseString();
		}else if(current == '{') {
			_ParseList();
		}
		else if (std::isdigit(current) || current == '-') // postive/negative numbers
		{
			m_CurrentString.push_back(current);
			_ParseNumber();
		}else if (current == '\'') {
			_ParseChar();
		}
		else //TODO: implement this later
		{
			//could be a variable reference, class/struct reference etc...
			m_CurrentString.push_back(current);
			_Backtrack();
			_ParseOther();
		}

		m_CurrentState = ParserState::Default;
	}

	void Parser::_ParseArrayDeclaration(ArrayDeclarationReturn& output)
	{
		m_CurrentErrorState = "Array declaration";
		auto parsed = _ParseBrackets(']',false);
		m_CurrentString.clear();
		if (!parsed.tokens.empty()) {
			m_CurrentString = parsed.tokens.at(0);
		}

		if (m_CurrentString.empty()) {
			output.Tokens.push_back(_CreateToken(TokenType::DynamicArrayDef,""));
		}else {
			if (m_CurrentString.find_first_not_of("0123456789") == std::string::npos) {
				output.Tokens.push_back(_CreateToken(TokenType::StaticArrayDef,m_CurrentString));
			}
			else if (m_CurrentString == "...") {
				output.Tokens.push_back(_CreateToken(TokenType::StaticArrayDef,"..."));
			}else {
				output.error = true;
				output.errormsg = "Array declaration syntax error only expected numbers or ...";
				output.advice = "Either define a static size array by putting a size or a dynamic size array by leaving the square brackets empty";
				output.lastIndex = m_CurrentTokenIndex;
			}
		}
		m_CurrentString.clear();
		char current = _GetNextChar();
		while (IsSpace(current)) {
			current = _GetNextChar();
		}
		_VerifyCondition(current != ']',25,"Array declaration");
		if (current == '[') {
			_ParseArrayDeclaration(output);
		}else  {
			if (current != '\0')
				_Backtrack();
		}


	}

	int Parser::_ParsePointerDeclaration() {
		char current = _GetNextChar();
		int pointers = 0;
		while (current == '*') {
			current = _GetNextChar();
			pointers++;
		}
		_VerifyCondition(std::isspace(current) || current == '[' ,6);
		current = _SkipSpaces();
		_VerifyCondition(current!= '*',26);

		if (!IsSpace(current) && current != '\0') {
			_Backtrack();
		}
		return pointers;

	}

	Error Parser::_CreateError(std::string& ErrorMsg, std::string& Advice, std::string& ErrorType) {
		Error err;
		err.ErrorMessage = ErrorMsg;
		err.Advice = Advice;
		err.ErrorType = ErrorType;
		int i = m_CurrentTokenIndex;
		if (m_Buffer[i] == '\n')
			i--;
		if (m_Buffer[i] == '\n')
			i--;
		if ( m_Buffer[i] == '\0') {
			i-=2;
		}
		int j = i;

		while (j < m_Buffer.length() && m_Buffer[j] != '\n' && m_Buffer[j] != ';') {
			j++;
		}

		while (i >= 0 && m_Buffer[i] != '\n' && m_Buffer[i] != ';') {
			i--;
		}
		err.ErrorCause = m_Buffer.substr(i + 1, j - i - 1);
		i++;
		err.to = m_CurrentTokenIndex-i;
		err.from = m_TokenIndexStart-i;

		err.line = m_CurrentLine;
		return err;

	}
	void Parser::_RaiseError(Error& err) {
		PrintError(err);
		CLEAR_HALT();
	}

	void Parser::_VerifyCondition(bool condition, std::string Error, std::string Advice, std::string ErrorType) {
		if (!condition) {
			auto err = _CreateError(Error,Advice,ErrorType);
			if (!IsSubParser) {
				_RaiseError(err);
			}else {
				m_ProgramInfo.Errors.push_back(err);
			}
		}
	}



	void Parser::_VariableNameState()
	{
		char current = _GetNextChar();

		current = _SkipSpaces();
		bool IsType = g_DataTypes.contains(_GetLastToken().Data) || _IsTypeDeclared(_GetLastToken().Data);
		bool variableState = false;
		bool bracketState = false;
		int pointers = 0;
		int prevTokenIndex = 0;
		if ((current == ':' || g_OperatorMap.contains(Str(current))) && current != '*') {
			_Backtrack();
			_VerifyCondition(!IsType,7);
			m_CurrentState = ParserState::Default;
			return;
		}
		if (current == '(') {
			_Backtrack();
			m_CurrentState = ParserState::Default;

			return;
		}

		if (current == '*') {
			prevTokenIndex = m_CurrentTokenIndex;
			variableState = true;
			_Backtrack();
			pointers = _ParsePointerDeclaration();
			current = _GetNextChar();
		}


		m_CurrentString.clear();
		ArrayDeclarationReturn ArrayDeclarations;
		if (current == '[') {
			prevTokenIndex = m_CurrentTokenIndex;

			_ParseArrayDeclaration(ArrayDeclarations);
			current = _GetNextChar();
			bracketState = true;
		}
		m_CurrentString.clear();
		_VerifyCondition(!std::isdigit(current), 7,m_CurrentTokenIndex-1);
		if (current == '\n' || current == '\0' || !IsVarNameChar(current)) {
			_VerifyCondition(!(variableState && bracketState) , 8);

			if (!IsVarNameChar(current) && current != '\0' && current != '\n') {
				_VerifyCondition(!(IsType && g_OperatorMap.contains(Str(current))), 10);
				_VerifyCondition(!IsType, 9);
			}
			if (bracketState || variableState) {
				_VerifyCondition(!IsType,8);
				m_CurrentTokenIndex = prevTokenIndex;
			}
			m_CurrentState = ParserState::Default;
			_Backtrack();
			return;
		}



		int commas = 0;
		int vars = 0;
		_VerifyCondition(!ArrayDeclarations.error,ArrayDeclarations.errormsg,ArrayDeclarations.advice,"Array declaration error",prevTokenIndex-1,ArrayDeclarations.lastIndex-1);
		for (int i = 0; i < pointers; i++) {
			_PushToken(TokenType::PointerDef,"*");
		}
		for (auto tok :ArrayDeclarations.Tokens) {
			_PushToken(tok);
		}
		bool ExpectingComma = false;
		int lastValidVar = m_CurrentTokenIndex-1;
		while ((current != '\0' || current != '\n') && (IsVarNameChar(current) || IsSpace(current)) ) {
			_VerifyCondition(!(m_CurrentString.empty() && std::isdigit(current)),11, m_CurrentTokenIndex-1,-1);

			if (!IsSpace(current)) {
				m_CurrentString += current;
			}
			current = _GetNextChar();

			if (IsSpace(current)) {
				ExpectingComma = true;
			}


			_VerifyCondition(!(ExpectingComma && IsVarNameChar(current)),12,lastValidVar);


			if (current == ',') {
				ExpectingComma = false;
				lastValidVar = m_CurrentTokenIndex-1;
				_VerifyCondition(!m_CurrentString.empty(),28);
				_PushToken(TokenType::VariableName, m_CurrentString);
				_PushToken(TokenType::Comma,"");
				m_CurrentString.clear();
				current = _GetNextChar();
				commas++;
				if (current == ',') {
					commas++;
				}
				vars ++;
			}
			_VerifyCondition(current != ',',28);
		}
		if (!m_CurrentString.empty()) {
			_PushToken(TokenType::VariableName, m_CurrentString);
			vars++;

		}
		_VerifyCondition(commas < vars,28);
		if (!IsSpace(current)) {
			_Backtrack();
		}
		m_CurrentString.clear();

		m_CurrentState = ParserState::Default;
	}

	void Parser::_FunctionParameterState()
	{
		char current = _GetNextChar();
		int curtok = m_CurrentTokenIndex;
		current = _SkipSpaces();
		int skipped = m_CurrentTokenIndex-curtok;
		m_CurrentString.clear();
		_VerifyCondition(current == '(', 29,m_TokenIndexStart+skipped);

		m_CurrentErrorState = "function parameters";
		auto info = _ParseBrackets(')',true);

		_PushToken({ .TokenType = TokenType::StartFunctionParameters, .Data = "" });
		int ind = 0;
		for (const auto& i: info.tokens)
		{
			ProgramInfo ParameterTokens = _SubParse(i);
			CLEAR_VERIFY(!ParameterTokens.Tokens.empty(),"Tokens in function parameter empty");
			_VerifyCondition(g_DataTypes.contains(ParameterTokens.Tokens.at(0).Data) || _IsTypeDeclared(ParameterTokens.Tokens.at(0).Data),33,info.indexes.at(ind),m_CurrentTokenIndex-2,std::string(TokenToString(ParameterTokens.Tokens.at(0).TokenType)));
			for (const Token& tok :ParameterTokens.Tokens) {
				_PushToken(tok);
			}
			_PushToken(TokenType::Comma,",");
			ind++;
		}
		if (_GetLastToken().TokenType == TokenType::Comma) {
			m_ProgramInfo.Tokens.pop_back();
		}
		_PushToken({ .TokenType = TokenType::EndFunctionParameters, .Data = "" });
		m_CurrentState = ParserState::Default;
		current = _SkipSpaces();
		if (current != ')')
			_Backtrack();
	}

	void Parser::_FunctionNameState()
	{
		char current = _GetNextChar();

		current = _SkipSpaces();
		m_CurrentString.clear();
		if (current == '(')
		{
			_Backtrack();
			m_CurrentState = ParserState::FunctionParameters;
			_PushToken({ .TokenType = TokenType::Lambda, .Data = ""});
			return;
		}

		while (IsVarNameChar(current))
		{
			m_CurrentString += current;
			current = _GetNextChar();
		}

		if (current =='(')
			_Backtrack();

		_PushToken({ .TokenType = TokenType::FunctionName, .Data = m_CurrentString });
		m_CurrentString.clear();

		_VerifyCondition(current != '\n',29);
		m_CurrentState = ParserState::FunctionParameters;
	}


	void Parser::_OperatorState()
	{
		_Backtrack();
		std::string before = Str(_GetNextChar());
		std::string h = before;
		char current  = before[0];
		while (g_OperatorMap.contains(Str(current)))
		{
			current = _GetNextChar();

			if (!g_OperatorMap.contains(Str(current)))
				break;

			h+=current;
		}

		ParserMapValue value;
		std::string data;
		_Backtrack();

		if (g_OperatorMap.contains(h))
		{
			value = g_OperatorMap.at(h);
			data = h;
		}
		else
		{

			value = g_OperatorMap.at(before);
			data = before;
			m_CurrentTokenIndex -= (h.size()-1);

		}
		if (value.TokenToPush != TokenType::None)
			_PushToken({ .TokenType = value.TokenToPush, .Data = data });

		m_CurrentState = value.NextState;
	}

	void Parser::_AsterisksState() {
		auto token = _GetLastToken();
		auto tok =token.TokenType;

		if (tok == TokenType::VariableReference || tok == TokenType::RValueChar || tok == TokenType::RValueNumber || tok == TokenType::RValueString || tok == TokenType::CloseBracket) {
			_PushToken(TokenType::MulOp,"*");
		}else {
			_PushToken(TokenType::DereferenceOp,"*");
		}
		m_CurrentState = ParserState::RValue;

	}

	void Parser::_AmpersandState() {
		auto token = _GetLastToken();
		auto tok =token.TokenType;

		if (tok == TokenType::VariableReference || tok == TokenType::RValueChar || tok == TokenType::RValueNumber || tok == TokenType::RValueString || tok == TokenType::CloseBracket) {
			_PushToken(TokenType::BitwiseAnd,"&");
		}else {
			_PushToken(TokenType::AddressOp,"&");
		}
		m_CurrentState = ParserState::RValue;

	}


	void Parser::_IndentationState()
	{
		size_t tabWidth = 4;
		char next = _GetNextChar();

		if (next == '\n')
		{
			_EndLine();
			next = _GetNextChar();
		}

		bool indenting = true;
		size_t totalSpaces = 0;

		while (indenting)
		{
			if (next == '\t')
			{
				totalSpaces += tabWidth;
				next = _GetNextChar();
			}
			else if (next == ' ')
			{
				totalSpaces++;
				next = _GetNextChar();
			}
			else
			{
				indenting = false;
			}
		}

		size_t localIndents = totalSpaces / 4;

		if (localIndents > m_Indents)
		{
			_PushToken({ .TokenType = TokenType::StartIndentation });
			m_Indents = localIndents;
			m_ScopeStack.emplace_back();
		}

		while (m_Indents > localIndents)
		{
			_PushToken({ .TokenType = TokenType::EndIndentation });
			m_Indents--;
			m_ScopeStack.pop_back();
		}

		m_CurrentState = ParserState::Default;
		_Backtrack();
	}

	void Parser::_ParseHexLiteral() {
		m_CurrentString.clear();
		char current = _GetNextChar();
		while (!std::isspace(current) && !g_OperatorMap.contains(Str(current))) {
			_VerifyCondition(current == '0' || current == '1' || current == '2' || current == '3' || current == '4' || current == '5' || current == '6' || current == '7' || current == '8' || current == '9' || current == 'A' || current == 'B' || current == 'C' || current == 'D' || current == 'E' || current == 'F'  || current == 'a' || current == 'b' || current == 'c' || current == 'd' || current == 'e' || current == 'f',30);
			m_CurrentString += current;
			current = _GetNextChar();
		}

		if (!IsSpace(current)) {
			_Backtrack();
		}

		_PushToken(TokenType::RValueNumber,std::to_string(HexStringToInteger(m_CurrentString)));
		m_CurrentString.clear();
	}
	void Parser::_ParseBinaryLiteral() {
		m_CurrentString.clear();
		char current = _GetNextChar();
		while (!std::isspace(current) && !g_OperatorMap.contains(Str(current))) {
			_VerifyCondition(current == '0' || current == '1',31);
			m_CurrentString += current;
			current = _GetNextChar();
		}

		if (!IsSpace(current)) {
			_Backtrack();
		}

		_PushToken(TokenType::RValueNumber,std::to_string(BinaryStringToInteger(m_CurrentString)));
		m_CurrentString.clear();

	}


	void Parser::_ParseNumber()
	{
		char current = _GetNextChar();

		if (current == '\0')
		{
			_PushToken({ .TokenType = TokenType::RValueNumber, .Data = m_CurrentString });
			m_CurrentString.clear();
			return;
		}

		bool usedDecimal = false;
		if (current == 'b') {
			_VerifyCondition(m_CurrentString == "0", 22,"binary");
			_ParseBinaryLiteral();
			return;
		}

		if (current == 'x') {
			_VerifyCondition(m_CurrentString == "0", 22,"hex");
			_ParseHexLiteral();
			return;
		}

		while (std::isalnum(current) || current == '.')
		{
			m_CurrentString.push_back(current);
			if (current == '.' && usedDecimal) // need to throw some type of error again TODO
			{
				_VerifyCondition(false,21);
			}
			if (current == '.')
			{
				usedDecimal = true;
			}


			current = _GetNextChar();
		}
		_VerifyCondition(IsValidNumber(m_CurrentString),20,-1,m_CurrentTokenIndex-2);
		if (m_CurrentString == "-") {
			_PushToken(TokenType::SubOp,"-");
		}else {

			_PushToken({ .TokenType = TokenType::RValueNumber, .Data = m_CurrentString });
		}
		m_CurrentString.clear();
		if (!IsSpace(current))
			_Backtrack();
	}

	ProgramInfo Parser::_SubParse(std::string arg) {
		Parser subParser;
		subParser.InitParser();
		subParser.m_Buffer = arg;
		subParser.m_Buffer+=" ";
		subParser.m_ScopeStack = m_ScopeStack;
		subParser.IsSubParser = true;
		ProgramInfo info = subParser.ParseProgram();

		if (!info.Errors.empty()) {
			auto cause = info.Errors.front();
			_VerifyCondition(false,cause.ErrorMessage,cause.Advice,cause.ErrorType,m_TokenIndexStart,m_TokenIndexStart+(cause.to-cause.from));
			// _Vali(cause.ErrorMessage,cause.Advice,cause.);
			// _RaiseError();
		}
		return info;
	}


	void Parser::_ParseList() {
		m_CurrentErrorState = "List literal";
		auto  bracketInfo = _ParseBrackets('}',true);
		_PushToken(TokenType::StartArray,"{");

 		for (const std::string& arg : bracketInfo.tokens) {

			ProgramInfo info = _SubParse(arg);
			for (const Token& tok :info.Tokens) {
				_PushToken(tok);
			}
			_PushToken(TokenType::Comma, "");
		}
		if (_GetLastToken().TokenType == TokenType::Comma) {
			m_ProgramInfo.Tokens.pop_back();
		}


		_PushToken(TokenType::EndArray,"}");
		m_CurrentState = ParserState::Default;
	}


	void Parser::_ParseChar() {
		char current = _GetNextChar();
		char data = current;
		if (current == '\\') {
			current = _GetNextChar();
			if (current == '\'') {
				data = '\'';
				current = 0;
			}
			else if(current == 'n'){
				data=  '\n';
			}
			else if(current == '\\') {
				data= '\\';
			}else if(current == 't') {
				data = '\t';
			}else if(current == 'r') {
				data =  '\r';
			}else if(current == 'b') {
				data= '\b';
			}else if(current == '0') {
				data+= '\0';
			}else if(current == 'f') {
				data = '\f';
			}else if(current == 'v') {
				data = '\v';
			}else if(current == 'a') {
				data = '\a';
			}
			else {
				std::string message = "Unknown char escape char \"\\"+ Str(current)+"\"";
				_VerifyCondition(false,13,m_TokenIndexStart+1);

			}

		}

		// if (current == '\'') {
		// 	current = '';
		// }
		_VerifyCondition(current!= '\'',14);


		_PushToken(TokenType::RValueChar,Str(data));
		current = _GetNextChar();
		_VerifyCondition(current == '\'',15);
	}


	void Parser::_ParseString()
	{
		char current = _GetNextChar();
		while (current != '"')
		{
			_VerifyCondition(!(current == '\n' || current == '\0'),23,m_TokenIndexStart+1);
			if (current == '\\') {
				current = _GetNextChar();
				if (current == '"') {
					m_CurrentString += '"';
				}
				else if(current == 'n'){
					m_CurrentString += '\n';
				}
				else if(current == '\\') {
					m_CurrentString += '\\';
				}else if(current == 't') {
					m_CurrentString += '\t';
				}else if(current == 'r') {
					m_CurrentString += '\r';
				}else if(current == 'b') {
					m_CurrentString += '\b';
				}else if(current == '0') {
					m_CurrentString+= '\0';
				}else if(current == 'f') {
					m_CurrentString = '\f';
				}else if(current == 'v') {
					m_CurrentString = '\v';
				}else if(current == 'a') {
					m_CurrentString = '\a';
				}

				else {
					m_CurrentString += '\\';
					m_CurrentString += current;

				}
			}else {
				m_CurrentString += current;

			}
			current = _GetNextChar();


		}

		_PushToken({ .TokenType = TokenType::RValueString, .Data = m_CurrentString });
		m_CurrentString.clear();
	}

	void Parser::_ParseOther()
	{
		char current = _GetNextChar();
		m_CurrentString.clear();

		while (IsVarNameChar(current ) && current)
		{
			m_CurrentString += current;

			current = _GetNextChar();
			if (current == '\n' || current == '\0' || IsSpace(current))
				break;
		}
		if (current == '(') {
			if (!m_CurrentString.empty())
				_Backtrack();
				return;
		}

		if (g_KeyWordMap.contains(m_CurrentString))
		{
			auto& value = g_KeyWordMap.at(m_CurrentString);
			_PushToken({ .TokenType = value.TokenToPush, .Data = m_CurrentString });
		}else {
			_PushToken(TokenType::VariableReference, m_CurrentString);
		}

		m_CurrentString.clear();
		m_CurrentState = ParserState::Default;
		_Backtrack();
	}
}
