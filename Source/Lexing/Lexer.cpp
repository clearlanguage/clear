#include "Lexer.h"
#include "Errors.h"
#include <sstream>
#include <map>
#include <fstream>
#include <iostream>
#include <Core/Log.h>
#include <cmath>
#include <Core/Utils.h>


namespace clear
{
	Lexer::Lexer()
	{
		m_StateMap[LexerState::Default]      = [this]() { _DefaultState(); };
		m_StateMap[LexerState::VariableName] = [this]() { _VariableNameState(); };
		m_StateMap[LexerState::RValue]       = [this]() { _ParsingRValueState(); };
		m_StateMap[LexerState::Operator]     = [this]() { _OperatorState(); };
		m_StateMap[LexerState::Indentation]  = [this]() { _IndentationState(); };
		m_StateMap[LexerState::FunctionName] = [this]() {_FunctionNameState();};
		m_StateMap[LexerState::FunctionParameters] = [this]() { _FunctionParameterState(); };
		m_StateMap[LexerState::ArrowState] = [this](){_ArrowState();};
		m_StateMap[LexerState::FunctionTypeState] = [this]() {_FunctionTypeState();};
		m_StateMap[LexerState::StructName] = [this]() { _StructNameState(); };
		m_StateMap[LexerState::FunctionParamaters] = [this]()  {_FunctionArgumentState(); };
		m_StateMap[LexerState::Comment] = [this]() { _CommentState(); };
		m_StateMap[LexerState::MultilineComment] = [this]() { _MultiLineCommentState(); };
		m_StateMap[LexerState::IndexOperator] = [this]() { _IndexOperatorState(); };
		m_StateMap[LexerState::AsterisksOperator] = [this]() {_AsterisksState();};
		m_StateMap[LexerState::AmpersandOperator] = [this]() {_AmpersandState();};
		m_StateMap[LexerState::Declaration] = [this](){_DeclarationState();};
		m_StateMap[LexerState::MinusOperator] = [this]() {_MinusOperator();};
		m_StateMap[LexerState::Increment] = [this]() {_IncrementOperator();};
		m_StateMap[LexerState::Restriction] = [this]() {_RestrictionState();};
		m_StateMap[LexerState::DotOp] = [this]() {_DotOpState();};


	}

	void Lexer::_PushToken(Token tok) {
		_PushToken(tok.TokenType,tok.Data);
	}


	void Lexer::_PushToken(const TokenType tok, const std::string &data)
	{
		TokenLocation location;
		location.from = m_TokenIndexStart;
		location.to = m_CurrentTokenIndex;
		location.line = m_CurrentLine;
		m_ProgramInfo.Tokens.push_back({ .TokenType = tok, .Data = data ,.Location = location});
	}

	Token Lexer::_GetLastToken() {
		if (m_ProgramInfo.Tokens.empty())
			return Token{.TokenType = TokenType::EndLine,.Data = ""};

		return m_ProgramInfo.Tokens.at(m_ProgramInfo.Tokens.size()-1);
	}

	Token Lexer::_GetLastToken(size_t x) {
		if (m_ProgramInfo.Tokens.empty() || x >= m_ProgramInfo.Tokens.size())
			return Token{.TokenType = TokenType::EndLine, .Data = ""};

		return m_ProgramInfo.Tokens.at(m_ProgramInfo.Tokens.size() - 1 - x);
	}


	char Lexer::_GetNextChar()
	{
		if(m_Buffer.length() > m_CurrentTokenIndex)
		{
			auto c = m_Buffer[m_CurrentTokenIndex++];
			return c;
		}

		return 0;
	}

	void Lexer::_Backtrack()
	{
		m_CurrentTokenIndex--;
	}

	// const bool Lexer::_IsEndOfFile()
	// {
	// 	return m_CurrentTokenIndex == m_Buffer.length();
	// }

	void Lexer::_ResetSecondState() {
		if (m_SecondState == LexerSecondaryState::None) {
			return;
		}

		if (m_SecondState == LexerSecondaryState::Declaration) {
			// auto tok = _GetLastToken().TokenType;
			// _VerifyCondition(tok== TokenType::EndFunctionArguments,40);

		}
		m_SecondState = LexerSecondaryState::None;
	}


	void Lexer::_EndLine()
	{
		if ( _GetLastToken().TokenType != TokenType::EndLine) {
			_PushToken({ .TokenType = TokenType::EndLine });
		}
		_ResetSecondState();
		m_CurrentLine++;
	}

	ProgramInfo Lexer::ParseProgram()
	{
		if (!IsSubLexer)
			m_ScopeStack.emplace_back();
		while (m_CurrentTokenIndex < m_Buffer.length() && !m_subLexerError)
		{
			m_TokenIndexStart = m_CurrentTokenIndex;
			m_StateMap.at(m_CurrentState)();
		}
		if (m_subLexerError) {
			return m_ProgramInfo;
		}
		CLEAR_PARSER_VERIFY(m_ProgramInfo.Errors.empty(),"99");
		while (m_Indents > 0)
		{
			_PushToken({ .TokenType = TokenType::EndIndentation });
			m_Indents--;
		}
		_VerifyCondition(m_BracketStack.empty(),1);

		return m_ProgramInfo;
	}

	void Lexer::InitLexer()
	{
		m_ProgramInfo.Tokens.clear();
		m_ScopeStack.clear();
		m_CurrentTokenIndex = 0;
		m_CurrentErrorState.clear();
		m_Indents = 0;
		m_CurrentIndentLevel = 0;
		m_CurrentIndentationLevel = 0;
		m_LineStarted = false;
		m_CurrentState = LexerState::Default;
		m_Buffer.clear();
		m_CurrentString.clear();
	}


	ProgramInfo Lexer::CreateTokensFromFile(const std::filesystem::path& path)
	{
		InitLexer();
		m_File.open(path);

		if (!m_File.is_open())
		{
			std::cout << "failed to open file " << path << std::endl;
			return m_ProgramInfo;
		}

		std::string line;
		while(std::getline(m_File, line))
		{
			if(IsOnlyWhitespace(line)) continue;

			m_Buffer += line + '\n';
		}
		
		return ParseProgram();

	}

	 char Lexer::_SkipSpaces() {
		_Backtrack();
		char current = _GetNextChar();
		while (IsSpace(current))
			current = _GetNextChar();

		return current;
	 }

	void Lexer::_MinusOperator() {
		auto token = _GetLastToken();
		auto tok =token.TokenType;

		if (tok == TokenType::VariableReference || tok == TokenType::RValueChar || tok == TokenType::RValueNumber || tok == TokenType::RValueString || tok == TokenType::CloseBracket || tok == TokenType::EndFunctionArguments) {
			_PushToken(TokenType::SubOp,"-");
		}else {
			_PushToken(TokenType::Negation,"-");
		}
		m_CurrentState = LexerState::RValue;
	}

	void Lexer::_DeclarationState() {
		m_SecondState = LexerSecondaryState::Declaration;
		m_CurrentState = LexerState::Default;
	}



	void Lexer::_FunctionArgumentState() {
		_GetNextChar();

		char current = _SkipSpaces();
		m_CurrentString.clear();
		CLEAR_PARSER_VERIFY(current == '(', "149.FAS");

		m_CurrentErrorState = "function arguments";
		auto bracketsInfo = _ParseBrackets(')',true);
		m_CurrentState = LexerState::Default;
		int i = 0;
		for (const std::string& arg : bracketsInfo.tokens) {
			m_TokenIndexStart = bracketsInfo.indexes.at(i);
			ProgramInfo info = _SubParse(arg,true);
			for (const Token& tok :info.Tokens) {
				_PushToken(tok);
			}
			_PushToken(TokenType::Comma, "");
			i++;
		}
		if (_GetLastToken().TokenType == TokenType::Comma) {
			m_ProgramInfo.Tokens.pop_back();
		}

		_PushToken(TokenType::EndFunctionArguments, ")"); //TODO: change me to end function args
		current = _GetNextChar();
		_SkipSpaces();
		if (current != ')')
			_Backtrack();
	}


	void Lexer::_MultiLineCommentState() {
		char current = _GetNextChar();
		if (current =='\n')
			m_CurrentLine++;
		m_TokenIndexStart = m_CurrentTokenIndex-3;
		while (current!= '\0') {
			current = _GetNextChar();
			if (current == '\n')
				m_CurrentLine++;
			if (current == '*') {
				current = _GetNextChar();
				if (current == '\\') {
					m_CurrentState = LexerState::Default;
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

	std::string Lexer::_CleanBrackets(std::string x) {
		if (x.front() == '(' && x.back() == ')' ) {
			return x.substr(1, x.size() - 2);
		}
		return x;
	}

	void Lexer::_CommentState() {
		char current = _GetNextChar();
		while (current != '\n' && current != '\0') {
			current = _GetNextChar();
		}
		m_CurrentState = LexerState::Default;
		if (current == '\n')
			_Backtrack();
	}
	bool Lexer::_IsEndOfLine() {
		if (m_ProgramInfo.Tokens.empty())
			return true;
		TokenType tok = _GetLastToken().TokenType;
		return (tok == TokenType::EndLine);
	}

	void Lexer::_DotOpState() {
		_VerifyCondition(IsTokenOfType(_GetLastToken(1),"has_members"),51);
		m_CurrentState = LexerState::RValue;
	}


	void Lexer::_RestrictionState() {
		char current = _GetNextChar();

		current = _SkipSpaces();
		m_TokenIndexStart = m_CurrentTokenIndex-1;
		m_CurrentString.clear();

		bool expectingEnd = false;
		while (current!= '\n' && current != '\0' && current != ':' && current != '<')
		{
			_VerifyCondition(!(expectingEnd&&IsSpace(current)),37,-1,m_CurrentTokenIndex-2,"restriction");
			if (IsSpace(current)) {
				expectingEnd = true;
			}else {

				_VerifyCondition(IsVarNameChar(current),36,Str(current),"restriction");
			}
			m_CurrentString += current;
			current = _GetNextChar();
		}
		_VerifyCondition(!(std::isdigit(m_CurrentString.at(0))),35,"restriction");
		_VerifyCondition(!_IsTypeDeclared(m_CurrentString), 47,-1,m_CurrentTokenIndex-1,m_CurrentString);
		m_ScopeStack.back().RestrictionDeclarations.insert(m_CurrentString);
		_PushToken(TokenType::RestrictionName, m_CurrentString);
		m_CurrentString.clear();

		if (current == '<') {
			bool end = false;
			bool expectingEnd = false;
			current = _GetNextChar();
			while (current != '\0') {
				if (current == '>') {
					end = true;
					break;
				}
				_VerifyCondition(!(expectingEnd&&IsSpace(current)),37,-1,m_CurrentTokenIndex-2,"restriction");
				if (IsSpace(current)) {
					expectingEnd = true;
				}else {
					_VerifyCondition(IsVarNameChar(current),36,Str(current),"restriction");
				}
				m_CurrentString += current;
				current = _GetNextChar();
			}
			_VerifyCondition(end,48);
			_PushToken(TokenType::RestrictionTypeName, m_CurrentString);
		}else {
			_PushToken(TokenType::RestrictionTypeName, "type");

		}




		m_CurrentString.clear();
		m_CurrentState = LexerState::Default;
	}


	std::string Lexer::_GetCurrentErrorContext(std::string ErrorRef) {
		CLEAR_PARSER_VERIFY(!m_CurrentErrorState.empty(),ErrorRef)
		if (IsSubLexer) {
			return m_CurrentErrorState;
		}
		std::string ret = m_CurrentErrorState;
		m_CurrentErrorState.clear();
		return ret;
	}



	BracketParsingReturn Lexer::_ParseBrackets(char end, bool commas) {
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
						_VerifyCondition(g_CloserToOpeners.at(current) == stack.back(),18,Str(stack.back()),Str(g_CloserToOpeners.at(current)));
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

				if (!m_CurrentString.empty()) {
					ret.tokens.push_back('('+ m_CurrentString + ')');
					ExpectingValue = false;
				}
				else {
					_VerifyCondition(!ExpectingValue,32,ret.indexes.back()-2);
				}

				if (current == end) {
					_VerifyCondition(!ExpectingValue,32,ret.indexes.back()-2);
					m_CurrentString.clear();
					detectedEnd = true;
					break;
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
					// if (current == '\n') current = ' ';
					m_CurrentString += current;
			}


		}



		_VerifyCondition(detectedEnd, 27,ErrorReference, Str(end) );


		return ret;
	}


	void Lexer::_PushVariableReference(const std::string& x) {
		if (_GetLastToken().TokenType == TokenType::DotOp) {
			_PushToken(TokenType::MemberName,x);
		}else {
			_PushToken(TokenType::VariableReference, x);

		}
	}


	void Lexer::_IndexOperatorState() {
		char current = _GetNextChar();
		CLEAR_PARSER_VERIFY(current == '[', "318.IOS");

		m_CurrentErrorState = "array index";
		auto parsed= _ParseBrackets(']',false);
		_VerifyCondition(!parsed.tokens.empty(),24);
		if (parsed.tokens.empty())
			return;

		ProgramInfo info = _SubParse( parsed.tokens.at(0),false);
		for (const Token& tok :info.Tokens) {
			_PushToken(tok);
		}

		m_CurrentString.clear();
		m_CurrentState = LexerState::Default;

		_PushToken(TokenType::CloseBracket,"]");
		// m_CurrentString+= "INDEX_OP";

	}

	void Lexer::_DefaultState()
	{
		char current = _GetNextChar();

		if (current == '(')
		{
			if (!m_CurrentString.empty() || IsTokenOfType(_GetLastToken(),"callable"))
			{
				if (!m_CurrentString.empty())
				{
					_PushVariableReference(m_CurrentString);
				}
				if (IsTokenOfType(_GetLastToken(),"named_callable")) {
					m_CurrentString = _GetLastToken().Data;
				}

				_PushToken(TokenType::FunctionCall, m_CurrentString);
				m_CurrentState = LexerState::FunctionParamaters;
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
			return;
		}
		if (current == '{') {
			_VerifyCondition(m_CurrentString.empty(), 25,"list");
			_ParseList();
			return;
		}
		if (current == '\'') {
			_VerifyCondition(m_CurrentString.empty(), 25,"char");
			_ParseChar();
			return;
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
			if (((!g_OperatorMap.contains(Str(current)) && current != '\n' && current != ')') || ( current == '*' || current == '&' || current == '<')) && _IsTypeDeclared(m_CurrentString) && _GetLastToken().TokenType!= TokenType::DotOp) {
				_PushToken(TokenType::TypeIdentifier, m_CurrentString);
				m_CurrentState = LexerState::VariableName;
				m_CurrentString.clear();


				if (!IsSpace(current))
					_Backtrack();

				return;
			}
			else
			{
				_VerifyCondition(!_IsTypeDeclared(m_CurrentString),34);
				_PushVariableReference( m_CurrentString);
				m_CurrentString.clear();

			}
		}

		if (current == ':' || current == '\n')
		{
			m_CurrentState = LexerState::Indentation;
			m_CurrentString.clear();
			if (current == '\n' && m_BracketStack.empty())
				_EndLine();

			return;
		}

		if (g_OperatorMap.contains(Str(current)))
		{
			m_CurrentState = LexerState::Operator;
			m_CurrentString.clear();
		}

		if (current == '[') {
			m_CurrentState = LexerState::IndexOperator;
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

		_VerifyCondition(IsVarNameChar(current)||g_OperatorMap.contains(Str(current)) ||g_Openers.contains(current) || g_CloserToOpeners.contains(current) || std::isspace(current) ,41,Str(current));
	}
	void Lexer::_ArrowState()
	{
		if ((m_ProgramInfo.Tokens.size() > 1 &&
			_GetLastToken(1).TokenType == TokenType::EndFunctionParameters) || m_SecondState == LexerSecondaryState::Declaration)
		{
			m_CurrentState = LexerState::FunctionTypeState;
			return;
		}

		m_CurrentState = LexerState::Default;

	}
	void Lexer::_FunctionTypeState()
	{
		char current = _GetNextChar();

		//want to ignore all spaces in between type and variable
		current = _SkipSpaces();
		m_CurrentString.clear();

		//allow _ and any character from alphabet
		while (current != '\n' && current != '\0' && current != ':' && current!=';')
		{
			m_CurrentString += current;
			current = _GetNextChar();
		}

		_PushToken({ .TokenType = TokenType::FunctionType, .Data = m_CurrentString });
		bool containsdata = false;
		for (auto i : m_CurrentString) {
			if (!std::isspace(i)) {
				containsdata = true;
				break;
			}
		}
		_VerifyCondition(containsdata,40);
		ProgramInfo info = _SubParse(m_CurrentString,true);

		for (const Token& tok :info.Tokens) {
			_PushToken(tok);
		}

		_Backtrack();
		m_CurrentString.clear();
		m_CurrentState = LexerState::Default;

	}

	bool Lexer::_IsTypeDeclared(const std::string& type) {
		for (TypeScope& arg : m_ScopeStack) {
			if (arg.TypeDeclarations.contains(type) || arg.RestrictionDeclarations.contains(type)) {
				return true;
			}
		}
		return false;
	}

	bool Lexer::_IsRestrictionDeclared(const std::string &type) {
		for (TypeScope& arg : m_ScopeStack) {
			if (arg.RestrictionDeclarations.contains(type)) {
				return true;
			}
		}
		return false;
	}




	void Lexer::_StructNameState() {
		char current = _GetNextChar();

		current = _SkipSpaces();
		_VerifyCondition((current != ':' && current != '\n' &&current != '\0' && current != ';'),3,m_TokenIndexStart-1,m_CurrentTokenIndex-1);
		m_TokenIndexStart = m_CurrentTokenIndex-1;
		m_CurrentString.clear();
		bool expectingEnd = false;
		while (current!= '\n' && current != '\0' && current != ':')
		{
			_VerifyCondition(!(expectingEnd&&IsSpace(current)),37,-1,m_CurrentTokenIndex-2,"struct");
			if (IsSpace(current)) {
				expectingEnd = true;
			}else {

			_VerifyCondition(IsVarNameChar(current),36,Str(current),"struct");
			}
			m_CurrentString += current;
			current = _GetNextChar();
		}
		_VerifyCondition(!(std::isdigit(m_CurrentString.at(0))),35,"struct");
		_VerifyCondition(!_IsTypeDeclared(m_CurrentString), 4,-1,m_CurrentTokenIndex-1,m_CurrentString);
		m_ScopeStack.back().TypeDeclarations.insert(m_CurrentString);


		_PushToken(TokenType::StructName, m_CurrentString);
		m_CurrentString.clear();
		_Backtrack();
		m_CurrentState = LexerState::Default;
	}

	Token Lexer::_CreateToken(const TokenType tok, const std::string &data) {
		return Token{ .TokenType = tok, .Data = data };
	}

	void Lexer::_VerifyCondition(bool condition, std::string Error, std::string Advice, std::string ErrorType, int startIndex, int endIndex) {
		if ((!condition) && !IsSubLexer) {

		if (startIndex!= -1) {
			m_TokenIndexStart = startIndex;
		}
		if (endIndex != -1) {
			m_CurrentTokenIndex = endIndex;
			}
		}
		_VerifyCondition(condition, Error, Advice, ErrorType);
	}

	void Lexer::_VerifyCondition(bool condition, std::string Error, std::string Advice, std::string ErrorType, int startIndex) {
		if (!condition && !IsSubLexer) {
			if (startIndex!= -1) {
				m_TokenIndexStart = startIndex;
				}
			}

		_VerifyCondition(condition, Error, Advice, ErrorType);

	}



	void Lexer::_ParsingRValueState()
	{
		char current = _GetNextChar();

		//want to ignore all spaces in between = and actual variable
		current = _SkipSpaces();
		m_CurrentString.clear();
		if (m_BracketStack.empty())
			_VerifyCondition(current != '\n' && current != '\0' && !g_CloserToOpeners.contains(current),5,m_TokenIndexStart-1,m_CurrentTokenIndex-1);
		if (current == '\n') {
			return;
		}
		//brackets
		if (g_OperatorMap.contains(Str(current))) {
			m_CurrentState = LexerState::Operator;
			return;
		}
		if (current == '(')
		{
			m_BracketStack.push_back('(');
			_PushToken({ .TokenType = TokenType::OpenBracket, .Data = "(" });
			m_CurrentState = LexerState::RValue;
			return;
		}
		if (current == ')')
		{
			_PushToken({ .TokenType = TokenType::CloseBracket, .Data = ")" });
			m_CurrentState = LexerState::RValue;

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
		else if (std::isdigit(current) || current == '-') // positive/negative numbers
		{
			m_CurrentString.push_back(current);
			_ParseNumber();
		}else if (current == '\'') {
			_ParseChar();
		}
		else if(std::isalnum(current))
		{
			//could be a variable reference, class/struct reference etc...
			m_CurrentString.push_back(current);
			_Backtrack();
			_ParseOther();
		}else {
			_VerifyCondition(false,41,Str(current));
		}
		if (m_CurrentState == LexerState::RValue)
			m_CurrentState = LexerState::Default;
	}

	void Lexer::_ParseArrayDeclaration()
	{
		m_TokenIndexStart = m_CurrentTokenIndex-1;
		m_CurrentErrorState = "Array declaration";
		auto parsed = _ParseBrackets(']',false);
		m_CurrentString.clear();
		if (!parsed.tokens.empty()) {
			m_CurrentString = _CleanBrackets(parsed.tokens.at(0));
		}

		if (m_CurrentString.empty()) {
			_PushToken(TokenType::DynamicArrayDef,"");
		}else {
			if (m_CurrentString.find_first_not_of("0123456789") == std::string::npos) {
				_PushToken(TokenType::StaticArrayDef,m_CurrentString);
			}
			else if (m_CurrentString == "...") {
				_PushToken(TokenType::StaticArrayDef,"...");
			}else {
				_VerifyCondition(false,"Array declaration syntax error only expected numbers or ...","Either define a static size array by putting a size or a dynamic size array by leaving the square brackets empty","Array declaration error",m_TokenIndexStart,m_CurrentTokenIndex-1);

			}
		}
		m_CurrentString.clear();
		char current = _GetNextChar();
		// _VerifyCondition(IsSpace(current),47,m_CurrentTokenIndex-2);
		while (IsSpace(current)) {
			current = _GetNextChar();
		}
		_VerifyCondition(current != ']',25,"Array declaration");
		if (current == '[') {
			_ParseArrayDeclaration();
		}else  {
			if (current != '\0')
				_Backtrack();
		}


	}

	void Lexer::_ParsePointerDeclaration() {
		char current = _GetNextChar();
		while (current == '*') {
			_PushToken(TokenType::PointerDef,"*");
			current = _GetNextChar();
		}
		_VerifyCondition(std::isspace(current) || current == '[' ,6);
		current = _SkipSpaces();
		_VerifyCondition(current!= '*',26);

		if (!IsSpace(current) && current != '\0') {
			_Backtrack();
		}

	}

	void Lexer::_ParseGenericDeclaration() {
		char current = _GetNextChar();
		int currentLevel = 1;
		std::vector<std::string> tokens;
		std::vector<int> indexes;
		indexes.push_back(m_CurrentTokenIndex);
		while (currentLevel!= 0&& current != '\0') {
			current = _GetNextChar();
			if (current == '<') {
				currentLevel++;
			}
			else if (current == '>') {
				currentLevel--;
			}

			if (current == ',' && currentLevel == 1 || currentLevel == 0) {
				_VerifyCondition(!m_CurrentString.empty(),45);
				tokens.push_back(m_CurrentString);
				m_CurrentString.clear();
				indexes.push_back(m_CurrentTokenIndex);
				continue;
			}
			_VerifyCondition(!IsSpace(current),52,-1,m_CurrentTokenIndex+1);
			_VerifyCondition(IsVarNameChar(current) || current == '<' || current == '>' || current == ',',44);
			if (!(IsSpace(current) && m_CurrentString.empty())) {
				m_CurrentString+=current;
			}
		}

		_VerifyCondition(!tokens.empty(),43);
		_PushToken(TokenType::GenericDeclarationStart,"");
		for (auto &i : tokens) {
			auto program = _SubParse(i,false);
			_VerifyCondition(program.Tokens.at(0).TokenType == TokenType::TypeIdentifier || g_DataTypes.contains(program.Tokens.at(0).Data),46);
			for (const Token& tok :program.Tokens) {
				_PushToken(tok);
			}
			_PushToken(TokenType::Comma,",");
		}
		if (_GetLastToken().TokenType == TokenType::Comma) {
			m_ProgramInfo.Tokens.pop_back();
		}
		_PushToken(TokenType::GenericDeclarationEnd,"");
		current = _GetNextChar();
		_VerifyCondition(!IsVarNameChar(current),47,m_CurrentTokenIndex-2);
		current = _SkipSpaces();
		if (!IsSpace(current) && current != '\0') {
			_Backtrack();
		}
	}

	Error Lexer::_CreateError(std::string& ErrorMsg, std::string& Advice, std::string& ErrorType) {
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
	void Lexer::_RaiseError(Error& err) 
	{
		PrintError(err);
		CLEAR_HALT();
	}

	void Lexer::_VerifyCondition(bool condition, std::string Error, std::string Advice, std::string ErrorType) {
		if (!condition) {
			auto err = _CreateError(Error,Advice,ErrorType);
			if (!IsSubLexer) {
				_RaiseError(err);
			}else {
				m_subLexerError = true;
				m_ProgramInfo.Errors.push_back(err);
			}
		}
	}



	void Lexer::_VariableNameState()
	{
		char current = _GetNextChar();
		bool isDeclaration = IsTokenOfType(_GetLastToken(1),"is_declaration") && ( _GetLastToken().TokenType == TokenType::TypeIdentifier || g_DataTypes.contains(_GetLastToken().Data));
		current = _SkipSpaces();

		// if ((current == ':' || g_OperatorMap.contains(Str(current))) && current != '*' && current != '<') {
		// 	_Backtrack();
		// 	_VerifyCondition(!IsType,7);
		// 	m_CurrentState = LexerState::Default;
		// 	return;
		// }
		if (current == '(') {
			_Backtrack();
			m_CurrentState = LexerState::Default;

			return;
		}
		if (current == '<') {
			_Backtrack();
			_ParseGenericDeclaration();
			current = _GetNextChar();

		}
		if (current == '*') {
			_Backtrack();
			_ParsePointerDeclaration();
			current = _GetNextChar();
		}


		m_CurrentString.clear();
		if (current == '[') {
			_ParseArrayDeclaration();
			current = _GetNextChar();
		}
		m_CurrentString.clear();
		_VerifyCondition(!std::isdigit(current), 11,m_CurrentTokenIndex-1);
		_VerifyCondition(current != '*',26);
		if (m_NoVariableNames) {
			_VerifyCondition(!g_OperatorMap.contains(Str(current)), 10,m_CurrentTokenIndex-1);
			_VerifyCondition(std::isspace(current) || current == '\0',42,m_TokenIndexStart-1,m_CurrentTokenIndex-1);

			m_CurrentState = LexerState::Default;
			return;

		}
		if (current == '\n' || current == '\0' || !IsVarNameChar(current)) {
			_VerifyCondition(!g_OperatorMap.contains(Str(current)), 10);
			//
			// if (!IsVarNameChar(current) && current != '\0' && current != '\n') {
			// 	_VerifyCondition(!IsType, 9);
			// }
			_VerifyCondition(!isDeclaration,8);

			m_CurrentState = LexerState::Default;
			_Backtrack();
			return;
		}
		if (!isDeclaration) {
			_Backtrack();
			m_CurrentState = LexerState::Default;
			return;
		}


		int commas = 0;
		int vars = 0;

		bool ExpectingComma = false;
		int lastValidVar = m_CurrentTokenIndex-1;
		while ((current != '\0' && current != '\n') && (IsVarNameChar(current) || IsSpace(current)) ) {
			_VerifyCondition(!(m_CurrentString.empty() && std::isdigit(current)),11, m_CurrentTokenIndex-1,-1);

			if (!IsSpace(current)) {
				m_CurrentString += current;
			}
			current = _GetNextChar();

			if (IsSpace(current) && !m_CurrentString.empty()) {
				ExpectingComma = true;
			}


			_VerifyCondition(!(ExpectingComma && IsVarNameChar(current)),12,lastValidVar);


			if (current == ',') {
				ExpectingComma = false;
				_VerifyCondition(!m_CurrentString.empty(),28);
				_VerifyCondition(!g_KeyWordMap.contains(m_CurrentString),39,lastValidVar+1,m_CurrentTokenIndex-2,m_CurrentString);
				lastValidVar = m_CurrentTokenIndex-1;
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
			_VerifyCondition(!g_KeyWordMap.contains(m_CurrentString),39,m_CurrentString);
			_PushToken(TokenType::VariableName, m_CurrentString);
			vars++;

		}
		_VerifyCondition(commas < vars,28);
		if (!IsSpace(current)) {
			_Backtrack();
		}
		m_CurrentString.clear();

		m_CurrentState = LexerState::Default;
	}

	void Lexer::_FunctionParameterState()
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
			// auto ParameterTokens = _ParseFunctionParameter(i,info.indexes.at(ind),info.indexes.at(ind+1));
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
		m_CurrentState = LexerState::Default;
		current = _SkipSpaces();
		if (current != ')')
			_Backtrack();
	}

	void Lexer::_FunctionNameState()
	{
		char current = _GetNextChar();

		current = _SkipSpaces();
		m_CurrentString.clear();
		if (current == '(')
		{
			_Backtrack();
			m_CurrentState = LexerState::FunctionParameters;
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
		m_CurrentState = LexerState::FunctionParameters;
	}

	void Lexer::_IncrementOperator() {
		char current = _GetNextChar();
		char incrementType = _GetLastToken().Data.at(0);
		TokenType tok = incrementType == '+' ? TokenType::AddOp : TokenType::SubOp;
		if (current != incrementType) {
			_Backtrack();
			if (_GetLastToken().TokenType!= TokenType::Increment && _GetLastToken().TokenType!= TokenType::Decrement) {
				m_CurrentState = LexerState::RValue;
			}else {
				m_CurrentState = LexerState::Default;
			}
			return;
		}
		m_ProgramInfo.Tokens.pop_back();
		if (!IsTokenOfType(_GetLastToken(),"allow_op")) {
			if (tok == TokenType::SubOp) {
				tok = TokenType::Negation;
			}
		}
		_PushToken(tok,Str(incrementType));
		_PushToken(tok,Str(incrementType));
		while (current == incrementType) {
			_PushToken(tok,Str(incrementType));
			current = _GetNextChar();
		}
		_Backtrack();
		m_CurrentState = LexerState::RValue;
	}


	void Lexer::_OperatorState()
	{
		_Backtrack();
		std::string before = Str(_GetNextChar());
		std::string h = "";
		char current  = before.at(0);
		while (g_OperatorMap.contains(Str(current)) && !(h.size()>1 &&g_OperatorMap.contains(h)))
		{
			h+=current;
			current = _GetNextChar();
		}

		LexerMapValue value;
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

	void Lexer::_AsterisksState() {
		if (IsTokenOfType(_GetLastToken(),"allow_op")) {
			_PushToken(TokenType::MulOp,"*");
		}else {
			_PushToken(TokenType::DereferenceOp,"*");
		}
		m_CurrentState = LexerState::RValue;

	}

	void Lexer::_AmpersandState() {
		auto token = _GetLastToken();
		auto tok =token.TokenType;

		if (tok == TokenType::VariableReference || tok == TokenType::RValueChar || tok == TokenType::RValueNumber || tok == TokenType::RValueString || tok == TokenType::CloseBracket || tok == TokenType::EndFunctionArguments) {
			_PushToken(TokenType::BitwiseAnd,"&");
		}else {
			_PushToken(TokenType::AddressOp,"&");
		}
		m_CurrentState = LexerState::RValue;

	}


	void Lexer::_IndentationState()
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

		m_CurrentState = LexerState::Default;
		_Backtrack();
	}

	void Lexer::_ParseHexLiteral() {
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
	void Lexer::_ParseBinaryLiteral() {
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

	void Lexer::_ParseExponentNumber(std::string x) {
		m_TokenIndexStart = m_CurrentTokenIndex;
		char current = _GetNextChar();
		bool usedDecimal = false;
		m_CurrentString.clear();
		while (std::isalnum(current) || current == '.' || current == '_')
		{
			if (current != '_')
			{
				m_CurrentString.push_back(current);
			}

			_VerifyCondition(!(current == '.' && usedDecimal),21);

			if (current == '.')
			{
				usedDecimal = true;
			}


			current = _GetNextChar();
		}

		_VerifyCondition(IsValidNumber(m_CurrentString),20,-1,m_CurrentTokenIndex-2);
		_PushToken(TokenType::RValueNumber,std::to_string(std::stod(x)*std::pow(10.0,std::stod(m_CurrentString))));
		m_CurrentString.clear();
	}



	void Lexer::_ParseNumber()
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

		while (std::isalnum(current) || current == '.' || current == '_')
		{
			if (current == 'e') {
				_VerifyCondition(IsValidNumber(m_CurrentString),20,-1,m_CurrentTokenIndex-2);
				return _ParseExponentNumber(m_CurrentString);
			}
			if (current != '_')
			{
				m_CurrentString.push_back(current);
			}

			_VerifyCondition(!(current == '.' && usedDecimal),21);

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

	ProgramInfo Lexer::_SubParse(std::string arg, bool allowvarname) {
		Lexer subLexer;
		subLexer.InitLexer();
		subLexer.m_Buffer = arg;
		subLexer.m_Buffer+=" ";
		subLexer.m_ScopeStack = m_ScopeStack;
		subLexer.IsSubLexer = true;
		subLexer.m_NoVariableNames = !allowvarname;
		ProgramInfo info = subLexer.ParseProgram();

		if (!info.Errors.empty()) {
			auto cause = info.Errors.front();
			_VerifyCondition(false,cause.ErrorMessage,cause.Advice,cause.ErrorType,m_TokenIndexStart,m_TokenIndexStart+(cause.to-cause.from));
			// _Vali(cause.ErrorMessage,cause.Advice,cause.);
			// _RaiseError();
		}
		if (info.Tokens.front().TokenType == TokenType::OpenBracket && info.Tokens.back().TokenType == TokenType::CloseBracket) {
			info.Tokens.pop_back();
			info.Tokens.erase(info.Tokens.begin());
		}
		return info;
	}

	ProgramInfo Lexer::_SubParse(std::string arg) {
		Lexer subLexer;
		subLexer.InitLexer();
		subLexer.m_Buffer = arg;
		subLexer.m_Buffer+=" ";
		subLexer.m_ScopeStack = m_ScopeStack;
		subLexer.IsSubLexer = true;
		ProgramInfo info = subLexer.ParseProgram();

		if (!info.Errors.empty()) {
			auto cause = info.Errors.front();
			_VerifyCondition(false,cause.ErrorMessage,cause.Advice,cause.ErrorType,m_TokenIndexStart,m_TokenIndexStart+(cause.to-cause.from));
			// _Vali(cause.ErrorMessage,cause.Advice,cause.);
			// _RaiseError();
		}
		if (info.Tokens.front().TokenType == TokenType::OpenBracket && info.Tokens.back().TokenType == TokenType::CloseBracket) {
			info.Tokens.pop_back();
			info.Tokens.erase(info.Tokens.begin());
		}
		return info;
	}


	void Lexer::_ParseList() {
		m_CurrentErrorState = "List literal";
		auto  bracketInfo = _ParseBrackets('}',true);
		_PushToken(TokenType::StartArray,"{");

 		for (const std::string& arg : bracketInfo.tokens) {

			ProgramInfo info = _SubParse(arg,false);
			for (const Token& tok :info.Tokens) {
				_PushToken(tok);
			}
			_PushToken(TokenType::Comma, "");
		}
		if (_GetLastToken().TokenType == TokenType::Comma) {
			m_ProgramInfo.Tokens.pop_back();
		}


		_PushToken(TokenType::EndArray,"}");
		m_CurrentState = LexerState::Default;
	}


	void Lexer::_ParseChar() {
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
				data= '\0';
			}else if(current == 'f') {
				data = '\f';
			}else if(current == 'v') {
				data = '\v';
			}else if(current == 'a') {
				data = '\a';
			}
			else {
				std::string message = "\"\\"+ Str(current)+"\"";
				_VerifyCondition(false,13,m_TokenIndexStart+1,message);

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


	void Lexer::_ParseString()
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

	void Lexer::_ParseOther()
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

		if (_IsTypeDeclared(m_CurrentString) && _GetLastToken().TokenType != TokenType::DotOp) {
			_PushToken(TokenType::TypeIdentifier, m_CurrentString);
			m_CurrentString.clear();
			m_CurrentState= LexerState::VariableName;
			_Backtrack();
			return;

		}

		if (g_KeyWordMap.contains(m_CurrentString))
		{
			auto& value = g_KeyWordMap.at(m_CurrentString);
			_PushToken({ .TokenType = value.TokenToPush, .Data = m_CurrentString });
			if (g_DataTypes.contains(m_CurrentString)) {
				m_CurrentString.clear();
				m_CurrentState= LexerState::VariableName;
				_Backtrack();
				return;

			}
		}else {
			_PushVariableReference( m_CurrentString);
		}

		m_CurrentString.clear();
		m_CurrentState = LexerState::Default;
		_Backtrack();
	}
}
