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
		m_StateMap[ParserState::FunctionParamaters] = [this]()  {_FunctionParamaterState(); };
		m_StateMap[ParserState::Comment] = [this]() { _CommentState(); };
		m_StateMap[ParserState::MultilineComment] = [this]() { _MultiLineCommentState(); };
		m_StateMap[ParserState::IndexOperator] = [this]() { _IndexOperatorState(); };
		m_StateMap[ParserState::AsterisksOperator] = [this]() {_AsterisksState();};

	}

	void Parser::_PushToken(const TokenType tok, const std::string &data) 
	{
		m_ProgramInfo.Tokens.push_back({ .TokenType = tok, .Data = data });
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
		if ( _GetLastToken().TokenType != TokenType::EndLine)
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::EndLine });
		m_CurrentLine++;
	}

	ProgramInfo Parser::ParseProgram() 
	{
		while (m_CurrentTokenIndex < m_Buffer.length())
		{
			m_TokenIndexStart = m_CurrentTokenIndex;
			m_StateMap.at(m_CurrentState)();
		}

		while (m_Indents > 0)
		{
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::EndIndentation });
			m_Indents--;
		}

		return m_ProgramInfo;
	}

	void Parser::InitParser() 
	{
		m_ProgramInfo.Tokens.clear();
		m_CurrentTokenIndex = 0;
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

	void Parser::_FunctionParamaterState() {
		char current = _GetNextChar();

		current = _SkipSpaces();
		m_CurrentString.clear();
		CLEAR_VERIFY(current == '(', "expected ( after function call");

		// m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::StartFunctionParameters, .Data = "" });
		auto argList = _ParseBrackets(')',true);
		m_CurrentState = ParserState::Default;
		for (const std::string& arg : argList) {
			ProgramInfo info = _SubParse(arg);
			for (const Token& tok :info.Tokens) {
				m_ProgramInfo.Tokens.push_back(tok);
			}
			_PushToken(TokenType::Comma, "");
		}
		if (_GetLastToken().TokenType == TokenType::Comma) {
			m_ProgramInfo.Tokens.pop_back();
		}

		_PushToken(TokenType::CloseBracket,")");
		current = _GetNextChar();
		_SkipSpaces();		
		_Backtrack();
	}


	void Parser::_MultiLineCommentState() {
		char current = _GetNextChar();
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

	bool Parser::_IsLineClosed() {

		if (m_ProgramInfo.Tokens.empty())
			return true;
		TokenType tok = _GetLastToken().TokenType;
		return !(tok == TokenType::CloseBracket);
	}


	std::vector<std::string> Parser::_ParseBrackets(char end, bool commas) {
		char start = g_CloserToOpeners.at(end);
		char current = start;
		std::vector<std::string> argList;
		std::vector<char> stack;
		stack.push_back(start);
		bool detectedEnd = false;


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
						CLEAR_VERIFY(g_CloserToOpeners.at(current) == stack.back(),"Attempting to close wrong bracket");
						stack.pop_back();
					}
				}else {
					if ((current == '\'' || current == '"')) {
						if (m_Buffer[m_CurrentTokenIndex-2] != '\\') {
							CLEAR_VERIFY(current == stack.back(),"Closing unmatched string");
							stack.pop_back();
						}
					}
				}

			}
			if ( (current == end && stack.empty()) || (current == ',' && stack.size() == 1) || current == '\0')
			{
				CLEAR_VERIFY((current == ',' && commas) || current != ',',"Did not expect commas" );
				if (current == end)
					detectedEnd = true;

				if (!m_CurrentString.empty())
					argList.push_back(m_CurrentString);
				else {
					if (current == ',') {
						CLEAR_LOG_ERROR("Expected function Paramater after commas");
						CLEAR_HALT();
					}
				}

				m_CurrentString.clear();

			}
			else
			{
				if(!(std::isspace(current) && m_CurrentString.empty()))
					m_CurrentString += current;
			}


		}



		CLEAR_VERIFY(detectedEnd, "Expected " , end );

		return argList;

	}


	void Parser::_IndexOperatorState() {
		char current = _GetNextChar();
		CLEAR_VERIFY(current == '[', "index op should start with [");


		auto parsed= _ParseBrackets(']',false);
		CLEAR_VERIFY(!parsed.empty(), "Expected value inside brackets");

		ProgramInfo info = _SubParse( parsed[0]);
		for (const Token& tok :info.Tokens) {
			m_ProgramInfo.Tokens.push_back(tok);
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
					CLEAR_VERIFY(!IsValidNumber(m_CurrentString),"Cannot call a number")
					_PushToken(TokenType::VariableReference, m_CurrentString);
				}

				_PushToken(TokenType::FunctionCall, m_CurrentString);
				m_CurrentState = ParserState::FunctionParamaters;
				_Backtrack();

			}else {
				m_BracketStack.push_back('(');
			}

			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::OpenBracket, .Data = "(" });


			return;
		}
		if (current == '"' ) {
			CLEAR_VERIFY(m_CurrentString.empty(), "Attempting to close unopened string");
			_ParseString();
		}
		if (current == '{') {
			CLEAR_VERIFY(m_CurrentString.empty(), "Cannot start list during");
			_ParseList();
		}
		if (current == '\'') {
			CLEAR_VERIFY(m_CurrentString.empty(), "Attempting to close unopened char");
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
					m_ProgramInfo.Tokens.push_back({ .TokenType = value.TokenToPush, .Data = m_CurrentString });

				m_CurrentString.clear();
				if (!IsSpace(current))
					_Backtrack();
				return;

			}
			if ((!g_OperatorMap.contains(Str(current)) && current != '\n' && current != ')') || (( current == '*' || current == '&'))) {
				_PushToken(TokenType::VariableReference, m_CurrentString);
				m_CurrentState = ParserState::VariableName;
				m_CurrentString.clear();


				if (!IsSpace(current))
					_Backtrack();

				return;
			}else {
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
			CLEAR_VERIFY(!m_BracketStack.empty() && m_BracketStack.back() == '(', "Closing brackets unmatched");

			m_BracketStack.pop_back();
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::CloseBracket, .Data = ")"});

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
		m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::FunctionType, .Data =m_CurrentString });

		ProgramInfo info = _SubParse(m_CurrentString+" functype ");
		if (info.Tokens.back().TokenType ==  TokenType::VariableName && info.Tokens.back().Data == "functype") {
			info.Tokens.pop_back();
		}
		for (const Token& tok :info.Tokens) {
			m_ProgramInfo.Tokens.push_back(tok);
		}

		_Backtrack();
		m_CurrentString.clear();
		m_CurrentState = ParserState::Default;

	}

	void Parser::_StructNameState() {
		char current = _GetNextChar();

		current = _SkipSpaces();
		_VerifyCondition((current != ':' && current != '\n' &&current != '\0'),"Expected struct name after struct declaration","Maybe add a name after the struct keyword","StructNoName","struct");
		if (current == ':') {
			CLEAR_LOG_ERROR("Expected struct name?");
			CLEAR_HALT();
		}
		m_CurrentString.clear();
		while (IsVarNameChar(current))
		{
			m_CurrentString += current;
			current = _GetNextChar();
		}


		_PushToken(TokenType::StructName, m_CurrentString);
		m_CurrentString.clear();
		_Backtrack();
		m_CurrentState = ParserState::Default;
	}

	Token Parser::_CreateToken(const TokenType tok, const std::string &data) {
		return Token{ .TokenType = tok, .Data = data };
	}


	void Parser::_ParsingRValueState()
	{
		char current = _GetNextChar();

		//want to ignore all spaces in between = and actual variable
		current = _SkipSpaces();
		m_CurrentString.clear();

		//brackets
		if (g_OperatorMap.contains(Str(current))) {
			m_CurrentState = ParserState::Operator;
			return;
		}
		if (current == '(')
		{
			m_BracketStack.push_back('(');
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::OpenBracket, .Data = "(" });
			m_CurrentState = ParserState::RValue;
			return;
		}
		if (current == ')')
		{
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::CloseBracket, .Data = ")" });
			m_CurrentState = ParserState::RValue;
			
			CLEAR_VERIFY(!m_BracketStack.empty() && m_BracketStack.back() == '(', "closing brackets unmatched");
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

	void Parser::_ParseArrayDecleration(ArrayDeclarationReturn& output) {
		auto parsed = _ParseBrackets(']',false);
		m_CurrentString.clear();
		if (!parsed.empty()) {
			m_CurrentString = parsed.at(0);
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
			}
		}
		m_CurrentString.clear();
		char current = _GetNextChar();
		while (IsSpace(current)) {
			current = _GetNextChar();
		}
		CLEAR_VERIFY(current != ']',"Attempting to close unopened array decleration")
		if (current == '[') {
			_ParseArrayDecleration(output);
		}else  {
			if (current != '\0')
				_Backtrack();
		}


	}

	int Parser::_ParsePointerDecleration() {
		char current = _GetNextChar();
		int pointers = 0;
		while (current == '*') {
			current = _GetNextChar();
			pointers++;
		}
		current = _SkipSpaces();
		CLEAR_VERIFY(current != '*', "No spaces between pointer defs allowed");
		if (!IsSpace(current) && current != '\0') {
			_Backtrack();
		}
		return pointers;
		
	}

	Error Parser::_CreateError(std::string& ErrorMsg, std::string& Advice, std::string& ErrorType, std::string& Cause) {
		Error err;
		err.ErrorMessage = ErrorMsg;
		err.Advice = Advice;
		err.ErrorType = ErrorType;
		err.ErrorCause = Cause+m_Buffer.substr(m_TokenIndexStart,m_CurrentTokenIndex-m_TokenIndexStart);
		err.ErrorCause = replaceAll(err.ErrorCause,"\n","\\n");
		err.line = m_CurrentLine-1;
		if (err.line == 0) {
			err.line = 1;
		}
		return err;

	}
	void Parser::_RaiseError(Error& err) {
		PrintError(err);
		CLEAR_HALT();
	}

	void Parser::_VerifyCondition(bool condition, std::string Error, std::string Advice, std::string ErrorType,std::string Cause) {
		if (!condition) {
			auto err = _CreateError(Error,Advice,ErrorType,Cause);
			if (!IsSubParser) {
				_RaiseError(err);
			}else {
				m_ProgramInfo.Errors.push_back(err);
			}
		}
	}



	void Parser::_VariableNameState() {
		char current = _GetNextChar();

		current = _SkipSpaces();
		bool CompilerType = g_DataTypes.contains(_GetLastToken().Data);
		bool variableState = false;
		bool bracketState = false;
		int pointers = 0;
		int prevTokenIndex = 0;
		if ((current == ':' || g_OperatorMap.contains(Str(current))) && current != '*') {
			_Backtrack();
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
			pointers = _ParsePointerDecleration();
			current = _GetNextChar();
		}


		m_CurrentString.clear();
		ArrayDeclarationReturn ArrayDeclarations;
		if (current == '[') {
			prevTokenIndex = m_CurrentTokenIndex;

			_ParseArrayDecleration(ArrayDeclarations);
			current = _GetNextChar();
			bracketState = true;
		}
		m_CurrentString.clear();
		if (current == '\n' || current == '\0' || !IsVarNameChar(current)) {
			_VerifyCondition(!(variableState && bracketState) , "Expected variable name after type declaration","Maybe add a variable name after type declaration","MissingVariableName","");
			if (!IsVarNameChar(current) && current != '\0' && current != '\n') {

				CLEAR_VERIFY(!CompilerType, "cannot index compiler type");
			}
			if (bracketState || variableState) {
				m_CurrentTokenIndex = prevTokenIndex;
			}
			m_CurrentState = ParserState::Default;
			_Backtrack();
			return;
		}



		int commas = 0;
		int vars = 0;
		CLEAR_VERIFY(!ArrayDeclarations.error,ArrayDeclarations.errormsg);
		for (int i = 0; i < pointers; i++) {
			_PushToken(TokenType::PointerDef,"*");
		}
		for (auto tok :ArrayDeclarations.Tokens) {
			m_ProgramInfo.Tokens.push_back(tok);
		}
		while ((current != '\0' || current != '\n') && (IsVarNameChar(current) || IsSpace(current)) ) {
			if (!IsSpace(current)) {
				m_CurrentString += current;
			}
			current = _GetNextChar();

			if (current == ',') {
				CLEAR_VERIFY(!m_CurrentString.empty(),"Expected variable name after comma")
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
			CLEAR_VERIFY(current != ',',"Expected variable name after comma")
		}
		if (!m_CurrentString.empty()) {
			_PushToken(TokenType::VariableName, m_CurrentString);
			vars++;

		}
		CLEAR_VERIFY(commas < vars, "Expected variable names after comma did not expect trailing comma");
		if (!IsSpace(current)) {
			_Backtrack();
		}
		m_CurrentString.clear();

		m_CurrentState = ParserState::Default;
	}

	void Parser::_FunctionParameterState() 
	{
		char current = _GetNextChar();

		current = _SkipSpaces();
		m_CurrentString.clear();
		CLEAR_VERIFY(current == '(', "expected ( after function decleartion");

		std::vector<std::string> argList;
		bool detectedEnd = false;

		while (current != ')' && current != '\0') 
		{
			current = _GetNextChar();

			if (current==',' || current ==')' || current == '\0' )
			{
				if (current == ')') 
					detectedEnd = true;

				if (!m_CurrentString.empty())
					argList.push_back(m_CurrentString);

				m_CurrentString.clear();

			}
			else 
			{
				if(!(IsSpace(current) && m_CurrentString.empty()))
					m_CurrentString += current;
			}

		}

		CLEAR_VERIFY(detectedEnd, "Expected ) after function decleartion");
		m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::StartFunctionParameters, .Data = "" });

		for (const auto& i: argList) 
		{

			ProgramInfo info = _SubParse(i);
			for (const Token& tok :info.Tokens) {
				m_ProgramInfo.Tokens.push_back(tok);
			}

		}
		m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::EndFunctionParameters, .Data = "" });
		m_CurrentState = ParserState::Default;
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
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::Lambda, .Data = ""});
			return;
		}

		while (IsVarNameChar(current))
		{
			m_CurrentString += current;
			current = _GetNextChar();
		}

		if (current =='(')
			_Backtrack();

		m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::FunctionName, .Data = m_CurrentString });
		m_CurrentString.clear();

		CLEAR_VERIFY(current != '\n', "did not expect new line after function def")
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
			m_ProgramInfo.Tokens.push_back({ .TokenType = value.TokenToPush, .Data = data });

		m_CurrentState = value.NextState;
	}

	void Parser::_AsterisksState() {
		TokenType tok = _GetLastToken().TokenType;

		if (tok == TokenType::VariableReference || tok == TokenType::RValueChar || tok == TokenType::RValueNumber || tok == TokenType::RValueString) {
			_PushToken(TokenType::MulOp,"*");
		}else {
			_PushToken(TokenType::DereferenceOp,"");
		}
		m_CurrentState = ParserState::Default;

	}


	void Parser::_IndentationState()
	{
		char next = _GetNextChar();
		if (next == '\n')
			next = _GetNextChar();

		bool indenting = true;
		size_t localIndents = 0;

		while (indenting)
		{
			if (next == '\t')
			{
				localIndents++;
				next = _GetNextChar();
				continue;
			}

			size_t spaceCounter = 0;

			if (next == ' ')
			{
				spaceCounter = 1;

				while (next == ' ' && spaceCounter < 4)
				{
					next = _GetNextChar();
					spaceCounter++;
				}
			}

			if (spaceCounter == 4)
				localIndents++;
			else
				indenting = false;
		}

		if (localIndents > m_Indents)
		{
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::StartIndentation });
			m_Indents = localIndents;
		}

		while (m_Indents > localIndents)
		{
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::EndIndentation });
			m_Indents--;
		}

		m_CurrentState = ParserState::Default;
		_Backtrack();
	}

	void Parser::_ParseHexLiteral() {
		m_CurrentString.clear();
		char current = _GetNextChar();
		while (!std::isspace(current) && !g_OperatorMap.contains(Str(current))) {
			CLEAR_VERIFY(current == '0' || current == '1' || current == '2' || current == '3' || current == '4' || current == '5' || current == '6' || current == '7' || current == '8' || current == '9' || current == 'A' || current == 'B' || current == 'C' || current == 'D' || current == 'E' || current == 'F'  || current == 'a' || current == 'b' || current == 'c' || current == 'd' || current == 'e' || current == 'f',"Expected   hexadecimal characters only in hexadecimal literal");
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
			CLEAR_VERIFY(current == '0' || current == '1',"Expected 1 and 0 only in binary literal");
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
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::RValueNumber, .Data = m_CurrentString });
			m_CurrentString.clear();
			return;
		}

		bool usedDecimal = false;
		if (current == 'b') {
			CLEAR_VERIFY(m_CurrentString == "0", "expected binary literal to start with 0");
			_ParseBinaryLiteral();
			return;
		}

		if (current == 'x') {
			CLEAR_VERIFY(m_CurrentString == "0", "expected hex literal to start with 0");
			_ParseHexLiteral();
			return;
		}

		while (std::isalnum(current))
		{
			m_CurrentString.push_back(current);
			if (current == '.' && usedDecimal) // need to throw some type of error again TODO
			{
				CLEAR_LOG_ERROR("float cannot have two decimal points");
				CLEAR_HALT();
			}
			if (current == '.')
			{
				usedDecimal = true;
			}


			current = _GetNextChar();
		}

		CLEAR_VERIFY(IsValidNumber(m_CurrentString),"Expected a valid number");
		if (m_CurrentString == "-") {
			_PushToken(TokenType::SubOp,"-");
		}else {

			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::RValueNumber, .Data = m_CurrentString });
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
		subParser.IsSubParser = true;
		ProgramInfo info = subParser.ParseProgram();

		if (!info.Errors.empty()) {
			_RaiseError(info.Errors.front());
		}
		return info;
	}


	void Parser::_ParseList() {
		auto list = _ParseBrackets('}',true);
		_PushToken(TokenType::OpenBracket,"{");

 		for (const std::string& arg : list) {

			ProgramInfo info = _SubParse(arg);
			for (const Token& tok :info.Tokens) {
				m_ProgramInfo.Tokens.push_back(tok);
			}
			_PushToken(TokenType::Comma, "");
		}
		if (_GetLastToken().TokenType == TokenType::Comma) {
			m_ProgramInfo.Tokens.pop_back();
		}


		_PushToken(TokenType::CloseBracket,"}");
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
				CLEAR_LOG_ERROR("Unknown char escape char \"\\",current,'"');
				CLEAR_HALT();

			}

		}

		// if (current == '\'') {
		// 	current = '';
		// }
		CLEAR_VERIFY(current!= '\'',"No data in char") // Allow this?


		_PushToken(TokenType::RValueChar,Str(data));
		current = _GetNextChar();
		CLEAR_VERIFY(current == '\'', "unclosed char: expected ' after char ");


	}


	void Parser::_ParseString()
	{
		char current = _GetNextChar();
		while (current != '"')
		{
			CLEAR_VERIFY(!(current == '\n' || current == '\0'),"String never closed expected \"")
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

		m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::RValueString, .Data = m_CurrentString });
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
			m_ProgramInfo.Tokens.push_back({ .TokenType = value.TokenToPush, .Data = m_CurrentString });
		}else {
			_PushToken(TokenType::VariableReference, m_CurrentString);
		}

		m_CurrentString.clear();
		m_CurrentState = ParserState::Default;
		_Backtrack();
	}
}