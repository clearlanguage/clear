#include "Parser.h"
#include <sstream>
#include <functional>
#include <algorithm>
#include <map>
#include <fstream>
#include <iostream>
#include <iosfwd>
#include <Core/Log.h>
#include <Core/Utils.h>

bool isValidNumber(const std::string& str) 
{
	if (str.empty()) 
		return false; // Empty string is not a number
	
	bool hasDecimalPoint = false;
	bool hasSign = false;

	for (char c : str) 
	{
		if (c == '-' || c == '+') 
		{
			if (hasSign || !hasDecimalPoint) 
			{
				return false; // Invalid sign placement
			}

			hasSign = true;

		} else if (c == '.') 
		{
			if (hasDecimalPoint)
			{
				return false; // Multiple decimal points
			}
			
			hasDecimalPoint = true;

		} 
		else if (!std::isdigit(c)) 
		{
			return false; // Non-numeric character
		}
	}

	return true;
}

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
		m_StateMap[ParserState::FunctionParamters] = [this]()  {_FunctionParamterState(); };
		m_StateMap[ParserState::Comment] = [this]() { _CommentState(); };
		m_StateMap[ParserState::MultilineComment] = [this]() { _MultiLineCommentState(); };
		m_StateMap[ParserState::IndexOperator] = [this]() { _IndexOperatorState(); };

	}

	void Parser::_PushToken(const TokenType tok, const std::string &data) 
	{
		m_ProgramInfo.Tokens.push_back({ .TokenType = tok, .Data = data });
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
		m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::EndLine });
	}

	ProgramInfo Parser::ParseProgram() 
	{
		while (m_CurrentTokenIndex < m_Buffer.length())
		{
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


	void Parser::_FunctionParamterState() {
		char current = _GetNextChar();

		while (IsSpace(current))
			current = _GetNextChar();

		m_CurrentString.clear();
		CLEAR_VERIFY(current == '(', "expected ( after function call");
		std::vector<std::string> argList;
		bool detectedEnd = false;
		int opens =1;
		while (opens !=0 && current != '\0')
		{
			current = _GetNextChar();
			if (current == '(')
				opens++;
			if (current == ')')
				opens--;
			if ( (current ==')' && opens== 0) || (current == ',' && opens == 1) || current == '\0')
			{

				if (current == ')' )
					detectedEnd = true;

				if (!m_CurrentString.empty())
					argList.push_back(m_CurrentString);
				else {
					if (current == ',') {
					CLEAR_LOG_ERROR("Expected function Paramter after commas");
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

		CLEAR_VERIFY(detectedEnd, "Expected ) after function call");
		// m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::StartFunctionParameters, .Data = "" });
		m_CurrentState = ParserState::Default;
		for (const std::string& arg : argList) {
			Parser subParser;
			subParser.InitParser();
			subParser.m_Buffer = arg;
			subParser.m_Buffer+=" ";
			ProgramInfo info = subParser.ParseProgram();
			for (const Token& tok :info.Tokens) {
				m_ProgramInfo.Tokens.push_back(tok);
			}
			_PushToken(TokenType::Comma, "");
		}
		if (m_ProgramInfo.Tokens.at(m_ProgramInfo.Tokens.size()-1).TokenType == TokenType::Comma) {
			m_ProgramInfo.Tokens.pop_back();
		}

		_PushToken(TokenType::CloseBracket,")");
		current = _GetNextChar();
		while (IsSpace(current))
			current = _GetNextChar();
		
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

	bool Parser::_IsLineClosed() const {

		if (m_ProgramInfo.Tokens.empty())
			return true;

		return m_ProgramInfo.Tokens.at(m_ProgramInfo.Tokens.size()-1).TokenType == TokenType::EndLine;
	}


	void Parser::_IndexOperatorState() {
		char current = _GetNextChar();
		bool detectedEnd = false;
		int opens =1;

		CLEAR_VERIFY(current == '[', "index op should start with [");
		while (opens !=0 && current != '\0')
		{
			current = _GetNextChar();
			if (current == '[')
				opens++;
			if (current == ']')
				opens--;

			if (opens ==0 && current == ']') {
				detectedEnd = true;
				break;
			}

			if((!(IsSpace(current) && m_CurrentString.empty()) && current!= '\n'))
				m_CurrentString += current;


		}
		CLEAR_VERIFY(detectedEnd, "Expected ] after index call");
		Parser subParser;
		subParser.InitParser();
		subParser.m_Buffer = m_CurrentString;
		subParser.m_Buffer+=" ";
		ProgramInfo info = subParser.ParseProgram();
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
					CLEAR_VERIFY(!isValidNumber(m_CurrentString),"Cannot call a number")
					_PushToken(TokenType::VariableReference, m_CurrentString);
				}

				_PushToken(TokenType::FunctionCall, m_CurrentString);
				m_CurrentState = ParserState::FunctionParamters;
				_Backtrack();

			}
			m_BracketStack.push_back('(');
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::OpenBracket, .Data = "(" });


			return;
		}
		if (current == '"' ) {
			CLEAR_VERIFY(m_CurrentString.empty(), "Attempting to close unopened string");
			_ParseString();
		}
		if (current == '\'') {
			CLEAR_VERIFY(m_CurrentString.empty(), "Attempting to close unopened char");
			_ParseChar();
		}

		bool TreatAsNum = current == '.' && isValidNumber(m_CurrentString);
		if (IsVarNameChar(current) || TreatAsNum)
			m_CurrentString += current;

		if (!m_CurrentString.empty() && !IsVarNameChar(current) && !TreatAsNum)
		{
			if (g_KeyWordMap.contains(m_CurrentString) ) {
				auto& value = g_KeyWordMap.at(m_CurrentString);

				m_CurrentState = value.NextState;

				if (value.TokenToPush != TokenType::None)
					m_ProgramInfo.Tokens.push_back({ .TokenType = value.TokenToPush, .Data = m_CurrentString });

				m_CurrentString.clear();

			}else {
				if (isValidNumber(m_CurrentString)) {
					_PushToken(TokenType::RValueNumber, m_CurrentString);
					m_CurrentString.clear();

				}else {

				_PushToken(TokenType::VariableReference, m_CurrentString);
				m_CurrentState = ParserState::VariableName;
				m_CurrentString.clear();
				}
			}
		}

		if (current == ':' || current == '\n')
		{
			m_CurrentState = ParserState::Indentation;
			m_CurrentString.clear();
			return;
		}

		if (g_OperatorMap.contains(Str(current)) && !TreatAsNum)
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
		while (IsSpace(current))
			current = _GetNextChar();

		m_CurrentString.clear();

		//allow _ and any character from alphabet
		while (IsVarNameChar(current))
		{
			m_CurrentString += current;
			current = _GetNextChar();
		}

		m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::FunctionType, .Data =m_CurrentString });
		if(g_DataTypes.contains(m_CurrentString)) 
		{
			auto& value = g_KeyWordMap.at(m_CurrentString);
			if (value.TokenToPush != TokenType::None)
				m_ProgramInfo.Tokens.push_back({ .TokenType = value.TokenToPush, .Data =m_CurrentString });
		}
		else 
		{
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::VariableReference, .Data =m_CurrentString });
		}

		_Backtrack();
		m_CurrentString.clear();
		m_CurrentState = ParserState::Default;

	}

	void Parser::_StructNameState() {
		char current = _GetNextChar();

		while (IsSpace(current))
			current = _GetNextChar();

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


	void Parser::_ParsingRValueState()
	{
		char current = _GetNextChar();

		//want to ignore all spaces in between = and actual variable
		while (IsSpace(current))
			current = _GetNextChar();

		m_CurrentString.clear();

		//brackets
		if (current == '(')
		{
			m_BracketStack.push_back('(');
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::OpenBracket, .Data = "(" });
			m_CurrentState = ParserState::RValue;
			return;
		}
		else if (current == ')')
		{
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::CloseBracket, .Data = ")" });
			m_CurrentState = ParserState::RValue;
			
			CLEAR_VERIFY(!m_BracketStack.empty() && m_BracketStack.back() == '(', "closing brackets unmatched");
			m_BracketStack.pop_back();

			return;
		}
		else if (current == '"') //strings
		{
			_ParseString();
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

	void Parser::_VariableNameState() {
		char current = _GetNextChar();

		//want to ignore all spaces in between type and variable
		while (IsSpace(current))
			current = _GetNextChar();
		if (current == ':' || g_OperatorMap.contains(Str(current))) {
			_Backtrack();
			m_CurrentState = ParserState::Default;
			return;
		}
		m_CurrentString.clear();
		int commas = 0;
		int vars = 0;
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

		while (IsSpace(current))
			current = _GetNextChar();

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
			auto spL = Split(i);

			CLEAR_VERIFY(spL.size() == 2, "expected variable and type only");
		
			if(g_DataTypes.contains(spL.at(0))) 
			{
				auto& value = g_KeyWordMap.at(spL.at(0));
				if (value.TokenToPush != TokenType::None)
					m_ProgramInfo.Tokens.push_back({ .TokenType = value.TokenToPush, .Data = spL.at(0) });

			}
			else 
			{
				m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::VariableReference, .Data =spL.at(0) });
			}
			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::VariableName, .Data = spL.at(1) });

		}
		m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::EndFunctionParameters, .Data = "" });
		m_CurrentState = ParserState::Default;
		if (current != ')')
			_Backtrack();
	}


	void Parser::_FunctionNameState() 
	{
		char current = _GetNextChar();

		while (IsSpace(current))
			current = _GetNextChar();

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

		while (true)
		{
			if (std::isdigit(current))
			{
				m_CurrentString.push_back(current);
			}
			else if (current == '.' && usedDecimal) // need to throw some type of error again TODO
			{
				CLEAR_LOG_ERROR("float cannot have two decimal points");
				CLEAR_HALT();
			}
			else if (current == '.')
			{
				m_CurrentString.push_back(current);
				usedDecimal = true;
			}
			else
			{
				break;
			}

			current = _GetNextChar();
		}
		if (m_CurrentString == "-") {
			_PushToken(TokenType::SubOp,"-");
		}else {

			m_ProgramInfo.Tokens.push_back({ .TokenType = TokenType::RValueNumber, .Data = m_CurrentString });
		}
		m_CurrentString.clear();
		_Backtrack();
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