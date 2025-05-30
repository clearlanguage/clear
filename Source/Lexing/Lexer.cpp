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
		m_StateMap[LexerState::Default]      = [this]() { DefaultState(); };
		m_StateMap[LexerState::VariableName] = [this]() { VariableNameState(); };
		m_StateMap[LexerState::RValue]       = [this]() { ParsingRValueState(); };
		m_StateMap[LexerState::Operator]     = [this]() { OperatorState(); };
		m_StateMap[LexerState::Indentation]  = [this]() { IndentationState(); };
		m_StateMap[LexerState::FunctionName] = [this]() {FunctionNameState();};
		m_StateMap[LexerState::FunctionParameters] = [this]() { FunctionParameterState(); };
		m_StateMap[LexerState::ArrowState] = [this](){ArrowState();};
		m_StateMap[LexerState::FunctionTypeState] = [this]() {FunctionTypeState();};
		m_StateMap[LexerState::StructName] = [this]() { StructNameState(); };
		m_StateMap[LexerState::FunctionParamaters] = [this]()  {FunctionArgumentState(); };
		m_StateMap[LexerState::Comment] = [this]() { CommentState(); };
		m_StateMap[LexerState::MultilineComment] = [this]() { MultiLineCommentState(); };
		m_StateMap[LexerState::IndexOperator] = [this]() { IndexOperatorState(); };
		m_StateMap[LexerState::AsterisksOperator] = [this]() {AsterisksState();};
		m_StateMap[LexerState::AmpersandOperator] = [this]() {AmpersandState();};
		m_StateMap[LexerState::Declaration] = [this](){DeclarationState();};
		m_StateMap[LexerState::MinusOperator] = [this]() {MinusOperator();};
		m_StateMap[LexerState::Increment] = [this]() {IncrementOperator();};
		m_StateMap[LexerState::Restriction] = [this]() {RestrictionState();};
		m_StateMap[LexerState::DotOp] = [this]() {DotOpState();};
		m_StateMap[LexerState::ClassName] = [this]() {ClassNameState();};


	}

	void Lexer::PushToken(Token tok) {
		PushToken(tok.TokenType,tok.Data);
	}


	void Lexer::PushToken(const TokenType tok, const std::string &data)
	{
		TokenLocation location;
		location.from = m_TokenIndexStart;
		location.to = m_CurrentTokenIndex;
		location.line = m_CurrentLine;
		m_ProgramInfo.Tokens.push_back({ .TokenType = tok, .Data = data ,.Location = location});
	}

	Token Lexer::GetLastToken() {
		if (m_ProgramInfo.Tokens.empty())
			return Token{.TokenType = TokenType::EndLine,.Data = ""};

		return m_ProgramInfo.Tokens.at(m_ProgramInfo.Tokens.size()-1);
	}

	Token Lexer::GetLastToken(size_t x) {
		if (m_ProgramInfo.Tokens.empty() || x >= m_ProgramInfo.Tokens.size())
			return Token{.TokenType = TokenType::EndLine, .Data = ""};

		return m_ProgramInfo.Tokens.at(m_ProgramInfo.Tokens.size() - 1 - x);
	}


	char Lexer::GetNextChar()
	{
		if(m_Buffer.length() > m_CurrentTokenIndex)
		{
			auto c = m_Buffer[m_CurrentTokenIndex++];
			return c;
		}

		return 0;
	}

	void Lexer::Backtrack()
	{
		m_CurrentTokenIndex--;
	}

	// const bool Lexer::IsEndOfFile()
	// {
	// 	return m_CurrentTokenIndex == m_Buffer.length();
	// }

	void Lexer::ResetSecondState() {
		if (m_SecondState == LexerSecondaryState::None)
		{
			return;
		}

		if (m_SecondState == LexerSecondaryState::Declaration)
		{
			// auto tok = GetLastToken().TokenType;
			// VerifyCondition(tok== TokenType::EndFunctionArguments,40);

		}
		m_SecondState = LexerSecondaryState::None;
	}


	void Lexer::EndLine()
	{
		if ( GetLastToken().TokenType != TokenType::EndLine)
		{
			PushToken({ .TokenType = TokenType::EndLine });
		}
		ResetSecondState();
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
		if (m_subLexerError)
		{
			return m_ProgramInfo;
		}
		CLEAR_PARSER_VERIFY(m_ProgramInfo.Errors.empty(),"99");
		while (m_Indents > 0)
		{
			PushToken({ .TokenType = TokenType::EndIndentation });
			m_Indents--;
		}
		VerifyCondition(m_BracketStack.empty(),1);

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

	 char Lexer::SkipSpaces()
			{
		Backtrack();
		char current = GetNextChar();
		while (IsSpace(current))
			current = GetNextChar();

		return current;
	 }

	void Lexer::MinusOperator()
		{
		auto token = GetLastToken();
		auto tok =token.TokenType;

		if (tok == TokenType::VariableReference || tok == TokenType::RValueChar || tok == TokenType::RValueNumber || tok == TokenType::RValueString || tok == TokenType::CloseBracket || tok == TokenType::EndFunctionArguments || tok == TokenType::Increment || tok == TokenType::Decrement)
		{
			PushToken(TokenType::SubOp,"-");
		}else {
			PushToken(TokenType::Negation,"-");
		}
		m_CurrentState = LexerState::RValue;
	}

	void Lexer::DeclarationState() {
		m_SecondState = LexerSecondaryState::Declaration;
		m_CurrentState = LexerState::Default;
	}



	void Lexer::FunctionArgumentState() {
		GetNextChar();

		char current = SkipSpaces();
		m_CurrentString.clear();
		CLEAR_PARSER_VERIFY(current == '(', "149.FAS");

		m_CurrentErrorState = "function arguments";
		auto bracketsInfo = ParseBrackets(')',true);
		m_CurrentState = LexerState::Default;
		int i = 0;
		for (const std::string& arg : bracketsInfo.tokens)
		{
			m_TokenIndexStart = bracketsInfo.indexes.at(i);
			ProgramInfo info = SubParse(arg,true);
			for (const Token& tok :info.Tokens)
			{
				PushToken(tok);
			}
			PushToken(TokenType::Comma, "");
			i++;
		}
		if (GetLastToken().TokenType == TokenType::Comma)
		{
			m_ProgramInfo.Tokens.pop_back();
		}

		PushToken(TokenType::EndFunctionArguments, ")"); //TODO: change me to end function args
		current = GetNextChar();
		SkipSpaces();
		// if (current != ')')
		Backtrack();
	}


	void Lexer::MultiLineCommentState() {
		char current = GetNextChar();
		if (current =='\n')
			m_CurrentLine++;
		m_TokenIndexStart = m_CurrentTokenIndex-3;
		while (current!= '\0')
		{
			current = GetNextChar();
			if (current == '\n')
				m_CurrentLine++;
			if (current == '*')
			{
				current = GetNextChar();
				if (current == '\\')
				{
					m_CurrentState = LexerState::Default;
					return;
				}else {
					Backtrack();
				}
			}
		}
		int j = m_TokenIndexStart;
		while (j < m_Buffer.length() && m_Buffer[j] != '\n' && m_Buffer[j] != ';')
		{
			j++;
		}

		VerifyCondition(false,17,m_TokenIndexStart,j);
	}

	std::string Lexer::CleanBrackets(std::string x) {
		if (x.front() == '(' && x.back() == ')' )
		{
			return x.substr(1, x.size() - 2);
		}
		return x;
	}

	void Lexer::CommentState() {
		char current = GetNextChar();
		while (current != '\n' && current != '\0')
		{
			current = GetNextChar();
		}
		m_CurrentState = LexerState::Default;
		if (current == '\n')
			Backtrack();
	}
	bool Lexer::IsEndOfLine()
		{
		if (m_ProgramInfo.Tokens.empty())
			return true;
		TokenType tok = GetLastToken().TokenType;
		return (tok == TokenType::EndLine);
	}

	void Lexer::DotOpState()
		{
		VerifyCondition(IsTokenOfType(GetLastToken(1),"has_members"),51);
		m_CurrentState = LexerState::RValue;
	}


	void Lexer::RestrictionState() {
		char current = GetNextChar();

		current = SkipSpaces();
		m_TokenIndexStart = m_CurrentTokenIndex-1;
		m_CurrentString.clear();

		bool expectingEnd = false;
		while (current!= '\n' && current != '\0' && current != ':' && current != '<')
		{
			VerifyCondition(!(expectingEnd&&IsSpace(current)),37,-1,m_CurrentTokenIndex-2,"restriction");
			if (IsSpace(current))
			{
				expectingEnd = true;
			}else {

				VerifyCondition(IsVarNameChar(current),36,Str(current),"restriction");
			}
			m_CurrentString += current;
			current = GetNextChar();
		}
		VerifyCondition(!(std::isdigit(m_CurrentString.at(0))),35,"restriction");
		VerifyCondition(!IsTypeDeclared(m_CurrentString), 47,-1,m_CurrentTokenIndex-1,m_CurrentString);
		m_ScopeStack.back().RestrictionDeclarations.insert(m_CurrentString);
		PushToken(TokenType::RestrictionName, m_CurrentString);
		m_CurrentString.clear();

		if (current == '<')
		{
			bool end = false;
			bool expectingEnd = false;
			current = GetNextChar();
			while (current != '\0')
			{
				if (current == '>')
				{
					end = true;
					break;
				}
				VerifyCondition(!(expectingEnd&&IsSpace(current)),37,-1,m_CurrentTokenIndex-2,"restriction");
				if (IsSpace(current))
				{
					expectingEnd = true;
				}else {
					VerifyCondition(IsVarNameChar(current),36,Str(current),"restriction");
				}
				m_CurrentString += current;
				current = GetNextChar();
			}
			VerifyCondition(end,48);
			PushToken(TokenType::RestrictionTypeName, m_CurrentString);
		}else {
			PushToken(TokenType::RestrictionTypeName, "type");

		}




		m_CurrentString.clear();
		m_CurrentState = LexerState::Default;
	}


	std::string Lexer::GetCurrentErrorContext(std::string ErrorRef) {
		CLEAR_PARSER_VERIFY(!m_CurrentErrorState.empty(),ErrorRef)
		if (IsSubLexer)
		{
			return m_CurrentErrorState;
		}
		std::string ret = m_CurrentErrorState;
		m_CurrentErrorState.clear();
		return ret;
	}



	BracketParsingReturn Lexer::ParseBrackets(char end, bool commas) {
		char start = g_CloserToOpeners.at(end);
		char current = start;
		std::vector<char> stack;
		stack.push_back(start);
		bool detectedEnd = false;
		bool ExpectingValue = false;
		BracketParsingReturn ret;

		std::string ErrorReference = GetCurrentErrorContext("25410");

		ret.indexes.push_back(m_CurrentTokenIndex);
		while (!stack.empty() && current != '\0')
		{
			current = GetNextChar();
			if ((current == '\'' || current == '"') && !(stack.back() == '\'' || stack.back() == '"'))
			{
				stack.push_back(current);
			}else {

				if (!(stack.back() == '\'' || stack.back() == '"'))
				{
					if (g_Openers.contains(current))
					{
						stack.push_back(current);
					}
					if (g_CloserToOpeners.contains(current))
					{
						VerifyCondition(g_CloserToOpeners.at(current) == stack.back(),18,Str(stack.back()),Str(g_CloserToOpeners.at(current)));
						stack.pop_back();
					}
				}else {
					if ((current == '\'' || current == '"'))
					{
						if (m_Buffer[m_CurrentTokenIndex-2] != '\\')
						{
							// std::string type;
							// if (current == '"') {
							// 	type = "string";
							// }else {
							// 	type = "char";
							// }
							// VerifyCondition(current == stack.back(),19,type,Str(stack.back()),Str(current));
							if (current == stack.back())
								stack.pop_back();
						}
					}
				}

			}
			if ( (current == end && stack.empty()) || (current == ',' && stack.size() == 1) || current == '\0')
							{
				if (!((current == ',' && commas) || current!= ','))
				{
					VerifyCondition(false,16,ErrorReference);
				}
				VerifyCondition(!(current == ',' && ExpectingValue),38,ErrorReference);
				if (current ==',')
				{
					ExpectingValue = true;
				}

				if (!m_CurrentString.empty())
				{
					ret.tokens.push_back('('+ m_CurrentString + ')');
					ExpectingValue = false;
				}
				else {
					VerifyCondition(!ExpectingValue,32,ret.indexes.back()-2);
				}

				if (current == end)
				{
					VerifyCondition(!ExpectingValue,32,ret.indexes.back()-2);
					m_CurrentString.clear();
					detectedEnd = true;
					break;
				}

				m_CurrentString.clear();
				m_CurrentTokenIndex++;
				SkipSpaces();
				Backtrack();
				ret.indexes.push_back(m_CurrentTokenIndex);


			}
			else
			{
				if(!(std::isspace(current) && m_CurrentString.empty()))
					// if (current == '\n') current = ' ';
					m_CurrentString += current;
			}


		}



		VerifyCondition(detectedEnd, 27,ErrorReference, Str(end) );


		return ret;
	}


	void Lexer::PushVariableReference(const std::string& x)
				{
		if (GetLastToken().TokenType == TokenType::DotOp)
		{
			PushToken(TokenType::MemberName,x);
		}else {
			PushToken(TokenType::VariableReference, x);

		}
	}


	void Lexer::IndexOperatorState() {
		char current = GetNextChar();
		CLEAR_PARSER_VERIFY(current == '[', "318.IOS");

		m_CurrentErrorState = "array index";
		auto parsed= ParseBrackets(']',false);
		VerifyCondition(!parsed.tokens.empty(),24);
		if (parsed.tokens.empty())
			return;

		ProgramInfo info = SubParse( parsed.tokens.at(0),false);
		for (const Token& tok :info.Tokens)
		{
			PushToken(tok);
		}

		m_CurrentString.clear();
		m_CurrentState = LexerState::Default;

		PushToken(TokenType::CloseBracket,"]");
		// m_CurrentString+= "INDEX_OP";

	}

	void Lexer::DefaultState()
	{
		char current = GetNextChar();

		if (current == '(')
		{
			if (!m_CurrentString.empty() || IsTokenOfType(GetLastToken(),"callable"))
			{
				if (!m_CurrentString.empty())
				{
					PushVariableReference(m_CurrentString);
				}
				if (IsTokenOfType(GetLastToken(),"named_callable"))
				{
					m_CurrentString = GetLastToken().Data;
				}

				PushToken(TokenType::FunctionCall, m_CurrentString);
				m_CurrentState = LexerState::FunctionParamaters;
				Backtrack();

			}else {
				m_BracketStack.push_back('(');
			}

			PushToken({ .TokenType = TokenType::OpenBracket, .Data = "(" });


			return;
		}
		VerifyCondition(current!=']',25,"index operator");
		if (current == '"' )
		{
			VerifyCondition(m_CurrentString.empty(), 25,"string");
			ParseString();
			return;
		}
		if (current == '{') {
			VerifyCondition(m_CurrentString.empty(), 25,"list");
			ParseList();
			return;
		}
		if (current == '\'')
		{
			VerifyCondition(m_CurrentString.empty(), 25,"char");
			ParseChar();
			return;
		}

		if (std::isdigit(current) && m_CurrentString.empty())
		{
				m_CurrentString += current;
				ParseNumber();
				return;
		}

		if (IsVarNameChar(current))
			m_CurrentString += current;


		if (!m_CurrentString.empty() && !IsVarNameChar(current))
		{
			if (g_KeyWordMap.contains(m_CurrentString) )
			{
				auto& value = g_KeyWordMap.at(m_CurrentString);

				m_CurrentState = value.NextState;

				if (value.TokenToPush != TokenType::None)
					PushToken({ .TokenType = value.TokenToPush, .Data = m_CurrentString });

				m_CurrentString.clear();
				if (!IsSpace(current))
					Backtrack();
				return;

			}
			if (((!g_OperatorMap.contains(Str(current)) && current != '\n' && current != ')') || ( current == '*' || current == '&' || current == '<')) && IsTypeDeclared(m_CurrentString) && GetLastToken().TokenType!= TokenType::DotOp)
				{
				PushToken(TokenType::TypeIdentifier, m_CurrentString);
				m_CurrentState = LexerState::VariableName;
				m_CurrentString.clear();


				if (!IsSpace(current))
					Backtrack();

				return;
			}
			else
			{
				VerifyCondition(!IsTypeDeclared(m_CurrentString),34);
				PushVariableReference( m_CurrentString);
				m_CurrentString.clear();

			}
		}

		if (current == ':' || current == '\n')
		{
			m_CurrentState = LexerState::Indentation;
			m_CurrentString.clear();
			if (current == '\n' && m_BracketStack.empty())
				EndLine();

			return;
		}

		if (g_OperatorMap.contains(Str(current)))
			{
			m_CurrentState = LexerState::Operator;
			m_CurrentString.clear();
		}

		if (current == '[')
		{
			m_CurrentState = LexerState::IndexOperator;
			PushToken(TokenType::IndexOperator,"");
			PushToken(TokenType::OpenBracket,"[");
			Backtrack();

		}
		if (current == ')')
		{
			VerifyCondition(!m_BracketStack.empty() && m_BracketStack.back(),25,"Brackets");

			m_BracketStack.pop_back();
			PushToken({ .TokenType = TokenType::CloseBracket, .Data = ")"});

			return;
		}

		VerifyCondition(IsVarNameChar(current)||g_OperatorMap.contains(Str(current)) ||g_Openers.contains(current) || g_CloserToOpeners.contains(current) || std::isspace(current) ,41,Str(current));
	}
	void Lexer::ArrowState()
	{
		if ((m_ProgramInfo.Tokens.size() > 1 &&
			GetLastToken(1).TokenType == TokenType::EndFunctionParameters) || m_SecondState == LexerSecondaryState::Declaration)
		{
			m_CurrentState = LexerState::FunctionTypeState;
			return;
		}

		m_CurrentState = LexerState::Default;

	}
	void Lexer::FunctionTypeState()
	{
		char current = GetNextChar();

		//want to ignore all spaces in between type and variable
		current = SkipSpaces();
		m_CurrentString.clear();

		//allow _ and any character from alphabet
		while (current != '\n' && current != '\0' && current != ':' && current!=';')
		{
			m_CurrentString += current;
			current = GetNextChar();
		}

		PushToken({ .TokenType = TokenType::FunctionType, .Data = m_CurrentString });
		bool containsdata = false;
		for (auto i : m_CurrentString)
		{
			if (!std::isspace(i))
			{
				containsdata = true;
				break;
			}
		}
		VerifyCondition(containsdata,40);
		ProgramInfo info = SubParse(m_CurrentString,false);

		for (const Token& tok :info.Tokens)
		{
			PushToken(tok);
		}

		Backtrack();
		m_CurrentString.clear();
		m_CurrentState = LexerState::Default;

	}

	bool Lexer::IsTypeDeclared(const std::string& type) 
	{
		for (TypeScope& arg : m_ScopeStack)
		{
			if (arg.TypeDeclarations.contains(type) || arg.RestrictionDeclarations.contains(type))
			{
				return true;
			}
		}
		return false;
	}

	bool Lexer::IsRestrictionDeclared(const std::string &type) 
	{
		for (TypeScope& arg : m_ScopeStack)
		{
			if (arg.RestrictionDeclarations.contains(type))
			{
				return true;
			}
		}
		return false;
	}




	void Lexer::StructNameState() 
	{
		char current = GetNextChar();

		current = SkipSpaces();
		VerifyCondition((current != ':' && current != '\n' &&current != '\0' && current != ';'),3,m_TokenIndexStart-1,m_CurrentTokenIndex-1);
		m_TokenIndexStart = m_CurrentTokenIndex-1;
		m_CurrentString.clear();
		bool expectingEnd = false;
		while (current!= '\n' && current != '\0' && current != ':')
		{
			VerifyCondition(!(expectingEnd&&IsSpace(current)),37,-1,m_CurrentTokenIndex-2,"struct");
			if (IsSpace(current))
			{
				expectingEnd = true;
			}
			else 
			{
				VerifyCondition(IsVarNameChar(current),36,Str(current),"struct");
			}

			m_CurrentString += current;
			current = GetNextChar();
		}
		VerifyCondition(current == ':',53);
		VerifyCondition(!(std::isdigit(m_CurrentString.at(0))),35,"struct");
		VerifyCondition(!IsTypeDeclared(m_CurrentString), 4,-1,m_CurrentTokenIndex-1,m_CurrentString);
		m_ScopeStack.back().TypeDeclarations.insert(m_CurrentString);


		PushToken(TokenType::StructName, m_CurrentString);
		m_CurrentString.clear();
		Backtrack();
		m_CurrentState = LexerState::Default;
	}

	void Lexer::ClassNameState() 
	{
		char current = GetNextChar();

		current = SkipSpaces();
		VerifyCondition((current != ':' && current != '\n' &&current != '\0' && current != ';'),3,m_TokenIndexStart-1,m_CurrentTokenIndex-1);
		m_TokenIndexStart = m_CurrentTokenIndex-1;
		m_CurrentString.clear();

		while (current!= '\n' && current != '\0' && current != ':' && !IsSpace(current))
		{
			VerifyCondition(IsVarNameChar(current),36,Str(current),"struct");
			m_CurrentString += current;
			current = GetNextChar();
		}
		// VerifyCondition(current == ':',53);
		VerifyCondition(current == ':',54);
		VerifyCondition(!(std::isdigit(m_CurrentString.at(0))),35,"struct");
		m_ScopeStack.back().TypeDeclarations.insert(m_CurrentString);
		PushToken(TokenType::ClassName, m_CurrentString);
		m_CurrentString.clear();
		VerifyCondition(!IsTypeDeclared(m_CurrentString), 4,-1,m_CurrentTokenIndex-1,m_CurrentString);


		Backtrack();
		m_CurrentState = LexerState::Default;
	}

	Token Lexer::CreateToken(const TokenType tok, const std::string &data) {
		return Token{ .TokenType = tok, .Data = data };
	}

	void Lexer::VerifyCondition(bool condition, std::string Error, std::string Advice, std::string ErrorType, int startIndex, int endIndex) {
		if ((!condition) && !IsSubLexer)
		{
			if (startIndex!= -1)
			{
				m_TokenIndexStart = startIndex;
			}
			if (endIndex != -1)
			{
				m_CurrentTokenIndex = endIndex;
			}
		}
		VerifyCondition(condition, Error, Advice, ErrorType);
	}

	void Lexer::VerifyCondition(bool condition, std::string Error, std::string Advice, std::string ErrorType, int startIndex) {
		if (!condition && !IsSubLexer)
		{
			if (startIndex!= -1)
			{
				m_TokenIndexStart = startIndex;
			}
		}

		VerifyCondition(condition, Error, Advice, ErrorType);

	}



	void Lexer::ParsingRValueState()
	{
		char current = GetNextChar();

		//want to ignore all spaces in between = and actual variable
		current = SkipSpaces();
		m_CurrentString.clear();
		if (m_BracketStack.empty())
			VerifyCondition(current != '\n' && current != '\0' && !g_CloserToOpeners.contains(current),5,m_TokenIndexStart-1,m_CurrentTokenIndex-1);
		if (current == '\n')
		{
			return;
		}
		//brackets
		if (g_OperatorMap.contains(Str(current)))
		{
			m_CurrentState = LexerState::Operator;
			return;
		}
		if (current == '(')
		{
			m_BracketStack.push_back('(');
			PushToken({ .TokenType = TokenType::OpenBracket, .Data = "(" });
			m_CurrentState = LexerState::RValue;
			return;
		}
		if (current == ')')
		{
			PushToken({ .TokenType = TokenType::CloseBracket, .Data = ")" });
			m_CurrentState = LexerState::RValue;

			VerifyCondition(!m_BracketStack.empty() && m_BracketStack.back() == '(',1);
			m_BracketStack.pop_back();

			return;
		}
		if (current == '"') //strings
		{
			ParseString();
		}else if(current == '{') {
			ParseList();
		}
		else if (std::isdigit(current) || current == '-') // positive/negative numbers
		{
			m_CurrentString.push_back(current);
			ParseNumber();
		}else if (current == '\'') {
			ParseChar();
		}
		else if(std::isalnum(current) || current == '_')
		{
			//could be a variable reference, class/struct reference etc...
			m_CurrentString.push_back(current);
			Backtrack();
			ParseOther();
		}else {
			VerifyCondition(false,41,Str(current));
		}
		if (m_CurrentState == LexerState::RValue)
			m_CurrentState = LexerState::Default;
	}

	void Lexer::ParseArrayDeclaration()
	{
		m_TokenIndexStart = m_CurrentTokenIndex-1;
		m_CurrentErrorState = "Array declaration";
		auto parsed = ParseBrackets(']',false);
		m_CurrentString.clear();
		if (!parsed.tokens.empty())
		{
			m_CurrentString = CleanBrackets(parsed.tokens.at(0));
		}

		if (m_CurrentString.empty())
		{
			PushToken(TokenType::DynamicArrayDef,"");
		}
		else 
		{
			if (m_CurrentString.find_first_not_of("0123456789") == std::string::npos)
			{
				PushToken(TokenType::StaticArrayDef,m_CurrentString);
			}
			else if (m_CurrentString == "...")
			{
				PushToken(TokenType::StaticArrayDef,"...");
			}
			else
			{
				VerifyCondition(false,"Array declaration syntax error only expected numbers or ...","Either define a static size array by putting a size or a dynamic size array by leaving the square brackets empty","Array declaration error",m_TokenIndexStart,m_CurrentTokenIndex-1);
			}
		}
		m_CurrentString.clear();
		char current = GetNextChar();
		// VerifyCondition(IsSpace(current),47,m_CurrentTokenIndex-2);
		while (IsSpace(current))
		{
			current = GetNextChar();
		}
		VerifyCondition(current != ']',25,"Array declaration");
		if (current == '[')
		{
			ParseArrayDeclaration();
		}
		else  
		{
			if (current != '\0')
				Backtrack();
		}


	}

	void Lexer::ParsePointerDeclaration()
	{
		char current = GetNextChar();
		while (current == '*')
		{
			PushToken(TokenType::PointerDef,"*");
			current = GetNextChar();
		}
		VerifyCondition(std::isspace(current) || current == '[' ,6);
		current = SkipSpaces();
		VerifyCondition(current!= '*',26);

		if (!IsSpace(current) && current != '\0')
		{
			Backtrack();
		}

	}

	void Lexer::ParseGenericDeclaration() 
	{
		char current = GetNextChar();
		int currentLevel = 1;
		std::vector<std::string> tokens;
		std::vector<int> indexes;
		indexes.push_back(m_CurrentTokenIndex);
		while (currentLevel!= 0&& current != '\0')
		{
			current = GetNextChar();
			if (current == '<')
			{
				currentLevel++;
			}
			else if (current == '>')
			{
				currentLevel--;
			}

			if (current == ',' && currentLevel == 1 || currentLevel == 0)
			{
				VerifyCondition(!m_CurrentString.empty(),45);
				tokens.push_back(m_CurrentString);
				m_CurrentString.clear();
				indexes.push_back(m_CurrentTokenIndex);
				continue;
			}
			VerifyCondition(!IsSpace(current),52,-1,m_CurrentTokenIndex+1);
			VerifyCondition(IsVarNameChar(current) || current == '<' || current == '>' || current == ',',44);
			if (!(IsSpace(current) && m_CurrentString.empty()))
			{
				m_CurrentString+=current;
			}
		}

		VerifyCondition(!tokens.empty(),43);
		PushToken(TokenType::GenericDeclarationStart,"");
		for (auto &i : tokens)
		{
			auto program = SubParse(i,false);
			VerifyCondition(program.Tokens.at(0).TokenType == TokenType::TypeIdentifier || g_DataTypes.contains(program.Tokens.at(0).Data),46);
			for (const Token& tok :program.Tokens)
			{
				PushToken(tok);
			}
			PushToken(TokenType::Comma,",");
		}
		if (GetLastToken().TokenType == TokenType::Comma)
		{
			m_ProgramInfo.Tokens.pop_back();
		}
		PushToken(TokenType::GenericDeclarationEnd,"");
		current = GetNextChar();
		VerifyCondition(!IsVarNameChar(current),47,m_CurrentTokenIndex-2);
		current = SkipSpaces();
		if (!IsSpace(current) && current != '\0')
		{
			Backtrack();
		}
	}

	Error Lexer::CreateError(std::string& ErrorMsg, std::string& Advice, std::string& ErrorType) 
	{
		Error err;
		err.ErrorMessage = ErrorMsg;
		err.Advice = Advice;
		err.ErrorType = ErrorType;
		int i = m_CurrentTokenIndex;
		if (m_Buffer[i] == '\n')
			i--;
		if (m_Buffer[i] == '\n')
			i--;
		if ( m_Buffer[i] == '\0')
		{
			i-=2;
		}
		int j = i;

		while (j < m_Buffer.length() && m_Buffer[j] != '\n' && m_Buffer[j] != ';')
		{
			j++;
		}

		while (i >= 0 && m_Buffer[i] != '\n' && m_Buffer[i] != ';')
		{
			i--;
		}
		err.ErrorCause = m_Buffer.substr(i + 1, j - i - 1);
		i++;
		err.to = m_CurrentTokenIndex-i;
		err.from = m_TokenIndexStart-i;

		err.line = m_CurrentLine;
		return err;

	}
	void Lexer::RaiseError(Error& err) 
	{
		PrintError(err);
		CLEAR_HALT();
	}

	void Lexer::VerifyCondition(bool condition, std::string Error, std::string Advice, std::string ErrorType) 
	{
		if (!condition)
		{
			auto err = CreateError(Error,Advice,ErrorType);
			if (!IsSubLexer)
			{
				RaiseError(err);
			}else {
				m_subLexerError = true;
				m_ProgramInfo.Errors.push_back(err);
			}
		}
	}



	void Lexer::VariableNameState()
	{
		char current = GetNextChar();
		bool isDeclaration = IsTokenOfType(GetLastToken(1),"is_declaration") && ( GetLastToken().TokenType == TokenType::TypeIdentifier || g_DataTypes.contains(GetLastToken().Data));
		current = SkipSpaces();

		// if ((current == ':' || g_OperatorMap.contains(Str(current))) && current != '*' && current != '<') {
		// 	Backtrack();
		// 	VerifyCondition(!IsType,7);
		// 	m_CurrentState = LexerState::Default;
		// 	return;
		// }
		if (current == '(')
		{
			Backtrack();
			m_CurrentState = LexerState::Default;

			return;
		}
		if (current == '<')
		{
			Backtrack();
			ParseGenericDeclaration();
			current = GetNextChar();

		}
		if (current == '*')
		{
			Backtrack();
			ParsePointerDeclaration();
			current = GetNextChar();
		}


		m_CurrentString.clear();
		if (current == '[')
		{
			ParseArrayDeclaration();
			current = GetNextChar();
			if (current == '*')
			{
				Backtrack();
				ParsePointerDeclaration();
				current = GetNextChar();
			}
		}
		m_CurrentString.clear();
		VerifyCondition(!std::isdigit(current), 11,m_CurrentTokenIndex-1);
		VerifyCondition(current != '*',26);
		if (m_NoVariableNames)
		{
			VerifyCondition(!g_OperatorMap.contains(Str(current)), 10,m_CurrentTokenIndex-1);
			VerifyCondition(std::isspace(current) || current == '\0',42,m_TokenIndexStart-1,m_CurrentTokenIndex-1);

			m_CurrentState = LexerState::Default;
			return;

		}
		if (current == '\n' || current == '\0' || !IsVarNameChar(current))
		{
			VerifyCondition(!g_OperatorMap.contains(Str(current)), 10);
			//
			// if (!IsVarNameChar(current) && current != '\0' && current != '\n') {
			// 	VerifyCondition(!IsType, 9);
			// }
			VerifyCondition(!isDeclaration,8);

			m_CurrentState = LexerState::Default;
			Backtrack();
			return;
		}
		if (!isDeclaration)
		{
			Backtrack();
			m_CurrentState = LexerState::Default;
			return;
		}


		int commas = 0;
		int vars = 0;

		bool ExpectingComma = false;
		int lastValidVar = m_CurrentTokenIndex-1;
		while ((current != '\0' && current != '\n') && (IsVarNameChar(current) || IsSpace(current)) )
		{
			VerifyCondition(!(m_CurrentString.empty() && std::isdigit(current)),11, m_CurrentTokenIndex-1,-1);

			if (!IsSpace(current))
			{
				m_CurrentString += current;
			}
			current = GetNextChar();

			if (IsSpace(current) && !m_CurrentString.empty())
			{
				ExpectingComma = true;
			}


			VerifyCondition(!(ExpectingComma && IsVarNameChar(current)),12,lastValidVar);


			if (current == ',')
			{
				ExpectingComma = false;
				VerifyCondition(!m_CurrentString.empty(),28);
				VerifyCondition(!g_KeyWordMap.contains(m_CurrentString),39,lastValidVar+1,m_CurrentTokenIndex-2,m_CurrentString);
				lastValidVar = m_CurrentTokenIndex-1;
				PushToken(TokenType::VariableName, m_CurrentString);
				PushToken(TokenType::Comma,"");
				m_CurrentString.clear();
				current = GetNextChar();
				commas++;
				if (current == ',')
				{
					commas++;
				}
				vars ++;
			}
			VerifyCondition(current != ',',28);
		}
		if (!m_CurrentString.empty())
		{
			VerifyCondition(!g_KeyWordMap.contains(m_CurrentString),39,m_CurrentString);
			PushToken(TokenType::VariableName, m_CurrentString);
			vars++;

		}
		VerifyCondition(commas < vars,28);
		if (!IsSpace(current))
		{
			Backtrack();
		}
		m_CurrentString.clear();

		m_CurrentState = LexerState::Default;
	}

	void Lexer::FunctionParameterState()
	{
		char current = GetNextChar();
		int curtok = m_CurrentTokenIndex;
		current = SkipSpaces();
		int skipped = m_CurrentTokenIndex-curtok;
		m_CurrentString.clear();
		VerifyCondition(current == '(', 29,m_TokenIndexStart+skipped);

		m_CurrentErrorState = "function parameters";
		auto info = ParseBrackets(')',true);

		PushToken({ .TokenType = TokenType::StartFunctionParameters, .Data = "" });
		int ind = 0;

		for (const auto& i: info.tokens)
		{
			// auto ParameterTokens = ParseFunctionParameter(i,info.indexes.at(ind),info.indexes.at(ind+1));
			ProgramInfo ParameterTokens = SubParse(i);
			CLEAR_VERIFY(!ParameterTokens.Tokens.empty(),"Tokens in function parameter empty");
			VerifyCondition(g_DataTypes.contains(ParameterTokens.Tokens.at(0).Data) || IsTypeDeclared(ParameterTokens.Tokens.at(0).Data),33,info.indexes.at(ind),m_CurrentTokenIndex-2,std::string(TokenToString(ParameterTokens.Tokens.at(0).TokenType)));
			for (const Token& tok :ParameterTokens.Tokens)
			{
				PushToken(tok);
			}
			PushToken(TokenType::Comma,",");
			ind++;
		}
		if (GetLastToken().TokenType == TokenType::Comma)
		{
			m_ProgramInfo.Tokens.pop_back();
		}

		PushToken({ .TokenType = TokenType::EndFunctionParameters, .Data = "" });
		m_CurrentState = LexerState::Default;
		current = SkipSpaces();
		if (current != ')')
			Backtrack();
	}

	void Lexer::FunctionNameState()
	{
		char current = GetNextChar();

		current = SkipSpaces();
		m_CurrentString.clear();
		if (current == '(')
		{
			Backtrack();
			m_CurrentState = LexerState::FunctionParameters;
			PushToken({ .TokenType = TokenType::Lambda, .Data = ""});
			return;
		}

		while (IsVarNameChar(current))
		{
			m_CurrentString += current;
			current = GetNextChar();
		}

		if (current =='(')
			Backtrack();

		PushToken({ .TokenType = TokenType::FunctionName, .Data = m_CurrentString });
		m_CurrentString.clear();

		VerifyCondition(current != '\n',29);
		m_CurrentState = LexerState::FunctionParameters;
	}

	void Lexer::IncrementOperator() 
	{
		char current = GetNextChar();
		char incrementType = GetLastToken().Data.at(0);
		TokenType tok = incrementType == '+' ? TokenType::AddOp : TokenType::SubOp;
		if (current != incrementType)
		{
			Backtrack();
			if (GetLastToken().TokenType!= TokenType::Increment && GetLastToken().TokenType!= TokenType::Decrement)
			{
				m_CurrentState = LexerState::RValue;
			}
			else 
			{
				m_CurrentState = LexerState::Default;
			}
			return;
		}
		m_ProgramInfo.Tokens.pop_back();
		if (!IsTokenOfType(GetLastToken(),"allow_op"))
		{
			if (tok == TokenType::SubOp)
			{
				tok = TokenType::Negation;
			}
		}
		PushToken(tok,Str(incrementType));
		PushToken(tok,Str(incrementType));
		while (current == incrementType)
		{
			PushToken(tok,Str(incrementType));
			current = GetNextChar();
		}
		Backtrack();
		m_CurrentState = LexerState::RValue;
	}


	void Lexer::OperatorState()
	{
		Backtrack();
		std::string before = Str(GetNextChar());
		std::string h = "";
		char current  = before.at(0);
		while (g_OperatorMap.contains(Str(current)) && !(h.size()>1 &&g_OperatorMap.contains(h)))
		{
			h+=current;
			current = GetNextChar();
		}

		LexerMapValue value;
		std::string data;
		Backtrack();

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
			PushToken({ .TokenType = value.TokenToPush, .Data = data });

		m_CurrentState = value.NextState;
	}

	void Lexer::AsterisksState() 
	{
		if (IsTokenOfType(GetLastToken(),"allow_op"))
		{
			PushToken(TokenType::MulOp,"*");
		}
		else 
		{
			PushToken(TokenType::DereferenceOp,"*");
		}
		m_CurrentState = LexerState::RValue;

	}

	void Lexer::AmpersandState() 
	{
		auto token = GetLastToken();
		auto tok =token.TokenType;

		if (tok == TokenType::VariableReference || tok == TokenType::RValueChar || tok == TokenType::RValueNumber || tok == TokenType::RValueString || tok == TokenType::CloseBracket || tok == TokenType::EndFunctionArguments)
		{
			PushToken(TokenType::BitwiseAnd,"&");
		}else {
			PushToken(TokenType::AddressOp,"&");
		}
		m_CurrentState = LexerState::RValue;

	}


	void Lexer::IndentationState()
	{
		size_t tabWidth = 4;
		char next = GetNextChar();

		if (next == '\n')
		{
			EndLine();
			next = GetNextChar();
		}

		bool indenting = true;
		size_t totalSpaces = 0;

		while (indenting)
		{
			if (next == '\t')
			{
				totalSpaces += tabWidth;
				next = GetNextChar();
			}
			else if (next == ' ')
			{
				totalSpaces++;
				next = GetNextChar();
			}
			else
			{
				indenting = false;
			}
		}

		size_t localIndents = totalSpaces / 4;

		if (localIndents > m_Indents)
		{
			PushToken({ .TokenType = TokenType::StartIndentation });
			m_Indents = localIndents;
			m_ScopeStack.emplace_back();
		}

		while (m_Indents > localIndents)
		{
			PushToken({ .TokenType = TokenType::EndIndentation });
			m_Indents--;
			m_ScopeStack.pop_back();
		}

		m_CurrentState = LexerState::Default;
		Backtrack();
	}

	void Lexer::ParseHexLiteral() 
	{
		m_CurrentString.clear();
		char current = GetNextChar();
		while (!std::isspace(current) && !g_OperatorMap.contains(Str(current)))
		{

			VerifyCondition(current == '0' || current == '1' || current == '2' || current == '3' || current == '4' || current == '5' || current == '6' || current == '7' || current == '8' || current == '9' || current == 'A' || current == 'B' || current == 'C' || current == 'D' || current == 'E' || current == 'F'  || current == 'a' || current == 'b' || current == 'c' || current == 'd' || current == 'e' || current == 'f',30);
			m_CurrentString += current;
			current = GetNextChar();
		}

		if (!IsSpace(current))
		{
			Backtrack();
		}

		PushToken(TokenType::RValueNumber,std::to_string(HexStringToInteger(m_CurrentString)));
		m_CurrentString.clear();
	}
	void Lexer::ParseBinaryLiteral() 
	{
		m_CurrentString.clear();
		char current = GetNextChar();
		while (!std::isspace(current) && !g_OperatorMap.contains(Str(current)))
		{
			VerifyCondition(current == '0' || current == '1',31);
			m_CurrentString += current;
			current = GetNextChar();
		}

		if (!IsSpace(current))
		{
			Backtrack();
		}

		PushToken(TokenType::RValueNumber,std::to_string(BinaryStringToInteger(m_CurrentString)));
		m_CurrentString.clear();

	}

	void Lexer::ParseExponentNumber(std::string x)
	{
		m_TokenIndexStart = m_CurrentTokenIndex;
		char current = GetNextChar();
		bool usedDecimal = false;
		m_CurrentString.clear();
		while (std::isalnum(current) || current == '.' || current == '_')
		{
			if (current != '_')
			{
				m_CurrentString.push_back(current);
			}

			VerifyCondition(!(current == '.' && usedDecimal),21);

			if (current == '.')
			{
				usedDecimal = true;
			}


			current = GetNextChar();
		}

		VerifyCondition(IsValidNumber(m_CurrentString),20,-1,m_CurrentTokenIndex-2);
		PushToken(TokenType::RValueNumber,std::to_string(std::stod(x)*std::pow(10.0,std::stod(m_CurrentString))));
		m_CurrentString.clear();
	}



	void Lexer::ParseNumber()
	{
		char current = GetNextChar();

		if (current == '\0')
		{
			PushToken({ .TokenType = TokenType::RValueNumber, .Data = m_CurrentString });
			m_CurrentString.clear();
			return;
		}

		bool usedDecimal = false;
		if (current == 'b')
		{
			VerifyCondition(m_CurrentString == "0", 22,"binary");
			ParseBinaryLiteral();
			return;
		}

		if (current == 'x')
		{
			VerifyCondition(m_CurrentString == "0", 22,"hex");
			ParseHexLiteral();
			return;
		}

		while (std::isalnum(current) || current == '.' || current == '_')
		{
			if (current == 'e')
			{
				VerifyCondition(IsValidNumber(m_CurrentString),20,-1,m_CurrentTokenIndex-2);
				return ParseExponentNumber(m_CurrentString);
			}
			if (current != '_')
			{
				m_CurrentString.push_back(current);
			}

			VerifyCondition(!(current == '.' && usedDecimal),21);

			if (current == '.')
			{
				usedDecimal = true;
			}


			current = GetNextChar();
		}
		VerifyCondition(IsValidNumber(m_CurrentString),20,-1,m_CurrentTokenIndex-2);
		if (m_CurrentString == "-")
		{
			PushToken(TokenType::SubOp,"-");
		}
		else 
		{
			PushToken({ .TokenType = TokenType::RValueNumber, .Data = m_CurrentString });
		}
		m_CurrentString.clear();
		if (!IsSpace(current))
			Backtrack();
	}

	ProgramInfo Lexer::SubParse(std::string arg, bool allowvarname)
	{
		Lexer subLexer;
		subLexer.InitLexer();
		subLexer.m_Buffer = arg;
		subLexer.m_Buffer+=" ";
		subLexer.m_ScopeStack = m_ScopeStack;
		subLexer.IsSubLexer = true;
		subLexer.m_NoVariableNames = !allowvarname;
		ProgramInfo info = subLexer.ParseProgram();

		if (!info.Errors.empty())
		{
			auto cause = info.Errors.front();
			VerifyCondition(false,cause.ErrorMessage,cause.Advice,cause.ErrorType,m_TokenIndexStart,m_TokenIndexStart+(cause.to-cause.from));
			// Vali(cause.ErrorMessage,cause.Advice,cause.);
			// RaiseError();
		}
		if (info.Tokens.front().TokenType == TokenType::OpenBracket && info.Tokens.back().TokenType == TokenType::CloseBracket)
		{
			info.Tokens.pop_back();
			info.Tokens.erase(info.Tokens.begin());
		}
		return info;
	}

	ProgramInfo Lexer::SubParse(std::string arg) {
		Lexer subLexer;
		subLexer.InitLexer();
		subLexer.m_Buffer = arg;
		subLexer.m_Buffer+=" ";
		subLexer.m_ScopeStack = m_ScopeStack;
		subLexer.IsSubLexer = true;
		ProgramInfo info = subLexer.ParseProgram();

		if (!info.Errors.empty())
		{
			auto cause = info.Errors.front();
			VerifyCondition(false,cause.ErrorMessage,cause.Advice,cause.ErrorType,m_TokenIndexStart,m_TokenIndexStart+(cause.to-cause.from));
			// Vali(cause.ErrorMessage,cause.Advice,cause.);
			// RaiseError();
		}
		if (info.Tokens.front().TokenType == TokenType::OpenBracket && info.Tokens.back().TokenType == TokenType::CloseBracket)
		{
			info.Tokens.pop_back();
			info.Tokens.erase(info.Tokens.begin());
		}
		return info;
	}


	void Lexer::ParseList() {
		m_CurrentErrorState = "List literal";
		auto  bracketInfo = ParseBrackets('}',true);
		PushToken(TokenType::StartArray,"{");

 		for (const std::string& arg : bracketInfo.tokens)
 		{

			ProgramInfo info = SubParse(arg,false);
			for (const Token& tok :info.Tokens)
			{
				PushToken(tok);
			}
			PushToken(TokenType::Comma, "");
		}
		if (GetLastToken().TokenType == TokenType::Comma)
		{
			m_ProgramInfo.Tokens.pop_back();
		}


		PushToken(TokenType::EndArray,"}");
		m_CurrentState = LexerState::Default;
	}


	void Lexer::ParseChar() {
		char current = GetNextChar();
		char data = current;
		if (current == '\\')
		{
			current = GetNextChar();
			if (current == '\'')
			{
				data = '\'';
				current = 0;
			}
			else if(current == 'n')
			{
				data=  '\n';
			}
			else if(current == '\\')
			{
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
				VerifyCondition(false,13,m_TokenIndexStart+1,message);

			}

		}

		// if (current == '\'') {
		// 	current = '';
		// }
		VerifyCondition(current!= '\'',14);


		PushToken(TokenType::RValueChar,Str(data));
		current = GetNextChar();
		VerifyCondition(current == '\'',15);
	}


	void Lexer::ParseString()
	{
		char current = GetNextChar();
		while (current != '"')
		{
			VerifyCondition(!(current == '\n' || current == '\0'),23,m_TokenIndexStart+1);
			if (current == '\\')
			{
				current = GetNextChar();
				if (current == '"')
				{
					m_CurrentString += '"';
				}
				else if(current == 'n')
				{
					m_CurrentString += '\n';
				}
				else if(current == '\\')
				{
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
			current = GetNextChar();


		}

		PushToken({ .TokenType = TokenType::RValueString, .Data = m_CurrentString });
		m_CurrentString.clear();
	}

	void Lexer::ParseOther()
	{
		char current = GetNextChar();
		m_CurrentString.clear();

		while (IsVarNameChar(current ) && current)
		{
			m_CurrentString += current;

			current = GetNextChar();
			if (current == '\n' || current == '\0' || IsSpace(current))
				break;
		}
		if (current == '(')
			{
			if (!m_CurrentString.empty())
				Backtrack();
				return;
		}

		if (IsTypeDeclared(m_CurrentString) && GetLastToken().TokenType != TokenType::DotOp)
			{
			PushToken(TokenType::TypeIdentifier, m_CurrentString);
			m_CurrentString.clear();
			m_CurrentState= LexerState::VariableName;
			Backtrack();
			return;

		}

		if (g_KeyWordMap.contains(m_CurrentString))
		{
			auto& value = g_KeyWordMap.at(m_CurrentString);
			PushToken({ .TokenType = value.TokenToPush, .Data = m_CurrentString });
			if (g_DataTypes.contains(m_CurrentString))
			{
				m_CurrentString.clear();
				m_CurrentState= LexerState::VariableName;
				Backtrack();
				return;

			}
		}else {
			PushVariableReference( m_CurrentString);
		}

		m_CurrentString.clear();
		m_CurrentState = LexerState::Default;
		Backtrack();
	}
}
