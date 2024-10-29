#include "AST.h"

#include "API/LLVM/LLVMBackend.h"
#include "Core/Log.h"

#include <iostream>

namespace clear {

	AST::AST(ProgramInfo& info)
	{
		auto& tokens = info.Tokens;
		auto& builder = *LLVM::Backend::GetBuilder();

		//possibly add command line arguments in the future
		std::vector<Paramater> Paramaters;

		m_Root = Ref<ASTFunctionDecleration>::Create("main", VariableType::None, Paramaters);
		m_Stack.push({ m_Root, {} });

		for (size_t i = 0; i < tokens.size(); i++)
		{
			auto& currentRoot = m_Stack.top();
			auto& currentToken = tokens[i];
			auto& currentChildren = currentRoot.Node->GetChildren();

			switch (currentToken.TokenType)
			{
				case TokenType::Function:
				{
					AbstractType returnType = VariableType::None;
					Paramaters.clear();

					i++;
					std::string name = tokens[i].Data;

					i++;
					if (tokens[i].TokenType != TokenType::StartFunctionParameters)
					{
						break;
					}
					
					i++;
					Paramater currentParamater;
					while (tokens[i].TokenType != TokenType::EndFunctionParameters)
					{
						if (GetVariableTypeFromTokenType(tokens[i].TokenType) != VariableType::None)
						{
							currentParamater.Type = GetVariableTypeFromTokenType(tokens[i].TokenType);
						}
						else
						{
							currentParamater.Name = tokens[i].Data;
							Paramaters.push_back(currentParamater);

							AbstractType::RegisterVariableType(name + "::" + currentParamater.Name, currentParamater.Type);
						}

						i++;
					}

					if (tokens[i + 1].TokenType == TokenType::RightArrow)
					{
						i += 3;
						returnType = _GetFunctionTypeFromToken(tokens[i], tokens[i + 1].TokenType == TokenType::PointerDef);
					}

					Ref<ASTFunctionDecleration> funcDec = Ref<ASTFunctionDecleration>::Create(name, returnType, Paramaters);
					currentRoot.Node->PushChild(funcDec);
					m_Stack.push({ funcDec, returnType });

					break;
				}
				case TokenType::FunctionCall:
				{
					currentRoot.Node->PushChild(_CreateFunctionCall(tokens, currentRoot.Node->GetName(), i));
					break;
				}
				case TokenType::VariableName:
				{
					auto& previous = tokens[i - 1];

					AbstractType type;

					if (previous.TokenType == TokenType::VariableReference)
					{
						type = AbstractType(VariableType::UserDefinedType, TypeKind::Variable, previous.Data);
					}
					else if (previous.TokenType == TokenType::PointerDef)
					{
						auto& before = tokens[i - 2];
						type = AbstractType(before, TypeKind::Variable, true);
					}
					else
					{
						type = AbstractType(previous, TypeKind::Variable);
					}

					currentRoot.Node->PushChild(Ref<ASTVariableDecleration>::Create(currentRoot.Node->GetName() + "::" + currentToken.Data, type));
		
					AbstractType::RegisterVariableType(currentRoot.Node->GetName() + "::" + currentToken.Data, type);

					break;
				}
				case TokenType::Struct:
				{
					i++;

					CLEAR_VERIFY(tokens[i].TokenType == TokenType::StructName, "invalid token after struct");
					
					const std::string& structName = tokens[i].Data;
					
					while (tokens[i].TokenType != TokenType::StartIndentation)
						i++;

					i++;

					std::vector<AbstractType::MemberType> memberVars;

					while (tokens[i].TokenType != TokenType::EndIndentation &&
						   i < tokens.size())
					{
						if (tokens[i].TokenType == TokenType::EndLine)
						{
							i++;
							continue;
						}

						AbstractType::MemberType member;
						auto& [name, type] = member;

						if (tokens[i].TokenType == TokenType::VariableReference)
						{
							type = AbstractType(VariableType::UserDefinedType, TypeKind::Variable, tokens[i].Data);
						}
						else
						{
							type = GetVariableTypeFromTokenType(tokens[i].TokenType);
						}

						i++;

						name = tokens[i].Data;
						memberVars.push_back(member);

						i++;
					}

					AbstractType::CreateStructType(structName, memberVars);

					break;
				}
				case TokenType::ConditionalIf:
				{
					Ref<ASTIfExpression> ifExpr = Ref<ASTIfExpression>::Create();
					ifExpr->SetName(currentRoot.Node->GetName());

					//evaluate first condition
					ifExpr->PushChild(_CreateExpression(tokens, currentRoot.Node->GetName(), i, VariableType::Bool));
					
					Ref<ASTNodeBase> base = Ref<ASTNodeBase>::Create();
					base->SetName(currentRoot.Node->GetName());

					ifExpr->PushChild(base);

					currentRoot.Node->PushChild(ifExpr);
					m_Stack.push({ base, currentRoot.ExpectedReturnType });

					break;
				}
				case TokenType::Assignment:
				{
					auto& previous = tokens[i - 1];

					std::list<std::string> chain = _RetrieveChain(tokens, i);
					chain.push_back(previous.Data);
					chain.front() = currentRoot.Node->GetName() + "::" + chain.front();

					AbstractType type = _RetrieveAssignmentType(tokens, currentRoot.Node->GetName(), i);

					Ref<ASTBinaryExpression> binaryExpression = Ref<ASTBinaryExpression>::Create(BinaryExpressionType::Assignment, type);
					binaryExpression->PushChild(_CreateExpression(tokens, currentRoot.Node->GetName(), i, type));
					binaryExpression->PushChild(Ref<ASTVariableExpression>::Create(chain, true));

					currentRoot.Node->PushChild(binaryExpression);

					break;
				}
				case TokenType::EndIndentation:
				{
					if (m_Stack.size() > 1)
					{
						auto& top = m_Stack.top();

						//end of an if/elseif block so we need to check next token (TODO once else and else if implemented)
						if (top.Node->GetType() == ASTNodeType::Base)
						{
						}

						m_Stack.pop();
					}

					break;
				}
				case TokenType::Return:
				{
					Ref<ASTReturnStatement> returnStatement = Ref<ASTReturnStatement>::Create(currentRoot.ExpectedReturnType);
					if (i + 1 < tokens.size() && tokens[i].TokenType != TokenType::EndLine)
						returnStatement->PushChild(_CreateExpression(tokens, currentRoot.Node->GetName(), i, currentRoot.ExpectedReturnType));


					currentRoot.Node->PushChild(returnStatement);

					break;
				}
				default:
					break;
			}
		}
	}
	void AST::BuildIR(const std::filesystem::path& out)
	{
		if (!m_Root)
		{
			std::cout << "root was null" << std::endl;
			return;
		}

		auto& module = *LLVM::Backend::GetModule();

		std::error_code EC;
		llvm::raw_fd_stream stream(out.string(), EC);

		m_Root->Codegen();

		module.print(stream, nullptr);
	}
	Ref<ASTExpression> AST::_CreateExpression(std::vector<Token>& tokens, const std::string& root, size_t& start, const AbstractType& expected)
	{
		Ref<ASTExpression> expression = Ref<ASTExpression>::Create();

		struct Operator
		{
			Token Token;
			AbstractType ExpectedType;
		};

		std::stack<Operator> operators;

		static std::map<TokenType, int> s_Presedence = {
			{TokenType::DivOp,			  3},
			{TokenType::MulOp,			  3},
			{TokenType::LeftShift,		  3},
			{TokenType::RightShift,   	  3},
			{TokenType::BitwiseOr,		  3},
		    //{TokenType::BitwiseAnd ,        3},
			{TokenType::BitwiseNot,		  3},
			{TokenType::BitwiseXor,		  3},
			{TokenType::AddOp,			  2},
			{TokenType::SubOp,			  2},
			{TokenType::IsEqual,		  1}, 
			{TokenType::NotEqual,		  1},
			{TokenType::LessThan,		  1}, 
			{TokenType::GreaterThan,      1},
			{TokenType::LessThanEqual,    1},
			{TokenType::GreaterThanEqual, 1},
			{TokenType::OpenBracket,      0}
		};

		AbstractType currentExpectedType = expected;
		Token openBracket{ .TokenType = TokenType::OpenBracket };
		Token closeBracket{ .TokenType = TokenType::CloseBracket };

		bool addBracket = false;

		while (start < tokens.size() && tokens[start].TokenType != TokenType::EndLine && tokens[start].TokenType != TokenType::EndIndentation && tokens[start].TokenType != TokenType::Comma)
		{
			auto& token    = tokens[start];
			auto& previous = tokens[start - 1];

			if (token.TokenType == TokenType::VariableReference)
			{
				std::string startStr = { root + "::" + token.Data };
				std::list<std::string> ls = _RetrieveForwardChain(tokens, start);
				ls.push_front(startStr);

				start--;

				bool pointerFlag = previous.TokenType == TokenType::AddressOp;
				bool derferenceFlag = previous.TokenType == TokenType::DereferenceOp;
				expression->PushChild(Ref<ASTVariableExpression>::Create(ls, pointerFlag, derferenceFlag));

				auto& abstractType = AbstractType::GetVariableTypeFromName(startStr);

				if (pointerFlag)
				{
					currentExpectedType = AbstractType(VariableType::Pointer, abstractType.GetKind(), abstractType.Get(), abstractType.GetUserDefinedType());
					addBracket = true;
				}
				else 
				{
					currentExpectedType = AbstractType(abstractType.GetUnderlying(), abstractType.GetKind(), abstractType.GetUnderlying(), abstractType.GetUserDefinedType());
				}

			}
			else if (token.TokenType == TokenType::RValueNumber || 
					 token.TokenType == TokenType::RValueString || 
					 token.TokenType == TokenType::BooleanData)
			{
				expression->PushChild(Ref<ASTNodeLiteral>::Create(token.Data));
				currentExpectedType = AbstractType(token.Data);

				//cast to the largest value so overflow doesn't happen
				if (currentExpectedType.IsFloatingPoint())
					currentExpectedType = VariableType::Float64;
				else if (currentExpectedType.IsSigned())
					currentExpectedType = VariableType::Int64;
				else if (currentExpectedType.IsIntegral())
					currentExpectedType = VariableType::Uint64;	
			}
			else if (token.TokenType == TokenType::OpenBracket)
			{
				operators.push({ token, currentExpectedType });
			}
			else if (token.TokenType == TokenType::CloseBracket)
			{
				while (!operators.empty() && operators.top().Token.TokenType != TokenType::OpenBracket)
				{
					auto& top = operators.top();
					expression->PushChild(Ref<ASTBinaryExpression>::Create(GetBinaryExpressionTypeFromTokenType(top.Token.TokenType), top.ExpectedType));
					operators.pop();
				}

				if (!operators.empty())
					operators.pop();
			}
			else if (s_Presedence.contains(token.TokenType))
			{
				while (!operators.empty() && operators.top().Token.TokenType != TokenType::OpenBracket &&
					   s_Presedence[token.TokenType] <= s_Presedence[operators.top().Token.TokenType])
				{
					auto& top = operators.top();

					expression->PushChild(Ref<ASTBinaryExpression>::Create(GetBinaryExpressionTypeFromTokenType(top.Token.TokenType), top.ExpectedType));
					operators.pop();
				}

				operators.push({ token, currentExpectedType });

				if (addBracket)
				{
					operators.push({ openBracket, currentExpectedType });
					//we now need to insert the close bracket at the end of this expression

					size_t copy = start;

					while (copy < tokens.size() && tokens[copy].TokenType != TokenType::EndLine && tokens[copy].TokenType != TokenType::EndIndentation && tokens[copy].TokenType != TokenType::Comma)
					{
						copy++;
					}

					tokens.insert(tokens.begin() + copy, closeBracket);

					addBracket = false;
				}

			}

			start++;
		}


		while (!operators.empty())
		{
			auto& top = operators.top();
			expression->PushChild(Ref<ASTBinaryExpression>::Create(GetBinaryExpressionTypeFromTokenType(top.Token.TokenType), top.ExpectedType));
			operators.pop();
		}

		return expression;
	}

	Ref<ASTFunctionCall> AST::_CreateFunctionCall(std::vector<Token>& tokens, const std::string& root, size_t& i)
	{
		const std::string& name = tokens[i].Data;
		Ref<ASTFunctionCall> functionCall = Ref<ASTFunctionCall>::Create(name);

		i++;

		CLEAR_VERIFY(tokens[i].TokenType == TokenType::OpenBracket, "");
		i++;

		while (i < tokens.size() && tokens[i].TokenType != TokenType::CloseBracket && tokens[i].TokenType != TokenType::EndLine && tokens[i].TokenType != TokenType::EndIndentation)
		{
			if (tokens[i].TokenType == TokenType::Comma)
			{
				i++;
				continue;
			}

			functionCall->PushChild(_CreateExpression(tokens, root, i, AbstractType()));
		}


		return functionCall;
	}
	
	std::list<std::string> AST::_RetrieveChain(const std::vector<Token>& tokens, size_t current)
	{
		std::list<std::string> list;
		current -= 2;

		while (current > 0 && tokens[current].TokenType == TokenType::DotOp)
		{
			current--;
			list.push_back(tokens[current].Data);
			current--; //should be a dotop if expression is continuing
		}

		list.reverse();
		return list;
	}

	std::list<std::string> AST::_RetrieveForwardChain(const std::vector<Token>& tokens, size_t& current)
	{
		std::list<std::string> list;

		//(assuming token is currently a variable reference)

		current++;

		while (current < tokens.size() && tokens[current].TokenType == TokenType::DotOp)
		{
			current++;
			list.push_back(tokens[current].Data);
			current++;
		}

		return list;
	}

	AbstractType AST::_RetrieveAssignmentType(const std::vector<Token>& tokens, const std::string& currentFunctionName, size_t current)
	{
		current -= 2;

		bool isPointer = tokens[current].TokenType == TokenType::PointerDef;

		if (isPointer)
			current -= 1;

		while (current > 0 && tokens[current].TokenType != TokenType::EndLine && tokens[current].TokenType != TokenType::EndIndentation &&
			   (GetVariableTypeFromTokenType(tokens[current].TokenType) == VariableType::None || 
			   tokens[current].TokenType == TokenType::VariableReference || 
			   tokens[current].TokenType == TokenType::DotOp))
		{
			current--;
		}

		if (tokens[current].TokenType == TokenType::EndLine)
		{
			while (tokens[current].TokenType == TokenType::EndLine)
				current++;

			size_t currentCopy = current;
			std::list<std::string> ls = _RetrieveForwardChain(tokens, currentCopy);

			std::string name = currentFunctionName + "::" + tokens[current].Data;
			AbstractType type = AbstractType::GetVariableTypeFromName(name);

			for (auto& str : ls)
			{
				StructMetaData& structMetaData = AbstractType::GetStructInfo(type.GetUserDefinedType());
				CLEAR_VERIFY(structMetaData.Struct, "not a valid type ", type.GetUserDefinedType());

				size_t indexToNextType = structMetaData.Indices[str];
				type = structMetaData.Types[indexToNextType];
			}
			
			return type;
		}

		return AbstractType(tokens[current], TypeKind::VariableReference, isPointer);
	}

	AbstractType AST::_GetFunctionTypeFromToken(const Token& token, bool isPointer)
	{
		AbstractType type;

		VariableType variableType = GetVariableTypeFromTokenType(token.TokenType);
		auto& structMetaData = AbstractType::GetStructInfo(token.Data);

		std::string userDefinedType;
		VariableType currentType{};

		if (variableType != VariableType::None)
		{
			currentType = variableType;
		}
		else if (structMetaData.Struct)
		{
			userDefinedType = token.Data;
		}

		if (isPointer)
			return AbstractType(VariableType::Pointer, TypeKind::None, currentType, userDefinedType);
		
		return AbstractType(currentType, TypeKind::None, userDefinedType);
	}
}