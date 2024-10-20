#include "AST.h"

#include "API/LLVM/LLVMBackend.h"
#include "Core/Log.h"

#include <iostream>

namespace clear {

	AST::AST(const ProgramInfo& info)
	{
		auto& tokens = info.Tokens;
		auto& builder = *LLVM::Backend::GetBuilder();

		//possibly add command line arguments in the future
		std::vector<Paramater> Paramaters;

		m_Root = Ref<ASTFunctionDecleration>::Create("main", VariableType::None, Paramaters);
		m_Stack.push(m_Root);

		for (size_t i = 0; i < tokens.size(); i++)
		{
			auto& currentRoot =m_Stack.top();
			auto& currentToken = tokens[i];
			auto& currentChildren = currentRoot->GetChildren();

			switch (currentToken.TokenType)
			{
				case TokenType::Function:
				{
					VariableType returnType = VariableType::None;
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
						}

						i++;
					}

					Ref<ASTFunctionDecleration> funcDec = Ref<ASTFunctionDecleration>::Create(name, returnType, Paramaters);
					currentRoot->PushChild(funcDec);
					m_Stack.push(funcDec);

					break;
				}
				case TokenType::FunctionCall:
				{
					const std::string& name = tokens[i].Data;
					std::vector<Argument> args;

					i++;

					CLEAR_VERIFY(tokens[i].TokenType == TokenType::OpenBracket,"");
					i++;

					while (tokens[i].TokenType != TokenType::CloseBracket)
					{
						if (tokens[i].TokenType == TokenType::Comma)
						{
							i++;
							continue;
						}

						Argument arg;

						switch (tokens[i].TokenType)
						{
							case TokenType::RValueNumber:
							case TokenType::BooleanData:
							{
								arg.Field = AbstractType(tokens[i].Data);
								arg.Data = tokens[i].Data;

								args.push_back(arg);

								break;
							}
							case TokenType::VariableReference:
							{
								arg.Field = AbstractType(tokens[i], TypeKind::Variable);
								arg.Data = currentRoot->GetName() + "::" + tokens[i].Data;

								args.push_back(arg);

								break;
							}
							default:
							{
								CLEAR_ANNOTATED_HALT("tokens of all types haven't been dealt with yet"); //TODO
								break;
							}
						}

						i++;
					}

					currentRoot->PushChild(Ref<ASTFunctionCall>::Create(name, args));
					break;
				}
				case TokenType::VariableName:
				{
					auto& previous = tokens[i - 1];

					AbstractType type;

					if (previous.TokenType == TokenType::VariableReference)
						type = AbstractType(VariableType::UserDefinedType, TypeKind::Variable, previous.Data);
					else
						type = AbstractType(previous, TypeKind::Variable);

					currentRoot->PushChild(Ref<ASTVariableDecleration>::Create( currentRoot->GetName() + "::" + currentToken.Data, type));
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

					std::vector<Member> memberVars;

					while (tokens[i].TokenType == TokenType::VariableReference ||
						   GetVariableTypeFromTokenType(tokens[i].TokenType) != VariableType::None &&
						   i < tokens.size())
					{
						Member member;

						if (tokens[i].TokenType == TokenType::VariableReference)
						{
							member.Field = AbstractType(VariableType::UserDefinedType, TypeKind::Variable, tokens[i].Data);
						}
						else
						{
							member.Field = GetVariableTypeFromTokenType(tokens[i].TokenType);
						}

						i++;
						member.Name = tokens[i].Data;
						memberVars.push_back(member);

						i++;
					}

					currentRoot->PushChild(Ref<ASTStruct>::Create(structName, memberVars));
					break;
				}
				case TokenType::AddOp:
				case TokenType::SubOp:
				case TokenType::DivOp:
				case TokenType::MulOp:
					break;
				case TokenType::Assignment:
				{
					auto& previous = tokens[i - 1];
					auto& assignmentType = tokens[i - 2];
					AbstractType type(assignmentType);

					Ref<ASTBinaryExpression> binaryExpression = Ref<ASTBinaryExpression>::Create(BinaryExpressionType::Assignment, type);
					binaryExpression->PushChild(_CreateExpression(tokens, currentRoot->GetName(), i, type));
					binaryExpression->PushChild(Ref<ASTVariableExpression>::Create(currentRoot->GetName() + "::" + previous.Data));

					currentRoot->PushChild(binaryExpression);

					break;
				}
				case TokenType::EndIndentation:
				{
					if (m_Stack.size() > 1)
					{
						m_Stack.pop();
					}

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
	Ref<ASTExpression> AST::_CreateExpression(const std::vector<Token>& tokens, const std::string& root,
											  size_t& start, AbstractType expectedType)
	{
		Ref<ASTExpression> expression = Ref<ASTExpression>::Create();
		start += 1;

		std::stack<Token> operators;

		static std::map<TokenType, int> s_Presedence = {
			{TokenType::DivOp, 2},
			{TokenType::MulOp, 2},
			{TokenType::AddOp, 1},
			{TokenType::SubOp, 1},
			{TokenType::OpenBracket, 0}
		};

		while (start < tokens.size() && tokens[start].TokenType != TokenType::EndLine && tokens[start].TokenType != TokenType::EndIndentation)
		{
			auto& token = tokens[start];

			if (token.TokenType == TokenType::VariableReference)
			{
				expression->PushChild(Ref<ASTVariableExpression>::Create(root + "::" + token.Data));
			}
			else if (token.TokenType == TokenType::RValueNumber)
			{
				expression->PushChild(Ref<ASTNodeLiteral>::Create(token.Data));
			}
			else if (token.TokenType == TokenType::OpenBracket)
			{
				operators.push(token);
			}
			else if (token.TokenType == TokenType::CloseBracket)
			{
				while (!operators.empty() && operators.top().TokenType != TokenType::OpenBracket)
				{
					expression->PushChild(Ref<ASTBinaryExpression>::Create(GetBinaryExpressionTypeFromTokenType(operators.top().TokenType), expectedType));
					operators.pop();
				}

				if (!operators.empty())
					operators.pop();
			}
			else if (s_Presedence.contains(token.TokenType))
			{
				while (!operators.empty() && operators.top().TokenType != TokenType::OpenBracket &&
					s_Presedence[token.TokenType] <= s_Presedence[operators.top().TokenType])
				{
					expression->PushChild(Ref<ASTBinaryExpression>::Create(GetBinaryExpressionTypeFromTokenType(operators.top().TokenType), expectedType));
					operators.pop();
				}

				operators.push(token);
			}

			start++;
		}

		start--;

		while (!operators.empty())
		{
			expression->PushChild(Ref<ASTBinaryExpression>::Create(GetBinaryExpressionTypeFromTokenType(operators.top().TokenType), expectedType));
			operators.pop();
		}

		return expression;
	}
}