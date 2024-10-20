#include "AST.h"

#include "API/LLVM/LLVMBackend.h"
#include "Log.h"

#include <iostream>

namespace clear {

	AST::AST(const ProgramInfo& info)
	{
		auto& tokens = info.Tokens;
		auto& builder = *LLVM::Backend::GetBuilder();

		//possibly add command line arguments in the future
		std::vector<Paramter> paramters;

		m_Root = std::make_shared<ASTFunctionDecleration>("main", VariableType::None, paramters);
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
					paramters.clear();

					i++;
					std::string name = tokens[i].Data;

					i++;
					if (tokens[i].TokenType != TokenType::StartFunctionParameters)
					{
						break;
					}
					
					i++;
					Paramter currentParamter;
					while (tokens[i].TokenType != TokenType::EndFunctionParameters)
					{
						if (GetVariableTypeFromTokenType(tokens[i].TokenType) != VariableType::None)
						{
							currentParamter.Type = GetVariableTypeFromTokenType(tokens[i].TokenType);
						}
						else
						{
							currentParamter.Name = tokens[i].Data;
							paramters.push_back(currentParamter);
						}

						i++;
					}

					std::shared_ptr<ASTFunctionDecleration> funcDec = std::make_shared<ASTFunctionDecleration>(name, returnType, paramters);
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
							default:
							{
								CLEAR_ANNOTATED_HALT("tokens of all types haven't been dealt with yet"); //TODO
								break;
							}
						}

						i++;
					}

					std::shared_ptr<ASTFunctionCall> funcDec = std::make_shared<ASTFunctionCall>(name, args);
					currentRoot->PushChild(funcDec);

					break;
				}
				case TokenType::VariableName:
				{
					auto& previous = tokens[i - 1];

					AbstractType type;

					if (previous.TokenType == TokenType::VariableReference)
						type = AbstractType(VariableType::UserDefinedType, TypeKind::Variable, previous.Data);
					else
						type = previous;

					currentRoot->PushChild(std::make_shared<ASTVariableDecleration>(
						currentRoot->GetName() + currentToken.Data,
						type));

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

					currentRoot->PushChild(std::make_shared<ASTStruct>(structName, memberVars));
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

					std::shared_ptr<ASTBinaryExpression> binaryExpression = std::make_shared<ASTBinaryExpression>(BinaryExpressionType::Assignment);
					binaryExpression->PushChild(_CreateExpression(tokens, i));
					binaryExpression->PushChild(std::make_shared<ASTVariableExpression>(previous.Data));

					currentRoot->PushChild(binaryExpression);

					break;
				}

				case TokenType::RValueNumber:
				{
					currentRoot->PushChild(std::make_shared<ASTNodeLiteral>(currentToken.Data));
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
	std::shared_ptr<ASTExpression> AST::_CreateExpression(const std::vector<Token>& tokens, size_t& start)
	{
		std::shared_ptr<ASTExpression> expression = std::make_shared<ASTExpression>();
		start += 1;

		std::stack<Token> operators;

		static std::map<TokenType, int> s_Presedence = {
			{TokenType::DivOp, 2},
			{TokenType::MulOp, 2},
			{TokenType::AddOp, 1},
			{TokenType::SubOp, 1},
			{TokenType::OpenBracket, 0}
		};

		while (start < tokens.size() && tokens[start].TokenType != TokenType::EndLine)
		{
			auto& token = tokens[start];

			if (token.TokenType == TokenType::VariableReference)
			{
				expression->PushChild(std::make_shared<ASTVariableExpression>(token.Data));
			}
			else if (token.TokenType == TokenType::RValueNumber)
			{
				expression->PushChild(std::make_shared<ASTNodeLiteral>(token.Data));
			}
			else if (token.TokenType == TokenType::OpenBracket)
			{
				operators.push(token);
			}
			else if (token.TokenType == TokenType::CloseBracket)
			{
				while (!operators.empty() && operators.top().TokenType != TokenType::OpenBracket)
				{
					expression->PushChild(std::make_shared<ASTBinaryExpression>(GetBinaryExpressionTypeFromTokenType(operators.top().TokenType)));
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
					expression->PushChild(std::make_shared<ASTBinaryExpression>(GetBinaryExpressionTypeFromTokenType(operators.top().TokenType)));
					operators.pop();
				}

				operators.push(token);
			}

			start++;
		}

		while (!operators.empty())
		{
			expression->PushChild(std::make_shared<ASTBinaryExpression>(GetBinaryExpressionTypeFromTokenType(operators.top().TokenType)));
			operators.pop();
		}

		return expression;
	}
}