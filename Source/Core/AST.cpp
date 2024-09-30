#include "AST.h"

#include "API/LLVM/LLVMBackend.h"

#include <iostream>

namespace clear {

	AST::AST(const ProgramInfo& info)
	{
		auto& tokens = info.Tokens;

		//possibly add command line arguments in the future
		m_Root = std::make_shared<ASTFunctionDecleration>("main", VariableType::None, std::vector<Argument>());
		m_Stack.push(m_Root);

		for (size_t i = 0; i < tokens.size(); i++)
		{
			auto& currentRoot = m_Stack.top();
			auto& currentToken = tokens[i];
			auto& currentChildren = currentRoot->GetChildren();

			switch (currentToken.TokenType)
			{
				case TokenType::VariableName:
				{
					auto& previous = tokens[i - 1];

					currentRoot->PushChild(std::make_shared<ASTVariableDecleration>(
						currentToken.Data,
						GetVariableTypeFromTokenType(previous.TokenType)));

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

				case TokenType::EndLine:
				{
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