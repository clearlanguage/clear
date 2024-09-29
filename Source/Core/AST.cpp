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
				case TokenType::Assignment:
				{
					auto binaryExp = std::make_shared<ASTBinaryExpression>(GetBinaryExpressionTypeFromTokenType(currentToken.TokenType));
					m_Root->PushChild(binaryExp);
					m_Stack.push(binaryExp);
					break;
				}

				case TokenType::RValueNumber:
				{
					currentRoot->PushChild(std::make_shared<ASTNodeLiteral>(currentToken.Data));
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


}
