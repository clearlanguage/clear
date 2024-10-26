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
			auto& currentRoot = m_Stack.top();
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
							case TokenType::RValueString:
							case TokenType::BooleanData:
							{
								arg.Field = AbstractType(tokens[i].Data);
								arg.Data = tokens[i].Data;

								args.push_back(arg);

								break;
							}
							case TokenType::VariableReference:
							{
								arg.Field = AbstractType(tokens[i], TypeKind::VariableReference);
								arg.Data = currentRoot->GetName() + "::" + tokens[i].Data;

								std::list<std::string> chain = _RetrieveForwardChain(tokens, i);

								for (auto& str : chain)
									arg.Data += "." + str;

								args.push_back(arg);

								i--;

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

					currentRoot->PushChild(Ref<ASTVariableDecleration>::Create(currentRoot->GetName() + "::" + currentToken.Data, type));
		
					AbstractType::RegisterVariableType(currentRoot->GetName() + "::" + currentToken.Data, type);

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

					std::list<std::string> chain = _RetrieveChain(tokens, i);
					chain.push_back(previous.Data);
					chain.front() = currentRoot->GetName() + "::" + chain.front();

					AbstractType type = _RetrieveAssignmentType(tokens, currentRoot->GetName(), i);

					Ref<ASTBinaryExpression> binaryExpression = Ref<ASTBinaryExpression>::Create(BinaryExpressionType::Assignment, type);
					binaryExpression->PushChild(_CreateExpression(tokens, currentRoot->GetName(), i, type));
					binaryExpression->PushChild(Ref<ASTVariableExpression>::Create(chain, true));

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
	Ref<ASTExpression> AST::_CreateExpression(const std::vector<Token>& tokens, const std::string& root, size_t& start, const AbstractType& expected)
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

		AbstractType currentExpectedType = expected;

		while (start < tokens.size() && tokens[start].TokenType != TokenType::EndLine && tokens[start].TokenType != TokenType::EndIndentation)
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
				expression->PushChild(Ref<ASTVariableExpression>::Create(ls, pointerFlag));

				auto& abstractType = AbstractType::GetVariableTypeFromName(startStr);

				if (pointerFlag)
				{
					currentExpectedType = AbstractType(VariableType::Pointer);
				}
				else
				{
					std::string concatenated = root + "::" + token.Data;

					for (auto& str : ls)
						concatenated += "." + str;

					currentExpectedType = AbstractType(VariableType::None, TypeKind::Value, concatenated);
				}
			}
			else if (token.TokenType == TokenType::RValueNumber || 
					 token.TokenType == TokenType::RValueString)
			{
				expression->PushChild(Ref<ASTNodeLiteral>::Create(token.Data));
				currentExpectedType = AbstractType(token.Data);
				
				//assume the largest value then cast down to the storage type
				if (currentExpectedType.IsFloatingPoint())
					currentExpectedType = VariableType::Float64;
				else if (!currentExpectedType.IsSigned())
					currentExpectedType = VariableType::Uint64;
				else if (currentExpectedType.IsIntegral())
					currentExpectedType = VariableType::Int64;

			}
			else if (token.TokenType == TokenType::OpenBracket)
			{
				operators.push(token);
			}
			else if (token.TokenType == TokenType::CloseBracket)
			{
				while (!operators.empty() && operators.top().TokenType != TokenType::OpenBracket)
				{
					expression->PushChild(Ref<ASTBinaryExpression>::Create(GetBinaryExpressionTypeFromTokenType(operators.top().TokenType), currentExpectedType));
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
					expression->PushChild(Ref<ASTBinaryExpression>::Create(GetBinaryExpressionTypeFromTokenType(operators.top().TokenType), currentExpectedType));
					operators.pop();
				}

				operators.push(token);
			}

			start++;
		}

		start--;

		while (!operators.empty())
		{
			expression->PushChild(Ref<ASTBinaryExpression>::Create(GetBinaryExpressionTypeFromTokenType(operators.top().TokenType), currentExpectedType));
			operators.pop();
		}

		return expression;
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

		while (current > 0 && 
			   GetVariableTypeFromTokenType(tokens[current].TokenType) == VariableType::None || 
			   tokens[current].TokenType == TokenType::VariableReference || 
			   tokens[current].TokenType == TokenType::DotOp)
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
			auto& type = AbstractType::GetVariableTypeFromName(name);

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
}