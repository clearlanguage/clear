#include "AST.h"

#include "API/LLVM/LLVMBackend.h"
#include "Core/Log.h"
#include "ExpressionBuilder.h"

#include <iostream>
#include <queue>

namespace clear {

	AST::AST(ProgramInfo& info)
	{
		auto& tokens = info.Tokens;

		//possibly add command line arguments in the future
		std::vector<Paramater> parameters;

		//TODO: make mandatory for user to define
		m_Root = Ref<ASTFunctionDefinition>::Create("main", VariableType::None, parameters);
		m_Stack.push({ m_Root, {} });

		// variableReference, variableReference = someExpression, someExpression

		struct ReferenceToAssign
		{
			Ref<ASTNodeBase> Expression;
			AbstractType Type;
		};

		std::queue<ReferenceToAssign> variableReferencesToAssign;

		for (size_t i = 0; i < tokens.size(); i++)
		{
			auto& currentRoot = m_Stack.top();
			auto& currentToken = tokens[i];

			switch (currentToken.TokenType)
			{
				case TokenType::Declaration:
				{
					i++;
					CLEAR_VERIFY(tokens[i].TokenType == TokenType::VariableReference, "must be variable reference");

					std::string& name = tokens[i].Data;
					i += 3;
					AbstractType type = VariableType::None;

					parameters.clear();

					while (i < tokens.size() && tokens[i].TokenType != TokenType::EndFunctionArguments)
					{
						bool isPointer = tokens[i + 1].TokenType == TokenType::PointerDef || tokens[i + 1].TokenType == TokenType::MulOp;
						bool isVariadic = tokens[i].TokenType == TokenType::Ellipsis;
						parameters.push_back({ "", _GetTypeFromToken(tokens[i], isPointer), isVariadic });

						if (isPointer)
							i++;

						i++;
						if (tokens[i].TokenType == TokenType::VariableReference || tokens[i].TokenType == TokenType::VariableName)
							i++;

						if (tokens[i].TokenType == TokenType::Comma)
							i++;
					}

					AbstractType returnType;
					i++;

					if (tokens[i].TokenType == TokenType::RightArrow)
					{
						i += 2;

						bool isPointer = tokens[i + 1].TokenType == TokenType::PointerDef;
						returnType = _GetTypeFromToken(tokens[i], isPointer);							
					}

					currentRoot.Node->PushChild(Ref<ASTFunctionDecleration>::Create(name, returnType, parameters));

					break;
				}
				case TokenType::Function:
				{
					AbstractType returnType = VariableType::None;
					parameters.clear();

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
						while (tokens[i].TokenType == TokenType::Comma || tokens[i].TokenType == TokenType::EndLine)
							i++;

						AbstractType type;
						if ((type = _GetTypeFromToken(tokens[i], tokens[i + 1].TokenType == TokenType::PointerDef)) && tokens[i].TokenType != TokenType::EndFunctionParameters)
						{
							currentParamater.Type = type;

							if (tokens[i + 1].TokenType == TokenType::PointerDef)
								i++;
						}
						else if(tokens[i].TokenType != TokenType::EndFunctionParameters)
						{
							currentParamater.Name = tokens[i].Data;
							parameters.push_back(currentParamater);

							AbstractType::RegisterVariableType(name + "::" + currentParamater.Name, currentParamater.Type);
						}

						if (tokens[i].TokenType != TokenType::EndFunctionParameters)
							i++;
					}

					if (tokens[i + 1].TokenType == TokenType::RightArrow)
					{
						i += 3;
						returnType = _GetTypeFromToken(tokens[i], tokens[i + 1].TokenType == TokenType::PointerDef);
					}

					Ref<ASTFunctionDefinition> funcDec = Ref<ASTFunctionDefinition>::Create(name, returnType, parameters);
					currentRoot.Node->PushChild(funcDec);
					m_Stack.push({ funcDec, returnType });

					break;
				}
				case TokenType::FunctionCall:
				{
					ExpressionBuilder builder(tokens, currentRoot.Node->GetName(), i);
					currentRoot.Node->PushChild(builder.CreateFunctionCall());
					break;
				}
				case TokenType::VariableName:
				{
					auto& previous = tokens[i - 1];

					AbstractType type;

					if (previous.TokenType == TokenType::VariableReference)
					{
						type = AbstractType(VariableType::None, TypeKind::Variable, previous.Data);
					}
					else if (previous.TokenType == TokenType::PointerDef)
					{
						auto& before = tokens[i - 2];
						type = AbstractType(before, TypeKind::Variable, true);
					}
					else if (previous.TokenType == TokenType::StaticArrayDef)
					{
						while (i > 0 && tokens[i].TokenType != TokenType::EndLine)
							i--;

						i++;
						AbstractType typeToGet;
						while ((typeToGet = _GetTypeFromToken(tokens[i], tokens[i + 1].TokenType == TokenType::PointerDef)))
						{
							i++;
							if (tokens[i].TokenType == TokenType::PointerDef)
								i++;

							type = typeToGet;
						}

						while (tokens[i].TokenType == TokenType::StaticArrayDef)
						{
							type = AbstractType(type, std::stoll(tokens[i].Data));
							i++;
						}
					}
					else
					{
						type = AbstractType(previous, TypeKind::Variable);
					}

					currentRoot.Node->PushChild(Ref<ASTVariableDeclaration>::Create(currentRoot.Node->GetName() + "::" + currentToken.Data, type));
		
					AbstractType::RegisterVariableType(currentRoot.Node->GetName() + "::" + currentToken.Data, type);

					break;
				}
				case TokenType::While:
				{
					Ref<ASTWhileLoop> whileNode = Ref<ASTWhileLoop>::Create();
					whileNode->SetName(currentRoot.Node->GetName());

					i++;

					ExpressionBuilder builder(tokens, currentRoot.Node->GetName(), i);

					whileNode->PushChild(builder.Create(VariableType::Bool));

					Ref<ASTNodeBase> base = Ref<ASTNodeBase>::Create();
					base->SetName(currentRoot.Node->GetName());

					whileNode->PushChild(base);

					currentRoot.Node->PushChild(whileNode);

					m_Stack.push({ whileNode, currentRoot.ExpectedReturnType });
					m_Stack.push({ base,      currentRoot.ExpectedReturnType });

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
					i++;
					ExpressionBuilder builder(tokens, currentRoot.Node->GetName(), i);
					ifExpr->PushChild(builder.Create(VariableType::Bool));
					
					Ref<ASTNodeBase> base = Ref<ASTNodeBase>::Create();
					base->SetName(currentRoot.Node->GetName());

					ifExpr->PushChild(base);

					currentRoot.Node->PushChild(ifExpr);
					m_Stack.push({ ifExpr, currentRoot.ExpectedReturnType });
					m_Stack.push({ base,   currentRoot.ExpectedReturnType });

					break;
				}
				case TokenType::Assignment:
				{
					if (!variableReferencesToAssign.empty())
					{
						i++;
						while (!variableReferencesToAssign.empty())
						{
							auto& ref = variableReferencesToAssign.front();

							Ref<ASTBinaryExpression> binaryExpression = Ref<ASTBinaryExpression>::Create(BinaryExpressionType::Assignment, ref.Type);
							
							ExpressionBuilder builder(tokens, currentRoot.Node->GetName(), i);

							binaryExpression->PushChild(builder.Create(ref.Type));
							binaryExpression->PushChild(ref.Expression);

							currentRoot.Node->PushChild(binaryExpression);

							variableReferencesToAssign.pop();

							if (tokens[i].TokenType == TokenType::Comma)
								i++;
						}

						continue;
					}


					auto& previous = tokens[i - 1];

					if (tokens[i + 1].TokenType == TokenType::StartArray)
					{
						std::list<std::string> chain = _RetrieveChain(tokens, i);
						chain.push_back(previous.Data);
						chain.front() = currentRoot.Node->GetName() + "::" + chain.front();

						i -= 2;
						AbstractType type = _RetrieveAssignmentType(tokens, currentRoot.Node->GetName(), i);
						i += 3;

						Ref<ASTArrayInitializer> initializer = Ref<ASTArrayInitializer>::Create();
						initializer->PushChild(Ref<ASTVariableExpression>::Create(chain));

						_CreateArrayInitializer(initializer, tokens, currentRoot.Node->GetName(), i, type);

						currentRoot.Node->PushChild(initializer);

						continue;
					}


					std::list<std::string> chain = _RetrieveChain(tokens, i);
					chain.push_back(previous.Data);
					chain.front() = currentRoot.Node->GetName() + "::" + chain.front();

					i -= 2;
					AbstractType type = _RetrieveAssignmentType(tokens, currentRoot.Node->GetName(), i);
					i += 3;

					Ref<ASTBinaryExpression> binaryExpression = Ref<ASTBinaryExpression>::Create(BinaryExpressionType::Assignment, type);

					ExpressionBuilder builder(tokens, currentRoot.Node->GetName(), i);

					binaryExpression->PushChild(builder.Create(type));
					binaryExpression->PushChild(Ref<ASTVariableExpression>::Create(chain));

					currentRoot.Node->PushChild(binaryExpression);

					break;
				}
				case TokenType::PlusAssign:
				case TokenType::MultiplyAssign:
				case TokenType::DivideAssign:
				case TokenType::ModuloAssign:
				case TokenType::MinusAssign:
				{
					auto& previous = tokens[i - 1];

					std::list<std::string> chain = _RetrieveChain(tokens, i);
					chain.push_back(previous.Data);
					chain.front() = currentRoot.Node->GetName() + "::" + chain.front();

					AbstractType type = _RetrieveAssignmentType(tokens, currentRoot.Node->GetName(), i);

					Ref<ASTBinaryExpression> operationExpression = Ref<ASTBinaryExpression>::Create(GetBinaryExpressionTypeFromTokenType(tokens[i].TokenType), type);
					Ref<ASTBinaryExpression> assignmentExpression = Ref<ASTBinaryExpression>::Create(BinaryExpressionType::Assignment, type);

					ExpressionBuilder builder(tokens, currentRoot.Node->GetName(), i);

					operationExpression->PushChild(builder.Create(type));
					operationExpression->PushChild(Ref<ASTVariableExpression>::Create(chain));

					assignmentExpression->PushChild(operationExpression);
					assignmentExpression->PushChild(Ref<ASTVariableExpression>::Create(chain));

					currentRoot.Node->PushChild(assignmentExpression);

					break;
				}
				case TokenType::EndIndentation:
				{
					if (m_Stack.size() <= 1)
					{
						continue;
					}

					auto& top = m_Stack.top();

					if (top.Node->GetType() == ASTNodeType::Base &&
						(i + 1) < tokens.size() &&
						(tokens[i + 1].TokenType == TokenType::Else ||
						tokens[i + 1].TokenType == TokenType::ElseIf))
					{
						size_t start = i + 1;

						if (tokens[start].TokenType == TokenType::Else)
							i += 4;
						else
							i += 1;

						m_Stack.pop();

						CLEAR_VERIFY(!m_Stack.empty(), "stack was empty");

						auto& newTop = m_Stack.top();

						CLEAR_VERIFY(newTop.Node->GetType() == ASTNodeType::IfExpression || newTop.Node->GetType() == ASTNodeType::WhileLoop, "top was not an if expression");

						Ref<ASTNodeBase> astNode = Ref<ASTNodeBase>::Create();

						if (tokens[start].TokenType == TokenType::ElseIf)
						{
							CLEAR_VERIFY(newTop.Node->GetType() == ASTNodeType::IfExpression,"");
							ExpressionBuilder builder(tokens, currentRoot.Node->GetName(), i);
							newTop.Node->PushChild(builder.Create(VariableType::Bool));
						}

						astNode->SetName(currentRoot.Node->GetName());
						newTop.Node->PushChild(astNode);

						m_Stack.push({ astNode, newTop.ExpectedReturnType });
					}
					else
					{
						m_Stack.pop();

						if (m_Stack.size() == 1)
							continue;

						auto& newTop = m_Stack.top();

						if (newTop.Node->GetType() == ASTNodeType::IfExpression || newTop.Node->GetType() == ASTNodeType::WhileLoop)
							m_Stack.pop();
					}

					break;
				}
				case TokenType::Return:
				{
					bool createReturn = m_Stack.top().Node->GetType() != ASTNodeType::Base;
					Ref<ASTReturnStatement> returnStatement = Ref<ASTReturnStatement>::Create(currentRoot.ExpectedReturnType, createReturn);
					if (i + 1 < tokens.size() && tokens[i].TokenType != TokenType::EndLine)
					{
						ExpressionBuilder builder(tokens, currentRoot.Node->GetName(), i);
						currentRoot.Node->PushChild(builder.Create(currentRoot.ExpectedReturnType));
					}

					currentRoot.Node->PushChild(returnStatement);

					break;
				}
				case TokenType::VariableReference:
				{
					if (tokens[i + 1].TokenType == TokenType::FunctionCall)
						continue;

					std::list<std::string> chain = _RetrieveChain(tokens, i);
					chain.push_back(currentToken.Data);
					chain.front() = currentRoot.Node->GetName() + "::" + chain.front();

					AbstractType type = _RetrieveAssignmentType(tokens, currentRoot.Node->GetName(), i);

					ExpressionBuilder builder(tokens, currentRoot.Node->GetName(), i);
					Ref<ASTExpression> expression = builder.Create({});

					if(i < tokens.size() && (tokens[i].TokenType == TokenType::Assignment || tokens[i].TokenType == TokenType::Comma))
						variableReferencesToAssign.push({ expression, type });
					else
						currentRoot.Node->PushChild(expression);

					i--;

					break;
				}
				case TokenType::EndLine:
				{
					CLEAR_VERIFY(variableReferencesToAssign.empty(), "deal with me later");
					break;
				}
				case TokenType::Continue:
				{
					currentRoot.Node->PushChild(Ref<ASTContinue>::Create());
					break;
				}
				case TokenType::Break:
				{
					currentRoot.Node->PushChild(Ref<ASTBreak>::Create());
					break;
				}
				default:
					break;
			}
		}

		CLEAR_VERIFY(m_Stack.size() == 1, "program wasn't parsed properly");

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

	void AST::_CreateArrayInitializer(Ref<ASTArrayInitializer>& initializer, std::vector<Token>& tokens, const std::string& root, size_t& i, const AbstractType& expected)
	{
		std::vector<size_t> currentIndex = { 0, 0 };

		size_t innerIndex = 0;

		i++;

		while (tokens[i].TokenType != TokenType::EndLine)
		{
			while (tokens[i].TokenType == TokenType::StartArray || 
				   tokens[i].TokenType == TokenType::EndArray   || 
				   tokens[i].TokenType == TokenType::Comma)
			{

				if (tokens[i].TokenType == TokenType::StartArray)
				{
					currentIndex.push_back(0);
					innerIndex = 0;
				}
				else if (tokens[i].TokenType == TokenType::EndArray)
				{
					if (!currentIndex.empty())
					{
						currentIndex.pop_back();
						
						if(!currentIndex.empty())
							currentIndex.back()++;
					}
				}
				else if (tokens[i].TokenType == TokenType::Comma)
				{
					innerIndex++;
				}
				
				i++;
			}

			if (tokens[i].TokenType != TokenType::EndLine)
			{
				ExpressionBuilder builder(tokens, root, i);
				initializer->PushChild(builder.Create(expected));

				currentIndex.back() = innerIndex;
				initializer->PushElementIndex(currentIndex);
			}
		}
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
		bool isPointer = tokens[current].TokenType == TokenType::PointerDef;

		while (current > 0 && 
			   tokens[current].TokenType != TokenType::EndLine && 
			   tokens[current].TokenType != TokenType::EndIndentation &&
			   tokens[current].TokenType != TokenType::Comma && 
			   tokens[current].TokenType != TokenType::StaticArrayDef &&
			   (GetVariableTypeFromTokenType(tokens[current].TokenType) == VariableType::None || 
			   tokens[current].TokenType == TokenType::VariableReference || 
			   tokens[current].TokenType == TokenType::DotOp))
		{
			current--;
		}

		if (isPointer)
			current += 1;


		if (tokens[current].TokenType == TokenType::DereferenceOp || tokens[current].TokenType == TokenType::StaticArrayDef)
			current++;

		if (tokens[current].TokenType == TokenType::EndLine || tokens[current].TokenType == TokenType::Comma)
		{
			while (tokens[current].TokenType == TokenType::EndLine || tokens[current].TokenType == TokenType::Comma)
				current++;

			bool shouldDerference = false;

			if (tokens[current].TokenType == TokenType::DereferenceOp)
			{
				shouldDerference = true;
				current++;
			}

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
			
			if (shouldDerference)
				return AbstractType(type.GetUnderlying(), TypeKind::None, type.GetUserDefinedType());

			return type;
		}


		current++;
		return AbstractType::GetVariableTypeFromName(currentFunctionName + "::" + tokens[current].Data);
	}

	AbstractType AST::_GetTypeFromToken(const Token& token, bool isPointer)
	{
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
			return AbstractType(VariableType::Pointer, TypeKind::Variable, currentType, userDefinedType);
		
		if (variableType != VariableType::None)
			return AbstractType(currentType, TypeKind::Variable, userDefinedType);
		
		return AbstractType();
	}

	AbstractType AST::_GetTypeFromList(std::list<std::string>& list)
	{
		AbstractType type = AbstractType::GetVariableTypeFromName(list.front());
		list.pop_front();

		while (!list.empty())
		{
			StructMetaData& structMetaData = AbstractType::GetStructInfo(type.GetUserDefinedType());
			CLEAR_VERIFY(structMetaData.Struct, "not a valid type ", type.GetUserDefinedType());

			size_t indexToNextType = structMetaData.Indices[list.front()];
			type = structMetaData.Types[indexToNextType];
			list.pop_front();
		}

		return type;
	}
	
	bool AST::_IsUnary(const Token& token)
	{
		switch (token.TokenType)
		{
			case TokenType::Increment:
			case TokenType::Decrement:
			case TokenType::BitwiseNot:
			case TokenType::DereferenceOp:
			case TokenType::AddressOp:
				return true;
			default:
				break;
		}

		return false;
	}
}