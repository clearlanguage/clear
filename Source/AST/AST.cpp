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
		m_Root = Ref<ASTFunctionDefinition>::Create("main", Ref<Type>::Create(TypeID::None), parameters);
		m_Stack.push({ m_Root, {} });

		// variableReference, variableReference = someExpression, someExpression

		struct ReferenceToAssign
		{
			Ref<ASTNodeBase> Expression;
			Ref<Type> Type;
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
					Ref<Type> type = Ref<Type>::Create(TypeID::None);

					parameters.clear();

					while (i < tokens.size() && tokens[i].TokenType != TokenType::EndFunctionArguments)
					{
						while(tokens[i].TokenType == TokenType::OpenBracket || tokens[i].TokenType == TokenType::CloseBracket || tokens[i].TokenType == TokenType::Comma)
						{
							i++;
						}

						if(tokens[i].TokenType == TokenType::EndFunctionArguments)
							break;

						bool isPointer = tokens[i + 1].TokenType == TokenType::PointerDef || tokens[i + 1].TokenType == TokenType::MulOp;
						bool isVariadic = tokens[i].TokenType == TokenType::Ellipsis;

						if(!isVariadic)
							parameters.push_back({ "", _GetTypeFromToken(tokens[i], isPointer), isVariadic });
						else 
							parameters.push_back({ "", Ref<Type>::Create(TypeID::None), isVariadic });


						if (isPointer)
							i++;

						i++;
						if (tokens[i].TokenType == TokenType::VariableReference || tokens[i].TokenType == TokenType::VariableName)
							i++;

						if (tokens[i].TokenType == TokenType::Comma)
							i++;
					}

					Ref<Type> returnType;
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
					Ref<Type> returnType = Ref<Type>::Create(TypeID::None);
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

						Ref<Type> type;
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

							Type::RegisterVariableType(name + "::" + currentParamater.Name, currentParamater.Type);
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

					Ref<Type> type;
                    size_t copy = i;

					while(!g_DataTypes.contains(tokens[copy].Data) && tokens[copy].TokenType != TokenType::TypeIdentifier)
					{
						copy--;
					}

					type = Ref<Type>::Create(tokens[copy]);

					copy++;

					while(tokens[copy].TokenType == TokenType::PointerDef || tokens[copy].TokenType == TokenType::StaticArrayDef)
					{
						if(tokens[copy].TokenType == TokenType::PointerDef)
						{
							type = Ref<Type>::Create(type); //pointer
						}
						else if (tokens[copy].TokenType == TokenType::StaticArrayDef)
						{
							type = Ref<Type>::Create(type, std::stoull(tokens[copy].Data));
						}

						copy++;
					}

					currentRoot.Node->PushChild(Ref<ASTVariableDeclaration>::Create(currentRoot.Node->GetName() + "::" + currentToken.Data, type));
		
					Type::RegisterVariableType(currentRoot.Node->GetName() + "::" + currentToken.Data, type);

					break;
				}
				case TokenType::While:
				{
					Ref<ASTWhileLoop> whileNode = Ref<ASTWhileLoop>::Create();
					whileNode->SetName(currentRoot.Node->GetName());

					i++;

					ExpressionBuilder builder(tokens, currentRoot.Node->GetName(), i);

					whileNode->PushChild(builder.Create(Ref<Type>::Create(TypeID::Bool)));

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

					std::vector<MemberType> memberVars;

					while (tokens[i].TokenType != TokenType::EndIndentation &&
						   i < tokens.size())
					{
						if (tokens[i].TokenType == TokenType::EndLine)
						{
							i++;
							continue;
						}

						MemberType member;
						auto& [type, name] = member;


						type = Ref<Type>::Create(tokens[i]);

						i++;

						name = tokens[i].Data;
						memberVars.push_back(member);

						i++;
					}

					Ref<Type> dummy = Ref<Type>::Create(structName, memberVars);

					break;
				}
				case TokenType::ConditionalIf:
				{
					Ref<ASTIfExpression> ifExpr = Ref<ASTIfExpression>::Create();
					ifExpr->SetName(currentRoot.Node->GetName());

					//evaluate first condition
					i++;
					ExpressionBuilder builder(tokens, currentRoot.Node->GetName(), i);
					ifExpr->PushChild(builder.Create(Ref<Type>::Create(TypeID::Bool)));
					
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

							Ref<ASTBinaryExpression> binaryExpression = Ref<ASTBinaryExpression>::Create(BinaryExpressionType::Assignment);
							
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
						i++;

						Ref<ASTArrayInitializer> initializer = Ref<ASTArrayInitializer>::Create();
						initializer->PushChild(Ref<ASTVariableExpression>::Create(currentRoot.Node->GetName() + "::" + previous.Data));

						_CreateArrayInitializer(initializer, tokens, currentRoot.Node->GetName(), i, {});

						currentRoot.Node->PushChild(initializer);

						continue;
					}

					Ref<Type> type = _GetAssignmentType(tokens, currentRoot.Node->GetName(), i);
					i++;

					Ref<ASTBinaryExpression> binaryExpression = Ref<ASTBinaryExpression>::Create(BinaryExpressionType::Assignment);

					ExpressionBuilder builder(tokens, currentRoot.Node->GetName(), i);

					binaryExpression->PushChild(builder.Create(type));
					binaryExpression->PushChild(Ref<ASTVariableExpression>::Create(currentRoot.Node->GetName() + "::" + previous.Data));

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

					Ref<Type> type = _GetAssignmentType(tokens, currentRoot.Node->GetName(), i);

					Ref<ASTBinaryExpression> operationExpression = Ref<ASTBinaryExpression>::Create(Type::GetBinaryExpressionTypeFromToken(tokens[i].TokenType));
					Ref<ASTBinaryExpression> assignmentExpression = Ref<ASTBinaryExpression>::Create(BinaryExpressionType::Assignment);

					ExpressionBuilder builder(tokens, currentRoot.Node->GetName(), i);

					operationExpression->PushChild(builder.Create(type));
					operationExpression->PushChild(Ref<ASTVariableExpression>::Create(currentRoot.Node->GetName() + "::" + previous.Data));

					assignmentExpression->PushChild(operationExpression);
					assignmentExpression->PushChild(Ref<ASTVariableExpression>::Create(currentRoot.Node->GetName() + "::" + previous.Data));

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
							newTop.Node->PushChild(builder.Create(Ref<Type>::Create(TypeID::Bool)));
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
						returnStatement->PushChild(builder.Create(currentRoot.ExpectedReturnType));
					}

					currentRoot.Node->PushChild(returnStatement);

					break;
				}
				case TokenType::VariableReference:
				{
					if (tokens[i + 1].TokenType == TokenType::FunctionCall)
						continue;

					ExpressionBuilder builder(tokens, currentRoot.Node->GetName(), i);
					Ref<ASTExpression> expression = builder.Create({});

					if(i < tokens.size() && (tokens[i].TokenType == TokenType::Assignment || tokens[i].TokenType == TokenType::Comma))
						variableReferencesToAssign.push({ expression });
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

	void AST::_CreateArrayInitializer(Ref<ASTArrayInitializer>& initializer, std::vector<Token>& tokens, const std::string& root, size_t& i, const Ref<Type>& expected)
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

    Ref<Type> AST::_GetAssignmentType(const std::vector<Token> &tokens, const std::string &currentFunctionName, size_t current)
    {

		while(tokens[current].TokenType != TokenType::VariableName && 
			  tokens[current].TokenType != TokenType::VariableReference)
		{
			current--;
		}

		Token token = tokens[current];
		token.Data = currentFunctionName + "::" + token.Data;

		Ref<Type> currentType = Ref<Type>::Create(token);

		current++;
        //TODO: maybe remove?
		while(tokens[current].TokenType == TokenType::PointerDef || tokens[current].TokenType == TokenType::StaticArrayDef)
		{
			if(tokens[current].TokenType == TokenType::PointerDef)
				currentType = Ref<Type>::Create(currentType); //pointer
			else if(tokens[current].TokenType == TokenType::StaticArrayDef)
				currentType = Ref<Type>::Create(currentType, std::stoull(tokens[current].Data)); //array

			current++;
		}

        return currentType;
    }

    Ref<Type> AST::_GetTypeFromToken(const Token& token, bool isPointer)
	{
		Ref<Type> type = Ref<Type>::Create(token, isPointer);

		if(type->Get())
			return type;

		return {};
	}

	Ref<Type> AST::_GetTypeFromList(std::list<std::string>& list)
	{
		Ref<Type> type = Type::GetVariableTypeFromName(list.front());
		list.pop_front();

		while (!list.empty())
		{
			StructMetaData& structMetaData = Type::GetStructMetaData(type->GetUserDefinedTypeIdentifer());
			CLEAR_VERIFY(structMetaData.Struct, "not a valid type ", type->GetUserDefinedTypeIdentifer());

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