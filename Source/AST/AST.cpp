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

		//TODO: make mandatory for user to define
		m_Root = Ref<ASTFunctionDefinition>::Create("main", VariableType::None, Paramaters);
		m_Stack.push({ m_Root, {} });

		// variableReference, variableReference = someExpression, someExpression

		struct ReferenceToAssign
		{
			Ref<ASTVariableExpression> Expression;
			AbstractType Type;
		};

		std::queue<ReferenceToAssign> variableReferencesToAssign;

		for (size_t i = 0; i < tokens.size(); i++)
		{
			auto& currentRoot = m_Stack.top();
			auto& currentToken = tokens[i];
			auto& currentChildren = currentRoot.Node->GetChildren();
			switch (currentToken.TokenType)
			{
				case TokenType::Declaration:
				{
					i++;
					CLEAR_VERIFY(tokens[i].TokenType == TokenType::VariableReference, "must be variable reference");

					std::string& name = tokens[i].Data;
					i += 3;
					AbstractType type = VariableType::None;

					std::vector<Paramater> paramaters;

					while (i < tokens.size() && tokens[i].TokenType != TokenType::EndFunctionParameters)
					{
						bool isPointer = tokens[i + 1].TokenType == TokenType::PointerDef || tokens[i + 1].TokenType == TokenType::MulOp;
						bool isVariadic = tokens[i].TokenType == TokenType::Ellipsis;
						paramaters.push_back({ "", _GetTypeFromToken(tokens[i], isPointer), isVariadic });

						if (isPointer)
							i++;

						i++;
						if (tokens[i].TokenType == TokenType::VariableReference || tokens[i].TokenType == TokenType::VariableName)
							i++;

						if (tokens[i].TokenType == TokenType::Comma)
							i++;

						if (tokens[i].TokenType == TokenType::EndFunctionParameters)
							break;
					}

					AbstractType returnType;
					i++;

					if (tokens[i].TokenType == TokenType::RightArrow)
					{
						i += 2;

						bool isPointer = tokens[i + 1].TokenType == TokenType::PointerDef;
						returnType = _GetTypeFromToken(tokens[i], isPointer);							
					}

					currentRoot.Node->PushChild(Ref<ASTFunctionDecleration>::Create(name, returnType, paramaters));

					break;
				}
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
						while (tokens[i].TokenType == TokenType::Comma || tokens[i].TokenType == TokenType::EndLine)
							i++;

						AbstractType type;
						if ((type = _GetTypeFromToken(tokens[i], (tokens[i + 1].TokenType == TokenType::PointerDef || tokens[i + 1].TokenType == TokenType::MulOp))) && tokens[i].TokenType != TokenType::EndFunctionParameters)
						{
							currentParamater.Type = type;

							if (tokens[i + 1].TokenType == TokenType::PointerDef || tokens[i + 1].TokenType == TokenType::MulOp)
								i++;
						}
						else if(tokens[i].TokenType != TokenType::EndFunctionParameters)
						{
							currentParamater.Name = tokens[i].Data;
							Paramaters.push_back(currentParamater);

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

					Ref<ASTFunctionDefinition> funcDec = Ref<ASTFunctionDefinition>::Create(name, returnType, Paramaters);
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

					currentRoot.Node->PushChild(Ref<ASTVariableDecleration>::Create(currentRoot.Node->GetName() + "::" + currentToken.Data, type));
		
					AbstractType::RegisterVariableType(currentRoot.Node->GetName() + "::" + currentToken.Data, type);

					break;
				}
				case TokenType::While:
				{
					Ref<ASTWhileLoop> whileNode = Ref<ASTWhileLoop>::Create();
					whileNode->SetName(currentRoot.Node->GetName());

					i++;
					whileNode->PushChild(_CreateExpression(tokens, currentRoot.Node->GetName(), i, VariableType::Bool));

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
					ifExpr->PushChild(_CreateExpression(tokens, currentRoot.Node->GetName(), i, VariableType::Bool));
					
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
						while (!variableReferencesToAssign.empty())
						{
							auto& ref = variableReferencesToAssign.front();

							Ref<ASTBinaryExpression> binaryExpression = Ref<ASTBinaryExpression>::Create(BinaryExpressionType::Assignment, ref.Type);
							binaryExpression->PushChild(_CreateExpression(tokens, currentRoot.Node->GetName(), i, ref.Type));
							binaryExpression->PushChild(ref.Expression);

							currentRoot.Node->PushChild(binaryExpression);

							variableReferencesToAssign.pop();

							if (tokens[i].TokenType == TokenType::Comma)
								i++;
						}

						continue;
					}

					auto& previous = tokens[i - 1];
					bool  shouldDereference = tokens[i - 2].TokenType == TokenType::DereferenceOp;

					std::list<std::string> chain = _RetrieveChain(tokens, i);
					chain.push_back(previous.Data);
					chain.front() = currentRoot.Node->GetName() + "::" + chain.front();

					i -= 2;
					AbstractType type = _RetrieveAssignmentType(tokens, currentRoot.Node->GetName(), i);
					i += 3;

					Ref<ASTBinaryExpression> binaryExpression = Ref<ASTBinaryExpression>::Create(BinaryExpressionType::Assignment, type);
					binaryExpression->PushChild(_CreateExpression(tokens, currentRoot.Node->GetName(), i, type));
					binaryExpression->PushChild(Ref<ASTVariableExpression>::Create(chain, false, shouldDereference));

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
					bool  shouldDereference = tokens[i - 2].TokenType == TokenType::DereferenceOp;

					std::list<std::string> chain = _RetrieveChain(tokens, i);
					chain.push_back(previous.Data);
					chain.front() = currentRoot.Node->GetName() + "::" + chain.front();

					AbstractType type = _RetrieveAssignmentType(tokens, currentRoot.Node->GetName(), i);

					Ref<ASTBinaryExpression> operationExpression = Ref<ASTBinaryExpression>::Create(GetBinaryExpressionTypeFromTokenType(tokens[i].TokenType), type);
					Ref<ASTBinaryExpression> assignmentExpression = Ref<ASTBinaryExpression>::Create(BinaryExpressionType::Assignment, type);

					operationExpression->PushChild(_CreateExpression(tokens, currentRoot.Node->GetName(), i, type));
					operationExpression->PushChild(Ref<ASTVariableExpression>::Create(chain, false , shouldDereference));

					assignmentExpression->PushChild(operationExpression);
					assignmentExpression->PushChild(Ref<ASTVariableExpression>::Create(chain, true, shouldDereference));

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
							newTop.Node->PushChild(_CreateExpression(tokens, currentRoot.Node->GetName(), i, VariableType::Bool));
						}

						astNode->SetName(currentRoot.Node->GetName());
						newTop.Node->PushChild(astNode);

						m_Stack.push({ astNode, newTop.ExpectedReturnType });
					}
					else
					{
						m_Stack.pop();

						if (m_Stack.empty())
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
						returnStatement->PushChild(_CreateExpression(tokens, currentRoot.Node->GetName(), i, currentRoot.ExpectedReturnType));


					currentRoot.Node->PushChild(returnStatement);

					break;
				}
				case TokenType::VariableReference:
				{
					if (tokens[i + 1].TokenType == TokenType::FunctionCall)
						continue;

					UnaryExpressionType unaryType = UnaryExpressionType::None;

					if ((unaryType = GetPostUnaryExpressionTypeFromTokenType(tokens[i + 1].TokenType)) != UnaryExpressionType::None)
					{
						currentRoot.Node->PushChild(_CreateExpression(tokens, currentRoot.Node->GetName(), i, currentRoot.ExpectedReturnType));
						continue;
					}

					bool  shouldDereference = tokens[i - 1].TokenType == TokenType::DereferenceOp;

					std::list<std::string> chain = _RetrieveChain(tokens, i);
					chain.push_back(currentToken.Data);
					chain.front() = currentRoot.Node->GetName() + "::" + chain.front();

					AbstractType type = _RetrieveAssignmentType(tokens, currentRoot.Node->GetName(), i);

					variableReferencesToAssign.push({ Ref<ASTVariableExpression>::Create(chain, true, shouldDereference), type });
					
					CLEAR_VERIFY(tokens[i + 1].TokenType == TokenType::Comma || tokens[i + 1].TokenType == TokenType::Assignment, "cannot have variable reference here");

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
	Ref<ASTExpression> AST::_CreateExpression(std::vector<Token>& tokens, const std::string& root, size_t& start, const AbstractType& expected)
	{
		Ref<ASTExpression> expression = Ref<ASTExpression>::Create();

		static std::map<TokenType, int32_t> s_Presedence = {
			{TokenType::Increment,		  4}, 
			{TokenType::Decrement,		  4}, 
			{TokenType::BitwiseNot,		  4}, 
			{TokenType::DereferenceOp,    4}, 
			{TokenType::AddressOp,		  4},
			{TokenType::Power,			  4},
			{TokenType::DivOp,			  3},
			{TokenType::MulOp,			  3},
			{TokenType::LeftShift,		  3},
			{TokenType::RightShift,   	  3},
			{TokenType::BitwiseOr,		  3},
		    {TokenType::BitwiseAnd ,      3},
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

		struct Operator
		{
			BinaryExpressionType BinaryExpression;
			UnaryExpressionType UnaryExpression;
			AbstractType ExpectedType;
			bool IsOpenBracket = false;
			int32_t Presedence = 0;
		};

		std::stack<Operator> operators;

		AbstractType currentExpectedType = expected;
		Token openBracket{ .TokenType = TokenType::OpenBracket };
		Token closeBracket{ .TokenType = TokenType::CloseBracket };
		bool addBracket = false;
		
		static std::set<TokenType> s_Terminators = { TokenType::EndLine, TokenType::EndIndentation, TokenType::Comma,  TokenType::EndFunctionParameters};
			
		while (start < tokens.size() && 
			  !s_Terminators.contains(tokens[start].TokenType))
		{
			auto& token    = tokens[start];
			auto& previous = tokens[start - 1];

			UnaryExpressionType postType = start + 1 < tokens.size() ? GetPostUnaryExpressionTypeFromTokenType(tokens[start + 1].TokenType) : UnaryExpressionType::None;
			UnaryExpressionType unaryType = start - 1 < tokens.size() ? GetUnaryExpressionTypeFromTokenType(tokens[start - 1].TokenType) : UnaryExpressionType::None;

			if (token.TokenType == TokenType::VariableReference)
			{
				if (start + 1 < tokens.size() && tokens[start + 1].TokenType == TokenType::FunctionCall)
				{
					start++;

					expression->PushChild(_CreateFunctionCall(tokens, root, start));
					
					if (unaryType != UnaryExpressionType::None)
					{
						operators.push({BinaryExpressionType::None, unaryType, currentExpectedType, false, 4});
					}
					
					if (postType != UnaryExpressionType::None)
					{
						operators.push({ BinaryExpressionType::None, postType, currentExpectedType, false, 4 });
					}

					start++;
					continue;
				}

				std::string startStr = { root + "::" + token.Data };
				std::list<std::string> ls = _RetrieveForwardChain(tokens, start);
				ls.push_front(startStr);

				start--;
				
				bool pointerFlag = previous.TokenType == TokenType::AddressOp;
				bool derferenceFlag = previous.TokenType == TokenType::DereferenceOp;
				
				expression->PushChild(Ref<ASTVariableExpression>::Create(ls, pointerFlag, derferenceFlag));

				if (unaryType != UnaryExpressionType::None)
				{
					operators.push({ BinaryExpressionType::None, unaryType, currentExpectedType, false, 4 });
				}

				if (postType != UnaryExpressionType::None)
				{
					operators.push({ BinaryExpressionType::None, postType, currentExpectedType, false, 4 });
				}

				AbstractType abstractType = _GetTypeFromList(ls);

				if (pointerFlag)
				{
					currentExpectedType = AbstractType(VariableType::Pointer, abstractType.GetKind(), abstractType.Get(), abstractType.GetUserDefinedType());
					if(tokens[start - 1].TokenType != TokenType::OpenBracket)
						addBracket = true;
				}
				else if (abstractType.IsPointer())
				{
					currentExpectedType = abstractType;
					if (tokens[start - 1].TokenType != TokenType::OpenBracket)
						addBracket = true;
				}
				else 
				{
					currentExpectedType = AbstractType(abstractType.GetUnderlying(), abstractType.GetKind(), abstractType.GetUnderlying(), abstractType.GetUserDefinedType());
				}

			}
			else if (token.TokenType == TokenType::RValueNumber || 
					 token.TokenType == TokenType::RValueString || 
					 token.TokenType == TokenType::BooleanData  ||
					 token.TokenType == TokenType::Null)
			{		
				expression->PushChild(Ref<ASTNodeLiteral>::Create(token.Data));
			
				if (unaryType != UnaryExpressionType::None)
				{
					CLEAR_VERIFY(unaryType != UnaryExpressionType::Increment &&
						unaryType != UnaryExpressionType::PostIncrement &&
						unaryType != UnaryExpressionType::Decrement &&
						unaryType != UnaryExpressionType::PostDecrement, "bad unary type");

					operators.push({ BinaryExpressionType::None, unaryType, currentExpectedType, false, 4 });
				}

				if (postType != UnaryExpressionType::None)
				{
					CLEAR_VERIFY(unaryType != UnaryExpressionType::Increment &&
							     unaryType != UnaryExpressionType::PostIncrement &&
							     unaryType != UnaryExpressionType::Decrement &&
							     unaryType != UnaryExpressionType::PostDecrement, "bad unary type");

					operators.push({ BinaryExpressionType::None, postType, currentExpectedType, false, 4 });
				}

				currentExpectedType = AbstractType(token.Data);

				if (currentExpectedType.IsFloatingPoint())
					currentExpectedType = VariableType::Float64;
				else if (currentExpectedType.IsSigned())
					currentExpectedType = VariableType::Int64;
				else if (currentExpectedType.IsIntegral())
					currentExpectedType = VariableType::Uint64;	
			}
			else if (token.TokenType == TokenType::OpenBracket)
			{
				if (unaryType != UnaryExpressionType::None)
				{
					operators.push({ BinaryExpressionType::None, unaryType, currentExpectedType, false, 4 });
				}

				operators.push({ BinaryExpressionType::None, UnaryExpressionType::None, currentExpectedType, true, 0 });
			}
			else if (token.TokenType == TokenType::CloseBracket)
			{
				while (!operators.empty() && !operators.top().IsOpenBracket)
				{
					auto& top = operators.top();

					if(top.BinaryExpression != BinaryExpressionType::None)
						expression->PushChild(Ref<ASTBinaryExpression>::Create(top.BinaryExpression, top.ExpectedType));
					else 
						expression->PushChild(Ref<ASTUnaryExpression>::Create(top.UnaryExpression));

					operators.pop();
				}

				if (!operators.empty())
					operators.pop();

				if (postType != UnaryExpressionType::None)
				{
					operators.push({ BinaryExpressionType::None, postType, currentExpectedType, false, 4 });
				}
			}
			else if (!_IsUnary(token) && s_Presedence.contains(token.TokenType))
			{
				while (!operators.empty() && !operators.top().IsOpenBracket &&
					   s_Presedence[token.TokenType] <= operators.top().Presedence)
				{
					auto& top = operators.top();

					if (top.BinaryExpression != BinaryExpressionType::None)
						expression->PushChild(Ref<ASTBinaryExpression>::Create(top.BinaryExpression, top.ExpectedType));
					else
						expression->PushChild(Ref<ASTUnaryExpression>::Create(top.UnaryExpression));

					operators.pop();
				}

				if (token.TokenType == TokenType::DivOp)
					operators.push({ BinaryExpressionType::Div, UnaryExpressionType::None, VariableType::Float64, false, s_Presedence.at(token.TokenType)});
				else if (token.TokenType == TokenType::Power)
					operators.push({ BinaryExpressionType::Pow, UnaryExpressionType::None, VariableType::Float64, false, s_Presedence.at(token.TokenType) });
				else 
					operators.push({ GetBinaryExpressionTypeFromTokenType(token.TokenType), UnaryExpressionType::None, currentExpectedType, false, s_Presedence.at(token.TokenType)});

				if (addBracket)
				{
					operators.push({ BinaryExpressionType::None, UnaryExpressionType::None , currentExpectedType, true, 0 });

					size_t copy = start;

					while (copy < tokens.size() && !s_Terminators.contains(tokens[copy].TokenType))
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

			if (top.BinaryExpression != BinaryExpressionType::None)
				expression->PushChild(Ref<ASTBinaryExpression>::Create(top.BinaryExpression, top.ExpectedType));
			else
				expression->PushChild(Ref<ASTUnaryExpression>::Create(top.UnaryExpression));
			
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

		auto& expectedTypes = g_FunctionToExpectedTypes.at(name);


		uint32_t k = 0;
		while (i < tokens.size() && 
			  tokens[i].TokenType != TokenType::CloseBracket && 
			  tokens[i].TokenType != TokenType::EndIndentation && 
			  tokens[i].TokenType != TokenType::EndFunctionParameters)
		{
			if (tokens[i].TokenType == TokenType::Comma || tokens[i].TokenType == TokenType::EndLine)
			{
				i++;
				continue;
			}

			auto type = k < expectedTypes.size() && !expectedTypes[k].IsVariadic ? expectedTypes[k] : Paramater{};
			functionCall->PushChild(_CreateExpression(tokens, root, i, type.Type));

			k++;
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
		bool isPointer = tokens[current].TokenType == TokenType::PointerDef;

		if (isPointer)
			current -= 1;

		while (current > 0 && 
			   tokens[current].TokenType != TokenType::EndLine && 
			   tokens[current].TokenType != TokenType::EndIndentation &&
			   tokens[current].TokenType != TokenType::Comma &&
			   (GetVariableTypeFromTokenType(tokens[current].TokenType) == VariableType::None || 
			   tokens[current].TokenType == TokenType::VariableReference || 
			   tokens[current].TokenType == TokenType::DotOp))
		{
			current--;
		}

		if (tokens[current].TokenType == TokenType::DereferenceOp)
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

		return AbstractType(tokens[current], TypeKind::VariableReference, isPointer);
	}

	AbstractType AST::_GetTypeFromToken(const Token& token, bool isPointer)
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
			return AbstractType(VariableType::Pointer, TypeKind::Variable, currentType, userDefinedType);
		
		return AbstractType(currentType, TypeKind::Variable, userDefinedType);
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