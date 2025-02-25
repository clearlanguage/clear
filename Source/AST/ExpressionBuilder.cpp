#include "ExpressionBuilder.h"

#include <stack>
#include <set>

namespace clear {

	static std::map<TokenType, int32_t> s_Precedence = 
	{
		{TokenType::DotOp,			    5},
	    {TokenType::IndexOperator,      5}, 
	    {TokenType::Power,              4}, 
	    {TokenType::Negation,           4},
	    {TokenType::Increment,          4},
	    {TokenType::Decrement,          4},
	    {TokenType::BitwiseNot,         4},
	    {TokenType::DereferenceOp,      4},
	    {TokenType::AddressOp,          4},
	    {TokenType::DivOp,              3},
	    {TokenType::MulOp,              3},
	    {TokenType::LeftShift,          3},
	    {TokenType::RightShift,         3},
	    {TokenType::AddOp,              2},
	    {TokenType::SubOp,              2},
	    {TokenType::BitwiseAnd,         1}, 
	    {TokenType::BitwiseXor,         1},
	    {TokenType::BitwiseOr,          1},
	    {TokenType::IsEqual,            0}, 
	    {TokenType::NotEqual,           0},
	    {TokenType::LessThan,           0},
	    {TokenType::GreaterThan,        0},
	    {TokenType::LessThanEqual,      0},
	    {TokenType::GreaterThanEqual,   0},
	    {TokenType::OpenBracket,       -1}  
	};

	static std::set<TokenType> s_Terminators = 
	{ 
		TokenType::EndLine, 
		TokenType::EndIndentation, 
		TokenType::Comma,  
		TokenType::EndFunctionArguments, 
		TokenType::EndArray,
		TokenType::Assignment, 
		TokenType::StartIndentation
	};

	static std::set<TokenType> s_FunctionTerminators =
	{
		TokenType::EndFunctionArguments
	};

	static std::set<TokenType> s_RValues =
	{
		TokenType::RValueNumber,
		TokenType::RValueString,
		TokenType::BooleanData,
		TokenType::Null
	};

	struct Operator
	{
		BinaryExpressionType BinaryExpression;
		UnaryExpressionType UnaryExpression;
		Ref<Type> ExpectedType;
		bool IsOpenBracket = false;
		int32_t Precedence = 0;
	};

	ExpressionBuilder::ExpressionBuilder(const std::vector<Token>& tokens, const std::string& rootName, size_t& i)
		: m_Tokens(tokens), m_RootName(rootName), m_Index(i)
	{
	}

	Ref<ASTExpression> ExpressionBuilder::Create(const Ref<Type>& expectedType)
	{
		Ref<ASTExpression> expression = Ref<ASTExpression>::Create();
		std::stack<Operator> operators;

		Ref<Type> currentExpectedType = expectedType;

		if(expectedType && expectedType->GetID() != TypeID::None)
			operators.push({ BinaryExpressionType::None, UnaryExpressionType::Cast, currentExpectedType, false, -1 });

		size_t typeIndex = 0;

		while (m_Index < m_Tokens.size() && !s_Terminators.contains(m_Tokens[m_Index].TokenType))
		{
			UnaryExpressionType unaryType = m_Index - 1 < m_Tokens.size() ? Type::GetUnaryExpressionTypeFromToken(m_Tokens[m_Index - 1].TokenType) : UnaryExpressionType::None;

			int64_t tempIndex = (int64_t)m_Index - 1;

			while (unaryType != UnaryExpressionType::None && VerifyUnaryTypeWithToken(m_Tokens[m_Index], unaryType))
			{
				operators.push({ BinaryExpressionType::None, unaryType, currentExpectedType, false, 4 });

				if (--tempIndex > 0)
				{
					unaryType = Type::GetUnaryExpressionTypeFromToken(m_Tokens[tempIndex].TokenType);
				}
				else
				{
					unaryType = UnaryExpressionType::None;
				}
			}

			if (m_Tokens[m_Index].TokenType == TokenType::VariableReference)
			{
				expression->PushChild(CreateVariableReferenceExpression());
			}
			else if (m_Tokens[m_Index].TokenType == TokenType::MemberName)
			{
				expression->PushChild(Ref<ASTMemberAccess>::Create(m_Tokens[m_Index].Data));
			}
			else if (s_RValues.contains(m_Tokens[m_Index].TokenType))
			{
				expression->PushChild(Ref<ASTNodeLiteral>::Create(m_Tokens[m_Index]));

				while(m_Tokens[m_Index].TokenType == TokenType::DotOp || m_Tokens[m_Index].TokenType == TokenType::VariableReference)
				{
					m_Index++;
				}
			}
			else if (m_Tokens[m_Index].TokenType == TokenType::OpenBracket)
			{
				operators.push({ BinaryExpressionType::None, UnaryExpressionType::None, currentExpectedType, true, 0 });
			}
			else if (m_Tokens[m_Index].TokenType == TokenType::CloseBracket)
			{
				while (!operators.empty() && !operators.top().IsOpenBracket)
				{
					auto& top = operators.top();

					if (top.BinaryExpression != BinaryExpressionType::None)
						expression->PushChild(Ref<ASTBinaryExpression>::Create(top.BinaryExpression));
					else
						expression->PushChild(Ref<ASTUnaryExpression>::Create(top.UnaryExpression, top.ExpectedType));

					operators.pop();
				}
				
				if (!operators.empty())
					operators.pop();
			}
			else if (!IsTokenUnary(m_Tokens[m_Index]) && s_Precedence.contains(m_Tokens[m_Index].TokenType))
			{
				while (!operators.empty() && !operators.top().IsOpenBracket && s_Precedence[m_Tokens[m_Index].TokenType] <= operators.top().Precedence)
				{
					auto& top = operators.top();

					if (top.BinaryExpression != BinaryExpressionType::None)
						expression->PushChild(Ref<ASTBinaryExpression>::Create(top.BinaryExpression));
					else
						expression->PushChild(Ref<ASTUnaryExpression>::Create(top.UnaryExpression, top.ExpectedType));

					operators.pop();
				}

				if (m_Tokens[m_Index].TokenType == TokenType::DivOp)
				{
					operators.push({ BinaryExpressionType::Div, 
									 UnaryExpressionType::None, 
						             Ref<Type>::Create(TypeID::Float64), 
								     false, s_Precedence.at(m_Tokens[m_Index].TokenType) });

				}
				else if (m_Tokens[m_Index].TokenType == TokenType::Power)
				{
					operators.push({ BinaryExpressionType::Pow, 
									 UnaryExpressionType::None, 
									 Ref<Type>::Create(TypeID::Float64), 
									 false, s_Precedence.at(m_Tokens[m_Index].TokenType) });
				}
				else
				{
					operators.push({ Type::GetBinaryExpressionTypeFromToken(m_Tokens[m_Index].TokenType), 
									 UnaryExpressionType::None, currentExpectedType, 
									 false, s_Precedence.at(m_Tokens[m_Index].TokenType) });
				}
			}

			UnaryExpressionType postType = m_Index + 1 < m_Tokens.size() ? Type::GetPostUnaryExpressionTypeFromToken(m_Tokens[m_Index + 1].TokenType) : UnaryExpressionType::None;

			tempIndex = (int64_t)m_Index + 1;
			bool change = false;

			while (postType != UnaryExpressionType::None && VerifyPostUnaryTypeWithToken(m_Tokens[m_Index], postType))
			{
				operators.push({ BinaryExpressionType::None, postType, currentExpectedType, false, 4 });
				change = true;

				if (++tempIndex < m_Tokens.size())
				{
					postType = Type::GetUnaryExpressionTypeFromToken(m_Tokens[tempIndex].TokenType);
				}
				else
				{
					postType = UnaryExpressionType::None;
				}
			}

			m_Index = change ? tempIndex : m_Index + 1;
		}

		while (!operators.empty())
		{
			auto& top = operators.top();

			if (top.BinaryExpression != BinaryExpressionType::None)
				expression->PushChild(Ref<ASTBinaryExpression>::Create(top.BinaryExpression));
			else
				expression->PushChild(Ref<ASTUnaryExpression>::Create(top.UnaryExpression, top.ExpectedType));

			operators.pop();
		}

		return expression;
	}

	Ref<ASTNodeBase> ExpressionBuilder::CreateVariableReferenceExpression()
	{
		if (m_Index + 1 < m_Tokens.size() && m_Tokens[m_Index + 1].TokenType == TokenType::FunctionCall)
		{
			m_Index++;
			return CreateFunctionCall();
		}

		return Ref<ASTVariableExpression>::Create(m_RootName + "::" + m_Tokens[m_Index].Data); 
	}

	Ref<ASTFunctionCall> ExpressionBuilder::CreateFunctionCall()
	{
		CLEAR_VERIFY(m_Index < m_Tokens.size(), "");

		const std::string& name = m_Tokens[m_Index].Data;
		Ref<ASTFunctionCall> functionCall = Ref<ASTFunctionCall>::Create(name);

		m_Index++;

		CLEAR_VERIFY(m_Index < m_Tokens.size() && m_Tokens[m_Index].TokenType == TokenType::OpenBracket, "");
		
		auto& expectedTypes = g_FunctionMetaData.at(name).Parameters;

		m_Index++;

		uint32_t k = 0;

		while (m_Index < m_Tokens.size() && !s_FunctionTerminators.contains(m_Tokens[m_Index].TokenType))
		{
			if (m_Tokens[m_Index].TokenType == TokenType::Comma || m_Tokens[m_Index].TokenType == TokenType::EndLine)
			{
				m_Index++;
				continue;
			}

			Paramater type = k < expectedTypes.size() && !expectedTypes[k].IsVariadic ? expectedTypes[k] : Paramater{};
			functionCall->PushChild(Create(type.Type));

			k++;
		}


		return functionCall;
	}

	Ref<Type> ExpressionBuilder::GetBaseTypeFromList(const std::list<std::string>& list, size_t dereferenceCount)
	{
		CLEAR_VERIFY(!list.empty(), "");

		Ref<Type> type = Type::GetVariableTypeFromName(list.front());
		CLEAR_VERIFY(type, "");

		while(dereferenceCount > 0 && type->IsPointer())
		{
			type = type->GetUnderlying();
			dereferenceCount--;
		}
		
		CLEAR_VERIFY(dereferenceCount == 0, "cannot dereference a non pointer type");

		auto it = list.begin();
		it++;

		for(; it != list.end(); it++)
		{
			StructMetaData& structMetaData = Type::GetStructMetaData(type->GetUserDefinedTypeIdentifer());
			CLEAR_VERIFY(structMetaData.Struct, "not a valid type ", type->GetUserDefinedTypeIdentifer());

			size_t indexToNextType = structMetaData.Indices[*it];
			type = structMetaData.Types[indexToNextType];
		}

		return type;
	}

	bool ExpressionBuilder::IsTokenUnary(const Token& token)
	{
		switch (token.TokenType)
		{
			case TokenType::Increment:
			case TokenType::Decrement:
			case TokenType::BitwiseNot:
			case TokenType::DereferenceOp:
			case TokenType::AddressOp:
			case TokenType::Negation:
				return true;
			default:
				break;
		}

		return false;
	}

	bool ExpressionBuilder::VerifyUnaryTypeWithToken(const Token& token, UnaryExpressionType type)
	{
		if (token.TokenType == TokenType::VariableReference)
		{
			return true; //good with all types
		}

		if (s_RValues.contains(token.TokenType))
		{
			switch (type)
			{
				case clear::UnaryExpressionType::BitwiseNot:
				case clear::UnaryExpressionType::Negation:
				case clear::UnaryExpressionType::Cast:
					return true;
				default:
					return false;
			}
		}

		if (token.TokenType == TokenType::OpenBracket)
			return true;

		return false;
	}

    bool ExpressionBuilder::VerifyPostUnaryTypeWithToken(const Token &token, UnaryExpressionType type)
    {
        if (token.TokenType == TokenType::VariableReference)
		{
			return true; //good with all types
		}

		if (s_RValues.contains(token.TokenType))
		{
			switch (type)
			{
				case clear::UnaryExpressionType::BitwiseNot:
				case clear::UnaryExpressionType::Negation:
				case clear::UnaryExpressionType::Cast:
					return true;
				default:
					return false;
			}
		}

		if (token.TokenType == TokenType::CloseBracket)
			return true;

		return false;
    }
}