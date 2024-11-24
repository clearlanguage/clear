#pragma once

#include "ASTNode.h"

namespace clear {

	class ExpressionBuilder
	{
	public:
		ExpressionBuilder(const std::vector<Token>& tokens, const std::string& rootName, size_t& i);
		~ExpressionBuilder() = default;

		Ref<ASTExpression>   Create(const Ref<Type>& expectedType);
		Ref<ASTNodeBase>	 CreateVariableReferenceExpression();
		Ref<ASTFunctionCall> CreateFunctionCall();

		static Ref<Type> GetBaseTypeFromList(const std::list<std::string>& list, size_t dereferenceCount = 0);

		static bool IsTokenUnary(const Token& token);

	private:
		bool VerifyUnaryTypeWithToken(const Token& token, UnaryExpressionType type);
		bool VerifyPostUnaryTypeWithToken(const Token& token, UnaryExpressionType type);

	private:
		const std::vector<Token>& m_Tokens;
		std::string m_RootName;
		size_t& m_Index;
	};


}