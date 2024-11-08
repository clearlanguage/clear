#pragma once

#include "ASTNode.h"

namespace clear {

	class ExpressionBuilder
	{
	public:
		ExpressionBuilder(const std::vector<Token>& tokens, const std::string& rootName, size_t& i);
		~ExpressionBuilder() = default;

		Ref<ASTExpression>	 Create(const AbstractType& expectedType);
		Ref<ASTNodeBase>	 CreateVariableReferenceExpression();
		Ref<ASTFunctionCall> CreateFunctionCall();
		
		std::vector<AbstractType> TypeAnalysis(size_t index);

		std::list<std::string> GetVariableChain();
		std::list<std::string> GetVariableChain(size_t index);

		static AbstractType GetBaseTypeFromList(const std::list<std::string>& list);

		static bool IsTokenUnary(const Token& token);

	private:
		bool VerifyUnaryTypeWithToken(const Token& token, UnaryExpressionType type);

	private:
		const std::vector<Token>& m_Tokens;
		std::string m_RootName;
		size_t& m_Index;
	};


}