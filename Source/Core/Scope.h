#pragma once 

#include "AST/ASTNode.h"


namespace clear {

	class Scope
	{
	public:
		Scope() = default;
		~Scope();

	private:
		Ref<ASTFunctionDecleration> m_CurrentFunction;
		std::set<std::string> m_VariableDeclerations;
		std::set<std::string> m_StructDeclerations;
		std::set<std::string> m_FunctionDeclerations;
	};

}