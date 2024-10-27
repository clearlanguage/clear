#pragma once 

#include "AST/ASTNode.h"

#include <set>


namespace clear {

	class Scope
	{
	public:
		Scope(const std::string& name, const Ref<ASTFunctionDecleration>& function);
		~Scope();

		void DeclareVariable(const std::string& name);
		void DeclareStruct(const std::string& name);
		void DeclareFunction(const std::string& function) = delete;

	private:
		Ref<ASTFunctionDecleration> m_CurrentFunction;
		std::set<std::string> m_VariableDeclerations;
		std::set<std::string> m_StructDeclerations;
		std::set<std::string> m_FunctionDeclerations;

		std::string m_Name;
	};

}