#include "Scope.h"

#include "Types.h"

namespace clear {

	Scope::Scope(const std::string& name, const Ref<ASTFunctionDefinition>& function)
		: m_Name(name), m_CurrentFunction(function)
	{
	}

	Scope::~Scope()
	{
		for (auto& name : m_VariableDeclerations)
		{
			AbstractType::RemoveVariableType(name);
		}

		for (auto& name : m_StructDeclerations)
		{
			AbstractType::RemoveStructType(name);
		}
	}

	void Scope::DeclareVariable(const std::string& name)
	{
		CLEAR_VERIFY(!m_VariableDeclerations.contains(name), "variable already declared");
		m_VariableDeclerations.insert(name);
	}

	void Scope::DeclareStruct(const std::string& name)
	{
		CLEAR_VERIFY(!m_StructDeclerations.contains(name), "variable already declared");
		m_StructDeclerations.insert(name);
	}
}