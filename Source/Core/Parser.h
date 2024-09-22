#pragma once 

#include <string>
#include <vector>
#include <filesystem>
#include <fstream>


namespace alkhat {

	enum class TokenType
	{
		None = 0, VariableName, VariableTypeInt, 
		VariableTypeChar, VariableTypeString, 
		AssignmentOp, MultiplyOp
	};

	struct Token
	{
		TokenType TokenType = TokenType::None;
		std::string Data = "";
	};

	class Parser
	{
	public:
		Parser(const std::filesystem::path& path);
		~Parser() = default;

		std::vector<Token> Build();

	private:
		std::filesystem::path m_Path;
		std::ifstream m_File;
	};
	
}