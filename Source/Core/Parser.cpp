#include "Parser.h"

#include <iostream>

namespace alkhat {

	Parser::Parser(const std::filesystem::path& path)
		: m_Path(path)
	{
	}
	std::vector<Token> Parser::Build()
	{
		std::vector<Token> tokens;

		m_File.open(m_Path);

		if (!m_File.is_open())
		{
			std::cout << "failed to open file " << m_Path << std::endl;
			return tokens;
		}

		std::string line; 

		while (std::getline(m_File, line))
		{
			std::string current;

			for (size_t i = 0; i < line.length(); i++)
			{
				if (std::isspace(line[i]))
					continue;

				if (std::isalnum(line[i]))
				{
					while (std::isalnum(line[i]))
					{
						current += line[i];
					}

					//run something with text

					current.clear();
				}
				

			}
		}

		return tokens;
	}

}