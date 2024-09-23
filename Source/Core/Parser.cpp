#include "Parser.h"

#include <iostream>
#include <fstream>
#include <map>

namespace alkhat
{

    std::vector<std::string> Parser::ParseFile(const std::filesystem::path& path)
    {
        std::ifstream file(path);

        if (!file.is_open())
        {
            std::cout << "failed to open file " << path << std::endl;
            return {};
        }

        std::string buffer;
        std::string line;

        while (std::getline(file, line))
        {
            buffer += line + '\n'; 
        }

        std::vector<std::string> split;
        std::string current = "";

        for (size_t i = 0; i < buffer.length(); i++)
        { 
            if (buffer[i] == '\n' || buffer[i] == '\t' || buffer[i] == '\b')
            {
                if (!current.empty())
                {
                    split.push_back(current);
                    current.clear();
                }

                continue;
            }

            current += buffer[i];
        }

        return split;
    }

			}
		}

		return tokens;
	}
}