#include "Utils.h"
#include <string>
#include <vector>
#include <sstream>

namespace clear {

    bool IsSpace(char c)
    {
        return c == ' ' || c == '\t';
    }

    std::string Str(char hello)
    {
        return std::string(1,hello);
    }

    std::vector<std::string> Split(const std::string& str) 
    {
        std::istringstream iss(str);
        std::vector<std::string> words;
        std::string word;
        while (iss >> word) 
            words.push_back(word);
        
        return words;

    }

    bool IsVarNameChar(char c)
    {
        return std::isalnum(c) || c == '_';
    }
}