//
// Created by Kareem Fares on 9/29/24.
//



#include "Utils.h"
#include <string>
#include <vector>
#include <sstream>

namespace clear{
    bool isspace(char c){
       if (c ==' ' || c == '\t'){
         return true;
       }
       return false;
    }

    std::string str(char hello){
        return std::string(1,hello);
    }
    std::vector<std::string> split(const std::string& str) {
        std::istringstream iss(str);
        std::vector<std::string> words;
        std::string word;
        while (iss >> word) {
            words.push_back(word);
        }
        return words;

    }
    bool isvarnamechar(char c){
      return std::isalnum(c) || c == '_';
    }
}