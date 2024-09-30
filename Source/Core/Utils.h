//
// Created by Kareem Fares on 9/29/24.
//

#pragma once
#include <string>
#include <vector>

namespace clear {
    extern bool isspace(char c);
    extern std::string str(char c);
    extern std::vector<std::string> split(const std::string& str);
    extern bool isvarnamechar(char c);
}