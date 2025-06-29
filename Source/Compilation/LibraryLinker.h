/* //
// Created by Kareem Fares on 5/30/25.
//

#pragma once
#include "Lexing/Token.h"
#include <vector>

namespace clear{

    struct HeaderFunc {
        std::vector<Token> returnType;
        std::string name;
        std::vector<std::vector<Token>> args;
    };

    std::string RunClangPreprocess(const std::string& headerFile);
    std::vector<HeaderFunc> ExtractFunctions(const std::string& code);
    std::vector<Token> TranslateCTypeToClearLang(const std::string& ctype);
} */