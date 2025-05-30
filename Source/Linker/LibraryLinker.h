//
// Created by Kareem Fares on 5/30/25.
//

#pragma once


namespace clear{

    struct HeaderFunc {
        std::string returnType;
        std::string name;
        std::vector<std::string> args;
    };

    std::string RunClangPreprocess(const std::string& headerFile);
    std::vector<HeaderFunc> ExtractFunctions(const std::string& code);
    std::string TranslateCTypeToClearLang(const std::string& ctype);
}