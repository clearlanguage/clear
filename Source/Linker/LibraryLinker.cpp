#include <iostream>
#include <fstream>
#include <sstream>
#include <regex>
#include <vector>
#include <string>
#include <cstdlib>
#include "LibraryLinker.h"
#include "Core/TypeRegistry.h"
#include <Core/Log.h>
#include <algorithm>
#include <cctype>
#include <string>

inline std::string Trim(const std::string& str)
{
    size_t first = str.find_first_not_of(" \t\n\r\f\v");
    if (first == std::string::npos)
        return ""; // no content

    size_t last = str.find_last_not_of(" \t\n\r\f\v");
    return str.substr(first, (last - first + 1));
}


namespace clear {
    std::string RunClangPreprocess(const std::string& headerFile) {
        const std::string tempFile = "clang_output.txt";
        const std::string command = "clang -E -v " + headerFile + " > " + tempFile + " 2>&1";

        int result = std::system(command.c_str());
        if (result != 0) {
            std::cerr << "Failed to run clang preprocess command\n";
            return "";
        }

        std::ifstream inFile(tempFile);
        std::stringstream buffer;
        buffer << inFile.rdbuf();
        inFile.close();

        std::remove(tempFile.c_str()); // Cleanup
        return buffer.str();
    }

    std::vector<HeaderFunc> ExtractFunctions(const std::string& headerFile) {
        std::string code = RunClangPreprocess(headerFile);

        if (code.empty()) {
            std::cerr << "Error: Could not preprocess file.\n";
            CLEAR_HALT();
        }

        std::vector<HeaderFunc> funcs;

        std::regex pattern(R"(([\w\s\*\d]+?)\s+([\w_][\w\d_]*)\s*\(([^)]*)\)\s*;)");
        std::smatch match;
        auto begin = code.cbegin();
        auto end = code.cend();

        while (std::regex_search(begin, end, match, pattern)) {
            HeaderFunc f;
            std::string rawReturnType = match[1].str();

            // Clean up whitespace and newlines
            rawReturnType.erase(std::remove(rawReturnType.begin(), rawReturnType.end(), '\n'), rawReturnType.end());
            rawReturnType.erase(std::remove(rawReturnType.begin(), rawReturnType.end(), '\r'), rawReturnType.end());
            rawReturnType = std::regex_replace(rawReturnType, std::regex("^\\s+|\\s+$"), ""); // trim

            f.returnType = TranslateCTypeToClearLang(rawReturnType);
            f.name = match[2].str();

            std::string argsStr = match[3].str();
            std::stringstream ss(argsStr);
            std::string arg;
            while (std::getline(ss, arg, ',')) {
                if (!arg.empty())
                    f.args.push_back(TranslateCTypeToClearLang(arg));
            }

            funcs.push_back(f);
            begin = match.suffix().first;
        }

        return funcs;
    }
    std::string TranslateCTypeToClearLang(const std::string& ctype)
    {
        std::string clean = ctype;
    std::string type = clean;

    // Remove qualifiers
    const std::vector<std::string> qualifiers = { "extern", "const", "volatile", "static", "register", "inline" };
    for (const auto& q : qualifiers) {
        size_t pos;
        while ((pos = type.find(q)) != std::string::npos) {
            type.erase(pos, q.length());
        }
    }

    // Trim and normalize spaces
    type = Trim(type);
    while (type.find("  ") != std::string::npos)
        type = std::regex_replace(type, std::regex("  +"), " ");

    // Count pointer indirection levels (e.g., "int **" → 2)
    int pointerCount = 0;
    while (!type.empty() && (type.back() == '*' || type.back() == ' ')) {
        if (type.back() == '*')
            pointerCount++;
        type.pop_back();
    }

    type = Trim(type);

    // Map C base types to ClearLang base types
    static const std::unordered_map<std::string, std::string> typeMap = {
        { "void", "void" },
        { "char", "int8" },
        { "short", "int16" },
        { "int", "int32" },
        { "long", "int64" },
        { "float", "float32" },
        { "double", "float64" },
        { "unsigned char", "uint8" },
        { "unsigned short", "uint16" },
        { "unsigned int", "uint32" },
        { "unsigned long", "uint64" },
        { "signed char", "int8" },
        { "signed short", "int16" },
        { "signed int", "int32" },
        { "signed long", "int64" },
        { "long long", "int64" },  // Can be adapted if needed
        { "unsigned long long", "uint64" },
        { "size_t", "uint64" },
        {"long double","float64"},
        {"long int","int64"},
        {"long long int","int64"}
    };

    // Recombine multi-word types (e.g., "unsigned long")
    std::string baseType = type;
    if (typeMap.contains(baseType)) {
        baseType = typeMap.at(baseType);
    } else {
        CLEAR_LOG_ERROR("Unknown C type: ", type);
        baseType = type; // Keep unknown type name
    }

    // Apply pointer indirection (e.g., int** → int8** in clear)
    for (int i = 0; i < pointerCount; ++i) {
        baseType += "*";
    }

    return baseType;
    }

}
