#pragma once

#include <string>
#include <vector>
#include <set>

namespace clear {

    extern bool IsSpace(char c);
    extern std::string Str(char c);
    extern std::vector<std::string> Split(const std::string& str, const std::string& delimiter);
    extern std::vector<std::string> Split(const std::string& str, char delimiter = ' ');
    extern std::vector<std::string> Split(const std::string& str, const std::set<char>& delimters, bool multiple);
    extern bool IsVarNameChar(char c);
    extern bool IsValidNumber(const std::string_view& str);
    extern bool IsOnlyWhitespace(const std::string& str);
    extern int BinaryStringToInteger(const std::string& binaryString);
    extern int HexStringToInteger(const std::string& hexString);

    struct NumberInfo
    {
        bool Valid = false;
        bool IsFloatingPoint = false;
        bool IsSigned = false;
        size_t BitsNeeded = 0;
    };

    extern NumberInfo GetNumberInfoFromLiteral(const std::string_view& str);

}