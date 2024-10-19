#include "Utils.h"

#include <fast_float/fast_float.h>

#include <string>
#include <vector>
#include <sstream>
#include <charconv>



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
    int hexStringToInteger(const std::string& hexString) {
        int result = 0;
        int length = hexString.length();

        for (int i = 0; i < length; i++) {
            char hexChar = hexString[i];
            int hexValue;

            if (hexChar >= '0' && hexChar <= '9') {
                hexValue = hexChar - '0';
            } else if (hexChar >= 'A' && hexChar <= 'F') {
                hexValue = hexChar - 'A' + 10;
            } else if (hexChar >= 'a' && hexChar <= 'f') {
                hexValue = hexChar - 'a' + 10;
            }
            result = result * 16 + hexValue;
        }

        return result;
    }

    int binaryStringToInteger(const std::string& binaryString) {
        int result = 0;
        int length = binaryString.length();

        for (int i = 0; i < length; i++) {
            if (binaryString[i] == '1') {
                result += pow(2, length - i - 1);
            }
        }

        return result;
    }

    bool IsVarNameChar(char c)
    {
        return std::isalnum(c) || c == '_';
    }

	bool IsValidNumber(const std::string_view& str)
	{
		double result;
		auto [ptr, ec] = fast_float::from_chars(str.data(), str.data() + str.size(), result);
		return ec == std::errc() && ptr == str.data() + str.size();
	}

    NumberInfo GetNumberInfoFromLiteral(const std::string_view& str)
    {
        NumberInfo info;

        double result;
        auto [ptr, ec] = fast_float::from_chars(str.data(), str.data() + str.size(), result);

        if (ec != std::errc())
            return info;

        info.Valid = true;

        auto testTypeAgainst = [&info, result](auto type)
            {
                using T = decltype(type);

                if (result == (double)((T)(result)))
                {
                    if constexpr (std::is_signed_v<T>)
                    {
                        info.IsSigned = true;
                    }

                    if constexpr (std::is_floating_point_v<T>)
                    {
                        info.IsFloatingPoint = true;
                    }

                    info.BitsNeeded = sizeof(T) * 8;
                    return true;
                }
                return false;
            };


        if (testTypeAgainst(uint8_t{}) || testTypeAgainst(uint16_t{}) || testTypeAgainst(uint32_t{}) || testTypeAgainst(uint64_t{}))
            return info;

        if (testTypeAgainst(int8_t{}) || testTypeAgainst(int16_t{}) || testTypeAgainst(int32_t{}) || testTypeAgainst(int64_t{}))
            return info;

        if (testTypeAgainst(float{}) || testTypeAgainst(double{}))
            return info;


        return info;
    }
}