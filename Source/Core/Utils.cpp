#include "Utils.h"

#include <fast_float/fast_float.h>
//#include <charconv> replacement

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