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
        std::string s(1, hello);
        return s;
    }


    std::vector<std::string> Split(const std::string& str, const std::string& delimiter)
    {
        std::vector<std::string> words;

        size_t position = 0;
        size_t start = 0;

        while ((position = str.find(delimiter, start)) != std::string::npos)
        {
            words.push_back(str.substr(start, position - start));  
            start = position + delimiter.length();  
        }

        words.push_back(str.substr(start));

        return words;
    }

    std::vector<std::string> Split(const std::string& str, char delimiter)
    {
        std::vector<std::string> words;

        size_t position = 0;
        size_t start = 0;

        while ((position = str.find_first_of(delimiter, start)) != std::string::npos)
        {
            words.push_back(str.substr(start, position - start));
            start = position + 1;
        }

        words.push_back(str.substr(start));

        return words;
    }

    int HexStringToInteger(const std::string& hexString)
    {
        int result = 0;
        int length = hexString.length();

        for (int i = 0; i < length; i++) 
        {
            char hexChar = hexString[i];
            int hexValue = 0;

            if (hexChar >= '0' && hexChar <= '9') 
            {
                hexValue = hexChar - '0';
            } 
            else if (hexChar >= 'A' && hexChar <= 'F') 
            {
                hexValue = hexChar - 'A' + 10;
            } 
            else if (hexChar >= 'a' && hexChar <= 'f') 
            {
                hexValue = hexChar - 'a' + 10;
            }

            result = result * 16 + hexValue;
        }
           
        return result;
    }

    int BinaryStringToInteger(const std::string& binaryString) 
    {
        int result = 0;
        int length = binaryString.length();

        for (int i = 0; i < length; i++) 
        {
            if (binaryString[i] == '1') 
            {
                result += pow(2, length - i - 1);
            }
        }

        return result;
    }

    bool IsOnlyWhitespace(const std::string &str)
    {
        for (char c : str) 
        {
            if (!std::isspace(c)) 
            {
                return false;  
            }
        }
        
        return true;  
    }

    bool IsVarNameChar(char c)
    {
        return std::isalnum(c) || c == '_';
    }

	bool IsValidNumber(const std::string_view& str)
	{
		double result;
		auto [ptr, ec] = fast_float::from_chars(str.data(), str.data() + str.size(), result);  //same use case as std::from_chars just more portable
		return ec == std::errc() && ptr == str.data() + str.size();
	}

    NumberInfo GetNumberInfoFromLiteral(const std::string_view& str)
    {
        bool isFloatingPoint = str.find_first_of('.') != std::string_view::npos;

        NumberInfo info;

        double result;
        auto [ptr, ec] = fast_float::from_chars(str.data(), str.data() + str.size(), result);

        if (ec != std::errc() || ptr != str.data() + str.size())
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

        if (!isFloatingPoint && (testTypeAgainst(uint8_t{}) || testTypeAgainst(uint16_t{}) || testTypeAgainst(uint32_t{}) || testTypeAgainst(uint64_t{})))
            return info;

        if (!isFloatingPoint && (testTypeAgainst(int8_t{}) || testTypeAgainst(int16_t{}) || testTypeAgainst(int32_t{}) || testTypeAgainst(int64_t{})))
            return info;

        if (testTypeAgainst(float{}) || testTypeAgainst(double{}))
            return info;


        return info;
    }


    StringIterator::StringIterator(const std::string& str) : str_(str), index(0) {}

    char StringIterator::next() {
        if (index < str_.length()) {
            return str_[index++];
        } else {
            return '\0';
        }
    }

    void StringIterator::back() {
        if (index <1 )
            index--;
    }

}