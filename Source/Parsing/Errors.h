//
// Created by Kareem Fares on 10/24/24.
//

#pragma once
#include <string>
#include <map>

namespace clear{

    struct Error 
    {
        std::string ErrorType;
        std::string ErrorMessage;
        std::string Advice;
        std::string ErrorCause;
        size_t line;
        size_t from;
        size_t to;
    };

    struct ErrorReference {
        std::string ErrorMessage;
        std::string Advice;
        std::string ErrorType;
    };


    extern const std::vector<ErrorReference> g_ErrorsReference;

    void PrintError(const Error& err);

}



