//
// Created by Kareem Fares on 10/24/24.
//

#pragma once
#include <string>


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

    void PrintError(const Error& err);

}



