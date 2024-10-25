#include "Errors.h"
#include <iostream>

namespace clear {
    void PrintError(const Error& err) 
    {
        std::cout<<"Line number: " << err.line<<'\n';
        std::cout<<"Cause: " <<  err.ErrorCause<<'\n';
        std::cout<<"ErrorType: " <<err.ErrorType<<'\n';
        std::cout<<"ErrorMessage: " <<err.ErrorMessage<<'\n';
        std::cout<<"Advice: " << err.Advice <<'\n';
    }
}