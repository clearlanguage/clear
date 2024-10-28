#include "Errors.h"
#include <iostream>

namespace clear {
    void PrintError(const Error& err) 
    {
        int from =err.from;
        int to = err.to;
         std::cout<<"Line number: " << err.line<<'\n';

        std::cout <<  err.ErrorCause<<'\n';
        for (int i = 0; i <from; i++) {
            std::cout<<" ";
        }
        if (to == from) {
            std::cout<<"^";
        }else {
            for (int i = -1; i <to-from; i++) {
                std::cout<<"^";
            }
        }
        std::cout<<std::endl;
        std::cout<<"ErrorType: " <<err.ErrorType<<'\n';
        std::cout<<"ErrorMessage: " <<err.ErrorMessage<<'\n';
        std::cout<<"Advice: " << err.Advice <<'\n';
    }
}