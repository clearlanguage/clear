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


    const std::vector<ErrorReference> g_ErrorsReference = {
            {"Unclosed brackets","close your brackets brackets","Unclosed brackets"},
            {"Cannot perform operator on a type reference","idk man fix your code","Operator on type"},
            {"Expected struct name after struct declaration","Maybe add a name after the struct keyword","StructNoName"},
            {"Struct with name {} already exists","Change the name of the struct","Struct already defined"},
            {"Expected expression","Put an expression after operator","expected expression"},
            {"Expected variable name after type declaration not space","Put variable name after type","NoVariableName"},
            {"Cannot perform operator on a type reference","idk man fix your code","Operator on type"},
            {"Expected variable name after type declaration","Maybe add a variable name after type declaration","MissingVariableName"},
            {"Cannot index a type","If you meant to define an array specify the size of the array","Index operator on type"},
            {"Cannot use operator on a type","","operator on type"},
            {"Variable name cannot begin with a number","Change variable name so it does not begin with a number","Variable name begins with number"},
            {"Expected a comma between variables","Separate values using a comma","Missing variable separator"},
            {"Unknown char escape char \"\\{}\"","Reference existing escape chars and update your code","Unknown char escape character"},
            {"No data inside char literal","Put a character inside the char literal","Empty char literal"},
            {"expected ' after char ","Put a ' after the char","Unclosed char"}

        };
}