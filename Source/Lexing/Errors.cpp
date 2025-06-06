#include "Errors.h"
#include <iostream>
#include <vector>

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
        std::cout<<"ErrorMessage: " <<err.ErrorMessage<<'\n';
        std::cout<<"Advice: " << err.Advice <<'\n';
        std::cout<<"ErrorType: " <<err.ErrorType<<'\n';

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
            {R"(Unknown char escape char "\{}")","Reference existing escape chars and update your code","Unknown char escape character"},
            {"No data inside char literal","Put a character inside the char literal","Empty char literal"},
            {"expected ' after char ","Put a ' after the char","Unclosed char"},
            {"Did not expect commas in {}","Remove commas or fix brackets","Unexpected commas"},
        {"Unclosed multiline comment expected \\* at the end of the comment","Close the comment by adding *\\ at the end","multiline comment unclosed"},
            {"Attempting to close wrong bracket expected {} not {}","Change the closing bracket to the correct type","Wrong closing bracket"},
        {"Attempting to close unopened {} expected {} not {}","Change the string closer to the correct type","Wrong closing char"},
            {"Expected a valid number","Perhaps you forgot an operator in between","Invalid number"},
        {"Expected one decimal point in a float","Perhaps you forgot an operator in between two numbers","Invalid number two decimal points"},
        {"expected {} literal to begin with 0","Change the starting number for the literal to 0","{} literal not starting with 0"},
        {"Unclosed string expected \" at the end of the string","Close the string by adding a \" at the end of it","Unclosed string"},
        {"Expected value inside index operator","Perhaps you forgot to put a value","Index operator empty"},
        {"Attempting to close unopened {}","Perhaps you forgot to open the {}","Closing unopened {}"},
        {"No spaces between pointer definitions allowed","Remove the spaces if you were trying to define a pointer to a pointer otherwise check your code","Space between pointer definitions"},
          {"Unclosed brackets for {} expected {} at the end","Add the correct closing bracket at the end of {}","Unclosed {}"},
        {"Expected variable name after comma","Remove the comma or add a variable name after the comma","Expected variable name"},
        {"Expected ( after function declaration","Add brackets after function declaration and include the types inside","Missing brackets after function declaration"},
        {"Only expected hexadecimal characters in hexadecimal literal","Ensure there are only valid hexadecimal characters in the literal","Invalid char in hex literal"},
        {"Only expected 1 and 0 only in binary literal","Ensure there are only 1s and 0s in the literal","Invalid char in binary literal"},
        {"Expected value after comma","Remove extra comma or add an value or expression","Missing value after comma"},
        {"Function parameter can only begin with a type not a {}","Change the parameter so that it begins with a valid type","Function parameter begins with none type declaration"},
        {"Did not expect new line after type declaration  ","Add a variable name after type declaration","Expected variable name after type declaration"},
        {"{} name cannot start with a number","Change the name of the {} to not begin with a number maybe prefix it with '_'","{} name invalid"},
        {"Unexpected character \"{}\" in {} name","change struct name to only include valid characters","{} name invalid"},
        {"Did not expect a space in the middle of {} name","Separate words using underscores not spaces","Invalid {} name"},
        {"Expected a value not another comma","Remove the extra comma","Unexpected commas"},//38
        {"The identifier {} is a reserved keyword and cannot be used as a variable name","Please choose a different name for your variable. Avoid using reserved keywords as identifiers.","Variable name is keyword"}, //39
        {"Expected return type after arrow ","if you do not want to specify a type remove the arrow or add a type after","missing function type"}, //40
        {"Invalid character encountered: {}","Please review your input for any typos or special characters that may be causing the issue.","syntax error"}, //41
            {"Error 42","",""}, // 42
            {"Error 43 empty generic","",""},
            {"error 44 invalid char in generic"},
        {"Error 45 did not expect comma"}, //45
            {"Error 46 generic must include type"},//46
                {"error 47 space between var names"},      //47
{"Restriction with name {} already exists","Change the name of the restriction","restriction already defined"}, //48
                {"error 49","",""}, //49
                {"Cannot declare a variable with a restriction as type"}, // 50
                {"Error 51 cannot use dot op on this"}, // 51
            {"Error 52 did not expect trailing space in generic start"}, // 51
            {"Expected indentation missing :"},
            {"Place holder error too lazy to write this"},
{"The requested module '{}' could not be found in the import paths.","Ensure the module name is correct and that it exists in the import search directories.","Import not found"}, //40
{"The import alias '{}' is already in use and cannot be reused for another module.","Use a unique alias for each imported module to avoid naming conflicts.","Import alias conflict"}, //41


        };
}