//
// Created by Kareem Fares on 9/29/24.
//



#include "Utils.h"
#include <string>

namespace clear{
    bool isspace(char c){
       if (c ==' ' || c == '\t'){
         return true;
       }
       return false;
    }

    std::string str(char hello){
        return std::string(1,hello);
    }
}