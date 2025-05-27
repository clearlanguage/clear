//
// Created by Kareem Fares on 5/27/25.
//

#include "Linker.h"
#include "AST/ASTNode.h"
#include "Parsing/Parser.h"
#include "Lexing/Lexer.h"
#include <iostream>

namespace clear {


    void Linker::GenerateLibraries(Libraries* lib,const std::filesystem::path& file){
        Lexer lexer;
        ProgramInfo tokens = lexer.CreateTokensFromFile(file);

        for (size_t i = 0; i < tokens.Tokens.size(); i++)
        {
            std::cout << "Token Type: " << TokenToString(tokens.Tokens[i].TokenType);
            std::cout << ", Data: " << tokens.Tokens[i].Data;
            std::cout << std::endl;
        }

    }



}