#include "Parsing/Parser.h"
#include "AST/AST.h"
#include "Core/Log.h"

#include "API/LLVM/LLVMBackend.h"

#include <iostream>
#include <filesystem>

using namespace clear;

int main()
{
    std::filesystem::path current = __FILE__;
    std::filesystem::current_path(current.parent_path());

    LLVM::Backend::Init();

    std::cout << "------PARSER TESTS--------" << std::endl;
    Parser parser;
    ProgramInfo info = parser.CreateTokensFromFile("Tests/test.cl");

    for (size_t i = 0; i < info.Tokens.size(); i++)
    {
        std::cout << "Token Type: " << TokenToString(info.Tokens[i].TokenType);
        std::cout << ", Data: " << info.Tokens[i].Data;
        std::cout << std::endl;
    }

    std::cout << "------AST TESTS--------" << std::endl;
    {
        AST ast(info);
        ast.BuildIR("Tests/test.ir");
    }


    LLVM::Backend::BuildModule();

    LLVM::Backend::Shutdown();

    return 0;
}
