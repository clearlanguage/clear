#include "Lexing/Lexer.h"
#include "AST/ASTNode.h"

#include "Parsing/Parser.h"
#include "API/LLVM/LLVMBackend.h"
#include "Core/TypeRegistry.h"


#include <iostream>
#include <filesystem>

using namespace clear;

int main()
{
    std::filesystem::path current = __FILE__;
    std::filesystem::current_path(current.parent_path());

    LLVM::Backend::Init();
    TypeRegistry::InitGlobal();

    std::cout << "------PARSER TESTS--------" << std::endl;
    Lexer parser;
    ProgramInfo info = parser.CreateTokensFromFile("Tests/new_test.cl");

    for (size_t i = 0; i < info.Tokens.size(); i++)
    {
        std::cout << "Token Type: " << TokenToString(info.Tokens[i].TokenType);
        std::cout << ", Data: " << info.Tokens[i].Data;
        std::cout << std::endl;
    }

    std::cout << "------AST TESTS--------" << std::endl;

    {
        auto& module  = *LLVM::Backend::GetModule();

        std::filesystem::path path = "Tests/test.ir";

		std::error_code EC;
		llvm::raw_fd_stream stream(path.string(), EC);

        Parser parser(info);

        auto parserResult = parser.GetResult();
        parserResult->PropagateSymbolTableToChildren();
        parserResult->Codegen();

        module.print(stream, nullptr);
    }

    LLVM::Backend::BuildModule(std::filesystem::current_path() / "Tests" / "output.o");
    LLVM::Backend::Shutdown();

    return 0;
}
