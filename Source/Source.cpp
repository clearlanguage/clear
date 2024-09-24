// clear.cpp : Defines the entry point for the application.
//


#include "Core/Parser.h"
#include "Core/ASTNode.h"

#include "API/LLVM/LLVMBackend.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"

#include <iostream>
#include <filesystem>

using namespace clear;

int main()
{
    std::filesystem::path current = __FILE__;
    std::filesystem::current_path(current.parent_path());

    LLVM::Backend::Init();


    std::cout << "------AST TESTS--------" << std::endl;
    {
        ASTBinaryExpression expression(BinaryExpressionType::Greater);

        expression.PushChild(std::make_shared<ASTNodeLiteral>(LiteralType::Float64, "10.4"));
        expression.PushChild(std::make_shared<ASTNodeLiteral>(LiteralType::Float64, "15.93"));

        llvm::Value* value = expression.Codegen();
        value->print(llvm::outs());
    }

    LLVM::Backend::GetModule()->print(llvm::outs(), nullptr);

    std::cout << "------PARSER TESTS--------" << std::endl;

    Parser parser;
    ProgramInfo info = parser.CreateTokensFromFile("test.cl");

    for (size_t i = 0; i < info.Tokens.size(); i++)
    {
        std::cout << "Token Type: " << TokenToString(info.Tokens[i].TokenType);
        std::cout << ", Data: " << info.Tokens[i].Data;
        std::cout << std::endl;
    }

    LLVM::Backend::Shutdown();
    return 0;
}
