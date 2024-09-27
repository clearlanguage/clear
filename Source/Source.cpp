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
        ASTFunctionDecleration function("main", VariableType::None, {});
        
        std::shared_ptr<ASTBinaryExpression> expression = std::make_shared<ASTBinaryExpression>(BinaryExpressionType::Greater);

        expression->PushChild(std::make_shared<ASTNodeLiteral>(LiteralType::Float64, "10.4"));
        expression->PushChild(std::make_shared<ASTNodeLiteral>(LiteralType::Float64, "15.93"));

        function.PushChild(expression);
        auto i = std::make_shared<ASTVariableDecleration>("my_var", VariableType::Int32);
        function.PushChild(i);

        llvm::Value* value = function.Codegen();
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
