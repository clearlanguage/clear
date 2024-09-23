// alkhat.cpp : Defines the entry point for the application.
//

#include <iostream>
#include <filesystem>
#include "Core/Parser.h"
#include "API/LLVM/LLVMBackend.h"
#include "Core/ASTNode.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"

using namespace alkhat;

int main()
{
    std::filesystem::path current = __FILE__;
    std::filesystem::current_path(current.parent_path());

    LLVM::Backend::Init();

    {
        ASTBinaryExpression expression(BinaryExpressionType::Greater);

        expression.PushChild(std::make_shared<ASTNodeLiteral>(LiteralType::Float64, "10.4"));
        expression.PushChild(std::make_shared<ASTNodeLiteral>(LiteralType::Float64, "15.93"));

        llvm::Value* value = expression.Codegen();
        value->print(llvm::outs());
    }

    LLVM::Backend::GetModule()->print(llvm::outs(), nullptr);

    LLVM::Backend::Shutdown();
    return 0;
}
