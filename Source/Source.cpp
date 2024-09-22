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

    std::error_code EC;
    llvm::raw_fd_ostream my_stream("file.ll", EC, llvm::sys::fs::OF_None);

    if (EC) {
        std::cout << "Failed to open file: " << EC.message() << std::endl;
        return 0;
    }

    ASTNodeLiteral literal(LiteralType::Int8, "5");

    literal.Codegen()->print(my_stream, true);

    my_stream.flush();

    LLVM::Backend::Shutdown();
    return 0;
}
