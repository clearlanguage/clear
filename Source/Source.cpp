#include "Lexing/Lexer.h"
#include "AST/ASTNode.h"
#include <Linker/Linker.h>
#include "Parsing/Parser.h"
#include "API/LLVM/LLVMBackend.h"
#include "Core/TypeRegistry.h"


#include <iostream>
#include <filesystem>

using namespace clear;

int main()
{
    Linker linker;
    std::filesystem::current_path("/Users/kareem/Documents/dev/projects/clear");
    BuildConfig config = {"Source/Tests/new_test.cl"};
    linker.Build(config);
    return 0;
}
