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
    std::filesystem::current_path(std::filesystem::path(__FILE__).parent_path());
    std::cout << std::filesystem::current_path();

    BuildConfig config = {"Tests/new_test.cl"};
    linker.Build(config);

    std::vector<int> a;
    std::vector<int> b;
    a.insert(a.begin(), b.begin(), b.end());

    return 0;
}
