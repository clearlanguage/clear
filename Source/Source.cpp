#include "Lexing/Lexer.h"
#include "AST/ASTNode.h"
#include "Parsing/Parser.h"
#include "Core/TypeRegistry.h"
#include "Linker/CompilationManager.h"
#include "Core/Log.h"

#include <toml++/toml.h>

#include <iostream>
#include <filesystem>

using namespace clear;

int main(int argc, char* argv[])
{   
    if(argc == 0) 
    {
        return 0;
    }


    std::cout << std::filesystem::current_path() << std::endl;
    std::filesystem::current_path(std::filesystem::path(__FILE__).parent_path());
    std::cout << std::filesystem::current_path() << std::endl;

    BuildConfig config = BuildConfig::BuildConfigFromToml("build.toml");

    CompilationManager manager(config);
    manager.LoadSources();
    manager.PropagateSymbolTables();
    manager.GenerateIRAndObjectFiles();
    manager.Emit();


    return 0;
}
