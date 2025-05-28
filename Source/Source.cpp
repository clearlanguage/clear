#include "Lexing/Lexer.h"
#include "AST/ASTNode.h"
#include "Parsing/Parser.h"
#include "Core/TypeRegistry.h"
#include "Linker/CompilationManager.h"
#include "Core/Log.h"
#include <chrono>

#include <toml++/toml.h>

#include <iostream>
#include <filesystem>

using namespace clear;

int main(int argc, char* argv[])
{   
    if(argc == 1)
    {
        std::cout<< "Clear language 0.5 dev branch\n";
        return 0;
    }else if(std::string(argv[1]) == "build") {
        auto start = std::chrono::high_resolution_clock::now();
        std::filesystem::current_path(std::filesystem::path(__FILE__).parent_path() / "Tests");
        std::cout << std::filesystem::current_path() << std::endl;

        BuildConfig config = BuildConfig::BuildConfigFromToml("build.toml");

        std::cout << "Compiling " <<config.ApplicationName << std::endl;
        CompilationManager manager(config);
        manager.LoadSources();
        manager.PropagateSymbolTables();
        manager.GenerateIRAndObjectFiles();
        manager.Emit();
        auto end = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double, std::milli> duration = end - start;
        std::cout << "Finished compiling " <<        config.ApplicationName << std::endl;
        std::cout << "Compilation took " << duration.count()/1000 << " s\n";



    }

    return 0;
}
