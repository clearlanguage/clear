#include "Lexing/Lexer.h"
#include "AST/ASTNode.h"
#include "Parsing/Parser.h"
#include "Core/TypeRegistry.h"
#include "Linker/CompilationManager.h"


#include <iostream>
#include <filesystem>

using namespace clear;

int main()
{
    std::filesystem::current_path(std::filesystem::path(__FILE__).parent_path());
    std::cout << std::filesystem::current_path();

    BuildConfig config;
    config.SourceDirectories.push_back(std::filesystem::current_path() / "Tests");
    config.OutputPath = std::filesystem::current_path() / "Tests" / "build";
    config.OutputFilename = "test_application";
    config.ApplicationName = "clear app";
    config.OutputFormat = BuildConfig::OutputFormatType::IR;
    config.OptimizationLevel = BuildConfig::OptimizationLevelType::Development;

    std::filesystem::path lib = std::filesystem::current_path() / "Tests" / "build" / "libs" / "libplayground.a";
    config.LibraryFilePaths.push_back(lib);


    CompilationManager manager(config);
    manager.LoadSources();
    manager.PropagateSymbolTables();
    manager.GenerateIRAndObjectFiles();
    manager.Emit();


    return 0;
}
