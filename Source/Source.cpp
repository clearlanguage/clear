#include "Lexing/Lexer.h"
#include "AST/ASTNode.h"
#include "Parsing/Parser.h"
#include "Core/TypeRegistry.h"
#include "Linker/CompilationManager.h"
#include "Core/Log.h"
#include <chrono>

#include <toml++/toml.h>
#include "Linker/LibraryLinker.h"
#include <iostream>
#include <filesystem>
#include <cstdlib>

using namespace clear;

int main(int argc, char* argv[])
{
    /* if(argc == 1)
    {
        std::cout<< "Clear language 0.5 dev branch\n";
        return 0;
    } */
    //else if(std::string(argv[1]) == "build")
    if (false) {
        std::filesystem::path current = __FILE__;
        std::filesystem::current_path(current.parent_path());


        std::cout << "------PARSER TESTS--------" << std::endl;
        Lexer parser;
        ProgramInfo info = parser.CreateTokensFromFile("Tests/test.cl");
        for (size_t i = 0; i < info.Tokens.size(); i++)
        {
            std::cout << "Token Type: " << TokenToString(info.Tokens[i].TokenType);
            std::cout << ", Data: " << info.Tokens[i].Data;
            std::cout << std::endl;
        }
    }
    if (true){
        auto start = std::chrono::high_resolution_clock::now();
        std::filesystem::current_path(std::filesystem::path(__FILE__).parent_path());
        std::cout << std::filesystem::current_path() << std::endl;
        
        CLEAR_LOG_INFO(LLVM_VERSION_STRING);

        BuildConfig config = BuildConfig::BuildConfigFromToml("Tests/build.toml");
        std::cout << "Compiling " <<config.ApplicationName << std::endl;

        
        config.StandardLibrary = std::filesystem::current_path().parent_path() / "Standard";
        CLEAR_LOG_INFO(config.StandardLibrary);
        
        CompilationManager manager(config); // comp
        manager.LoadSources();
        manager.PropagateSymbolTables();
        manager.GenerateIRAndObjectFiles();
        manager.Emit();

        auto end = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double, std::milli> duration = end - start;
        std::cout << "Finished compiling " <<        config.ApplicationName << std::endl;
        std::cout << "Compilation took " << duration.count()/1000 << " s\n";

        config.Serialize("Tests/build.toml");
    }



    return 0;
}
