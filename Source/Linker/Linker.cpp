//
// Created by Kareem Fares on 5/27/25.
//

#include "Linker.h"
#include "AST/ASTNode.h"
#include "Parsing/Parser.h"
#include "Lexing/Lexer.h"
#include <iostream>
#include <Core/TypeRegistry.h>

#include "API/LLVM/LLVMBackend.h"



namespace clear {
    std::filesystem::path TempDir = std::filesystem::temp_directory_path();

    Module::Module(const std::filesystem::path& path) {
        Path = path;
    }
    void Module::Build() {
        LLVM::Backend::Init();
        TypeRegistry::InitGlobal();
        std::cout << "------PARSER TESTS--------" << std::endl;
        Lexer lexer;
        ProgramInfo info = lexer.CreateTokensFromFile(Path.string());
        for (size_t i = 0; i < info.Tokens.size(); i++)
        {
            std::cout << "Token Type: " << TokenToString(info.Tokens[i].TokenType);
            std::cout << ", Data: " << info.Tokens[i].Data;
            std::cout << std::endl;
        }



        std::cout << "------AST TESTS--------" << std::endl;


        auto& module  = *LLVM::Backend::GetModule();


        std::filesystem::path parent_dir = Path.parent_path() / "build/";
        std::filesystem::path path =   parent_dir.string() + Path.filename().string() +".ir";
        std::filesystem::path obj_path = parent_dir.string() + Path.filename().string()+ ".o";

        std::error_code EC;
        llvm::raw_fd_stream stream(path.string(), EC);

        Parser parser(info);

        auto parserResult = parser.GetResult();
        parserResult->PropagateSymbolTableToChildren();
        parserResult->Codegen();

        module.print(stream, nullptr);


        LLVM::Backend::BuildModule(obj_path);

        LLVM::Backend::Shutdown();

    }




    void Linker::GenerateLibraries(Libraries& lib,const std::filesystem::path& file){
        Module module(file);
        module.Build();

    }

    int Linker::Build(BuildConfig config) {
        Libraries libraries;
        std::filesystem::path path = config.EntryPoint.parent_path().string() + "/build" ;

        if (!std::filesystem::exists(path)) {
            if (std::filesystem::create_directories(path)) {
            } else {
                std::cerr << "Failed to create directory: " << path << '\n';
            }
        }

        GenerateLibraries(libraries,config.EntryPoint);


        return 0;
    }




}