//
// Created by Kareem Fares on 5/27/25.
//

#include "Linker.h"
#include "AST/ASTNode.h"
#include "Parsing/Parser.h"
#include "Lexing/Lexer.h"
#include <iostream>
#include <Core/Log.h>
#include <Core/TypeRegistry.h>


#include "API/LLVM/LLVMBackend.h"



namespace clear {
    std::filesystem::path TempDir = std::filesystem::temp_directory_path();

    Module::Module(const std::filesystem::path& path) {
        Path = path;
    }

    void Module::CollectImportPaths(const std::shared_ptr<ASTNodeBase> &node, std::vector<std::string> &importPaths) {
        if (node->GetType() == ASTNodeType::Import) {
            auto importNode = std::dynamic_pointer_cast<ASTImport>(node);
            if (importNode) {
                importPaths.push_back(importNode->GetFilePath());
            }
        }

        // Recurse into children
        for (const auto& child : node->GetChildren()) {
            CollectImportPaths(child, importPaths);
        }

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


        std::filesystem::path parentDir =   Path.parent_path() / "build/";
        std::filesystem::path path      =   parentDir.string() + Path.filename().string() +".ir";
        std::filesystem::path objFile   =   parentDir.string() + Path.filename().string() +".o";

        std::error_code EC;
        llvm::raw_fd_stream stream(path.string(), EC);

        Parser parser(info);

        auto parserResult = parser.GetResult();

        CollectImportPaths(parserResult, importPaths);

        parserResult->PropagateSymbolTableToChildren();
        parserResult->Codegen();
        // Print or use the collected file paths
        module.print(stream, nullptr);


        LLVM::Backend::BuildModule(objFile);

        LLVM::Backend::Shutdown();

    }




    void Linker::GenerateLibraries(Libraries& lib,const std::filesystem::path& file){
        Module module(file);
        module.Build();
        lib.LibImports.push_back(file.string());

        for (std::filesystem::path  imp: module.importPaths) {
            auto ext = imp.extension().string();
            auto x = file.parent_path() / imp;

            CLEAR_VERIFY(std::filesystem::exists(x),"Import path does not exist " + imp.string());
            if (ext == ".lib") {
                lib.LibImports.push_back(x.string());
            }else if(ext == ".cl") {
                GenerateLibraries(lib,x);

            }else {

                CLEAR_VERIFY(false,"bad import filetype");
            }
        }
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

        for (auto& lib : libraries.ClearImports) {
            std::cout << lib << '\n';
        }
        for (auto& lib : libraries.LibImports) {
            std::cout << lib << '\n';
        }

        return 0;
    }




}