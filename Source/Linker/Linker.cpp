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



namespace clear 
{
    std::filesystem::path TempDir = std::filesystem::temp_directory_path();

    Module::Module(const std::filesystem::path& path) 
        : m_Path(path)
    {
    }

    void Module::CollectImportPaths(const std::shared_ptr<ASTNodeBase>& node, std::vector<std::string>& importPaths) 
    {
        if (node->GetType() == ASTNodeType::Import) 
        {
            auto importNode = std::dynamic_pointer_cast<ASTImport>(node);

            if (importNode) 
            {
                importPaths.push_back(importNode->GetFilePath());
            }
        }

        for (const auto& child : node->GetChildren()) 
        {
            CollectImportPaths(child, importPaths);
        }

    }

    void Module::Build(llvm::Linker& linker) 
    {
        std::unique_ptr<llvm::Module> currentModule = std::make_unique<llvm::Module>("clear_application", *LLVM::Backend::GetContext());
        
        LLVM::Backend::SetCurrentModule(currentModule.get());
        TypeRegistry::InitGlobal();

        std::cout << "------PARSER TESTS--------" << std::endl;

        Lexer lexer;
        ProgramInfo info = lexer.CreateTokensFromFile(m_Path.string());

        for (size_t i = 0; i < info.Tokens.size(); i++)
        {
            std::cout << "Token Type: " << TokenToString(info.Tokens[i].TokenType);
            std::cout << ", Data: " << info.Tokens[i].Data;
            std::cout << std::endl;
        }

        std::cout << "------AST TESTS--------" << std::endl;


        std::filesystem::path parentDir =  m_Path.parent_path() / "build/";
        std::filesystem::path path      =  parentDir.string() + m_Path.filename().string() +".ir";
        std::filesystem::path objFile   =  parentDir.string() + m_Path.filename().string() +".o";

        std::error_code EC;
        llvm::raw_fd_stream stream(path.string(), EC);

        Parser parser(info);

        auto parserResult = parser.GetResult();

        CollectImportPaths(parserResult, m_ImportPaths);

        parserResult->PropagateSymbolTableToChildren();
        parserResult->Codegen();

        // Print or use the collected file paths
        currentModule->print(stream, nullptr);

        LLVM::Backend::BuildModule(objFile);

        linker.linkInModule(std::move(currentModule));

        LLVM::Backend::SetCurrentModule(nullptr);
    }


    void Linker::GenerateLibraries(Libraries& lib, const std::filesystem::path& file, llvm::Linker& linker)
    {
        Module module(file);
        module.Build(linker);

        lib.LibImports.push_back(file.string());

        for (std::filesystem::path importPath : module.GetImportPaths()) 
        {
            auto ext = importPath.extension().string();
            auto newImport = file.parent_path() / importPath;

            CLEAR_VERIFY(std::filesystem::exists(newImport), "Import path does not exist " + importPath.string());

            if (ext == ".lib") 
            {
                lib.LibImports.push_back(newImport.string());
            }
            else if(ext == ".cl") 
            {
                GenerateLibraries(lib, newImport, linker);
            }
            else 
            {
                CLEAR_VERIFY(false, "bad import filetype");
            }
        }
    }

    int Linker::Build(BuildConfig config) 
    {
        m_Context = std::make_shared<llvm::LLVMContext>();
		m_Builder = std::make_shared<llvm::IRBuilder<>>(*m_Context);
        m_MainModule = std::make_shared<llvm::Module>("clear_application", *m_Context);

        llvm::Linker linker(*m_MainModule);

        LLVM::Backend::SetContext(m_Context);
        LLVM::Backend::SetBuilder(m_Builder);

        Libraries libraries;
        std::filesystem::path path = config.EntryPoint.parent_path().string() + "/build" ;

        if (!std::filesystem::exists(path)) 
        {
            if (std::filesystem::create_directories(path)) {} 
            else 
            {
                std::cerr << "Failed to create directory: " << path << '\n';
            }
        }

        GenerateLibraries(libraries,config.EntryPoint, linker);

        for (auto& lib : libraries.ClearImports) 
        {
            std::cout << lib << '\n';
        }

        for (auto& lib : libraries.LibImports) 
        {
            std::cout << lib << '\n';
        }

        return 0;
    }




}