#pragma once

#include "API/LLVM/LLVMBackend.h"

#include <vector>
#include <string>
#include <filesystem>
#include <AST/ASTNode.h>

namespace clear 
{
    struct Libraries 
    {
        std::vector<std::string> ClearImports;
        std::vector<std::string> LibImports;
    };

    struct BuildConfig 
    {
        std::filesystem::path EntryPoint;
    };

    class Module 
    {
    public:
        Module(const std::filesystem::path& path);
        ~Module() = default;

        void Build(llvm::Linker& linker);

        auto& GetImportPaths() { return m_ImportPaths; }
        auto& GetPath() { return m_Path; }

    private:
        void CollectImportPaths(const std::shared_ptr<ASTNodeBase>& node,std::vector<std::string>& importPaths);

    private:
        std::filesystem::path m_Path;
        std::vector<std::string> m_ImportPaths;
    };

    class Linker 
    {
    public:
        Linker() = default;
        ~Linker() = default;
        
        int Build(BuildConfig config);

    private: 
        void GenerateLibraries(Libraries& lib, const std::filesystem::path& file, llvm::Linker& linker);

    private:
        std::shared_ptr<llvm::LLVMContext> m_Context;
        std::shared_ptr<llvm::IRBuilder<>> m_Builder;
        std::shared_ptr<llvm::Module> m_MainModule;


    };

}


