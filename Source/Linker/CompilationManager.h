#pragma once 

#include "Parsing/Parser.h"
#include "BuildConfig.h"

#include <unordered_map>

namespace clear 
{
    class CompilationManager
    {
    public:
        CompilationManager(const BuildConfig& config);
        ~CompilationManager() = default;

        void LoadSources();
        void LoadSourceFile(const std::filesystem::path& path);
        void PropagateSymbolTables();
        void GenerateIRAndObjectFiles();
        void Link();

    private:
        void LoadDirectory(const std::filesystem::path& path);
        void BuildModule(llvm::Module* module, const std::filesystem::path& path);

    private:
        LookupAstTable m_LookupTable;
        BuildConfig m_Config;
        std::shared_ptr<llvm::LLVMContext> m_Context;
        std::shared_ptr<llvm::IRBuilder<>> m_Builder;
        std::unique_ptr<llvm::Module> m_MainModule;

    };
}