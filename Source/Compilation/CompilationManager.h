#pragma once 

#include "Parsing/Parser.h"
#include "BuildConfig.h"
#include "Symbols/Module.h"


#include <unordered_map>
#include <unordered_set>
#include <filesystem>
#include "Diagnostics/DiagnosticsBuilder.h"

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
        void Emit();

    private:
        void LoadDirectory(const std::filesystem::path& path);
        void BuildModule(llvm::Module* module, const std::filesystem::path& path);
        void CheckErrors();

        void LinkToExecutableOrDynamic();
        void LinkToStaticLibrary();
        void OptimizeModule();

        void CodegenModule(const std::filesystem::path& path);

    private:
        BuildConfig m_Config;
        std::shared_ptr<Module> m_MainModule;
        std::shared_ptr<Module> m_Builtins;

        std::unordered_set<std::filesystem::path> m_GeneratedModules;
        std::unordered_map<std::filesystem::path, std::shared_ptr<Module>> m_Modules;
        DiagnosticsBuilder m_DiagnosticsBuilder;
    };
}