#pragma once 

#include "Parsing/Parser.h"
#include "BuildConfig.h"
#include "Symbols/Module.h"
#include "Diagnostics/DiagnosticsBuilder.h"

#include <unordered_map>
#include <unordered_set>
#include <filesystem>

namespace clear 
{
	struct CompilationUnit	
	{
		std::shared_ptr<Module>	CompilationModule;
		std::shared_ptr<ASTNodeBase> Ast;
		bool Compiled = false;
		bool InProgress = false;
	};
	
    class CompilationManager
    {
    public:
        CompilationManager(const BuildConfig& config);
        ~CompilationManager() = default;
		
		// returns true when an output was produced without errors
		bool RunPipeline();

        void LoadSources();
        void LoadSourceFile(const std::filesystem::path& path);
        void GenerateIRAndObjectFiles();
        void Emit();

    private:
        void LoadDirectory(const std::filesystem::path& path);
        void BuildModule(llvm::Module* module, const std::filesystem::path& path);
        bool CheckErrors();
		void CollectTopLevelSymbols();
		void CollectBaseClassNames();
		void CompileModules();
		void CompileModule(CompilationUnit& unit);
		void LinkModules();

        void LinkToExecutableOrDynamic();
        void LinkToStaticLibrary();
        void OptimizeModule();
        bool CreateTargetMachine();
        void PrepareForOptimization(llvm::Module& module);

        void CodegenModule(const std::filesystem::path& path);

		void LoadImports(std::shared_ptr<Module> module);
		std::optional<std::filesystem::path> ResolveImport(const std::filesystem::path& importingFile, std::filesystem::path name, std::filesystem::path* shadowedStandard = nullptr);
		std::vector<std::filesystem::path> StandardCandidates(const std::filesystem::path& name);
		void AddPreludeImports(std::shared_ptr<Module> module, const std::filesystem::path& path, const std::vector<Token>& tokens);
		

    private:
        BuildConfig m_Config;
        std::shared_ptr<Module> m_MainModule;
        std::shared_ptr<Module> m_Builtins;

        std::unordered_set<std::filesystem::path> m_GeneratedModules;
        std::unordered_map<std::filesystem::path, std::shared_ptr<Module>> m_Modules;
        DiagnosticsBuilder m_DiagnosticsBuilder;
		
		std::unordered_map<std::filesystem::path, CompilationUnit> m_CompilationUnits;
		bool m_Failed = false;
		std::unique_ptr<llvm::TargetMachine> m_TargetMachine;
    };
}
