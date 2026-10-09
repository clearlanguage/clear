#include "CompilationManager.h"

#include "AST/ASTNode.h"
#include "Core/Log.h"
#include "Sema/Sema.h"
#include "Symbols/SymbolOperations.h"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Verifier.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/SubtargetFeature.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>

namespace clear 
{
	namespace
	{
		// LLVM < 20: bool getHostCPUFeatures(StringMap<bool>&)
		template <typename Map>
		auto FillHostCPUFeatures(Map& out, int) -> decltype(llvm::sys::getHostCPUFeatures(out), void())
		{
			llvm::sys::getHostCPUFeatures(out);
		}

		// LLVM >= 20: StringMap<bool> getHostCPUFeatures()
		template <typename Map, typename... Args>
		auto FillHostCPUFeatures(Map& out, long, Args&... args) -> decltype(out = llvm::sys::getHostCPUFeatures(args...), void())
		{
			out = llvm::sys::getHostCPUFeatures(args...);
		}
	}

    CompilationManager::CompilationManager(const BuildConfig& config)
        : m_Config(config)
    {
        std::shared_ptr<llvm::LLVMContext> context = std::make_shared<llvm::LLVMContext>();
        m_Builtins = std::make_shared<Module>("__clrt_internal", context, nullptr, "__cltr_internal");
        m_MainModule = std::make_shared<Module>("main_module", context, m_Builtins, "main");
    }
	
	// every name used as a base class, in any file: those classes get a method table (see Sema::DeclareClassBodyNow)
	void CompilationManager::CollectBaseClassNames()
	{
		auto nameOf = [](std::shared_ptr<ASTNodeBase> node) -> std::string
		{
			if (auto subscript = std::dynamic_pointer_cast<ASTSubscript>(node))   // Base[int]
				node = subscript->Target;

			if (auto member = std::dynamic_pointer_cast<ASTBinaryExpression>(node)) // module.Base
				node = member->RightSide;

			auto variable = std::dynamic_pointer_cast<ASTVariable>(node);
			return variable ? variable->GetName().GetData() : "";
		};

		for (auto& [path, unit] : m_CompilationUnits)
		{
			auto root = std::dynamic_pointer_cast<ASTBlock>(unit.Ast);

			if (!root || root->Children.empty())
				continue;

			auto topLevel = std::dynamic_pointer_cast<ASTBlock>(root->Children[0]);

			for (auto& node : topLevel ? topLevel->Children : root->Children)
			{
				auto classNode = std::dynamic_pointer_cast<ASTClass>(node);

				if (auto generic = std::dynamic_pointer_cast<ASTGenericTemplate>(node))
					classNode = std::dynamic_pointer_cast<ASTClass>(generic->TemplateNode);

				if (!classNode)
					continue;

				for (auto& base : classNode->Bases)
					Sema::BaseClassNames.insert(nameOf(base));
			}
		}
	}

	bool CompilationManager::RunPipeline()
	{
		LoadSources();
		if (!CheckErrors()) return false;

		CollectBaseClassNames();
		CollectTopLevelSymbols();
		CompileModules();
		if (!CheckErrors()) return false;

		LinkModules();
		if (!CheckErrors()) return false;

		GenerateIRAndObjectFiles();
		if (!CheckErrors()) return false;

		Emit();
		return CheckErrors();
	}

    void CompilationManager::LoadSources()
    {
        for(const auto& dir : m_Config.SourceDirectories)
        {
            LoadDirectory(dir);
        }

        for(const auto& filename : m_Config.SourceFiles)
        {
            LoadSourceFile(filename);
        }
    }

    bool CompilationManager::CheckErrors() 
    {
		m_DiagnosticsBuilder.Dump();
		return !m_DiagnosticsBuilder.IsFatal() && !m_Failed;
    }

	void CompilationManager::CollectTopLevelSymbols()
	{
		//TODO
	}

	void CompilationManager::CompileModules()
	{
		for (auto& [path, unit] : m_CompilationUnits)
		{
			CompileModule(unit);

			if (m_DiagnosticsBuilder.IsFatal())
				return;
		}
	}

	void CompilationManager::CompileModule(CompilationUnit& unit)
	{
		//TODO may need mutex if we introduce parallel compilation
		if (unit.Compiled || unit.InProgress) return; // InProgress: an import cycle, the other side is already being compiled

		unit.InProgress = true;
		
		std::shared_ptr<ASTBlock> topLevel = std::dynamic_pointer_cast<ASTBlock>(std::dynamic_pointer_cast<ASTBlock>(unit.Ast)->Children[0]);
		
		// imported files are compiled first so their symbols exist (paths were resolved while loading)
		for (const auto& node : topLevel->Children)
		{
			std::shared_ptr<ASTImport> importNode = std::dynamic_pointer_cast<ASTImport>(node);
			if (!importNode) continue;

			auto it = m_CompilationUnits.find(importNode->Filepath);

			if (it != m_CompilationUnits.end())
				CompileModule(it->second);
		}

		Sema analyzer(unit.CompilationModule, m_DiagnosticsBuilder, m_CompilationUnits);
		analyzer.Visit(unit.Ast);

		if (m_DiagnosticsBuilder.IsFatal())
			return;
	
		CodegenContext ctx = unit.CompilationModule->GetCodegenContext();
		unit.Ast->Codegen(ctx);
		SymbolOps::FinalizeInitGlobals(*unit.CompilationModule->GetModule());
		unit.Compiled = true;
	}

	void CompilationManager::LinkModules()
	{
		llvm::Linker linker(*m_MainModule->GetModule());
		linker.linkInModule(m_Builtins->TakeModule());

		for (const auto& [path, unit] : m_CompilationUnits)
		{
			if (!unit.CompilationModule->GetModule()) continue;
			if (llvm::verifyModule(*unit.CompilationModule->GetModule(), &llvm::errs()))
			{
				std::println(stderr, "internal compiler error: invalid code generated for {}", path.string());
				m_Failed = true;
				continue;
			}

			linker.linkInModule(unit.CompilationModule->TakeModule());
		}
	}


    void CompilationManager::LoadSourceFile(const std::filesystem::path& path)
    {
        if(m_DiagnosticsBuilder.IsFatal())
        {
            return;
        }

        if(m_CompilationUnits.contains(path))
        {
            return;
        }

        // every file is tracked by one canonical path, however it was named
        std::filesystem::path canonical = std::filesystem::weakly_canonical(std::filesystem::absolute(path));

        if (canonical != path)
        {
            LoadSourceFile(canonical);
            return;
        }

        if(path.extension() != m_Config.TargetExtension)
        {
            return;
        }
        
        if (m_Config.Verbose)
            std::println("Loading source file {}" , path.string());
		
		std::shared_ptr<Module> newModule = std::make_shared<Module>(path.filename(), m_MainModule->GetContext(), m_Builtins, path);
		newModule->RuntimeChecks = m_Config.RuntimeChecksEnabled();
		newModule->ReportCopies = m_Config.ReportCopies;
		
        Lexer lexer(path, m_DiagnosticsBuilder);

        if(m_DiagnosticsBuilder.IsFatal())
        {
            m_DiagnosticsBuilder.Dump();
            return;
        }

        Parser parser(lexer.GetTokens(), newModule, m_DiagnosticsBuilder);

        if(m_DiagnosticsBuilder.IsFatal())
        {
            m_DiagnosticsBuilder.Dump();
            return;
        }

		m_CompilationUnits[path] = CompilationUnit { newModule, newModule->GetRoot() };

		LoadImports(newModule);
    }

	std::vector<std::filesystem::path> CompilationManager::StandardCandidates(const std::filesystem::path& name)
	{
		std::vector<std::filesystem::path> candidates;

		if (!m_Config.StandardLibrary.empty())
			candidates.push_back(m_Config.StandardLibrary / name);

		if (const char* fromEnvironment = std::getenv("CLEAR_STANDARD_DIR"))
			candidates.push_back(std::filesystem::path(fromEnvironment) / name);

#ifdef CLEAR_STANDARD_DIR
		candidates.push_back(std::filesystem::path(CLEAR_STANDARD_DIR) / name);
#endif

		return candidates;
	}

	std::optional<std::filesystem::path> CompilationManager::ResolveImport(const std::filesystem::path& importingFile, std::filesystem::path name, std::filesystem::path* shadowedStandard)
	{
		std::filesystem::path written = name;

		if (!name.has_extension())
			name += m_Config.TargetExtension;

		auto existing = [](const std::filesystem::path& candidate) -> std::optional<std::filesystem::path>
		{
			std::error_code ec;
			if (!std::filesystem::is_regular_file(candidate, ec))
				return std::nullopt;

			return std::filesystem::weakly_canonical(std::filesystem::absolute(candidate));
		};

		// import "std/list" always means the standard library, whatever is next to the importing file
		if (written.begin() != written.end() && written.begin()->string() == "std" && std::next(written.begin()) != written.end())
		{
			std::filesystem::path inside;
			for (auto part = std::next(written.begin()); part != written.end(); part++)
				inside /= *part;

			if (!inside.has_extension())
				inside += m_Config.TargetExtension;

			for (const auto& candidate : StandardCandidates(inside))
				if (auto resolved = existing(candidate))
					return resolved;

			return std::nullopt;
		}

		std::vector<std::filesystem::path> candidates = { importingFile.parent_path() / name };

		// a package: "colors" is its lib file, "colors/extra" a file inside it
		for (const auto& package : m_Config.Packages)
		{
			if (written.begin() == written.end() || written.begin()->string() != package.Name)
				continue;

			if (std::next(written.begin()) == written.end())
			{
				candidates.push_back(package.Entry);
			}
			else
			{
				std::filesystem::path inside;
				for (auto part = std::next(written.begin()); part != written.end(); part++)
					inside /= *part;

				if (!inside.has_extension())
					inside += m_Config.TargetExtension;

				candidates.push_back(package.Directory / inside);
			}
		}

		auto standard = StandardCandidates(name);
		candidates.insert(candidates.end(), standard.begin(), standard.end());

		std::filesystem::path self = std::filesystem::weakly_canonical(std::filesystem::absolute(importingFile));

		// a file next to the importer with the name of a standard module hides that module: worth saying so
		if (shadowedStandard)
		{
			auto local = existing(candidates[0]);

			for (const auto& candidate : standard)
			{
				auto resolved = existing(candidate);

				if (local && resolved && *local != *resolved && *local != self)
				{
					*shadowedStandard = *resolved;
					break;
				}
			}
		}

		for (const auto& candidate : candidates)
		{
			std::error_code ec;
			if (!std::filesystem::is_regular_file(candidate, ec))
				continue;

			// a file never imports itself (tests/math.cl importing the standard "math")
			std::filesystem::path resolved = std::filesystem::weakly_canonical(std::filesystem::absolute(candidate));

			if (resolved != self)
				return resolved;
		}

		return std::nullopt;
	}

	void CompilationManager::LoadImports(std::shared_ptr<Module> module)
	{
		auto root = module->GetRoot();

		if (root->Children.empty())
			return;

		auto topLevel = std::dynamic_pointer_cast<ASTBlock>(root->Children[0]);

		if (!topLevel)
			return;

		for (const auto& node : topLevel->Children)
		{
			auto importNode = std::dynamic_pointer_cast<ASTImport>(node);
			if (!importNode) continue;

			std::filesystem::path shadowed;
			auto resolved = ResolveImport(module->GetPath(), importNode->Filepath, &shadowed);

			if (!resolved)
			{
				Token location = importNode->Location;
				location.SetData(importNode->Filepath.string());
				m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::High, location, DiagnosticCode_ImportNotFound);
				continue;
			}

			if (!shadowed.empty())
			{
				Token location = importNode->Location;
				std::string written = importNode->Filepath.string();
				location.SetData(std::format("‘{}’ is {} in this folder, which hides the standard ‘{}’. Write import \"std/{}\" for the standard one, or rename the file",
											 written, resolved->filename().string(), written, written));
				m_DiagnosticsBuilder.Report(Stage::Parsing, Severity::Low, location, DiagnosticCode_ImportShadowsStandard, written.size() + 2);
			}

			// from here on the import refers to the file by its absolute path
			importNode->Filepath = *resolved;
			LoadSourceFile(*resolved);
		}
	}

    void CompilationManager::GenerateIRAndObjectFiles()
    {
	   if(m_DiagnosticsBuilder.IsFatal())
        {
            m_DiagnosticsBuilder.Dump();
            return;
        }

        if(llvm::verifyModule(*m_MainModule->GetModule(), &llvm::errs()))
        {
            std::println(stderr, "internal compiler error: failed to build module");
            m_Failed = true;

            return;
        }

        if (!CreateTargetMachine())
            return;

        // optimize before anything is written out, so both the emitted IR and the object file benefit
        PrepareForOptimization(*m_MainModule->GetModule());
        OptimizeModule();

        if(m_Config.EmitIntermiediateIR)
        {
            std::filesystem::path irPath = m_Config.OutputPath / m_Config.OutputFilename;
            irPath.replace_extension(".ll");
            std::error_code EC;
            llvm::raw_fd_ostream file(irPath.string(), EC, llvm::sys::fs::OF_None);
            
            m_MainModule->GetModule()->print(file, nullptr, true, true);
        }   

        BuildModule(m_MainModule->GetModule(), m_Config.OutputPath / m_Config.OutputFilename);
    }

    void CompilationManager::Emit()
    {
        if(m_DiagnosticsBuilder.IsFatal())
        {
            return;
        }

        if(m_Config.OutputFormat == BuildConfig::OutputFormatType::DynamicLibrary || 
           m_Config.OutputFormat == BuildConfig::OutputFormatType::Executable)
        {
            LinkToExecutableOrDynamic();
        }
        else if(m_Config.OutputFormat == BuildConfig::OutputFormatType::StaticLibrary)
        {
            LinkToStaticLibrary();
        }
        else if(m_Config.OutputFormat == BuildConfig::OutputFormatType::IR)
        {
            std::filesystem::path filepath = m_Config.OutputPath / m_Config.OutputFilename;
            std::filesystem::path objectPath = filepath;
            objectPath.replace_extension(".o");

            std::error_code EC;
            llvm::raw_fd_ostream file(filepath.string(), EC, llvm::sys::fs::OF_None);

            m_MainModule->GetModule()->print(file, nullptr);
            std::filesystem::remove(objectPath);
        }
        else if (m_Config.OutputFormat == BuildConfig::OutputFormatType::ObjectFile)
        {
            // dont delete object file
        }
    }

    void CompilationManager::LoadDirectory(const std::filesystem::path& path)
    {
        CLEAR_VERIFY(std::filesystem::exists(path), "directory does not exist");
        CLEAR_VERIFY(std::filesystem::is_directory(path), "not a valid directory");

        if (m_Config.Verbose)
            std::println("Loading directory {}", path.string());

        if(m_DiagnosticsBuilder.IsFatal())
        {
            return;
        }

        for(const auto& entry : std::filesystem::directory_iterator(path))
        {
            if(std::filesystem::is_directory(entry))
            {
                LoadDirectory(entry);
                continue;
            }

        
            LoadSourceFile(entry);

            if(m_DiagnosticsBuilder.IsFatal())
            {
                return;
            }
        }
    }

    bool CompilationManager::CreateTargetMachine()
    {
		if (m_TargetMachine)
			return true;

		llvm::InitializeNativeTarget();
		llvm::InitializeNativeTargetAsmPrinter();
		llvm::InitializeNativeTargetAsmParser();

		std::string targetTriple = llvm::sys::getDefaultTargetTriple();

		std::string error;
		const llvm::Target* target = llvm::TargetRegistry::lookupTarget(targetTriple, error);

		if (!target)
		{
			std::println(stderr, "error: no code generator for {}: {}", targetTriple, error);
			m_Failed = true;
			return false;
		}

		std::string cpu = m_Config.TargetCPU.empty() ? "generic" : m_Config.TargetCPU;
		std::string features;

		if (cpu == "native")
		{
			cpu = llvm::sys::getHostCPUName().str();

			llvm::SubtargetFeatures featureSet;
			llvm::StringMap<bool> hostFeatures;

			FillHostCPUFeatures(hostFeatures, 0);

			for (const auto& feature : hostFeatures)
				featureSet.AddFeature(feature.first(), feature.second);

			features = featureSet.getString();
		}

		llvm::CodeGenOptLevel codegenLevel = llvm::CodeGenOptLevel::Default;

		switch (m_Config.OptimizationLevel)
		{
			case BuildConfig::OptimizationLevelType::None:
			case BuildConfig::OptimizationLevelType::Debugging:    codegenLevel = llvm::CodeGenOptLevel::None; break;
			case BuildConfig::OptimizationLevelType::Development:  codegenLevel = llvm::CodeGenOptLevel::Less; break;
			case BuildConfig::OptimizationLevelType::Distribution: codegenLevel = llvm::CodeGenOptLevel::Aggressive; break;
		}

		llvm::TargetOptions options;
		m_TargetMachine.reset(target->createTargetMachine(targetTriple, cpu, features, options, llvm::Reloc::PIC_, std::nullopt, codegenLevel));

		return m_TargetMachine != nullptr;
    }

    void CompilationManager::PrepareForOptimization(llvm::Module& module)
    {
		module.setDataLayout(m_TargetMachine->createDataLayout());
		module.setTargetTriple(m_TargetMachine->getTargetTriple().str());

		bool isExecutable = m_Config.OutputFormat == BuildConfig::OutputFormatType::Executable;

		if (isExecutable)
		{
			for (llvm::GlobalVariable& global : module.globals())
			{
				if (!global.isDeclaration() && global.hasExternalLinkage())
					global.setLinkage(llvm::GlobalValue::InternalLinkage);
			}
		}

		for (llvm::Function& function : module)
		{
			if (function.isDeclaration())
				continue;

			// Clear has no exceptions, telling LLVM lets it drop unwind tables and optimize calls more freely
			function.addFnAttr(llvm::Attribute::NoUnwind);

			// an executable is one module, so everything but main is private to it: LLVM can inline it
			// everywhere and delete the original
			if (isExecutable && function.getName() != "main" && function.hasExternalLinkage())
				function.setLinkage(llvm::GlobalValue::InternalLinkage);

			if (m_TargetMachine->getTargetCPU() != "generic")
				function.addFnAttr("target-cpu", m_TargetMachine->getTargetCPU());

			if (!m_TargetMachine->getTargetFeatureString().empty())
				function.addFnAttr("target-features", m_TargetMachine->getTargetFeatureString());
		}
    }

    void CompilationManager::BuildModule(llvm::Module* module, const std::filesystem::path& path)
    {
		std::error_code EC;
		llvm::raw_fd_ostream dest(path.string() + ".o", EC, llvm::sys::fs::OF_None);

		if (EC)
		{
			std::println(stderr, "error: could not write {}.o: {}", path.string(), EC.message());
			m_Failed = true;
			return;
		}

		llvm::legacy::PassManager pass;

		if (m_TargetMachine->addPassesToEmitFile(pass, dest, nullptr, llvm::CodeGenFileType::ObjectFile))
		{
			std::println(stderr, "error: the target cannot emit object files");
			m_Failed = true;
			return;
		}

		pass.run(*module);
		dest.flush();
    }

    void CompilationManager::LinkToExecutableOrDynamic()
    {
        std::filesystem::path filepath = m_Config.OutputPath / m_Config.OutputFilename;
        std::filesystem::path objectPath = filepath;

        objectPath.replace_extension(".o");

        auto clangPath = llvm::sys::findProgramByName("clang");

        if (!clangPath) 
        {
            llvm::errs() << "clang not found on PATH!\n";
            m_Failed = true;
            return;
        }

        std::vector<std::string> args = { "clang" };

        if(m_Config.IncludeCStandard)
        {
            args.push_back("-std=c11");
            args.push_back("-lm");
        }

        if(m_Config.OutputFormat == BuildConfig::OutputFormatType::DynamicLibrary)
        {
            args.push_back("-shared");
        }

        auto WrapPath = [](std::filesystem::path& path)
        {
            return path.string();
        };

        args.push_back(WrapPath(objectPath));

        for(auto& dir : m_Config.LibraryDirectories)
        {
            std::string p = "-L" + WrapPath(dir);
            args.push_back(p);
        }

        for(auto& name : m_Config.LibraryNames)
        {
            std::string p = "-l:" + WrapPath(name);
            args.push_back(p);
        }

        for(auto& libpath : m_Config.LibraryFilePaths)
        {
            args.push_back(WrapPath(libpath));
        }

        args.push_back("-o");
        args.push_back(WrapPath(filepath));

        std::vector<llvm::StringRef> refs(args.size());
        std::copy(args.begin(), args.end(), refs.begin());
    
        int result = llvm::sys::ExecuteAndWait(clangPath.get(), refs);
        
        if (result != 0) 
        {
            llvm::outs() << "Executing clang command: ";
            for (const auto& arg : args)
                llvm::outs() << arg << " ";

            llvm::outs() << "\n";

            llvm::errs() << "Clang linking failed with exit code: " << result << "\n";
            m_Failed = true;
        }

        std::filesystem::remove(objectPath);
    }

    void CompilationManager::LinkToStaticLibrary()
    {
        CLEAR_UNREACHABLE("unimplemented");
    }

    void CompilationManager::OptimizeModule()
    {
        llvm::PipelineTuningOptions tuning;
        tuning.LoopVectorization = true;
        tuning.SLPVectorization = true;

        // the target machine gives the optimizer the CPU's cost model (vector widths, instruction costs)
        llvm::PassBuilder passBuilder(m_TargetMachine.get(), tuning);

        llvm::LoopAnalysisManager loopAM;
        llvm::FunctionAnalysisManager funcAM;
        llvm::CGSCCAnalysisManager cgsccAM;
        llvm::ModuleAnalysisManager moduleAM;

        passBuilder.registerModuleAnalyses(moduleAM);
        passBuilder.registerCGSCCAnalyses(cgsccAM);
        passBuilder.registerFunctionAnalyses(funcAM);
        passBuilder.registerLoopAnalyses(loopAM);

        passBuilder.crossRegisterProxies(loopAM, funcAM, cgsccAM, moduleAM);

        llvm::ModulePassManager modulePM;

        switch(m_Config.OptimizationLevel)
        {
            case BuildConfig::OptimizationLevelType::Development:
                modulePM = passBuilder.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O1); 
                break;

            case BuildConfig::OptimizationLevelType::Distribution:
                if(m_Config.FavourSize)
                    modulePM = passBuilder.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::Os); 
                else 
                    modulePM = passBuilder.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O3); 

                break;

            default:
                // no optimization, but coroutines (generators, async functions) still have to be split into their parts
                modulePM = passBuilder.buildO0DefaultPipeline(llvm::OptimizationLevel::O0);
                break;
        }


        modulePM.run(*m_MainModule->GetModule(), moduleAM);
    }

    void CompilationManager::CodegenModule(const std::filesystem::path& path)
    {
        if(m_GeneratedModules.contains(path)) 
            return;

        if(path.extension() == ".h") 
            return;
        
        if(!std::filesystem::exists(path))
        {
            std::filesystem::path stdLib = m_Config.StandardLibrary / path.filename();
            CLEAR_VERIFY(std::filesystem::exists(stdLib), "file ", path, " doesn't exist");
            
            if(m_GeneratedModules.contains(stdLib)) 
                return;
            
            m_GeneratedModules.insert(stdLib);
            CodegenModule(stdLib);
            
            return;
        }

        CLEAR_VERIFY(std::filesystem::exists(path), "file ", path, " doesn't exist");
        CLEAR_VERIFY(m_Modules.contains(path),  "file ", path, " not loaded");
    }
}
