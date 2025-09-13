#include "CompilationManager.h"

#include "AST/ASTNode.h"
#include "Core/Log.h"
#include "Sema/Sema.h"
#include <llvm/IR/LLVMContext.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>

namespace clear 
{
    CompilationManager::CompilationManager(const BuildConfig& config)
        : m_Config(config)
    {
        std::shared_ptr<llvm::LLVMContext> context = std::make_shared<llvm::LLVMContext>();
        m_Builtins = std::make_shared<Module>("__clrt_internal", context, nullptr, "__cltr_internal");
        m_MainModule = std::make_shared<Module>("main_module", context, m_Builtins, "main");
    }
	
	void CompilationManager::RunPipeline()
	{
		LoadSources();
		CollectTopLevelSymbols();
		CompileModules();
		LinkModules();
		GenerateIRAndObjectFiles();
		Emit();
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

    void CompilationManager::CheckErrors() 
    {
		if (m_DiagnosticsBuilder.IsFatal()) 
		{ 
			m_DiagnosticsBuilder.Dump();
        }
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
		}
	}

	void CompilationManager::CompileModule(CompilationUnit& unit)
	{
		//TODO may need mutex if we introduce parallel compilation
		if (unit.Compiled) return;
		
		std::shared_ptr<ASTBlock> topLevel = std::dynamic_pointer_cast<ASTBlock>(std::dynamic_pointer_cast<ASTBlock>(unit.Ast)->Children[0]);
		
		for (const auto& node : topLevel->Children)
		{
			std::shared_ptr<ASTImport> importNode = std::dynamic_pointer_cast<ASTImport>(node);
			if (!importNode) continue;

			std::filesystem::path parent = unit.CompilationModule->GetPath().parent_path();
			std::filesystem::path absolute = std::filesystem::absolute(parent / importNode->Filepath);
			CompileModule(m_CompilationUnits.at(absolute));
		}

		Sema analyzer(unit.CompilationModule, m_DiagnosticsBuilder, m_CompilationUnits);
		analyzer.Visit(unit.Ast);

		if (m_DiagnosticsBuilder.IsFatal())
		{
			m_DiagnosticsBuilder.Dump();
			return;
		}
	
		CodegenContext ctx = unit.CompilationModule->GetCodegenContext();
		unit.Ast->Codegen(ctx);
		unit.Compiled = true;
	}

	void CompilationManager::LinkModules()
	{
		llvm::Linker linker(*m_MainModule->GetModule());
		linker.linkInModule(m_Builtins->TakeModule());

		for (const auto& [path, unit] : m_CompilationUnits)
		{
			if (!unit.CompilationModule->GetModule()) continue;
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

        if(path.extension() != m_Config.TargetExtension)
        {
            return;
        }
        
        std::println("Loading source file {}" , path.string());
		
		std::shared_ptr<Module> newModule = std::make_shared<Module>(path.filename(), m_MainModule->GetContext(), m_Builtins, path);
		
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
            std::println("failed to build module");
            m_MainModule->GetModule()->print(llvm::errs(), nullptr);

            return;
        }

        if(m_Config.EmitIntermiediateIR)
        {
            std::filesystem::path irPath = m_Config.OutputPath / m_Config.OutputFilename;
            irPath.replace_extension(".ll");
            std::error_code EC;
            llvm::raw_fd_ostream file(irPath.string(), EC, llvm::sys::fs::OF_None);
            
            m_MainModule->GetModule()->print(file, nullptr, true, true);
        }   

        BuildModule(m_MainModule->GetModule(), m_Config.OutputPath / m_Config.OutputFilename);
        OptimizeModule();
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

    void CompilationManager::BuildModule(llvm::Module* module, const std::filesystem::path& path)
    {
        std::string errorStr;
		llvm::raw_string_ostream errorStream(errorStr);

		llvm::InitializeNativeTarget();
		llvm::InitializeNativeTargetAsmPrinter();
		llvm::InitializeNativeTargetAsmParser();

		auto targetTriple = llvm::sys::getDefaultTargetTriple();

		std::string error;
		const llvm::Target* target = llvm::TargetRegistry::lookupTarget(targetTriple, error);
		CLEAR_VERIFY(target, "failed to find target");

		auto cpu = "generic";
		auto features = "";

		llvm::TargetOptions opt;
		auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, llvm::Reloc::PIC_);

		module->setDataLayout(targetMachine->createDataLayout());
		module->setTargetTriple(targetTriple);

		std::error_code EC;
		llvm::raw_fd_ostream dest(path.string() + ".o", EC, llvm::sys::fs::OF_None);

		CLEAR_VERIFY(!EC, "could not open file");

		llvm::legacy::PassManager pass;
		auto fileType = llvm::CodeGenFileType::ObjectFile;

		CLEAR_VERIFY(!(targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType)), "TargetMachine can't emit a file of this type");

		try
		{
			pass.run(*module);
			dest.flush();
		}
		catch (const std::exception& e)
		{
			CLEAR_UNREACHABLE(e.what());
		}
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
        }

        std::filesystem::remove(objectPath);
    }

    void CompilationManager::LinkToStaticLibrary()
    {
        CLEAR_UNREACHABLE("unimplemented");
    }

    void CompilationManager::OptimizeModule()
    {
        if(m_Config.OptimizationLevel == BuildConfig::OptimizationLevelType::None) 
           return;

        if(m_Config.OptimizationLevel == BuildConfig::OptimizationLevelType::Debugging) 
            return;

        llvm::PassBuilder passBuilder;

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
                return;
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
