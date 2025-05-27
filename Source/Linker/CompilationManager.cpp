#include "CompilationManager.h"

#include "Core/Log.h"

namespace clear 
{
    CompilationManager::CompilationManager(const BuildConfig& config)
        : m_Config(config)
    {
        m_Context = std::make_shared<llvm::LLVMContext>();
		m_Builder = std::make_shared<llvm::IRBuilder<>>(*m_Context);
        m_MainModule = std::make_unique<llvm::Module>(m_Config.ApplicationName, *m_Context);
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

    void CompilationManager::LoadSourceFile(const std::filesystem::path& path)
    {
        if(m_LookupTable.contains(path))
        {
            return;
        }

        if(path.extension() != m_Config.TargetExtension)
        {
            return;
        }

        Lexer lexer;
        ProgramInfo info = lexer.CreateTokensFromFile(path.string());

        auto& lookup = m_LookupTable.emplace(path, AstLookupInfo(m_Context)).first->second;
        lookup.Registry.RegisterBuiltinTypes();

        Parser parser(info, lookup.Registry);
        lookup.Node = parser.GetResult();
    }

    void CompilationManager::PropagateSymbolTables()
    {
        for(auto& [filepath, ast] : m_LookupTable)
        {
            ast.Node->PropagateSymbolTableToChildren();
        }
    }

    void CompilationManager::GenerateIRAndObjectFiles()
    {
        llvm::Linker linker(*m_MainModule);

        for(auto& [filepath, ast] : m_LookupTable)
        {
            auto currentModule = std::make_unique<llvm::Module>(filepath.string(), *m_Context);

            CodegenContext context(m_LookupTable, filepath.parent_path(), *m_Context, *m_Builder, *currentModule, ast.Registry);
            
            ast.Node->Codegen(context);

            if(linker.linkInModule(std::move(currentModule)))
            {
                CLEAR_UNREACHABLE("linking error");
            }
        }

        BuildModule(m_MainModule.get(), m_Config.OutputPath / m_Config.OutputFilename);
    }

    void CompilationManager::Link()
    {
        //TODO: 
    }

    void CompilationManager::LoadDirectory(const std::filesystem::path& path)
    {
        CLEAR_VERIFY(std::filesystem::exists(path), "directory does not exist");
        CLEAR_VERIFY(std::filesystem::is_directory(path), "not a valid directory");

        for(const auto& entry : std::filesystem::directory_iterator(path))
        {
            if(std::filesystem::is_directory(entry))
            {
                LoadDirectory(entry);
                continue;
            }

            LoadSourceFile(entry);
        }
    }

    void CompilationManager::BuildModule(llvm::Module* module, const std::filesystem::path& path)
    {
        std::string errorStr;
		llvm::raw_string_ostream errorStream(errorStr);

		bool isValid = llvm::verifyModule(*module, &errorStream);

		if (isValid) 
			llvm::errs() << "Module verification failed:\n" << errorStream.str() << "\n";
		else 
        	llvm::outs() << "Module verified successfully!\n";
			

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
		llvm::raw_fd_ostream dest(path.string(), EC, llvm::sys::fs::OF_None);

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
}