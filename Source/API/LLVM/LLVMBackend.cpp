#include "LLVMBackend.h"

#include "Core/Log.h"

#include <filesystem>

namespace clear {
	namespace LLVM {

		void Backend::Init()
		{
			s_Context = std::make_shared<llvm::LLVMContext>();
			s_Module  = std::make_shared<llvm::Module>("clear", *s_Context);
			s_Builder = std::make_shared<llvm::IRBuilder<>>(*s_Context);
		}

		void Backend::Shutdown()
		{
			s_Builder.reset();
			s_Module.reset();
			s_Context.reset();
		}

		void Backend::BuildModule(const std::filesystem::path& path) //TODO: be able to change output filepath
		{
			std::string errorStr;
			llvm::raw_string_ostream errorStream(errorStr);

			bool isValid = llvm::verifyModule(*s_Module, &errorStream);

			if (isValid) 
				llvm::errs() << "Module verification failed:\n" << errorStream.str() << "\n";
			else 
				llvm::outs() << "Module verified successfully!\n";
			

			llvm::InitializeNativeTarget();
			llvm::InitializeNativeTargetAsmPrinter();
			llvm::InitializeNativeTargetAsmParser();

			auto TargetTriple = llvm::sys::getDefaultTargetTriple();

			std::string error;
			const llvm::Target* target = llvm::TargetRegistry::lookupTarget(TargetTriple, error);
			CLEAR_VERIFY(target, "failed to find target");

			auto cpu = "generic";
			auto features = "";

			llvm::TargetOptions opt;
			auto targetMachine = target->createTargetMachine(TargetTriple, cpu, features, opt, llvm::Reloc::PIC_);

			s_Module->setDataLayout(targetMachine->createDataLayout());
			s_Module->setTargetTriple(TargetTriple);

			std::error_code EC;
			llvm::raw_fd_ostream dest(path.string(), EC, llvm::sys::fs::OF_None);

			CLEAR_VERIFY(!EC, "could not open file");

			llvm::legacy::PassManager pass;
			auto fileType = llvm::CodeGenFileType::ObjectFile;

			CLEAR_VERIFY(!(targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType)), "TargetMachine can't emit a file of this type");


			try
			{
				pass.run(*s_Module);
				dest.flush();
			}
			catch (const std::exception& e)
			{
				CLEAR_UNREACHABLE(e.what());
			}
		}

	}
}