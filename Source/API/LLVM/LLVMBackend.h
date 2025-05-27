#pragma once 


#include "LLVMInclude.h"

#include <memory>
#include <filesystem>

namespace clear {
	namespace LLVM {

		//TODO: need to make this support multiple modules
		class Backend
		{
		public:
			static void BuildModule(const std::filesystem::path& path);

			static void SetCurrentModule(llvm::Module* newModule);
			static void SetContext(std::shared_ptr<llvm::LLVMContext> newContext);
			static void SetBuilder(std::shared_ptr<llvm::IRBuilder<>> newBuilder);
			
			static const auto& GetBuilder() { return s_Builder; }
			static const auto& GetModule()  { return s_Module; }
			static const auto& GetContext() { return s_Context; }

		private:
			inline static std::shared_ptr<llvm::LLVMContext> s_Context;
			inline static llvm::Module*     				 s_Module;
			inline static std::shared_ptr<llvm::IRBuilder<>> s_Builder;
		};

	}
}