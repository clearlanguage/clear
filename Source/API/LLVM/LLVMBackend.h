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
			static void Init();
			static void Shutdown();

			static void BuildModule(const std::filesystem::path& path);
			
			static const auto& GetBuilder() { return s_Builder; }
			static const auto& GetModule()  { return s_Module; }
			static const auto& GetContext() { return s_Context; }

		private:
			inline static std::shared_ptr<llvm::LLVMContext> s_Context;
			inline static std::shared_ptr<llvm::Module>      s_Module;
			inline static std::shared_ptr<llvm::IRBuilder<>> s_Builder;
		};

	}
}