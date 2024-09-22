#pragma once 

#include <memory>

#include "LLVMInclude.h"

namespace alkhat {
	namespace LLVM {

		class Backend
		{
		public:
			static void Init();
			static void Shutdown();
			
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