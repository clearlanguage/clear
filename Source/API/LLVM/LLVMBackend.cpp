#include "LLVMBackend.h"

namespace alkhat {
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

	}
}