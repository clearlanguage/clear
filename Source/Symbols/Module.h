#pragma once 

#include "AST/ASTNode.h"
#include "Compilation/BuildConfig.h"
#include "Symbols/TypeRegistry.h"

#include "Symbols/Symbol.h"

#include <llvm/ADT/ArrayRef.h>
#include <llvm/IR/Module.h>
#include <memory>
#include <unordered_map>

namespace clear 
{
    class Module : public std::enable_shared_from_this<Module>
    {
    public:
        Module(const std::string& name, std::shared_ptr<llvm::LLVMContext> context, std::shared_ptr<Module> builtins, const std::filesystem::path& path);
        Module(Module* parent, const std::string& name, std::shared_ptr<Module> builtins);
        ~Module() = default;

        std::shared_ptr<Module> EmplaceOrReturn(const std::string& moduleName);
        std::shared_ptr<Module> Return(const std::string& moduleName);
        void InsertModule(const std::string& name, std::shared_ptr<Module> module_);

        void Codegen(const BuildConfig& config);
        void Link();

        llvm::Module* GetModule()  { return m_Module.get(); }
        std::unique_ptr<llvm::Module> TakeModule() { return std::move(m_Module); }
        
        std::shared_ptr<llvm::LLVMContext> GetContext() { return m_Context; }
        std::shared_ptr<ASTBlock> GetRoot() { return m_Root; }
		std::shared_ptr<TypeRegistry> GetTypeRegistry() { return m_TypeRegistry; }
		const auto& GetName() { return m_ModuleName; }
		const auto& GetPath() { return m_ModulePath; }

        CodegenContext GetCodegenContext();
		
		std::optional<std::shared_ptr<Symbol>> Lookup(llvm::StringRef symbol);

        std::shared_ptr<Type> GetTypeFromToken(const Token& token);
		
		void ExposeSymbol(llvm::StringRef symbolName, std::shared_ptr<Symbol> symbol);
		const auto& GetExposedSymbols() const { return m_ExposedSymbols; }

    private:
        std::string m_ModuleName;
		std::filesystem::path m_ModulePath;

        std::unique_ptr<llvm::Module> m_Module;
        std::shared_ptr<llvm::LLVMContext> m_Context;
        std::shared_ptr<llvm::IRBuilder<>> m_Builder;
		
		std::unordered_map<std::string, std::shared_ptr<Symbol>> m_ExposedSymbols;
        std::unordered_map<std::string, std::shared_ptr<Module>> m_ContainedModules;
        std::shared_ptr<ASTBlock> m_Root;

        std::shared_ptr<TypeRegistry> m_TypeRegistry;

        bool m_CodeGenerated = false;
        bool m_IsBuiltin = false;
    };
}
