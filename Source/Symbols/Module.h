#pragma once 

#include "AST/ASTNode.h"
#include "Compilation/BuildConfig.h"
#include "Symbols/TypeRegistry.h"

#include "Symbols/Symbol.h"
#include "Sema/SymbolTable.h"

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

		// the file's top-level scopes after semantic analysis, used to analyse its generics from other files
		std::vector<SymbolTable> GlobalScopes;

		// its consts with a known integer value, so other files can use them as constants too (array sizes...)
		std::unordered_map<Symbol*, std::pair<int64_t, std::shared_ptr<Type>>> ConstantValues;

    private:
        std::string m_ModuleName;
		std::filesystem::path m_ModulePath;

        // the context must outlive the llvm module, so it is declared (and therefore destroyed) first
        std::shared_ptr<llvm::LLVMContext> m_Context;
        std::unique_ptr<llvm::Module> m_Module;
        std::shared_ptr<llvm::IRBuilder<>> m_Builder;
		
		std::unordered_map<std::string, std::shared_ptr<Symbol>> m_ExposedSymbols;
        std::unordered_map<std::string, std::shared_ptr<Module>> m_ContainedModules;
        std::shared_ptr<ASTBlock> m_Root;

        std::shared_ptr<TypeRegistry> m_TypeRegistry;

        bool m_CodeGenerated = false;

    public:
        bool RuntimeChecks = false; // emit bounds/null/overflow/division checks (from the build configuration)
        bool ReportCopies = false;  // a note at each copy (clearc --copies)
        bool m_IsBuiltin = false;
    };
}
