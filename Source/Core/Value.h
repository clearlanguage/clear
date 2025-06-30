#pragma once 

#include "Symbols/TypeRegistry.h"
#include "Lexing/Token.h"

#include "Symbols/SymbolTable.h"

#include <list>
#include <memory>

namespace clear {

	class Value 
	{
	public:
		using ConstantPair = std::pair<llvm::Value*, llvm::Type*>;

	public:
		Value() = default;
		Value(const Token& rValue, std::shared_ptr<SymbolTable> tbl, llvm::LLVMContext& context, llvm::Module& module);

		~Value() = default;

		static ConstantPair GetConstantString(const std::string& data, llvm::LLVMContext& context, llvm::Module& module);
		static ConstantPair GetConstant(const std::shared_ptr<Type>& type, const Token& data, llvm::LLVMContext& context, llvm::Module& module);

		llvm::Value* Get() { return m_Value; }
		std::shared_ptr<Type>& GetType() { return m_Type; }

	private:
		std::shared_ptr<Type> m_Type;
		Token m_Token;
		llvm::Value* m_Value = nullptr;
	};

}