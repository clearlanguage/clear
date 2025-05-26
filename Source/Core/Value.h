#pragma once 

#include "Type.h"
#include "Lexing/Tokens.h"

#include <list>
#include <memory>

namespace clear {

	class Value 
	{
	public:
		using ConstantPair = std::pair<llvm::Value*, llvm::Type*>;

	public:
		Value() = default;
		Value(const Token& rValue);

		~Value() = default;

		static ConstantPair GetConstantString(const std::string& data);
		static ConstantPair GetConstant(const std::shared_ptr<Type>& type, const std::string& data);

		llvm::Value* Get() { return m_Value; }
		std::shared_ptr<Type>& GetType() { return m_Type; }

	private:
		std::shared_ptr<Type> m_Type;
		std::string  m_Data;
		llvm::Value* m_Value = nullptr;
	};

}