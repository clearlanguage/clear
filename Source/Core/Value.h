#pragma once 

#include "Type.h"
#include "Ref.h"

#include <list>

namespace clear {

	struct VariableMetaData
	{
		llvm::AllocaInst* Alloca = nullptr;
		Ref<Type> Type;
		std::string Name;
	};

	class Value 
	{
	public:
		using ConstantPair = std::pair<llvm::Value*, llvm::Type*>;

	public:
		Value() = default;
		Value(const Token& rValue);
		Value(const Ref<Type>& type, const std::string& data);
		Value(const Ref<Type>& type, const std::string& data, const std::list<Value>& chain, llvm::Value* value);

		~Value();

		static llvm::Value*		 CastValue(llvm::Value* value, const Ref<Type>& to, const Ref<Type>& from);
		static llvm::Value*		 CastValue(llvm::Value* value, llvm::Type* to, llvm::Type* from, const Ref<Type>& fromType);
		static llvm::Value*		 CastValue(llvm::Value* value, llvm::Type* to, llvm::Type* from, bool isSigned);
		static ConstantPair		 GetConstantString(const std::string& data);
		static ConstantPair		 GetConstant(const Ref<Type>& type, const std::string& data);
		static VariableMetaData& GetVariableMetaData(const std::string& name);

		static Ref<Value> Cast(const Ref<Value>& casting, Ref<Type> to);

		static void RegisterVariable(llvm::AllocaInst* alloc, const std::string& name, const Ref<Type>& type);
		static void RemoveVariable(const std::string& name);

		inline const llvm::Value*	   Get()	   const { return m_Value; }
		inline const std::list<Value>& GetChain()  const { return m_Chain; }
		inline const std::string&      GetData()   const { return m_Data; }

		inline llvm::Value* Get() { return m_Value; }
		inline Ref<Type>& GetType() { return m_Type; }

	private:
		Ref<Type> m_Type;
		std::string  m_Data;
		std::list<Value> m_Chain; 
		llvm::Value* m_Value = nullptr;
	};

}