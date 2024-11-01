#pragma once 

#include "Types.h"
#include "Ref.h"

#include <list>

namespace clear {

	struct VariableMetaData
	{
		llvm::AllocaInst* Alloca = nullptr;
		AbstractType Type;
		std::string Name;
	};

	class Value 
	{
	public:
		using ConstantPair = std::pair<llvm::Value*, llvm::Type*>;

	public:
		Value() = default;
		Value(const std::string& rValue);
		Value(const AbstractType& type, const std::string& data);
		Value(const AbstractType& type, const std::string& data, const std::list<Value>& chain, llvm::Value* value);

		~Value();

		static llvm::Value*		 CastValue(llvm::Value* value, AbstractType to);
		static ConstantPair		 GetConstantString(const std::string& data);
		static ConstantPair		 GetConstantDataArray();
		static ConstantPair		 GetConstant(const AbstractType& type, const std::string& data);
		static VariableMetaData& GetVariableMetaData(const std::string& name);

		static Ref<Value> Cast(const Ref<Value>& casting, AbstractType to);

		static void RegisterVariable(llvm::AllocaInst* alloc, const std::string& name, const AbstractType& type);
		static void RemoveVariable(const std::string& name);

		inline const llvm::Value*	   Get()	   const { return m_Value; }
		inline const AbstractType&	   GetType()   const { return m_Type; }
		inline const std::list<Value>& GetChain()  const { return m_Chain; }
		inline const std::string&      GetData()   const { return m_Data; }

		inline llvm::Value* Get() { return m_Value; }

	private:
		AbstractType m_Type;
		std::string  m_Data;
		std::list<Value> m_Chain; 
		llvm::Value* m_Value = nullptr;
	};

}