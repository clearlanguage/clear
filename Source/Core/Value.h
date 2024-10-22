#pragma once 

#include "Types.h"
#include "Ref.h"

#include <list>

namespace clear {

	//deals with constants, variables declerations and references
	class Value 
	{
	public:
		Value() = default;
		Value(const std::string& rValue);
		Value(const AbstractType& type, const std::string& data);
		Value(const AbstractType& type, const std::string& data, const std::list<Value>& chain, llvm::Value* value);

		~Value();

		static llvm::Value*		 CastValue(llvm::Value* value, AbstractType to);
		static llvm::Value*		 GetConstantString(const std::string& data);
		static llvm::Value*		 GetConstant(const AbstractType& type, const std::string& data);
		static llvm::AllocaInst* GetVariable(const std::string& name);

		static Ref<Value> Cast(const Ref<Value>& casting, AbstractType to);

		static void RegisterVariable(llvm::AllocaInst* alloc, const std::string& name);
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