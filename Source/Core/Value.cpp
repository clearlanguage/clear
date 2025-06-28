#include "Value.h"

#include "API/LLVM/LLVMInclude.h"
#include "Log.h"
#include "TypeRegistry.h"

namespace clear
{
	static size_t s_StringCount = 0;

	Value::ConstantPair Value::GetConstant(const std::shared_ptr<Type>& type, const Token& data, llvm::LLVMContext& context, llvm::Module& module)
	{
		if(data.GetData() == "null") return { llvm::ConstantPointerNull::get(llvm::PointerType::get(context, 0)), nullptr} ;

		std::string hash = type->GetHash();

		if(hash == "int8")  return {llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),  (int8_t)data.AsInt(), true), nullptr};
		if(hash == "int16") return {llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), (int16_t)data.AsInt(), true), nullptr};
		if(hash == "int32") return {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), (int32_t)data.AsInt(), true), nullptr};
		if(hash == "int64") return {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), (int64_t)data.AsInt(), true), nullptr};

		if(hash == "uint8")  return {llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), (uint8_t)data.AsUInt(), true), nullptr};
		if(hash == "uint16") return {llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), (uint16_t)data.AsUInt(), true), nullptr};
		if(hash == "uint32") return {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), (uint32_t)data.AsUInt(), true), nullptr};
		if(hash == "uint64") return {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), (uint64_t)data.AsUInt(), true), nullptr};

		if(hash == "float32") return {llvm::ConstantFP::get(llvm::Type::getFloatTy(context), (float)data.AsFloat()), nullptr};
		if(hash == "float64") return {llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), (double)data.AsFloat()), nullptr};

		if(hash == "bool") return {llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), data.AsBool()), nullptr};
		
		if(hash == "int8*") return GetConstantString(data.GetData(), context, module);
		if(hash == "void*") return { llvm::ConstantPointerNull::get((llvm::PointerType*)type->Get()), nullptr };


		CLEAR_UNREACHABLE("invalid hash for constant ", hash);
	}


	Value::ConstantPair Value::GetConstantString(const std::string& data, llvm::LLVMContext& context, llvm::Module& module)
	{
		llvm::Constant* strConstant = llvm::ConstantDataArray::getString(context, data, true);

		llvm::Type* type = strConstant->getType();

		llvm::GlobalVariable* globalStr = new llvm::GlobalVariable(
			module,
			type,
			true,
			llvm::GlobalValue::PrivateLinkage,
			strConstant,
			"str" + std::to_string(s_StringCount++));

		llvm::Constant* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
		llvm::Constant* indices[] = {zero, zero};
		llvm::Constant* strPtr = llvm::ConstantExpr::getGetElementPtr(
			globalStr->getValueType(),
			globalStr,
			indices);

		return {strPtr, globalStr->getValueType()};
	}

	Value::Value(const Token& rValue, TypeRegistry& typeRegistry, llvm::LLVMContext& context, llvm::Module& module)
		: m_Type(typeRegistry.GetTypeFromToken(rValue)), m_Token(rValue)
	{
		auto [value, type] = Value::GetConstant(m_Type, m_Token, context, module);
		m_Value = value;
	}

}