#include "Value.h"

#include "API/LLVM/LLVMBackend.h"
#include "Log.h"
#include "TypeRegistry.h"

namespace clear
{
	static size_t s_StringCount = 0;

	Value::ConstantPair Value::GetConstant(const std::shared_ptr<Type>& type, const std::string& data)
	{
		auto &context = *LLVM::Backend::GetContext();

		std::string hash = type->GetHash();

		if(hash == "int8")  return {llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),  (int8_t)std::stoll(data), true), nullptr};
		if(hash == "int16") return {llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), (int16_t)std::stoll(data), true), nullptr};
		if(hash == "int32") return {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), (int32_t)std::stoll(data), true), nullptr};
		if(hash == "int64") return {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), (int64_t)std::stoll(data), true), nullptr};

		if(hash == "uint8")  return {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context),  (uint8_t)std::stoull(data), true), nullptr};
		if(hash == "uint16") return {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), (uint16_t)std::stoull(data), true), nullptr};
		if(hash == "uint32") return {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), (uint32_t)std::stoull(data), true), nullptr};
		if(hash == "uint64") return {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), (uint64_t)std::stoull(data), true), nullptr};

		if(hash == "float32") return {llvm::ConstantFP::get(llvm::Type::getFloatTy(context), (float)std::stod(data)), nullptr};
		if(hash == "float64") return {llvm::ConstantFP::get(llvm::Type::getFloatTy(context), (double)std::stod(data)), nullptr};

		if(hash == "bool") return {llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), data == "true" ? 1 : 0), nullptr};
		
		if(hash == "int8*") return GetConstantString(data);
		if(hash == "null_type*") return { llvm::ConstantPointerNull::get((llvm::PointerType*)type->Get()), nullptr };


		CLEAR_UNREACHABLE("invalid hash for constant ", hash);
	}


	Value::ConstantPair Value::GetConstantString(const std::string& data)
	{
		auto &module = *LLVM::Backend::GetModule();
		auto &context = *LLVM::Backend::GetContext();

		llvm::Constant *strConstant = llvm::ConstantDataArray::getString(context, data, true);

		llvm::Type *type = strConstant->getType();

		llvm::GlobalVariable *globalStr = new llvm::GlobalVariable(
			module,
			type,
			true,
			llvm::GlobalValue::PrivateLinkage,
			strConstant,
			"str" + std::to_string(s_StringCount++));

		llvm::Constant *zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
		llvm::Constant *indices[] = {zero, zero};
		llvm::Constant *strPtr = llvm::ConstantExpr::getGetElementPtr(
			globalStr->getValueType(),
			globalStr,
			indices);

		return {strPtr, globalStr->getValueType()};
	}

	Value::Value(const Token &rValue)
		: m_Type(TypeRegistry::GetGlobal()->GetTypeFromToken(rValue)), m_Data(rValue.Data)
	{
		auto [value, type] = Value::GetConstant(m_Type, m_Data);
		m_Value = value;
	}

}