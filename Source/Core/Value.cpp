#include "Value.h"

#include "API/LLVM/LLVMBackend.h"
#include "Log.h"

namespace clear {

	static std::map<std::string, VariableMetaData> s_VariableMetaData;

	static size_t s_StringCount = 0;

	Value::ConstantPair Value::GetConstant(const AbstractType& type, const std::string& data)
	{
		auto& context = *LLVM::Backend::GetContext();

		switch (type.Get())
		{
			case VariableType::Int8:    return { llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),  (int8_t)std::stoi(data),     true), nullptr };
			case VariableType::Int16:   return { llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), (int16_t)std::stoi(data),    true), nullptr };
			case VariableType::Int32:   return { llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), (int32_t)std::stoi(data),    true), nullptr };
			case VariableType::Int64:   return { llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), (int64_t)std::stoll(data),   true), nullptr };
			case VariableType::Uint8:   return { llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),  (uint8_t)std::stoull(data),  false), nullptr };
			case VariableType::Uint16:  return { llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), (uint16_t)std::stoull(data), false), nullptr };
			case VariableType::Uint32:  return { llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), (uint32_t)std::stoull(data), false), nullptr };
			case VariableType::Uint64:  return { llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), (uint64_t)std::stoull(data), false), nullptr };
			case VariableType::Float32: return { llvm::ConstantFP::get(llvm::Type::getFloatTy(context),  (float)std::stod(data)), nullptr };
			case VariableType::Float64: return { llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), (double)std::stod(data)), nullptr };
			case VariableType::Bool:	return { llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), data == "true" ? 1 : 0), nullptr };
			case VariableType::String:	return Value::GetConstantString(data);
			case VariableType::Pointer: return { llvm::ConstantPointerNull::get((llvm::PointerType*)type.GetLLVMType()), nullptr };
			case VariableType::None:
			default:
				return { nullptr, nullptr };
		}

	}

	VariableMetaData& Value::GetVariableMetaData(const std::string& name)
	{
		static VariableMetaData s_NullVariableMetaData;
		return s_VariableMetaData.contains(name) ? s_VariableMetaData.at(name) : s_NullVariableMetaData;
	}

	void Value::RegisterVariable(llvm::AllocaInst* alloc, const std::string& name, const AbstractType& type)
	{
		s_VariableMetaData[name] = { .Alloca = alloc, .Type = type, .Name = name };
	}

	void Value::RemoveVariable(const std::string& name)
	{
		s_VariableMetaData.erase(name);
	}

	Value::Value(const AbstractType& type, const std::string& data, const std::list<Value>& chain, llvm::Value* value)
		: m_Type(type), m_Data(data), m_Chain(chain), m_Value(value)
	{
	}

	Value::ConstantPair Value::GetConstantString(const std::string& data)
	{
		auto& module  = *LLVM::Backend::GetModule();
		auto& context = *LLVM::Backend::GetContext();

		llvm::Constant* strConstant = llvm::ConstantDataArray::getString(context, data, true);

		llvm::Type* type = strConstant->getType();

		llvm::GlobalVariable* globalStr = new llvm::GlobalVariable(
			module,
			type,
			true,
			llvm::GlobalValue::PrivateLinkage,
			strConstant,
			"str" + std::to_string(s_StringCount++)
		);

		llvm::Constant* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
		llvm::Constant* indices[] = { zero, zero };
		llvm::Constant* strPtr = llvm::ConstantExpr::getGetElementPtr(
			globalStr->getValueType(),
			globalStr,
			indices
		);

		return { strPtr, globalStr->getValueType() };
	}

	Value::Value(const std::string& rValue)
		: m_Type(rValue), m_Data(rValue)
	{
		auto [value, type] = Value::GetConstant(m_Type, m_Data);

		m_Value = value;
		if (m_Type.Get() == VariableType::String)
			m_Type.SetLLVMUnderlyingType(type);
	}

	Value::Value(const AbstractType& type, const std::string& data)
		: m_Type(type), m_Data(data)
	{
		if (m_Type.GetKind() == TypeKind::RValue)
		{
			m_Value = Value::GetConstant(m_Type, m_Data).first;
		}
		else if (m_Type.GetKind() == TypeKind::Variable)
		{
			auto& builder = *LLVM::Backend::GetBuilder();

			CLEAR_VERIFY(!s_VariableMetaData.contains(m_Data), "variable already declared");

			auto value = builder.CreateAlloca(type.GetLLVMType(), nullptr, m_Data);
			s_VariableMetaData[m_Data] = { .Alloca = value, .Type = m_Type, .Name = m_Data };

		}
		else if (m_Type.GetKind() == TypeKind::VariableReference)
		{
			CLEAR_VERIFY(s_VariableMetaData.contains(m_Data), "variable does not exist");

			llvm::AllocaInst* value = s_VariableMetaData.at(m_Data).Alloca;
			m_Value = value;
		}
		else
		{
			CLEAR_UNREACHABLE("kind of none is not allowed");
		}

	}
	
	Value::~Value()
	{
	}

	llvm::Value* Value::CastValue(llvm::Value* value, const AbstractType& to, const AbstractType& from)
	{
		llvm::Type* fromType = value->getType();
		return Value::CastValue(value, to.GetLLVMType(), fromType, from);
	}

	llvm::Value* Value::CastValue(llvm::Value* value, llvm::Type* to, llvm::Type* from, const AbstractType& fromType)
	{
		auto& builder = *LLVM::Backend::GetBuilder();

		llvm::Type* toType = to;

		if (from == toType)
			return value;

		if (from->isIntegerTy() && to->isIntegerTy())
		{
			return builder.CreateIntCast(value, toType, fromType.IsSigned(), "cast");
		}
		else if (from->isIntegerTy() && to->isFloatingPointTy())
		{
			if (fromType.IsSigned())
				return builder.CreateSIToFP(value, toType, "cast");  
			else
				return builder.CreateUIToFP(value, toType, "cast");  
		}
		else if (from->isFloatingPointTy() && to->isIntegerTy())
		{
			if (fromType.IsSigned())
				return builder.CreateFPToSI(value, toType, "cast");  
			else
				return builder.CreateFPToUI(value, toType, "cast"); 
		}
		else if (from->isFloatingPointTy() && to->isFloatingPointTy())
		{
			return builder.CreateFPCast(value, toType, "cast");
		}
		else if (from->isPointerTy() && to->isPointerTy())
		{
			return builder.CreatePointerCast(value, toType, "cast");
		}
		else if (from->isIntegerTy() && to->isPointerTy())
		{
			return builder.CreateIntToPtr(value, toType, "cast");
		}

		CLEAR_UNREACHABLE("failed to find right cast type");

		return nullptr;
	}

	llvm::Value* Value::CastValue(llvm::Value* value, llvm::Type* to, llvm::Type* from, bool isSigned)
	{
		auto& builder = *LLVM::Backend::GetBuilder();

		llvm::Type* toType = to;

		if (from == to)
			return value;

		if (from->isIntegerTy() && to->isIntegerTy())
		{
			return builder.CreateIntCast(value, toType, isSigned, "cast");
		}
		else if (from->isIntegerTy() && to->isFloatingPointTy())
		{
			if (isSigned)
				return builder.CreateSIToFP(value, to, "cast");  
			else
				return builder.CreateUIToFP(value, to, "cast");  
		}
		else if (from->isFloatingPointTy() && to->isIntegerTy())
		{
			if (isSigned)
				return builder.CreateFPToSI(value, toType, "cast");  
			else
				return builder.CreateFPToUI(value, toType, "cast"); 
		}
		else if (from->isFloatingPointTy() && to->isFloatingPointTy())
		{
			return builder.CreateFPCast(value, toType, "cast");
		}
		else if (from->isPointerTy() && to->isPointerTy())
		{
			return builder.CreatePointerCast(value, toType, "cast");
		}
		else if (from->isIntegerTy() && to->isPointerTy())
		{
			return builder.CreateIntToPtr(value, toType, "cast");
		}

		CLEAR_UNREACHABLE("failed to find right cast type");

		return nullptr;
	}

	Ref<Value> Value::Cast(const Ref<Value>& casting, AbstractType to)
	{
		auto& builder = *LLVM::Backend::GetBuilder();

		llvm::Value* newValue = Value::CastValue(casting->Get(), to);
		return Ref<Value>::Create(to, casting->GetData(), casting->GetChain(), newValue);
	}
}