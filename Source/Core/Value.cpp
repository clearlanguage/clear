#include "Value.h"

#include "API/LLVM/LLVMBackend.h"
#include "Log.h"

namespace clear {

	static std::map<std::string, VariableMetaData> s_VariableMetaData;

	static size_t s_StringCount = 0;

	llvm::Value* Value::GetConstant(const AbstractType& type, const std::string& data)
	{
		auto& context = *LLVM::Backend::GetContext();

		switch (type.Get())
		{
			case VariableType::Int8:    return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),  (int8_t)std::stoi(data),     true);
			case VariableType::Int16:   return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), (int16_t)std::stoi(data),    true);
			case VariableType::Int32:   return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), (int32_t)std::stoi(data),    true);
			case VariableType::Int64:   return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), (int64_t)std::stoll(data),   true);
			case VariableType::Uint8:   return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),  (uint8_t)std::stoull(data),  false);
			case VariableType::Uint16:  return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), (uint16_t)std::stoull(data), false);
			case VariableType::Uint32:  return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), (uint32_t)std::stoull(data), false);
			case VariableType::Uint64:  return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), (uint64_t)std::stoull(data), false);
			case VariableType::Float32: return llvm::ConstantFP::get(llvm::Type::getFloatTy(context),  (float)std::stod(data));
			case VariableType::Float64: return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), (double)std::stod(data));
			case VariableType::Bool:	return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), data == "true" ? 1 : 0);
			case VariableType::String:	return Value::GetConstantString(data);
			case VariableType::None:
			default:
				return nullptr;
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

	llvm::Value* Value::GetConstantString(const std::string& data)
	{
		auto& module = *LLVM::Backend::GetModule();
		auto& context = *LLVM::Backend::GetContext();

		llvm::Constant* strConstant = llvm::ConstantDataArray::getString(context, data, true);

		llvm::Type* type = strConstant->getType();

		llvm::GlobalVariable* globalStr = new llvm::GlobalVariable(
			module,
			type,
			true,
			llvm::GlobalValue::PrivateLinkage,
			strConstant,
			".str" + std::to_string(s_StringCount++)
		);

		llvm::Constant* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
		llvm::Constant* indices[] = { zero, zero };
		llvm::Constant* strPtr = llvm::ConstantExpr::getGetElementPtr(
			globalStr->getValueType(),
			globalStr,
			indices
		);

		return strPtr;
	}

	Value::Value(const std::string& rValue)
		: m_Type(rValue), m_Data(rValue), m_Value(Value::GetConstant(m_Type, m_Data))
	{
	}

	Value::Value(const AbstractType& type, const std::string& data)
		: m_Type(type), m_Data(data)
	{
		if (m_Type.GetKind() == TypeKind::RValue)
		{
			m_Value = Value::GetConstant(m_Type, m_Data);
		}
		else if (m_Type.GetKind() == TypeKind::Variable)
		{
			auto& builder = *LLVM::Backend::GetBuilder();

			CLEAR_VERIFY(!s_VariableMetaData.contains(m_Data), "variable already declared");

			if (m_Type.Get() == VariableType::UserDefinedType)
			{
				auto& structType = m_Type.GetUserDefinedType();

				auto value = builder.CreateAlloca(AbstractType::GetStructType(structType), nullptr, m_Data);
				s_VariableMetaData[m_Data] = { .Alloca = value, .Type = m_Type, .Name = m_Data };

				m_Value = value;
			}
			else
			{
				auto variableType = m_Type.Get();
				auto value = builder.CreateAlloca(GetLLVMVariableType(variableType), nullptr, m_Data);
				s_VariableMetaData[m_Data] = { .Alloca = value, .Type = m_Type, .Name = m_Data };

				m_Value = value;
			}
		}
		else if (m_Type.GetKind() == TypeKind::VariableReference)
		{
			CLEAR_VERIFY(s_VariableMetaData.contains(m_Data), "variable does not exist");

			llvm::AllocaInst* value = s_VariableMetaData.at(m_Data).Alloca;
			m_Value = value;
		}
		else
		{
			CLEAR_ANNOTATED_HALT("kind of none is not allowed");
		}

	}
	
	Value::~Value()
	{
	}

	llvm::Value* Value::CastValue(llvm::Value* value, AbstractType to)
	{
		auto& builder = *LLVM::Backend::GetBuilder();
		llvm::Type* fromType = value->getType();
		llvm::Type* toType = to.GetLLVMType();

		if (fromType == toType)
			return value;

		if (fromType->isIntegerTy() && to.IsIntegral())
		{
			return builder.CreateIntCast(value, toType, to.IsSigned());
		}
		else if (fromType->isIntegerTy() && to.IsFloatingPoint())
		{
			if (to.IsSigned())
				return builder.CreateSIToFP(value, toType);  // Signed int to float
			else
				return builder.CreateUIToFP(value, toType);  // Unsigned int to float
		}
		else if (fromType->isFloatingPointTy() && to.IsIntegral())
		{
			// Float to integer cast 
			if (to.IsSigned())
				return builder.CreateFPToSI(value, toType);  // Float to signed int
			else
				return builder.CreateFPToUI(value, toType);  // Float to unsigned int
		}
		else if (fromType->isFloatingPointTy() && to.IsFloatingPoint())
		{
			// Float to float cast
			return builder.CreateFPCast(value, toType);
		}
		else if (fromType->isPointerTy() && to.IsPointer())
		{
			// Pointer to pointer cast
			return builder.CreatePointerCast(value, toType);
		}
		else if (fromType->isIntegerTy() && to.IsPointer())
		{
			// Integer to pointer cast
			return builder.CreateIntToPtr(value, toType);
		}
		else if (fromType->isPointerTy() && to.IsIntegral())
		{
			// Pointer to integer cast
			return builder.CreatePtrToInt(value, toType);
		}

		CLEAR_ANNOTATED_HALT("failed to find right cast type");
		return nullptr;
	}

	Ref<Value> Value::Cast(const Ref<Value>& casting, AbstractType to)
	{
		auto& builder = *LLVM::Backend::GetBuilder();

		llvm::Value* newValue = Value::CastValue(casting->Get(), to);
		return Ref<Value>::Create(to, casting->GetData(), casting->GetChain(), newValue);
	}
}