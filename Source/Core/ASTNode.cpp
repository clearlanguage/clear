#include "ASTNode.h"

#include "API/LLVM/LLVMBackend.h"

namespace alkhat {

	void ASTNodeBase::PushChild(const std::shared_ptr<ASTNodeBase>& child)
	{
		m_Children.push_back(child);
	}
	void ASTNodeBase::RemoveChild(const std::shared_ptr<ASTNodeBase>& child)
	{
		auto it = std::find(m_Children.begin(), m_Children.end(), child);
		if (it != m_Children.end())
			m_Children.erase(it);
	}
	void ASTNodeBase::SetParent(const std::shared_ptr<ASTNodeBase>& parent)
	{
		m_Parent = parent;
	}
	void ASTNodeBase::RemoveParent()
	{
		m_Parent.reset();
	}
	ASTNodeLiteral::ASTNodeLiteral(LiteralType type, const std::string& data)
		: m_Type(type), m_Data(data)
	{
	}
	llvm::Value* ASTNodeLiteral::Codegen()
	{
		auto& context = *LLVM::Backend::GetContext();

		//TODO: add strings

		switch (m_Type)
		{
			case LiteralType::Int8:	   return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),   (uint8_t)std::stoull(m_Data),  true);
			case LiteralType::Int16:   return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context),  (int16_t)std::stoi(m_Data),    true);
			case LiteralType::Int32:   return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),  (int32_t)std::stoi(m_Data),    true);
			case LiteralType::Int64:   return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context),  (int64_t)std::stoll(m_Data),   true);
			case LiteralType::Uint8:   return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),   (uint8_t)std::stoull(m_Data),  false);
			case LiteralType::Uint16:  return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context),  (uint16_t)std::stoull(m_Data), false);
			case LiteralType::Uint32:  return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),  (uint32_t)std::stoull(m_Data), false);
			case LiteralType::Uint64:  return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context),  (uint64_t)std::stoull(m_Data), false);
			case LiteralType::Float32: return llvm::ConstantFP::get(llvm::Type::getFloatTy(context),   (float)std::stod(m_Data));
			case LiteralType::Float64: return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context),  (double)std::stod(m_Data));
			case LiteralType::None:	
			default:
				return nullptr;
		}
	}
}