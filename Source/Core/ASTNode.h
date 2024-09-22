#pragma once 

#include <memory>
#include <vector>

#include <llvm/IR/Value.h>

namespace alkhat {

	enum class ASTNodeType
	{
		Base = 0, 
		Literal
	};

	class ASTNodeBase
	{
	public:
		ASTNodeBase() = default;
		virtual ~ASTNodeBase() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::Base; }
		virtual llvm::Value* Codegen() { return nullptr; };

		void PushChild(const std::shared_ptr<ASTNodeBase>& child);
		void RemoveChild(const std::shared_ptr<ASTNodeBase>& child);

		void SetParent(const std::shared_ptr<ASTNodeBase>& parent);
		void RemoveParent();

		const auto  GetParent()   const { return m_Parent; }
		const auto& GetChildren() const { return m_Children; }

	private:
		std::shared_ptr<ASTNodeBase> m_Parent = nullptr;
		std::vector<std::shared_ptr<ASTNodeBase>> m_Children;
	};

	enum class LiteralType
	{
		None = 0, String, Int8, Int16,
		Int32, Int64, Uint8, Uint16, Uint32, Uint64, 
		Float32, Float64, Bool
	};

	class ASTNodeLiteral : public ASTNodeBase
	{
	public:
		ASTNodeLiteral(LiteralType type, const std::string& data);
		virtual ~ASTNodeLiteral() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::Literal; }
		virtual llvm::Value* Codegen() override;
		

	private:
		LiteralType m_Type;
		std::string m_Data;
	};
}