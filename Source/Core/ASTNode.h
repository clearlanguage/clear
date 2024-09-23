#pragma once 

#include <memory>
#include <vector>

#include <llvm/IR/Value.h>

namespace alkhat {

	enum class ASTNodeType
	{
		Base = 0, 
		Literal, 
		BinaryExpression
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

	enum class BinaryExpressionType
	{
		None = 0, Add, Sub, Mul, 
		Div, Mod, Less, LessEq, 
		Greater, GreaterEq, Eq
	};

	class ASTBinaryExpression : public ASTNodeBase
	{
	public:
		ASTBinaryExpression(BinaryExpressionType type);
		virtual ~ASTBinaryExpression() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::BinaryExpression; }
		virtual llvm::Value* Codegen() override;


		inline const BinaryExpressionType GetExpression() const { return m_Expression; }
		
	private:
		const bool _IsMathExpression() const;

		llvm::Value* _CreateMathExpression(llvm::Value* LHS, llvm::Value* RHS);
		llvm::Value* _CreateCmpExpression(llvm::Value* LHS, llvm::Value* RHS);

	private:
		BinaryExpressionType m_Expression;
	};
}