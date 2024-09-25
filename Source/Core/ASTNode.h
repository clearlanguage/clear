#pragma once 

#include <memory>
#include <vector>

#include <llvm/IR/Value.h>

namespace clear {

	enum class ASTNodeType
	{
		Base = 0, Literal, BinaryExpression, 
		VariableExpression, VariableDecleration, 
		FunctionDecleration
	};

	//
	// ---------------------- BASE -----------------------
	//
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
	//
	// -------------------------------------------------------
	//

	enum class LiteralType
	{
		None = 0, String, Int8, Int16,
		Int32, Int64, Uint8, Uint16, Uint32, Uint64, 
		Float32, Float64, Bool
	};

	//
	// ---------------------- LITERAL -----------------------
	//
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

	//
	// -----------------------------------------------------------
	//

	enum class BinaryExpressionType
	{
		None = 0, Add, Sub, Mul, 
		Div, Mod, Less, LessEq, 
		Greater, GreaterEq, Eq
	};

	//
	// ---------------------- BINARY EXPRESSION -----------------------
	//
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
	//
	// ------------------------------------------------------------------
	//

	enum class VariableType
	{
		None = 0, Int8, Int16, Int32, Int64, 
		Uint8, Uint16, Uint32, Uint64, Bool, 
		Float32, Float64, Struct, Object //(struct and object will need implementing later)
	};

	struct Argument
	{
		std::string Name;
		VariableType Type;
	};

	//
	// ---------------------- FUNCTION DECELRATION -----------------------
	//
	class ASTFunctionDecleration : public ASTNodeBase
	{
	public:
		ASTFunctionDecleration(const std::string& name, VariableType returnType, const std::vector<Argument>& arugments);
		virtual ~ASTFunctionDecleration() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::FunctionDecleration; }
		virtual llvm::Value* Codegen() override;

		inline const auto  GetArg(size_t idx) const { return m_FunctionArgs[idx]; };
		inline const auto& GetArgs() const { return m_FunctionArgs; }

	private:
		llvm::Type* _GetType(VariableType type);

	private:
		std::string m_Name;
		VariableType m_ReturnType;
		std::vector<Argument> m_Arguments;
		std::vector<llvm::Value*> m_FunctionArgs;
	};
	//
	// -------------------------------------------------------------------
	//


	//
	// ---------------------- VARIABLE DECELRATION -----------------------
	//
	//TODO:
	class ASTVariableDecleration : public ASTNodeBase
	{
	public:
		ASTVariableDecleration(const std::string& name);
		virtual ~ASTVariableDecleration() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::VariableDecleration; }
		virtual llvm::Value* Codegen() override;

		inline const std::string& GetName() const { return m_Name; }

	private:
		std::string m_Name;
	};

	//
	// ---------------------- VARIABLE EXPRESSION -----------------------
	//
	//TODO:
	class ASTVariableExpression : public ASTNodeBase
	{
	public:
		ASTVariableExpression(const std::string& name);
		virtual ~ASTVariableExpression() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::VariableExpression; }
		virtual llvm::Value* Codegen() override;

		inline const std::string& GetName() const { return m_Name; }

	private:
		std::string m_Name;
	};

	//
	// ------------------------------------------------------------------
	//
}