#pragma once

#include "Core/Type.h"
#include "Core/Value.h"

#include "Lexing/Tokens.h"
#include "SymbolTable.h"

#include <memory>
#include <string>

namespace clear 
{
    enum class ASTNodeType
	{
		Base = 0, Literal, BinaryExpression,
		VariableExpression, VariableDecleration,
		FunctionDefinition, FunctionDecleration,
		ReturnStatement, Expression, Struct,
		FunctionCall, IfExpression, WhileLoop,
		UnaryExpression, Break, Continue, 
		ArrayInitializer, MemberAccess, AssignmentOperator, 
		VariableReference, Import
	};

	struct CodegenResult
	{
		llvm::Value* CodegenValue = nullptr;
		std::shared_ptr<Type> CodegenType;
	};

    class ASTNodeBase
	{
	public:
		ASTNodeBase();
		virtual ~ASTNodeBase() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::Base; }
		virtual CodegenResult Codegen();

		void Push(const std::shared_ptr<ASTNodeBase>& child);
		void Remove(const std::shared_ptr<ASTNodeBase>& child);

		void PropagateSymbolTableToChildren();

		void CreateSymbolTable();

		std::shared_ptr<SymbolTable> GetSymbolTable() { return m_SymbolTable; }

		const auto& GetChildren() const { return m_Children; }
	
	private:
		void PropagateSymbolTable(const std::shared_ptr<SymbolTable>&);

	private:
		std::vector<std::shared_ptr<ASTNodeBase>> m_Children;
		std::shared_ptr<SymbolTable> m_SymbolTable;
	};

	class ASTNodeLiteral : public ASTNodeBase
	{
	public:
		ASTNodeLiteral(const Token& data);
		virtual ~ASTNodeLiteral() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Literal; }
		virtual CodegenResult Codegen() override;

	private:
		Value m_Constant;
	};

	class ASTBinaryExpression : public ASTNodeBase
	{
	public:
		ASTBinaryExpression(BinaryExpressionType type);
		virtual ~ASTBinaryExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::BinaryExpression; }
		virtual CodegenResult Codegen() override;

		inline const BinaryExpressionType GetExpression() const { return m_Expression; }

		static CodegenResult HandleMathExpression(CodegenResult& lhs, CodegenResult& rhs,   BinaryExpressionType type);
		static CodegenResult HandleMathExpressionF(CodegenResult& lhs, CodegenResult& rhs,  BinaryExpressionType type);
		static CodegenResult HandleMathExpressionSI(CodegenResult& lhs, CodegenResult& rhs, BinaryExpressionType type);
		static CodegenResult HandleMathExpressionUI(CodegenResult& lhs, CodegenResult& rhs, BinaryExpressionType type);

	private:
		void HandleTypePromotion(CodegenResult& lhs, CodegenResult& rhs);

		bool IsMathExpression()    const;
		bool IsCmpExpression()     const;
		bool IsBitwiseExpression() const;

		CodegenResult HandleCmpExpression(CodegenResult& lhs, CodegenResult& rhs);
		CodegenResult HandleCmpExpressionF(CodegenResult& lhs, CodegenResult& rhs);
		CodegenResult HandleCmpExpressionSI(CodegenResult& lhs, CodegenResult& rhs);
		CodegenResult HandleCmpExpressionUI(CodegenResult& lhs, CodegenResult& rhs);

		CodegenResult HandleBitwiseExpression(CodegenResult& lhs, CodegenResult& rhs);
		CodegenResult HandlePointerArithmetic(CodegenResult& lhs, CodegenResult& rhs);

	private:
		BinaryExpressionType m_Expression;
	};

	class ASTVariableDeclaration : public ASTNodeBase
	{
	public:
		ASTVariableDeclaration(const std::string& name, std::shared_ptr<Type> type);
		virtual ~ASTVariableDeclaration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::VariableDecleration; }
		virtual CodegenResult Codegen() override;

	private:
		std::string m_Name;
		std::shared_ptr<Type> m_Type;
	};

	class ASTVariableReference : public ASTNodeBase
	{
	public:
		ASTVariableReference(const std::string& name);
		virtual ~ASTVariableReference() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::VariableReference; }
		virtual CodegenResult Codegen() override;

	private:
		std::string m_Name;
	};

	class ASTVariableExpression : public ASTNodeBase
	{
	public:
		ASTVariableExpression(const std::string& name);
		virtual ~ASTVariableExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::VariableExpression; }
		virtual CodegenResult Codegen() override;

	private:
		std::string m_Name;
	};

	enum class AssignmentOperatorType 
	{
		Normal = 0, Mul, Div, Add, Sub, Mod
	};

	class ASTAssignmentOperator : public ASTNodeBase
	{
	public:
		ASTAssignmentOperator(AssignmentOperatorType type);
		virtual ~ASTAssignmentOperator() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::AssignmentOperator; }
		virtual CodegenResult Codegen();

	private:
		void HandleDifferentTypes(CodegenResult& storage, CodegenResult& data);

	private:
		AssignmentOperatorType m_Type;
	};

	class ASTStruct : public ASTNodeBase
	{
	public:
		ASTStruct() = default;
		virtual ~ASTStruct() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Struct; }
		virtual CodegenResult Codegen() override;
	};

	class ASTFunctionDefinition : public ASTNodeBase 
	{
	public:
		ASTFunctionDefinition(const std::string& name, const std::shared_ptr<Type>& returnType, const std::vector<Parameter>& parameters);
		virtual ~ASTFunctionDefinition() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionDefinition; }
		virtual CodegenResult Codegen() override;

	private:
		std::vector<Parameter> m_Parameters;
		std::string m_Name;
		std::shared_ptr<Type> m_ReturnType;
	};

	class ASTFunctionCall : public ASTNodeBase
	{
	public:
		ASTFunctionCall(const std::string& name);
		virtual ~ASTFunctionCall() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionCall; }
		virtual CodegenResult Codegen() override;
		
	private:
		std::string m_Name;
	};

	class ASTFunctionDecleration : public ASTNodeBase
	{
	public:
		ASTFunctionDecleration(const std::string& name, const std::shared_ptr<Type>& expectedReturnType, const std::vector<Parameter>& types);
		virtual ~ASTFunctionDecleration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionDecleration; }
		virtual CodegenResult Codegen() override;

	private:
		std::vector<Parameter> m_Parameters;
		std::shared_ptr<Type> m_ReturnType;
		std::string m_Name;
	};

	class ASTExpression : public ASTNodeBase
	{
	public:
		ASTExpression() = default;
		virtual ~ASTExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Expression; }
		virtual CodegenResult Codegen() override;
	};


	class ASTArrayInitializer : public ASTNodeBase
	{
	public:
		ASTArrayInitializer() = default;
		virtual ~ASTArrayInitializer() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::ArrayInitializer; }
		virtual CodegenResult Codegen() override;

		void SetIndices(const std::vector<std::vector<size_t>>& indices);

	private:
		void VerifyArray(std::shared_ptr<ArrayType> type, 
						 const std::vector<size_t>& index);

		std::shared_ptr<Type> GetElementType(std::shared_ptr<Type> type);

		std::shared_ptr<Type> GetInnerType(std::shared_ptr<Type> type, size_t index);

	private:
		std::vector<std::vector<size_t>> m_Indices;
	};

	class ASTImport : public ASTNodeBase
	{
	public:
		ASTImport(const std::string& filepath);
		virtual ~ASTImport() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Import; }
		virtual CodegenResult Codegen() override;

		const std::string& GetFilePath() const { return m_Filepath; }

	private:
		std::string m_Filepath;
	};

}