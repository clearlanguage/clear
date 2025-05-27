#pragma once

#include "Core/Type.h"
#include "Core/Value.h"

#include "Lexing/Tokens.h"
#include "SymbolTable.h"

#include <memory>
#include <string>
#include <filesystem>

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
		VariableReference, Import, Member
	};

	struct CodegenResult
	{
		llvm::Value* CodegenValue = nullptr;
		std::shared_ptr<Type> CodegenType;
	};

	class ASTNodeBase;

	struct AstLookupInfo
	{
		std::shared_ptr<ASTNodeBase> Node;
		TypeRegistry Registry;

		AstLookupInfo(std::shared_ptr<llvm::LLVMContext> context) : Registry(context) {}
	};

	using LookupAstTable = std::unordered_map<std::filesystem::path, AstLookupInfo>;

	struct CodegenContext 
	{
    	LookupAstTable& LookupTable;

		std::filesystem::path CurrentDirectory;

		llvm::LLVMContext& Context;
        llvm::IRBuilder<>& Builder;
        llvm::Module&      Module;

		TypeRegistry& Registry;

    	CodegenContext(LookupAstTable& map, const std::filesystem::path& path, llvm::LLVMContext& context, 
					   llvm::IRBuilder<>& builder, llvm::Module& module, TypeRegistry& registry) 
			: LookupTable(map), CurrentDirectory(path), Context(context), Builder(builder), Module(module), 
			  Registry(registry)
		{
		}
	};

    class ASTNodeBase
	{
	public:
		ASTNodeBase();
		virtual ~ASTNodeBase() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::Base; }
		virtual CodegenResult Codegen(CodegenContext&);

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
		virtual CodegenResult Codegen(CodegenContext&) override;

	private:
		Token m_Token;
	};

	class ASTBinaryExpression : public ASTNodeBase
	{
	public:
		ASTBinaryExpression(BinaryExpressionType type);
		virtual ~ASTBinaryExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::BinaryExpression; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		inline const BinaryExpressionType GetExpression() const { return m_Expression; }

		static CodegenResult HandleMathExpression(CodegenResult& lhs, CodegenResult& rhs,   BinaryExpressionType type, CodegenContext& ctx);
		static CodegenResult HandleMathExpressionF(CodegenResult& lhs, CodegenResult& rhs,  BinaryExpressionType type, CodegenContext& ctx);
		static CodegenResult HandleMathExpressionSI(CodegenResult& lhs, CodegenResult& rhs, BinaryExpressionType type, CodegenContext& ctx);
		static CodegenResult HandleMathExpressionUI(CodegenResult& lhs, CodegenResult& rhs, BinaryExpressionType type, CodegenContext& ctx);

	private:
		void HandleTypePromotion(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx);

		bool IsMathExpression()    const;
		bool IsCmpExpression()     const;
		bool IsBitwiseExpression() const;

		CodegenResult HandleCmpExpression(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx);
		CodegenResult HandleCmpExpressionF(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx);
		CodegenResult HandleCmpExpressionSI(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx);
		CodegenResult HandleCmpExpressionUI(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx);

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
		virtual CodegenResult Codegen(CodegenContext&) override;

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
		virtual CodegenResult Codegen(CodegenContext&) override;

	private:
		std::string m_Name;
	};

	class ASTVariableExpression : public ASTNodeBase
	{
	public:
		ASTVariableExpression(const std::string& name);
		virtual ~ASTVariableExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::VariableExpression; }
		virtual CodegenResult Codegen(CodegenContext&) override;

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
		virtual CodegenResult Codegen(CodegenContext&);

	private:
		void HandleDifferentTypes(CodegenResult& storage, CodegenResult& data, CodegenContext& ctx);

	private:
		AssignmentOperatorType m_Type;
	};

	class ASTStruct : public ASTNodeBase
	{
	public:
		ASTStruct() = default;
		virtual ~ASTStruct() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Struct; }
		virtual CodegenResult Codegen(CodegenContext&) override;
	};

	class ASTFunctionDefinition : public ASTNodeBase 
	{
	public:
		ASTFunctionDefinition(const std::string& name, const std::shared_ptr<Type>& returnType, const std::vector<Parameter>& parameters);
		virtual ~ASTFunctionDefinition() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionDefinition; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		const std::string& GetName() const { return m_Name; }

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
		virtual CodegenResult Codegen(CodegenContext&) override;
		
	private:
		std::string m_Name;
	};

	class ASTFunctionDecleration : public ASTNodeBase
	{
	public:
		ASTFunctionDecleration(const std::string& name, const std::shared_ptr<Type>& expectedReturnType, const std::vector<Parameter>& types);
		virtual ~ASTFunctionDecleration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionDecleration; }
		virtual CodegenResult Codegen(CodegenContext&) override;

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
		virtual CodegenResult Codegen(CodegenContext&) override;
	};

	//TODO: change to InitializerList (can be used to initialize structs as well in future)
	class ASTArrayInitializer : public ASTNodeBase
	{
	public:
		ASTArrayInitializer() = default;
		virtual ~ASTArrayInitializer() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::ArrayInitializer; }
		virtual CodegenResult Codegen(CodegenContext&) override;

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
		ASTImport(const std::filesystem::path& filepath);
		virtual ~ASTImport() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Import; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		const std::filesystem::path& GetFilePath() const { return m_Filepath; }

	private:
		std::filesystem::path m_Filepath;
	};

	class ASTMember : public ASTNodeBase
	{
	public:
		ASTMember(const std::string& name);
		virtual ~ASTMember() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Member; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		const std::string& GetName() const {  return m_MemberName; }

	private:
		std::string m_MemberName;
	};

	class ASTMemberAccess : public ASTNodeBase
	{
	public:
		ASTMemberAccess(bool isValueReference);
		virtual ~ASTMemberAccess() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::MemberAccess; }
		virtual CodegenResult Codegen(CodegenContext&) override;

	private:
		bool m_ValueReference;
	};

}