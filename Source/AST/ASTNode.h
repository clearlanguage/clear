#pragma once

#include "Core/Type.h"
#include "Core/Value.h"

#include "Lexing/Tokens.h"
#include "SymbolTable.h"
#include "Linker/LibraryLinker.h"

#include <memory>
#include <string>
#include <filesystem>

namespace clear 
{
    enum class ASTNodeType
	{
		Base = 0, Literal, BinaryExpression, VariableDecleration,
		FunctionDefinition, FunctionDecleration,
		ReturnStatement, Expression, Struct,
		FunctionCall, IfExpression, WhileLoop,
		UnaryExpression, Break, Continue, 
		InitializerList, MemberAccess, AssignmentOperator, Import, Member, 
		Variable, ForLoop, InferredDecleration, Class, LoopControlFlow
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
		std::filesystem::path StdLibraryDirectory;

		llvm::LLVMContext& Context;
        llvm::IRBuilder<>& Builder;
        llvm::Module&      Module;

		TypeRegistry& Registry;

		std::shared_ptr<Type> ReturnType;
		llvm::AllocaInst*  ReturnAlloca = nullptr;
		llvm::BasicBlock*  ReturnBlock = nullptr;
		llvm::BasicBlock* LoopConditionBlock = nullptr;
		llvm::BasicBlock* LoopEndBlock = nullptr;


		bool WantAddress;

    	CodegenContext(LookupAstTable& map, const std::filesystem::path& path, llvm::LLVMContext& context, 
					   llvm::IRBuilder<>& builder, llvm::Module& module, TypeRegistry& registry) 
			: LookupTable(map), CurrentDirectory(path), Context(context), Builder(builder), Module(module), 
			  Registry(registry)
		{
		}
	};

    class ASTNodeBase : public std::enable_shared_from_this<ASTNodeBase>
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

		auto& GetChildren() { return m_Children; }
	
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

		CodegenResult HandleMathExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);
		static CodegenResult HandleMathExpression(CodegenResult& lhs, CodegenResult& rhs,   BinaryExpressionType type, CodegenContext& ctx);
		static CodegenResult HandleMathExpressionF(CodegenResult& lhs, CodegenResult& rhs,  BinaryExpressionType type, CodegenContext& ctx);
		static CodegenResult HandleMathExpressionSI(CodegenResult& lhs, CodegenResult& rhs, BinaryExpressionType type, CodegenContext& ctx);
		static CodegenResult HandleMathExpressionUI(CodegenResult& lhs, CodegenResult& rhs, BinaryExpressionType type, CodegenContext& ctx);
		static CodegenResult HandlePointerArithmetic(CodegenResult& lhs, CodegenResult& rhs, BinaryExpressionType type, CodegenContext& ctx);


	private:
		static void HandleTypePromotion(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx);

		bool IsMathExpression()    const;
		bool IsCmpExpression()     const;
		bool IsBitwiseExpression() const;
		bool IsLogicalOperator()   const;

		CodegenResult HandleCmpExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);
		CodegenResult HandleCmpExpression(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx);
		CodegenResult HandleCmpExpressionF(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx);
		CodegenResult HandleCmpExpressionSI(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx);
		CodegenResult HandleCmpExpressionUI(CodegenResult& lhs, CodegenResult& rhs, CodegenContext& ctx);

		CodegenResult HandleBitwiseExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);
		CodegenResult HandleLogicalExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);

		CodegenResult HandleArrayIndex(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);
			
	private:
		BinaryExpressionType m_Expression;
	};

	class ASTVariableDeclaration : public ASTNodeBase
	{
	public:
		ASTVariableDeclaration(const std::string& name, const TypeDescriptor& type);
		virtual ~ASTVariableDeclaration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::VariableDecleration; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		const auto& GetName() const { return m_Name; }

	private:
		std::string m_Name;
		TypeDescriptor m_Type;
	};

	class ASTInferredDecleration : public ASTNodeBase 
	{
	public:
		ASTInferredDecleration(const std::string& name, bool isConst = false);
		virtual ~ASTInferredDecleration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::InferredDecleration; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		const auto& GetName() const { return m_Name; }

	private:
		std::string m_Name;
		bool m_IsConst;
	};

	class ASTVariable : public ASTNodeBase
	{
	public:
		ASTVariable(const std::string& name);
		virtual ~ASTVariable() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Variable; }
		virtual CodegenResult Codegen(CodegenContext&) override;

	private:
		std::string m_Name;
	};

	enum class AssignmentOperatorType 
	{
		Initialize, Normal, Mul, Div, Add, Sub, Mod
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


	class ASTFunctionDefinition : public ASTNodeBase
	{
	public:
		ASTFunctionDefinition(const std::string& name, const TypeDescriptor& returnType, const std::vector<UnresolvedParameter>& parameters);
		virtual ~ASTFunctionDefinition() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionDefinition; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		const std::string& GetName() const { return m_Name; }
		void SetName(const std::string& name) { m_Name = name;}

		void Instantiate(FunctionInstance& functionData, CodegenContext& ctx);

		const auto& GetParameters() const { return m_ResolvedParams; }

	private:
		std::vector<UnresolvedParameter> m_Parameters;
		std::string m_Name;
		TypeDescriptor m_ReturnType;

		std::vector<Parameter> m_ResolvedParams;
		std::shared_ptr<Type> m_ResolvedReturnType;
	};

	class ASTFunctionCall : public ASTNodeBase
	{
	public:
		ASTFunctionCall(const std::string& name);
		virtual ~ASTFunctionCall() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionCall; }
		virtual CodegenResult Codegen(CodegenContext&) override;
				
	private:
		void BuildArgs(CodegenContext& ctx, std::vector<llvm::Value*>& args, std::vector<Parameter>& params);
		void CastArgs(CodegenContext& ctx, std::vector<llvm::Value*>& args, std::vector<Parameter>& params, FunctionTemplate&);
	
	private:
		std::string m_Name;
	};

	class ASTFunctionDecleration : public ASTNodeBase
	{
	public:
		ASTFunctionDecleration(const std::string& name, const TypeDescriptor& expectedReturnType, const std::vector<UnresolvedParameter>& types);
		virtual ~ASTFunctionDecleration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionDecleration; }
		virtual CodegenResult Codegen(CodegenContext&) override;

	private:
		std::vector<UnresolvedParameter> m_Parameters;
		TypeDescriptor m_ReturnType;
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

	class ASTInitializerList : public ASTNodeBase
	{
	public:
		ASTInitializerList() = default;
		virtual ~ASTInitializerList() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::InitializerList; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		void SetIndices(const std::vector<std::vector<size_t>>& indices);

	private:
		void DoInitForArray(CodegenContext& ctx, CodegenResult storage);
		void DoInitForStruct(CodegenContext& ctx, CodegenResult storage);

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
		ASTImport(const std::filesystem::path& filepath, const std::string& alias = "");
		virtual ~ASTImport() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Import; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		const std::filesystem::path& GetFilePath() const { return m_Filepath; }
		const std::string& GetAlias() const { return m_Alias; }

	private:
		void ProcessCImport(const std::filesystem::path& path,  CodegenContext& ctx);
		FunctionInstance ParseHeader(const HeaderFunc& function, CodegenContext& ctx);
		Parameter GetInfoFromArg(const std::vector<Token>& arg, CodegenContext& ctx);

		void ProcessTypes(const std::filesystem::path& path, CodegenContext& ctx);

	private:
		std::filesystem::path m_Filepath;
		std::string m_Alias;
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
		ASTMemberAccess();
		virtual ~ASTMemberAccess() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::MemberAccess; }
		virtual CodegenResult Codegen(CodegenContext&) override;

	private:
		CodegenResult DoMemberAccessForAddress(CodegenContext& ctx, CodegenResult parent);
		CodegenResult DoMemberAccessForValue(CodegenContext& ctx, CodegenResult parent);

	};

	class ASTReturn : public ASTNodeBase 
	{
	public:
		ASTReturn() = default;
		virtual ~ASTReturn() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::ReturnStatement; }
		virtual CodegenResult Codegen(CodegenContext&) override;

	private:
		void EmitDefaultReturn(CodegenContext& ctx);
	};

	class ASTUnaryExpression : public ASTNodeBase 
	{
	public:
		ASTUnaryExpression(UnaryExpressionType type);
		virtual ~ASTUnaryExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::UnaryExpression; }
		virtual CodegenResult Codegen(CodegenContext&) override;

	private: 
		UnaryExpressionType m_Type;
	};

	class ASTIfExpression : public ASTNodeBase 
	{
	public:
		ASTIfExpression() = default;
		virtual ~ASTIfExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::IfExpression; }
		virtual CodegenResult Codegen(CodegenContext&) override;
	};

	class ASTWhileExpression : public ASTNodeBase
	{
	public:
		ASTWhileExpression() = default;
		virtual ~ASTWhileExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::WhileLoop; }
		virtual CodegenResult Codegen(CodegenContext&) override;
	};

	class ASTForExpression : public ASTNodeBase 
	{
	public:
		ASTForExpression(const std::string& name);
		virtual ~ASTForExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::ForLoop; }
		virtual CodegenResult Codegen(CodegenContext&) override;
	
	private:
		std::string m_Name;
	};

	class ASTStruct : public ASTNodeBase
	{
	public:
		ASTStruct(const TypeDescriptor& structTy);
		virtual ~ASTStruct() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Struct; }
		virtual CodegenResult Codegen(CodegenContext&) override;

	private:
		TypeDescriptor m_StructTy;
	};

	class ASTClass : public ASTNodeBase
	{
	public:
		ASTClass() = default;
		virtual ~ASTClass() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Class; }
		virtual CodegenResult Codegen(CodegenContext&) override;
	};
	


	class ASTLoopControlFlow : public ASTNodeBase
	{
	public:
		ASTLoopControlFlow(TokenType jumpTy);
		virtual ~ASTLoopControlFlow() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::LoopControlFlow; }
		virtual CodegenResult Codegen(CodegenContext&) override;

	private:
		TokenType m_JumpTy;
	};


}