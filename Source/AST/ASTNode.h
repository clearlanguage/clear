#pragma once

#include "Core/Type.h"
#include "Core/Value.h"

#include "Lexing/Token.h"
#include "SymbolTable.h"
#include "Core/Log.h"
#include "Core/Operator.h"

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
		Variable, ForLoop, InferredDecleration, Class, LoopControlFlow, 
		DefaultArgument, Trait, Raise, TryCatch, DefaultInitializer, 
		Enum, Defer, TypeResolver,TypeSpecifier
	};

	struct CodegenResult
	{
		llvm::Value* CodegenValue = nullptr;
		std::shared_ptr<Type> CodegenType;
		std::vector<std::shared_ptr<Type>> TupleTypes;
		std::vector<llvm::Value*> TupleValues;
		bool IsTuple = false;
		std::string Data; // used for accessing enums currently 
	};

	class ASTNodeBase;

	struct CodegenContext 
	{
		std::filesystem::path CurrentDirectory;
		std::filesystem::path StdLibraryDirectory;

		llvm::LLVMContext& Context;
        llvm::IRBuilder<>& Builder;
        llvm::Module&      Module;

		std::shared_ptr<Type> ReturnType;
		llvm::AllocaInst*  ReturnAlloca = nullptr;
		llvm::BasicBlock*  ReturnBlock = nullptr;
		llvm::BasicBlock* LoopConditionBlock = nullptr;
		llvm::BasicBlock* LoopEndBlock = nullptr;

		bool WantAddress;
		bool Thrown = false;

		std::vector<std::shared_ptr<ASTNodeBase>> DeferredCalls;

    	CodegenContext(const std::filesystem::path& path, llvm::LLVMContext& context, 
					   llvm::IRBuilder<>& builder, llvm::Module& module) 
			: CurrentDirectory(path), Context(context), Builder(builder), Module(module)
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

		void CreateSymbolTable(std::shared_ptr<llvm::LLVMContext> context);

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
		std::optional<Value> m_Value;
	};

	class ASTBinaryExpression : public ASTNodeBase
	{
	public:
		ASTBinaryExpression(OperatorType type);
		virtual ~ASTBinaryExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::BinaryExpression; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		inline const OperatorType GetExpression() const { return m_Expression; }

		CodegenResult HandleMathExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);
		static CodegenResult HandleMathExpression(CodegenResult& lhs, CodegenResult& rhs,   OperatorType type, CodegenContext& ctx,  std::shared_ptr<SymbolTable> tbl);
		static CodegenResult HandleMathExpressionF(CodegenResult& lhs, CodegenResult& rhs,  OperatorType type, CodegenContext& ctx,  std::shared_ptr<SymbolTable> tbl);
		static CodegenResult HandleMathExpressionSI(CodegenResult& lhs, CodegenResult& rhs, OperatorType type, CodegenContext& ctx,  std::shared_ptr<SymbolTable> tbl);
		static CodegenResult HandleMathExpressionUI(CodegenResult& lhs, CodegenResult& rhs, OperatorType type, CodegenContext& ctx,  std::shared_ptr<SymbolTable> tbl);
		static CodegenResult HandlePointerArithmetic(CodegenResult& lhs, CodegenResult& rhs, OperatorType type, CodegenContext& ctx,  std::shared_ptr<SymbolTable> tbl);


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
		CodegenResult HandleMemberAccess(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);	

		CodegenResult HandleMember(CodegenResult& lhs, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);

		std::shared_ptr<StructType> GetStruct(std::shared_ptr<Type> type);

	private:
		OperatorType m_Expression;
	};

	class ASTVariableDeclaration : public ASTNodeBase
	{
	public:
		ASTVariableDeclaration(const std::string& name);
		virtual ~ASTVariableDeclaration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::VariableDecleration; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		const auto& GetName() const { return m_Name; }
		const TypeDescriptor GetVariableType() const { CLEAR_UNREACHABLE("depricated"); return {}; }

	private:
		std::string m_Name;
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

		const std::string& GetName() const { return m_Name; }

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
		ASTFunctionDefinition(const std::string& name);
		virtual ~ASTFunctionDefinition() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionDefinition; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		const std::string& GetName() const { return m_Name; }
		void SetName(const std::string& name) { m_Name = name;}

		void Instantiate(FunctionInstance& functionData, CodegenContext& ctx);

		auto& GetUnresolvedParams() { return m_Parameters; }
		const auto& GetUnresolvedReturnType() const { return m_ReturnType; }

		const auto& GetParameters() const { return m_ResolvedParams; }
		const auto& GetReturnType() const { return m_ResolvedReturnType; }

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
				
		void SetName(const std::string& name) { m_Name = name; }
		const std::string& GetName() const { return m_Name; }

		void PushPrefixArgument(llvm::Value* value, std::shared_ptr<Type> type) 
		{ 
			m_PrefixArguments.emplace_back(value, type); 
		}

	private:
		void BuildArgs(CodegenContext& ctx, std::vector<llvm::Value*>& args, std::vector<Parameter>& params);
		void CastArgs(CodegenContext& ctx, std::vector<llvm::Value*>& args, std::vector<Parameter>& params, FunctionTemplate&);
	
	private:
		std::string m_Name;
		std::vector<std::pair<llvm::Value*, std::shared_ptr<Type>>> m_PrefixArguments; // value, type
	};

	class ASTFunctionDecleration : public ASTNodeBase
	{
	public:
		bool InsertDecleration = true;

	public:
		ASTFunctionDecleration(const std::string& name);
		virtual ~ASTFunctionDecleration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionDecleration; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		const auto& GetParameters() { return m_Parameters; }
		const auto& GetReturnType() { return m_ReturnType; }
		const auto& GetName() 		{ return m_Name; }

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

	class ASTInitializerList : public ASTNodeBase
	{
	public:
		ASTInitializerList(bool firstTime = false) : m_FirstTimeInitialized(firstTime) {};
		virtual ~ASTInitializerList() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::InitializerList; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		void SetIndices(const std::vector<std::vector<size_t>>& indices);

	private:
		void DoInitForArray(CodegenContext& ctx, CodegenResult storage);
		void DoInitForStruct(CodegenContext& ctx, CodegenResult storage);

		std::shared_ptr<Type> GetElementType(std::shared_ptr<Type> type);
		std::shared_ptr<Type> GetInnerType(std::shared_ptr<Type> type, size_t index);


		std::pair<llvm::Value*, std::shared_ptr<Type>> GetBasePointer(size_t index, 
																	  llvm::Value* elemPtr, 
																	  std::shared_ptr<Type> innerType,
																	  size_t startingIndex, 
																	  CodegenContext& ctx, 
																	  size_t offset = 0);

	private:
		std::vector<std::vector<size_t>> m_Indices;
		bool m_FirstTimeInitialized; 
	};

	/* class ASTImport : public ASTNodeBase
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
	}; */

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
		ASTUnaryExpression(OperatorType type);
		virtual ~ASTUnaryExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::UnaryExpression; }
		virtual CodegenResult Codegen(CodegenContext&) override;

	private: 
		OperatorType m_Type;
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
		ASTWhileExpression();
		virtual ~ASTWhileExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::WhileLoop; }
		virtual CodegenResult Codegen(CodegenContext&) override;
	};

	class ASTTypeSpecifier : public ASTNodeBase
	{
	public:
		bool IsVariadic = false;

	public:
		ASTTypeSpecifier(const std::string& name);
		virtual ~ASTTypeSpecifier() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::TypeSpecifier; }
		virtual CodegenResult Codegen(CodegenContext&) override;


	private:
		std::string m_Name;
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
		ASTStruct(const std::string& name);
		virtual ~ASTStruct() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Struct; }
		virtual CodegenResult Codegen(CodegenContext&) override;

	private:
		std::string m_Name;
	};

	class ASTClass : public ASTNodeBase
	{
	public:
		ASTClass() = default;
		ASTClass(const std::string& name);
		virtual ~ASTClass() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Class; }
		virtual CodegenResult Codegen(CodegenContext&) override;

	private:
		std::string m_Name;
	};

	class ASTTrait : public ASTNodeBase
	{
	public:
		ASTTrait(const std::string& name);
		virtual ~ASTTrait() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Trait; }
		virtual CodegenResult Codegen(CodegenContext&) override;
	private:
		std::string m_Name;
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

	class ASTDefaultArgument : public ASTNodeBase
	{
	public:
		ASTDefaultArgument(size_t index) : m_Index(index) {};
		virtual ~ASTDefaultArgument() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::DefaultArgument; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		size_t GetIndex() const { return m_Index; }

	private:
		size_t m_Index;
	};

	class ASTRaise : public ASTNodeBase
	{
	public:
		ASTRaise() = default;
		virtual ~ASTRaise() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Raise; }
		virtual CodegenResult Codegen(CodegenContext&) override;
	};

	class ASTTryCatch : public ASTNodeBase
	{
	public:
		ASTTryCatch() = default;
		virtual ~ASTTryCatch() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::TryCatch; }
		virtual CodegenResult Codegen(CodegenContext&) override;
	};

	class ASTDefaultInitializer : public ASTNodeBase
	{
	public:
		ASTDefaultInitializer() = default;
		virtual ~ASTDefaultInitializer() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::DefaultInitializer; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		static void RecursiveCallConstructors(llvm::Value* value, std::shared_ptr<Type> type, CodegenContext& ctx, std::shared_ptr<SymbolTable> tbl, bool isGlobal = false);

	};

	class ASTEnum : public ASTNodeBase
	{
	public:
		ASTEnum(const std::string& enumName, const std::vector<std::string>& names = {});
		virtual ~ASTEnum() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Enum; }
		virtual CodegenResult Codegen(CodegenContext&) override;

		void AddEnumName(const std::string& name)
		{
			m_Names.push_back(name);
		}

	private:
		std::string m_EnumName;
		std::vector<std::string> m_Names;
	};	

	class ASTDefer : public ASTNodeBase 
	{
	public:
		ASTDefer() = default;
		virtual ~ASTDefer() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Defer; }
		virtual CodegenResult Codegen(CodegenContext&) override;
	};

	class ASTTypeResolver : public ASTNodeBase 
	{
	public:
		ASTTypeResolver(const std::vector<Token>& tokens = {});
		virtual ~ASTTypeResolver() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::TypeResolver; }
		virtual CodegenResult Codegen(CodegenContext&) override; // goal is to return a type

		void PushToken(const Token& token)
		{
			m_Tokens.push_back(token);
		}
			
	private:
		std::vector<Token> m_Tokens;
		std::optional<CodegenResult> m_Type;
	};	
}