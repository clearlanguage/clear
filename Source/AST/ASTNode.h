#pragma once

#include "Symbols/FunctionCache.h"
#include "Symbols/Type.h"
#include "Core/Value.h"

#include "Lexing/Token.h"
#include "Symbols/SymbolTable.h"
#include "Core/Operator.h"

#include "Symbols/Symbol.h"
#include "Symbols/TypeRegistry.h"

#include <llvm/ADT/SmallVector.h>
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
		Enum, Defer, TypeResolver,TypeSpecifier, TernaryExpression, 
		Switch, ListExpr, StructExpr
	};

	class ASTNodeBase;
	class Module;

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

		std::shared_ptr<clear::Module> ClearModule;
		std::shared_ptr<clear::Module> ClearModuleSecondary; // used for function calls where a function is being called from another module
		std::shared_ptr<TypeRegistry> TypeReg;

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
		virtual Symbol Codegen(CodegenContext&);

		virtual void Print() {}

		void Push(const std::shared_ptr<ASTNodeBase>& child);
		void Remove(const std::shared_ptr<ASTNodeBase>& child);

		void PropagateSymbolTableToChildren();

		void CreateSymbolTable();
		void SetSymbolTable(std::shared_ptr<SymbolTable> tbl);

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
		virtual Symbol Codegen(CodegenContext&) override;

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
		virtual Symbol Codegen(CodegenContext&) override;
		virtual void Print() override;

		inline const OperatorType GetExpression() const { return m_Expression; }

		Symbol HandleMathExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);
		static Symbol HandleMathExpression(Symbol& lhs, Symbol& rhs,   OperatorType type, CodegenContext& ctx,  std::shared_ptr<SymbolTable> tbl);
		static Symbol HandleMathExpressionF(Symbol& lhs, Symbol& rhs,  OperatorType type, CodegenContext& ctx,  std::shared_ptr<SymbolTable> tbl);
		static Symbol HandleMathExpressionSI(Symbol& lhs, Symbol& rhs, OperatorType type, CodegenContext& ctx,  std::shared_ptr<SymbolTable> tbl);
		static Symbol HandleMathExpressionUI(Symbol& lhs, Symbol& rhs, OperatorType type, CodegenContext& ctx,  std::shared_ptr<SymbolTable> tbl);
		static Symbol HandlePointerArithmetic(Symbol& lhs, Symbol& rhs, OperatorType type, CodegenContext& ctx,  std::shared_ptr<SymbolTable> tbl);

	private:
		bool IsMathExpression()    const;
		bool IsCmpExpression()     const;
		bool IsBitwiseExpression() const;
		bool IsLogicalOperator()   const;

		Symbol HandleCmpExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);
		Symbol HandleCmpExpression(Symbol& lhs, Symbol& rhs, CodegenContext& ctx);
		Symbol HandleCmpExpressionF(Symbol& lhs, Symbol& rhs, CodegenContext& ctx);
		Symbol HandleCmpExpressionSI(Symbol& lhs, Symbol& rhs, CodegenContext& ctx);
		Symbol HandleCmpExpressionUI(Symbol& lhs, Symbol& rhs, CodegenContext& ctx);

		Symbol HandleBitwiseExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);
		Symbol HandleLogicalExpression(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);

		Symbol HandleArrayIndex(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);
		Symbol HandleMemberAccess(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);	

		Symbol HandleMember(Symbol& lhs, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);
		Symbol HandleMemberEnum(Symbol& lhs, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);
		Symbol HandleModuleAccess(Symbol& lhs, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);

	private:
		OperatorType m_Expression;
	};

	class ASTVariableDeclaration : public ASTNodeBase
	{
	public:
		ASTVariableDeclaration(const std::string& name);
		virtual ~ASTVariableDeclaration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::VariableDecleration; }
		virtual Symbol Codegen(CodegenContext&) override;

		const auto& GetName() const { return m_Name; }
		const auto& GetResolvedType() const { return m_Type; }

	private:
		std::string m_Name;
		std::shared_ptr<Type> m_Type;
	};

	class ASTVariable : public ASTNodeBase
	{
	public:
		ASTVariable(const std::string& name);
		virtual ~ASTVariable() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Variable; }
		virtual Symbol Codegen(CodegenContext&) override;
		virtual void Print() override;

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
		virtual Symbol Codegen(CodegenContext&);

	private:
		void HandleDifferentTypes(Symbol& storage, Symbol& data, CodegenContext& ctx);

	private:
		AssignmentOperatorType m_Type;
	};


	class ASTFunctionDefinition : public ASTNodeBase
	{
	public:
		ASTFunctionDefinition(const std::string& name);
		virtual ~ASTFunctionDefinition() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionDefinition; }
		virtual Symbol Codegen(CodegenContext&) override;

		const std::string& GetName() const { return m_Name; }
		void SetName(const std::string& name) { m_Name = name;}

		void Instantiate(FunctionInstance& functionData, CodegenContext& ctx);

		void AddGeneric(const std::string& genericName)
		{
			m_GenericTypes.push_back(genericName);
		}

		void AddPrefixParam(const Parameter& param)
		{
			m_PrefixParams.push_back(param);
		}

		const auto& GetParameters() const { return m_ResolvedParams; }
		const auto& GetReturnType() const { return m_ResolvedReturnType; }

		void RegisterGenerics(CodegenContext& ctx);
		void RemoveGenerics(CodegenContext& ctx);

		std::shared_ptr<ASTFunctionDefinition> ShallowCopy();

	private:
		std::string m_Name;

		std::vector<Parameter> m_ResolvedParams;
		llvm::SmallVector<Parameter> m_PrefixParams;
		std::shared_ptr<Type> m_ResolvedReturnType;

		llvm::SmallVector<std::string> m_GenericTypes;
	};

	class ASTFunctionCall : public ASTNodeBase
	{
	public:
		ASTFunctionCall(const std::string& name);
		virtual ~ASTFunctionCall() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionCall; }
		virtual Symbol Codegen(CodegenContext&) override;
				
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
		virtual Symbol Codegen(CodegenContext&) override;

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
		virtual Symbol Codegen(CodegenContext&) override;
	};

	class ASTListExpr : public ASTNodeBase 
	{	
	public:
		ASTListExpr() = default;
		virtual ~ASTListExpr() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::ListExpr; }
		virtual Symbol Codegen(CodegenContext&) override;
	};

	class ASTStructExpr : public ASTNodeBase 
	{
	public:
		ASTStructExpr() = default;
		virtual ~ASTStructExpr() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::StructExpr; }
		virtual Symbol Codegen(CodegenContext&) override;

	private:
		llvm::Constant* GetDefaultValue(llvm::Type* type);
	};

	/* class ASTImport : public ASTNodeBase
	{
	public:
		ASTImport(const std::filesystem::path& filepath, const std::string& alias = "");
		virtual ~ASTImport() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Import; }
		virtual Symbol Codegen(CodegenContext&) override;

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
		virtual Symbol Codegen(CodegenContext&) override;

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
		virtual Symbol Codegen(CodegenContext&) override;

	private:
		void EmitDefaultReturn(CodegenContext& ctx);
	};

	class ASTUnaryExpression : public ASTNodeBase 
	{
	public:
		ASTUnaryExpression(OperatorType type);
		virtual ~ASTUnaryExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::UnaryExpression; }
		virtual Symbol Codegen(CodegenContext&) override;

	private: 
		OperatorType m_Type;
	};

	class ASTIfExpression : public ASTNodeBase 
	{
	public:
		ASTIfExpression() = default;
		virtual ~ASTIfExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::IfExpression; }
		virtual Symbol Codegen(CodegenContext&) override;
	};

	class ASTWhileExpression : public ASTNodeBase
	{
	public:
		ASTWhileExpression();
		virtual ~ASTWhileExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::WhileLoop; }
		virtual Symbol Codegen(CodegenContext&) override;
	};


	class ASTTernaryExpression : public ASTNodeBase
	{
	public:
		ASTTernaryExpression();
		virtual ~ASTTernaryExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::TernaryExpression; }
		virtual Symbol Codegen(CodegenContext&) override;
		virtual void Print() override;
	};

	class ASTTypeSpecifier : public ASTNodeBase
	{
	public:
		ASTTypeSpecifier(const std::string& name);
		virtual ~ASTTypeSpecifier() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::TypeSpecifier; }
		virtual Symbol Codegen(CodegenContext&) override;

	public:
		bool IsVariadic = false;

	private:
		std::string m_Name;
	};

	class ASTForExpression : public ASTNodeBase 
	{
	public:
		ASTForExpression(const std::string& name);
		virtual ~ASTForExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::ForLoop; }
		virtual Symbol Codegen(CodegenContext&) override;
	
	private:
		std::string m_Name;
	};

	class ASTStruct : public ASTNodeBase
	{
	public:
		ASTStruct(const std::string& name);
		virtual ~ASTStruct() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Struct; }
		virtual Symbol Codegen(CodegenContext&) override;

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
		virtual Symbol Codegen(CodegenContext&) override;

		void AddGeneric(std::string_view name)
		{
			m_Generics.push_back(std::string(name));
		}

		void Instantiate(CodegenContext& ctx, llvm::ArrayRef<std::shared_ptr<Type>> aliasTypes = {});

	private:
		std::string m_Name;
		llvm::SmallVector<std::string> m_Generics;
	};

	class ASTTrait : public ASTNodeBase
	{
	public:
		ASTTrait(const std::string& name);
		virtual ~ASTTrait() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Trait; }
		virtual Symbol Codegen(CodegenContext&) override;
	private:
		std::string m_Name;
	};
	
	class ASTLoopControlFlow : public ASTNodeBase
	{
	public:
		ASTLoopControlFlow(std::string jumpTy);
		virtual ~ASTLoopControlFlow() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::LoopControlFlow; }
		virtual Symbol Codegen(CodegenContext&) override;

	private:
		std::string m_JumpTy;
	};

	class ASTDefaultArgument : public ASTNodeBase
	{
	public:
		ASTDefaultArgument(size_t index) : m_Index(index) {};
		virtual ~ASTDefaultArgument() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::DefaultArgument; }
		virtual Symbol Codegen(CodegenContext&) override;

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
		virtual Symbol Codegen(CodegenContext&) override;
	};

	class ASTTryCatch : public ASTNodeBase
	{
	public:
		ASTTryCatch() = default;
		virtual ~ASTTryCatch() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::TryCatch; }
		virtual Symbol Codegen(CodegenContext&) override;
	};

	class ASTDefaultInitializer : public ASTNodeBase
	{
	public:
		ASTDefaultInitializer() = default;
		virtual ~ASTDefaultInitializer() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::DefaultInitializer; }
		virtual Symbol Codegen(CodegenContext&) override;

		static void RecursiveCallConstructors(llvm::Value* value, std::shared_ptr<Type> type, CodegenContext& ctx, std::shared_ptr<SymbolTable> tbl, bool isGlobal = false);

	};

	class ASTEnum : public ASTNodeBase
	{
	public:
		ASTEnum(const std::string& enumName, const std::vector<std::string>& names = {});
		virtual ~ASTEnum() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Enum; }
		virtual Symbol Codegen(CodegenContext&) override;

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
		virtual Symbol Codegen(CodegenContext&) override;
	};

	class ASTTypeResolver : public ASTNodeBase 
	{
	public:
		ASTTypeResolver(const std::vector<Token>& tokens = {});
		virtual ~ASTTypeResolver() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::TypeResolver; }
		virtual Symbol Codegen(CodegenContext&) override; 

		void PushToken(const Token& token)
		{
			m_Tokens.push_back(token);
		}

	private:
		Symbol Inferred();
		std::shared_ptr<Type> ResolveArray(CodegenContext& ctx, size_t& i, int64_t& k);
		std::shared_ptr<Type> ResolveGeneric(Symbol& classTemplate, CodegenContext& ctx, size_t& i, int64_t& k);
	
	private:
		std::vector<Token> m_Tokens;
	};	

	class ASTSwitch : public ASTNodeBase 
	{
	public:
		ASTSwitch() = default;
		virtual ~ASTSwitch() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Switch; }
		virtual Symbol Codegen(CodegenContext&) override; 
	};	
}