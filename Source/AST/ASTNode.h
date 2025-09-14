#pragma once

#include "Core/Log.h"
#include "Symbols/Type.h"
#include "Core/Value.h"

#include "Lexing/Token.h"
#include "Core/Operator.h"

#include "Symbols/Symbol.h"
#include "Symbols/TypeRegistry.h"

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/CodeGen/MachineOperand.h>
#include <llvm/Support/ErrorHandling.h>
#include <memory>
#include <string>
#include <filesystem>

namespace clear 
{
    enum class ASTNodeType
	{
		Base = 0, Literal, BinaryExpression, VariableDecleration,
		FunctionDefinition, FunctionDecleration,
		ReturnStatement, 
		FunctionCall, IfExpression, WhileLoop,
		UnaryExpression, Break, Continue, 
		MemberAccess, AssignmentOperator, Import,  
		Variable,  InferredDecleration, Class, LoopControlFlow, 
		DefaultArgument, DefaultInitializer, 
		Defer, TypeResolver,TypeSpecifier, TernaryExpression, 
		Switch, ListExpr, StructExpr, Block, Load, GenericTemplate,
		Subscript, ArrayType, WhenExpr, CastExpr, SizeofExpr, IsExpr
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

    	CodegenContext(const std::filesystem::path& path, llvm::LLVMContext& context, 
					   llvm::IRBuilder<>& builder, llvm::Module& module) 
			: CurrentDirectory(path), Context(context), Builder(builder), Module(module)
		{
		}
	};
	
	class Sema;

    class ASTNodeBase : public std::enable_shared_from_this<ASTNodeBase>
	{
	public:
		ASTNodeBase();
		virtual ~ASTNodeBase() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::Base; }
		virtual Symbol Codegen(CodegenContext&);
		virtual void Print() {}
	};
	
	class ASTBlock : public ASTNodeBase
	{
	public:
		ASTBlock();
		virtual ~ASTBlock() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Block; }
		virtual Symbol Codegen(CodegenContext&) override;
		
	public:
		std::vector<std::shared_ptr<ASTNodeBase>> Children;
	};

	class ASTNodeLiteral : public ASTNodeBase
	{
	public:
		ASTNodeLiteral(const Token& data);
		virtual ~ASTNodeLiteral() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Literal; }
		virtual Symbol Codegen(CodegenContext&) override;
		
		const auto& GetData() const { return m_Token; }

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
		static Symbol HandleMathExpression(Symbol& lhs, Symbol& rhs,   OperatorType type, CodegenContext& ctx);
		static Symbol HandleMathExpressionF(Symbol& lhs, Symbol& rhs,  OperatorType type, CodegenContext& ctx);
		static Symbol HandleMathExpressionSI(Symbol& lhs, Symbol& rhs, OperatorType type, CodegenContext& ctx);
		static Symbol HandleMathExpressionUI(Symbol& lhs, Symbol& rhs, OperatorType type, CodegenContext& ctx);
		static Symbol HandlePointerArithmetic(Symbol& lhs, Symbol& rhs, OperatorType type, CodegenContext& ctx);


	public:
		std::shared_ptr<ASTNodeBase> LeftSide;
		std::shared_ptr<ASTNodeBase> RightSide;
		std::shared_ptr<Type> ResultantType;

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

		Symbol HandleMemberAccess(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);	

		Symbol HandleMember(Symbol& lhs, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);
		Symbol HandleModuleAccess(Symbol& lhs, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);

	private:
		OperatorType m_Expression;	
	};
	

	class ASTVariableDeclaration : public ASTNodeBase
	{
	public:
		ASTVariableDeclaration(const Token& name);
		virtual ~ASTVariableDeclaration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::VariableDecleration; }
		virtual Symbol Codegen(CodegenContext&) override;
		
		const auto& GetName() const { return m_Name; }
		const auto& GetResolvedType() const { return m_Type; }
	
	public:
		std::shared_ptr<ASTNodeBase> TypeResolver;
		std::shared_ptr<ASTNodeBase> Initializer;
		std::shared_ptr<Symbol> Variable;
		std::shared_ptr<Type> ResolvedType;

	private:
		Token m_Name;
		std::shared_ptr<Type> m_Type;
	};

	class ASTVariable : public ASTNodeBase
	{
	public:
		ASTVariable(const Token& name);
		virtual ~ASTVariable() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Variable; }
		virtual Symbol Codegen(CodegenContext&) override;
		
		virtual void Print() override;

		const auto& GetName() const { return m_Name; }
	
	public:
		std::shared_ptr<Symbol> Variable;

	private:
		Token m_Name;
	};

	enum class AssignmentOperatorType 
	{
		None, Initialize, Normal, Mul, Div, Add, Sub, Mod
	};

	class ASTAssignmentOperator : public ASTNodeBase
	{
	public:
		ASTAssignmentOperator(AssignmentOperatorType type);
		virtual ~ASTAssignmentOperator() = default;
		virtual inline const ASTNodeType GetType() const { return ASTNodeType::AssignmentOperator; }
		virtual Symbol Codegen(CodegenContext&);
	

		AssignmentOperatorType GetAssignType() const { return m_Type; }

	public:
		std::shared_ptr<ASTNodeBase> Storage;
		std::shared_ptr<ASTNodeBase> Value;

	private:
		void HandleDifferentTypes(Symbol& storage, Symbol& data, CodegenContext& ctx);

	private:
		AssignmentOperatorType m_Type;
	};


	
	class ASTTypeSpecifier;
	class ASTDefaultArgument;

	class ASTFunctionDefinition : public ASTNodeBase
	{
	public:
		ASTFunctionDefinition(const std::string& name);
		virtual ~ASTFunctionDefinition() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionDefinition; }
		virtual Symbol Codegen(CodegenContext&) override;
		
		const std::string& GetName() const { return m_Name; }
		void SetName(const std::string& name) { m_Name = name;}

	public:
		std::vector<std::shared_ptr<ASTVariableDeclaration>> Arguments;
		std::shared_ptr<ASTNodeBase> ReturnType;
		std::shared_ptr<Type> ReturnTypeVal;
		std::shared_ptr<ASTBlock> CodeBlock;	
		std::shared_ptr<Module> SourceModule;
		std::shared_ptr<Symbol> FunctionSymbol = std::make_shared<Symbol>(Symbol::CreateFunction(nullptr));	
		llvm::Function::LinkageTypes Linkage = llvm::Function::ExternalLinkage;
		bool IsVariadic = false;

	private:
		std::string m_Name;
	};

	class ASTFunctionCall : public ASTNodeBase
	{
	public:
		ASTFunctionCall() = default;
		virtual ~ASTFunctionCall() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionCall; }
		virtual Symbol Codegen(CodegenContext&) override;
				
	public:
		std::shared_ptr<ASTNodeBase> Callee;
		std::vector<std::shared_ptr<ASTNodeBase>> Arguments;

	private:
		void BuildArgs(CodegenContext& ctx, std::vector<llvm::Value*>& args, std::vector<std::shared_ptr<Type>>& types);
		std::shared_ptr<ASTBinaryExpression> IsMemberFunction();
	};
	
	enum class SubscriptSemantic
	{
		None = 0, ArrayIndex, Generic
	};

	class ASTSubscript : public ASTNodeBase
	{
	public:
		ASTSubscript() = default;
		virtual ~ASTSubscript() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Subscript; }
		virtual Symbol Codegen(CodegenContext&) override;
		
	public:
		std::shared_ptr<ASTNodeBase> Target;
		llvm::SmallVector<std::shared_ptr<ASTNodeBase>> SubscriptArgs;
		SubscriptSemantic Meaning = SubscriptSemantic::None;
		std::shared_ptr<Symbol> GeneratedType;
	};

	class ASTFunctionDeclaration : public ASTNodeBase
	{
	public:
		ASTFunctionDeclaration(const std::string& name);
		virtual ~ASTFunctionDeclaration() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::FunctionDecleration; }
		virtual Symbol Codegen(CodegenContext&) override;

		const auto& GetName() 		{ return m_Name; }

	public:
		std::vector<std::shared_ptr<ASTTypeSpecifier>> Arguments;
		std::shared_ptr<ASTNodeBase> ReturnTypeNode;
		std::shared_ptr<Type> ReturnType;
		std::shared_ptr<Symbol> DeclSymbol;
		bool InsertDecleration = true;

	private:
		std::string m_Name;
	};

	class ASTListExpr : public ASTNodeBase 
	{	
	public:
		ASTListExpr() = default;
		virtual ~ASTListExpr() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::ListExpr; }
		virtual Symbol Codegen(CodegenContext&) override;

	public:
		std::vector<std::shared_ptr<ASTNodeBase>> Values;
		std::shared_ptr<Type> ListType;
	};

	class ASTStructExpr : public ASTNodeBase 
	{
	public:
		ASTStructExpr() = default;
		virtual ~ASTStructExpr() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::StructExpr; }
		virtual Symbol Codegen(CodegenContext&) override;

	public:
		std::vector<std::shared_ptr<ASTNodeBase>> Values;
		std::shared_ptr<ASTNodeBase> TargetType; 

	private:
		llvm::Constant* GetDefaultValue(llvm::Type* type);
	};

	class ASTReturn : public ASTNodeBase 
	{
	public:
		ASTReturn() = default;
		virtual ~ASTReturn() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::ReturnStatement; }
		virtual Symbol Codegen(CodegenContext&) override;
		

	public:
		std::shared_ptr<ASTNodeBase> ReturnValue;

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
		
		OperatorType GetOperatorType() const { return m_Type; }

	public:
		std::shared_ptr<ASTNodeBase> Operand;

	private: 
		OperatorType m_Type;
	};

	class ASTLoad : public ASTNodeBase 
	{
	public:
		ASTLoad() = default;
		virtual ~ASTLoad() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Load; }
		virtual Symbol Codegen(CodegenContext&) override;
	
	public:
		std::shared_ptr<ASTNodeBase> Operand;
	};
	

	struct ConditionalBlock
	{
		std::shared_ptr<ASTNodeBase> Condition;
		std::shared_ptr<ASTBlock> CodeBlock;
	};

	class ASTIfExpression : public ASTNodeBase 
	{
	public:
		ASTIfExpression() = default;
		virtual ~ASTIfExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::IfExpression; }
		virtual Symbol Codegen(CodegenContext&) override;

	public:
		std::vector<ConditionalBlock> ConditionalBlocks;
		std::shared_ptr<ASTBlock> ElseBlock;
	};

	class ASTWhileExpression : public ASTNodeBase
	{
	public:
		ASTWhileExpression();
		virtual ~ASTWhileExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::WhileLoop; }
		virtual Symbol Codegen(CodegenContext&) override;

	public:
		ConditionalBlock WhileBlock;
	};

	class ASTTernaryExpression : public ASTNodeBase
	{
	public:
		ASTTernaryExpression();
		virtual ~ASTTernaryExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::TernaryExpression; }
		virtual Symbol Codegen(CodegenContext&) override;
		virtual void Print() override;

	public:
		std::shared_ptr<ASTNodeBase> Condition;
		std::shared_ptr<ASTNodeBase> Truthy;
		std::shared_ptr<ASTNodeBase> Falsy;
	};
	
	class ASTTypeSpecifier : public ASTNodeBase
	{
	public:
		ASTTypeSpecifier(const std::string& name);
		virtual ~ASTTypeSpecifier() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::TypeSpecifier; }
		virtual Symbol Codegen(CodegenContext&) override;
		
		inline const auto& GetName() const { return m_Name; }

	public:
		bool IsVariadic = false;
		std::shared_ptr<ASTNodeBase> TypeResolver;
		std::shared_ptr<Type> ResolvedType;

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

		inline const auto& GetName() const { return m_Name; } 
		void SetName(llvm::StringRef newName) { m_Name = std::string(newName); }

	public:
		std::vector<std::shared_ptr<ASTTypeSpecifier>> Members;
		std::vector<std::shared_ptr<ASTNodeBase>> DefaultValues;
		std::vector<std::shared_ptr<ASTFunctionDefinition>> MemberFunctions;
		std::shared_ptr<Type> ClassTy;
	
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

	public:
		std::shared_ptr<ASTNodeBase> Value;
		
	private:
		size_t m_Index;
	};

	class ASTDefaultInitializer : public ASTNodeBase
	{
	public:
		ASTDefaultInitializer() = default;
		virtual ~ASTDefaultInitializer() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::DefaultInitializer; }
		virtual Symbol Codegen(CodegenContext&) override;


	public:
		std::shared_ptr<ASTNodeBase> Storage;
	};

	class ASTDefer : public ASTNodeBase 
	{
	public:
		ASTDefer() = default;
		virtual ~ASTDefer() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Defer; }
		virtual Symbol Codegen(CodegenContext&) override;

	public:
		std::shared_ptr<ASTNodeBase> Expr;
		
	};

	struct SwitchCase
	{
		std::vector<std::shared_ptr<ASTNodeBase>> Values;
		std::shared_ptr<ASTBlock> CodeBlock;
	};

	class ASTSwitch : public ASTNodeBase 
	{
	public:
		ASTSwitch() = default;
		virtual ~ASTSwitch() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Switch; }
		virtual Symbol Codegen(CodegenContext&) override; 

	public:
		std::shared_ptr<ASTBlock> DefaultCaseCodeBlock;
		std::vector<SwitchCase> Cases;
		std::shared_ptr<ASTNodeBase> Value;
	};	
	
	class ASTGenericTemplate : public ASTNodeBase
	{
	public:
		ASTGenericTemplate() = default;
		virtual ~ASTGenericTemplate() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::GenericTemplate; }
		virtual Symbol Codegen(CodegenContext&) override { return Symbol(); }
		
		std::string GetName();

	public:
		llvm::SmallVector<std::string> GenericTypeNames;
		std::shared_ptr<ASTNodeBase> TemplateNode;
	};

	class ASTArrayType : public ASTNodeBase 
	{
	public:
		ASTArrayType() = default;
		virtual ~ASTArrayType() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::ArrayType; }
		virtual Symbol Codegen(CodegenContext&) override { return Symbol(); }
		
	public:
		std::shared_ptr<Type> GeneratedArrayType;
		std::shared_ptr<ASTNodeBase> SizeNode;
		std::shared_ptr<ASTNodeBase> TypeNode;
	};

	class ASTImport : public ASTNodeBase 
	{
	public:
		ASTImport() = default;
		virtual ~ASTImport() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Import; }
		virtual Symbol Codegen(CodegenContext&) override { return Symbol(); }

		std::filesystem::path Filepath;
		std::string Namespace;
	};

	class ASTCastExpr : public ASTNodeBase 
	{
	public:
		ASTCastExpr() = default;
		virtual ~ASTCastExpr() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::CastExpr; }
		virtual Symbol Codegen(CodegenContext&) override;

		std::shared_ptr<ASTNodeBase> Object;
		std::shared_ptr<ASTNodeBase> TypeNode;
		std::shared_ptr<Type> TargetType;
	};

	class ASTSizeofExpr : public ASTNodeBase
	{
	public:
		ASTSizeofExpr() = default;
		virtual ~ASTSizeofExpr() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::SizeofExpr; }
		virtual Symbol Codegen(CodegenContext&) override;
		
		std::shared_ptr<ASTNodeBase> Object;
		uint64_t Size = 0;
	};

	class ASTIsExpr : public ASTNodeBase 
	{
	public:
		ASTIsExpr() = default;
		virtual ~ASTIsExpr() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::IsExpr; }
		virtual Symbol Codegen(CodegenContext&) override;
		
		std::shared_ptr<ASTNodeBase> Object;
		std::shared_ptr<ASTNodeBase> TypeNode;
		std::shared_ptr<Type> CompareType;
		bool AreTypesSame = false;
	};
} 
