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
		Subscript, ArrayType, WhenExpr, CastExpr, SizeofExpr, IsExpr,
		ForLoop, Enum, ConstantValue, Temporary, Zero, Construct, Slot,
		Assert, Contains, Intrinsic
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

		// `defer` statements waiting to run, one list per open block
		std::shared_ptr<std::vector<std::vector<std::shared_ptr<ASTNodeBase>>>> Defers = std::make_shared<std::vector<std::vector<std::shared_ptr<ASTNodeBase>>>>();
		size_t FunctionDeferBase = 0; // first block belonging to the current function
		size_t LoopDeferBase = 0;     // first block inside the innermost loop

		bool RuntimeChecks = false;

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

	public:
		Token Location; // where the node starts in the source, used for diagnostics (may be empty)
	};

	// best source location for a node, looking through nodes created by the compiler itself
	Token GetNodeLocation(const std::shared_ptr<ASTNodeBase>& node);

	// emits the code for the built-in print(...): values separated by spaces, then a newline
	void EmitBuiltinPrint(CodegenContext& ctx, llvm::ArrayRef<Symbol> values);
	
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
		Symbol HandlePower(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, CodegenContext& ctx);
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
		bool IsConst = false;
		bool IsParameter = false;

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
		None, Initialize, Normal, Mul, Div, Add, Sub, Mod,
		BitAnd, BitOr, BitXor, Shl, Shr
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

		const Token& GetNameToken() const { return m_NameToken; }
		void SetNameToken(const Token& token) { m_NameToken = token; }

	public:
		std::vector<std::shared_ptr<ASTVariableDeclaration>> Arguments;
		std::shared_ptr<ASTNodeBase> ReturnType;
		std::shared_ptr<Type> ReturnTypeVal;
		std::shared_ptr<ASTBlock> CodeBlock;	
		std::shared_ptr<Module> SourceModule;
		std::shared_ptr<Symbol> FunctionSymbol = std::make_shared<Symbol>(Symbol::CreateFunction(nullptr));	
		llvm::Function::LinkageTypes Linkage = llvm::Function::ExternalLinkage;
		bool IsVariadic = false;
		bool SignatureResolved = false; // semantic analysis progress, see Sema::DeclareFunction
		bool IsGenericInstance = false; // made from a generic template, reached through the template not by name
		bool BodyResolved = false;

	private:
		std::string m_Name;
		Token m_NameToken;
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
		llvm::SmallVector<std::shared_ptr<ASTNodeBase>> Arguments;
		//TODO: Make a different node for this
		std::shared_ptr<ClassType> ClassType;
		bool IsBuiltinPrint = false; // print(...) is lowered to printf by the compiler



	private:
		void BuildArgs(CodegenContext& ctx, std::vector<llvm::Value*>& args, std::vector<std::shared_ptr<Type>>& types);
		void ConvertArguments(CodegenContext& ctx, llvm::FunctionType* functionType, std::vector<llvm::Value*>& args, std::vector<std::shared_ptr<Type>>& types);
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
		bool IsStorage = false; // `*p` on the left of an assignment: produce the address instead of loading

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

	// for i in start..end / start..=end / for x in array
	class ASTForExpression : public ASTNodeBase
	{
	public:
		ASTForExpression() = default;
		virtual ~ASTForExpression() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::ForLoop; }
		virtual Symbol Codegen(CodegenContext&) override;

	public:
		Token VariableName;
		std::shared_ptr<ASTNodeBase> Start;     // range start, or null when iterating an array
		std::shared_ptr<ASTNodeBase> End;       // range end
		std::shared_ptr<ASTNodeBase> Iterable;  // array being iterated
		bool Inclusive = false;                 // ..= includes the end
		std::shared_ptr<ASTBlock> CodeBlock;

		// filled in by semantic analysis
		std::shared_ptr<Symbol> Variable;
		std::shared_ptr<Type> VariableType;
		std::shared_ptr<Type> IterableType;
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
		bool BodyDeclared = false;
		bool LazyMethods = false; // generic instance: methods are analysed on first use
	
	private:
		std::string m_Name;
	};
	
	class ASTLoopControlFlow : public ASTNodeBase
	{
	public:
		ASTLoopControlFlow(std::string jumpTy, const Token& token = Token());
		virtual ~ASTLoopControlFlow() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::LoopControlFlow; }
		virtual Symbol Codegen(CodegenContext&) override;

		const auto& GetToken() const { return m_Token; }

	private:
		std::string m_JumpTy;
		Token m_Token;
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
		std::vector<int64_t> Constants; // the evaluated values, filled in by semantic analysis
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
	
	// enum Color:
	//     Red
	//     Green = 5
	class ASTEnum : public ASTNodeBase
	{
	public:
		ASTEnum() = default;
		virtual ~ASTEnum() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Enum; }
		virtual Symbol Codegen(CodegenContext&) override { return Symbol(); }

	public:
		Token Name;
		std::vector<std::pair<Token, std::shared_ptr<ASTNodeBase>>> Members; // value is null when automatic
		std::shared_ptr<EnumType> EnumTy;
	};

	// an integer known at compile time (enum members, consts)
	class ASTConstantValue : public ASTNodeBase
	{
	public:
		ASTConstantValue(int64_t value, std::shared_ptr<Type> type) : Value(value), ValueType(type) {}
		virtual ~ASTConstantValue() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::ConstantValue; }
		virtual Symbol Codegen(CodegenContext&) override;

	public:
		int64_t Value = 0;
		std::shared_ptr<Type> ValueType;
	};

	// stores a value in a stack slot and produces its address (used to pass `self` for temporaries)
	class ASTTemporary : public ASTNodeBase
	{
	public:
		ASTTemporary() = default;
		virtual ~ASTTemporary() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Temporary; }
		virtual Symbol Codegen(CodegenContext&) override;

	public:
		std::shared_ptr<ASTNodeBase> Operand;
		std::shared_ptr<Type> ValueType;
	};

	// the all-zero value of a type (used for fields without a default)
	class ASTZero : public ASTNodeBase
	{
	public:
		ASTZero(std::shared_ptr<Type> type) : ValueType(type) {}
		virtual ~ASTZero() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Zero; }
		virtual Symbol Codegen(CodegenContext&) override;

	public:
		std::shared_ptr<Type> ValueType;
	};

	// a value supplied by the node that owns it during code generation (the object being constructed)
	class ASTSlot : public ASTNodeBase
	{
	public:
		ASTSlot(std::shared_ptr<Type> type) : ValueType(type) {}
		virtual ~ASTSlot() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Slot; }
		virtual Symbol Codegen(CodegenContext&) override { return Value; }

	public:
		std::shared_ptr<Type> ValueType; // type of Value (a pointer to the object)
		Symbol Value;
	};

	// Point(1, 2) on a class with __init__: default-initialize a stack value, then run __init__ on it
	class ASTConstruct : public ASTNodeBase
	{
	public:
		ASTConstruct() = default;
		virtual ~ASTConstruct() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Construct; }
		virtual Symbol Codegen(CodegenContext&) override;

	public:
		std::shared_ptr<Type> ClassTy;
		std::shared_ptr<ASTNodeBase> Initial;    // the default-initialized value
		std::shared_ptr<ASTSlot> Self;           // receives the address of the object
		std::shared_ptr<ASTFunctionCall> InitCall;
	};

	// assert condition, "message": stops the program with the location when the condition is false
	class ASTAssert : public ASTNodeBase
	{
	public:
		ASTAssert() = default;
		virtual ~ASTAssert() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Assert; }
		virtual Symbol Codegen(CodegenContext&) override;

	public:
		std::shared_ptr<ASTNodeBase> Condition;
		std::shared_ptr<ASTNodeBase> Message; // optional str
	};

	// `needle in haystack` for fixed arrays: compares every element
	class ASTContains : public ASTNodeBase
	{
	public:
		ASTContains() = default;
		virtual ~ASTContains() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Contains; }
		virtual Symbol Codegen(CodegenContext&) override;

	public:
		std::shared_ptr<ASTNodeBase> Needle;
		std::shared_ptr<ASTNodeBase> Haystack; // the array's storage
		std::shared_ptr<Type> ArrayTy;
		bool Negate = false;
	};

	// a call to a C library routine the compiler uses itself (strlen, strstr, ...), declared on demand
	class ASTIntrinsic : public ASTNodeBase
	{
	public:
		ASTIntrinsic(const std::string& name, std::shared_ptr<Type> resultType) : Name(name), ResultType(resultType) {}
		virtual ~ASTIntrinsic() = default;
		virtual inline const ASTNodeType GetType() const override { return ASTNodeType::Intrinsic; }
		virtual Symbol Codegen(CodegenContext&) override;

	public:
		std::string Name;
		std::vector<std::shared_ptr<ASTNodeBase>> Arguments;
		std::shared_ptr<Type> ResultType;
	};

	// branches to a panic when `ok` is false, code generation continues on the success path
	void EmitCheck(CodegenContext& ctx, llvm::Value* ok, const std::string& message, const Token& location, llvm::Value* detail = nullptr);

	// stops the program: prints "panic: <message>" with the source location to stderr and aborts
	void EmitPanic(CodegenContext& ctx, const std::string& message, const Token& location, llvm::Value* detail = nullptr);

	// runs every pending defer from the innermost block down to `downTo`, newest first
	void EmitDefers(CodegenContext& ctx, size_t downTo);

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
		std::shared_ptr<Module> HomeModule; // the file the template is written in
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
