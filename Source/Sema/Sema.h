#pragma once 

#include "ConstEval.h"
#include "Core/Value.h"
#include "Diagnostics/DiagnosticCode.h"
#include "Diagnostics/DiagnosticsBuilder.h"
#include "Sema/Infer.h"
#include "AST/ASTNode.h"
#include "Sema/NameMangling.h"
#include "Sema/SymbolTable.h"

#include <memory>
#include <unordered_set>

namespace clear 
{
	enum class ValueRequired : uint8_t
	{
		Any = 0, LValue, RValue
	};
	
	struct SemaContext
	{
		ValueRequired ValueReq = ValueRequired::Any;
		bool AssignmentTarget = false;         // the storage of `x = v`: obj[i] there becomes obj.__setitem__(i, v)
		std::shared_ptr<Type> TypeHint;
		llvm::SmallVector<std::shared_ptr<Type>> CallsiteArgs;
		bool AllowGenericInferenceFromArgs = true;
		bool GlobalState = true;
		bool InLoop = false;
		std::shared_ptr<Type> ReturnType; // of the function being analysed, null for void
		std::shared_ptr<Type> ExpectedType; // what the value being analysed will be converted to (gives lambdas their parameter types)
		ASTFunctionDefinition* InferReturnFor = nullptr; // a lambda whose return type comes from its body
		int CoroutineKind = 0;                 // inside a generator (1) or an async function (2)
		std::shared_ptr<Type> CoroutineValue;  // what a generator yields
	};
	
	struct CompilationUnit;

    class Sema
    {
	public:
		Sema(std::shared_ptr<Module> clearModule, DiagnosticsBuilder& builder, const std::unordered_map<std::filesystem::path, CompilationUnit>& compilationUnits);
		~Sema() = default;

		// names used as base classes anywhere in the program (collected before analysis starts)
		static inline std::unordered_set<std::string> BaseClassNames;

		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTBlock> ast, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTTypeSpecifier> typeSpec, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTVariableDeclaration> decl, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTNodeBase> ast, SemaContext context = {});
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTFunctionDefinition> func, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTFunctionCall> funcCall, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTReturn> returnStatement, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTBinaryExpression> binaryExpression, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTNodeLiteral> literal, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTVariable> variable, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTAssignmentOperator> assignment, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTUnaryExpression> unaryExpr, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTFunctionDeclaration> decl, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTClass> classExpr, SemaContext context);	
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTWhileExpression> whileExpr, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTForExpression> forExpr, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTEnum> enumNode, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTSwitch> switchNode, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTDefer> deferNode, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTAssert> assertNode, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTTupleExpr> tuple, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTLambda> lambda, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTFunctionTypeExpr> type, SemaContext context);
		std::shared_ptr<Type> FunctionTypeOf(const std::shared_ptr<ASTFunctionDefinition>& function);
		std::shared_ptr<Type> GetOptionalType(std::shared_ptr<Type> valueType);
		bool DeclareVariantType(std::shared_ptr<ASTEnum> enumNode);
		bool DeclareVariantBody(std::shared_ptr<ASTEnum> enumNode, SemaContext context);
		void DefineVariant(std::shared_ptr<ASTEnum> enumNode, SemaContext context);
		std::shared_ptr<ASTNodeBase> BuildVariantConstruct(std::shared_ptr<Type> variantType, size_t caseIndex, llvm::ArrayRef<std::shared_ptr<ASTNodeBase>> arguments,
														   const std::vector<std::pair<Token, std::shared_ptr<ASTNodeBase>>>& keywords, const Token& location);
		std::shared_ptr<ASTNodeBase> LowerVariantSwitch(std::shared_ptr<ASTSwitch> switchNode, std::shared_ptr<Type> variantType, SemaContext context);
		void DeclareInGlobalScope(const std::function<void()>& declare);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTDestructure> destructure, SemaContext context);
		std::shared_ptr<ASTNodeBase> VisitLen(std::shared_ptr<ASTFunctionCall> funcCall, SemaContext context);
		std::shared_ptr<ASTNodeBase> VisitMembership(std::shared_ptr<ASTBinaryExpression> expr, SemaContext context);
		std::shared_ptr<ASTNodeBase> AsValue(std::shared_ptr<ASTNodeBase> node);
		std::shared_ptr<ASTNodeBase> CallMethod(std::shared_ptr<ASTNodeBase> object, std::shared_ptr<Type> objectType, const std::string& name,
												std::vector<std::shared_ptr<ASTNodeBase>> arguments, const Token& location);

		// value of an integer expression known at compile time (literals, consts, enum members, arithmetic on those)
		std::optional<int64_t> EvaluateInteger(std::shared_ptr<ASTNodeBase> node);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTStructExpr> structExpr, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTGenericTemplate> generic, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTSubscript> subscript, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTArrayType> arrayType, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTListExpr> listExpr, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTIfExpression> ifExpr, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTImport> importExpr, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTTernaryExpression> ternaryExpr, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTCastExpr> castExpr, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTSizeofExpr> castExpr, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTIsExpr> castExpr, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTLoopControlFlow> controlFlow, SemaContext context);

	private:
		void Report(DiagnosticCode code, Token token);
		void Report(DiagnosticCode code, Token token, size_t width);
		void Warn(DiagnosticCode code, Token token, size_t width);
		std::shared_ptr<ASTNodeBase> VisitDeclaration(std::shared_ptr<ASTVariableDeclaration> decl, SemaContext context);
		void VisitTopLevel(std::shared_ptr<ASTBlock> ast, SemaContext context);

		bool DeclareFunction(std::shared_ptr<ASTFunctionDefinition> func, SemaContext context);
		void DefineFunction(std::shared_ptr<ASTFunctionDefinition> func, SemaContext context);

		bool DeclareClassType(std::shared_ptr<ASTClass> classExpr);
		bool DeclareClassBody(std::shared_ptr<ASTClass> classExpr, SemaContext context);
		bool DeclareClassBodyNow(std::shared_ptr<ASTClass> classExpr, SemaContext context);
		bool CheckTrait(std::shared_ptr<ClassType> classTy, std::shared_ptr<ClassType> trait, const Token& location);
		std::shared_ptr<ClassType> FindTrait(const std::string& name, std::shared_ptr<Module> home);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTMacro> macro, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTYield> yield, SemaContext context);
		std::shared_ptr<ASTNodeBase> Visit(std::shared_ptr<ASTAwait> await, SemaContext context);
		std::shared_ptr<ASTNodeBase> CoroutineMethod(std::shared_ptr<ASTFunctionCall> funcCall, std::shared_ptr<ASTNodeBase> object, std::shared_ptr<Type> type, const Token& name);
		std::shared_ptr<ASTNodeBase> VisitHash(std::shared_ptr<ASTFunctionCall> funcCall, SemaContext context);
		std::shared_ptr<ASTNodeBase> ExpandMacro(std::shared_ptr<ASTMacroCall> call, SemaContext context);
		std::shared_ptr<ASTNodeBase> VisitSuperCall(std::shared_ptr<ASTFunctionCall> funcCall, SemaContext context);
		void EnsureCopyDefined(std::shared_ptr<Type> type);
		bool CastAllowed(const std::shared_ptr<Type>& from, const std::shared_ptr<Type>& to);
		std::string ConversionAdvice(const std::shared_ptr<Type>& from, const std::shared_ptr<Type>& to);
		void AdaptLiterals(std::shared_ptr<ASTBinaryExpression> expr, const SemaContext& context);
		static bool ContainsByValue(const std::shared_ptr<Type>& type, const std::shared_ptr<Type>& target);

		// optionals: `a ?? b`, `a?.b`, `if r:` (r is its value inside), `if not r: return` (and after it)
		struct Narrowing { std::shared_ptr<Symbol> Variable; Token Name; std::shared_ptr<Type> Optional; };
		std::optional<Narrowing> NarrowableOptional(const std::shared_ptr<ASTNodeBase>& node);
		std::shared_ptr<ASTVariableDeclaration> NarrowedDeclaration(const Narrowing& narrowing);
		void CollectNarrowings(const std::shared_ptr<ASTNodeBase>& test, bool whenTrue, std::vector<Narrowing>& narrowings);
		bool EndNarrowing(const std::shared_ptr<ASTAssignmentOperator>& assignment);
		std::unordered_map<ASTVariableDeclaration*, Narrowing> m_NarrowingDeclarations; // the `let r = <value in r>` made for narrowing
		std::unordered_map<Symbol*, Narrowing> m_NarrowedNames;                         // ...and the names they declared
		std::shared_ptr<ASTNodeBase> OptionalTest(std::shared_ptr<ASTNodeBase> value, std::shared_ptr<Type> optional, bool hasValue);
		std::shared_ptr<ASTNodeBase> TestCondition(std::shared_ptr<ASTNodeBase> condition, bool hasValue);
		std::shared_ptr<ASTNodeBase> EvaluatedOnce(std::shared_ptr<ASTNodeBase> value, std::shared_ptr<Type> type);
		// slices: xs[a:b], s[i], len(s), for x in s, and arrays/lists passed where a []T is expected
		std::shared_ptr<ASTNodeBase> VisitSlice(std::shared_ptr<ASTSliceExpr> slice, SemaContext context);
		std::shared_ptr<ASTNodeBase> SliceOf(std::shared_ptr<ASTNodeBase> node, std::shared_ptr<Type> type, const Token& location);
		std::shared_ptr<ASTNodeBase> SliceIntrinsic(const std::string& name, std::shared_ptr<Type> result, std::vector<std::shared_ptr<ASTNodeBase>> arguments, const Token& location);
		std::shared_ptr<ASTNodeBase> VisitCoalesce(std::shared_ptr<ASTBinaryExpression> expr, SemaContext context);
		std::shared_ptr<ASTNodeBase> VisitOptionalChain(std::shared_ptr<ASTBinaryExpression> expr, SemaContext context, std::shared_ptr<ASTFunctionCall> call);
		std::vector<std::shared_ptr<ASTVariableDeclaration>> m_NarrowAfter; // set by `if not r: return`, used by the block it is in
		std::shared_ptr<ASTNodeBase> OwnedValue(std::shared_ptr<ASTNodeBase> node, std::shared_ptr<Type> type);
		std::shared_ptr<ASTNodeBase> ModuleMember(std::shared_ptr<ASTBinaryExpression> access);
		std::shared_ptr<ASTNodeBase> TextConcat(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, const Token& location);
		std::shared_ptr<ASTNodeBase> CallLibraryFunction(const std::string& name, std::vector<std::shared_ptr<ASTNodeBase>> arguments, const Token& location);
		std::shared_ptr<ASTNodeBase> TakeOwnership(std::shared_ptr<ASTNodeBase> node, std::shared_ptr<Type> type);
		std::shared_ptr<ASTNodeBase> WrittenTemporary(std::shared_ptr<ASTNodeBase> storage);
		std::shared_ptr<ASTNodeBase> CompoundValue(AssignmentOperatorType assignType, std::shared_ptr<ASTNodeBase> current, std::shared_ptr<ASTNodeBase> value);
		std::shared_ptr<ASTNodeBase> VisitPropertyAssign(std::shared_ptr<ASTAssignmentOperator> assignmentOp, std::shared_ptr<ASTFunctionCall> getter);
		void DefineClass(std::shared_ptr<ASTClass> classExpr, SemaContext context);
		void EnsureDefined(std::shared_ptr<ASTFunctionDefinition> function);
		bool AlwaysReturns(const std::shared_ptr<ASTNodeBase>& node);

		// converts `node` to `target` if that is implicitly allowed, reporting an error otherwise
		std::shared_ptr<ASTNodeBase> Coerce(std::shared_ptr<ASTNodeBase> node, std::shared_ptr<Type> target);
		bool IsImplicitlyConvertible(std::shared_ptr<Type> from, std::shared_ptr<Type> to, bool fromLiteral);
		bool IsConstantThatFits(std::shared_ptr<ASTNodeBase> node, std::shared_ptr<Type> target);
		std::shared_ptr<ASTNodeBase> CheckCall(std::shared_ptr<ASTFunctionCall> funcCall);
		std::shared_ptr<ASTNodeBase> CheckIndirectCall(std::shared_ptr<ASTFunctionCall> funcCall);
		std::shared_ptr<ASTNodeBase> LowerClassIteration(std::shared_ptr<ASTForExpression> forExpr, std::shared_ptr<ASTNodeBase> iterable);
		std::shared_ptr<ASTNodeBase> CompleteStructValues(std::shared_ptr<ASTStructExpr> structExpr);
		std::shared_ptr<ASTNodeBase> BuildConstruction(std::shared_ptr<ASTFunctionCall> funcCall, std::shared_ptr<ASTVariable> target);
		
		std::shared_ptr<ASTNodeBase> VisitBinaryExprArithmetic(std::shared_ptr<ASTBinaryExpression> binaryExpr, SemaContext context);	

		// operators on classes call their dunder methods (a + b -> a.__add__(b)); nullopt when no overload applies
		std::optional<std::shared_ptr<ASTNodeBase>> TryOperatorOverload(std::shared_ptr<ASTBinaryExpression> expr);
		bool CheckOperands(std::shared_ptr<ASTBinaryExpression> expr);
		std::shared_ptr<ASTNodeBase> AddressOf(std::shared_ptr<ASTNodeBase> node);
		std::shared_ptr<ASTNodeBase> VisitBinaryExprMemberAccess(std::shared_ptr<ASTBinaryExpression> binaryExpr, SemaContext context);	
		std::shared_ptr<ASTNodeBase> VisitBinaryExprBoolean(std::shared_ptr<ASTBinaryExpression> binaryExpr, SemaContext context);
		
		bool IsNodeValue(std::shared_ptr<ASTNodeBase> node);
		std::shared_ptr<Type> GetTypeFromNode(std::shared_ptr<ASTNodeBase> node);
		
		void ConstructSymbol(std::shared_ptr<Symbol> symbol, std::shared_ptr<ASTNodeBase> clonnedNode);
		void ChangeNameOfNode(llvm::StringRef newName, std::shared_ptr<ASTNodeBase> clonnedNode);
		
		std::pair<std::optional<SymbolEntry>, size_t> LookupSymbol(llvm::StringRef name);
		std::optional<std::shared_ptr<Symbol>> LookupInModules(llvm::StringRef name);
		std::shared_ptr<Symbol> InstantiateFromValues(std::shared_ptr<ASTVariable> target, std::shared_ptr<Symbol> genericSymbol, size_t scopeIndex, 
													  llvm::ArrayRef<std::shared_ptr<ASTNodeBase>> values);

		std::shared_ptr<Symbol> SolveConstraints(llvm::StringRef name, std::shared_ptr<Symbol> genericSymbol, size_t scopeIndex, llvm::ArrayRef<Symbol> substitutedArgs);


    private:
		std::vector<SymbolTable> m_ScopeStack;
		std::unordered_map<ASTNodeBase*, std::shared_ptr<Symbol>> m_PendingInstances;
		std::unordered_map<Symbol*, int64_t> m_ConstantValues; // consts whose value is a known integer
		std::optional<int64_t> KnownConstant(Symbol* symbol, std::shared_ptr<Type>* type = nullptr); // also the consts of imported files (type: theirs)
		std::unordered_set<Symbol*> m_ConstSymbols;
		std::unordered_set<std::string> m_FailedDeclarations;
		std::unordered_map<std::string, std::string> m_ImportedFrom; // a name brought in by a plain import: its file
		std::unordered_map<std::string, std::pair<std::string, std::string>> m_AmbiguousImports; // ...defined by two of them
		std::shared_ptr<Module> m_LookupModule; // the home file of a generic being instantiated from another file
		static inline std::unordered_map<std::string, std::shared_ptr<Symbol>> m_GenericInstances; // shared: List[int] is one type in every file
		std::unordered_map<ClassType*, std::shared_ptr<ASTClass>> m_ClassNodes; // so a base class's body can be declared first
		std::unordered_set<ASTClass*> m_ClassesInProgress;
		size_t m_MacroCounter = 0;
		std::unordered_set<Symbol*> m_LocalVariables; // locals and parameters: owning values can be moved out of them
		bool m_ViewsAllowed = false;                  // yield hands out views, it does not take ownership

		// moves out of locals, followed through the function body so an emptied variable is not used again
		using MovedSet = std::unordered_map<Symbol*, Token>; // variable -> where it was moved
		struct BranchMoves
		{
			MovedSet Start; MovedSet Out; bool AnyLive = false;
			std::unordered_map<Symbol*, Token> StaleStart, StaleOut; // pointers made stale, per branch the same way
		};
		struct LoopMoves { size_t FirstLocal = 0; MovedSet AtBreak; MovedSet AtContinue; size_t FirstCandidate = 0; };

		MovedSet m_Moved;
		bool m_Unreachable = false;                     // after return, break or continue nothing runs
		std::vector<LoopMoves> m_LoopMoves;
		std::unordered_map<Symbol*, size_t> m_LocalOrder; // declaration order, to tell variables from outside a loop
		size_t m_LocalCounter = 0;
		std::unordered_set<Type*> m_BorrowingClosures; // lambdas holding pointers to local variables

		// copies out of local variables become moves where the variable is not used again (the last use)
		struct CopyCandidate { Symbol* Variable = nullptr; size_t Use = 0; std::shared_ptr<ASTCopy> Copy; std::shared_ptr<ASTNodeBase> Storage; bool Valid = true; };
		struct FunctionCopies
		{
			std::vector<CopyCandidate> Candidates;
			std::vector<std::shared_ptr<ASTCopy>> Copies;          // every copy made in the function (for --copies)
			std::unordered_map<Symbol*, size_t> Uses;              // how many times each variable was used so far
			std::unordered_map<ASTVariable*, size_t> UseOf;        // which use a variable node was
			std::unordered_set<Symbol*> NeverMove;                 // used in a defer, captured, or looked into by a pointer
			bool InDefer = false;

			// let w = words[i] where w is only read and words does not change meanwhile: w looks at the item instead
			struct View { std::shared_ptr<ASTVariableDeclaration> Declaration; std::shared_ptr<ASTCopy> Copy; Symbol* Variable; Symbol* Source; size_t Since; };
			std::vector<View> Views;
			size_t Clock = 0;                                      // counts uses, in the order they are written
			std::unordered_map<Symbol*, size_t> LastUse;
			std::unordered_map<Symbol*, std::vector<size_t>> Writes; // uses that may change the variable
			std::unordered_set<Symbol*> NotViewable;               // moved out, or used in a loop it was not declared in
		};
		bool m_ReadingUse = false; // the variable being visited is only read (len(x), x.field as a value...)
		std::shared_ptr<ASTNodeBase> AddressOfRead(const std::shared_ptr<ASTNodeBase>& value);
		FunctionCopies m_Copies;
		void NoteUse(const std::shared_ptr<ASTVariable>& variable, ValueRequired valueRequired);
		void NeverMove(const std::shared_ptr<ASTNodeBase>& node);
	public:
		void KeepLentArguments(llvm::ArrayRef<std::shared_ptr<ASTNodeBase>> arguments, size_t firstCandidate);
	private:
		void FinishCopies();

		// lambda x: ... with no types to go on: analysed again for each set of argument types it is called with
		struct LambdaTemplate
		{
			std::shared_ptr<ASTLambda> Lambda;
			std::vector<std::pair<Token, std::shared_ptr<Type>>> Captures;
			std::vector<std::shared_ptr<Type>> ParameterTypes; // null where the lambda did not write one
			std::shared_ptr<Type> DeclaredReturn;
			std::unordered_map<std::string, std::string> Instances; // argument types -> the __call_N__ made for them
		};
		std::unordered_map<Type*, LambdaTemplate> m_LambdaTemplates;
		std::shared_ptr<ASTFunctionDefinition> BuildClosureCall(const std::string& name, const std::shared_ptr<ASTLambda>& lambda, std::shared_ptr<ASTNodeBase> body,
																std::shared_ptr<Type> closureType, const std::vector<std::pair<Token, std::shared_ptr<Type>>>& captures,
																const std::vector<std::shared_ptr<Type>>& parameterTypes, std::shared_ptr<Type> declaredReturn);
		std::shared_ptr<ASTNodeBase> CallLambdaTemplate(std::shared_ptr<ASTFunctionCall> funcCall, std::shared_ptr<Type> calleeType, std::shared_ptr<ClassType> closureType);
		std::string InstantiateLambdaCall(std::shared_ptr<ClassType> closureType, const std::vector<std::shared_ptr<Type>>& argumentTypes, const Token& location);

		static inline std::unordered_set<std::string> m_GenericMethodNames; // shared by every file
		// generic functions' `function(T) -> U` parameters (each given a type parameter __callable_i of its own)
		static inline std::unordered_map<ASTGenericTemplate*, std::unordered_map<std::string, std::shared_ptr<ASTFunctionTypeExpr>>> m_CallablePatterns;
		std::shared_ptr<ASTNodeBase> CallGenericMethod(std::shared_ptr<ASTFunctionCall> funcCall, std::shared_ptr<ASTBinaryExpression> member, std::shared_ptr<Type> objectType,
													   std::shared_ptr<ClassType> classType, const std::string& name);
		bool BindCallable(std::shared_ptr<ASTFunctionTypeExpr> pattern, std::shared_ptr<Type> actual, llvm::ArrayRef<std::string> names,
						  std::unordered_map<std::string, std::shared_ptr<Type>>& bindings, const Token& location);
		ASTVariable* m_Reinitialised = nullptr;         // `x = v`: x is given a new value, not read

		void RecordMove(const std::shared_ptr<ASTVariable>& variable);
		void CheckNotMoved(const std::shared_ptr<ASTVariable>& variable);
		BranchMoves BeginBranches();
		void BeginBranch(BranchMoves& branches);
		void EndBranch(BranchMoves& branches);
		void EndBranches(BranchMoves& branches, bool fallsThrough);
		void BeginLoop();
		void EndLoop(const MovedSet& beforeLoop);

		// let p = &xs[0] ... xs.push(v) ... p: the push may have moved the items, p would point at freed memory
		struct ElementPointer { Symbol* Container = nullptr; std::string ContainerName; std::string ContainerPath; };
		std::unordered_map<Symbol*, ElementPointer> m_ElementPointers;
		std::unordered_map<Symbol*, Token> m_StalePointers; // pointer -> the call that changed its container
		void NoteElementPointer(const std::shared_ptr<ASTVariableDeclaration>& decl);
		void NoteContainerChange(const std::shared_ptr<ASTNodeBase>& container, const Token& change);
		void CheckStalePointer(const std::shared_ptr<ASTVariable>& variable);
		bool m_Returning = false;                     // analysing a return value: locals are moved, not copied
		size_t m_MacroDepth = 0;                      // catches `class A(B)` / `class B(A)`

		struct LazyBody
		{
			std::vector<SymbolTable> Scopes;
			std::shared_ptr<Module> LookupModule;
			std::shared_ptr<Type> ClassTy;
		};

		// generic methods: one template per class and name, a method made for each set of type arguments
		struct GenericMethod
		{
			std::shared_ptr<ASTGenericTemplate> Template;
			LazyBody Context;
			std::unordered_map<std::string, std::string> Instances; // type arguments -> the method made for them
		};
		// shared by every file's analysis: a List[int] made in one file has its methods analysed when another uses them
		static inline std::unordered_map<Type*, std::unordered_map<std::string, GenericMethod>> m_GenericMethods;
		static inline std::unordered_map<ASTFunctionDefinition*, LazyBody> m_LazyBodies; // methods of generic instances not analysed yet
		std::shared_ptr<Module> m_Module;
		DiagnosticsBuilder& m_DiagBuilder;
		ConstEval m_ConstantEvaluator;
		Infer m_TypeInferEngine;
		NameMangler m_NameMangler;
		
		const std::unordered_map<std::filesystem::path, CompilationUnit>& m_CompilationUnits;
    };
}
