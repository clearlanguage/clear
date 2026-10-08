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
		std::shared_ptr<Type> TypeHint;
		llvm::SmallVector<std::shared_ptr<Type>> CallsiteArgs;
		bool AllowGenericInferenceFromArgs = true;
		bool GlobalState = true;
		bool InLoop = false;
		std::shared_ptr<Type> ReturnType; // of the function being analysed, null for void
	};
	
	struct CompilationUnit;

    class Sema
    {
	public:
		Sema(std::shared_ptr<Module> clearModule, DiagnosticsBuilder& builder, const std::unordered_map<std::filesystem::path, CompilationUnit>& compilationUnits);
		~Sema() = default;

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
		std::shared_ptr<ASTNodeBase> VisitDeclaration(std::shared_ptr<ASTVariableDeclaration> decl, SemaContext context);

		// converts `node` to `target` if that is implicitly allowed, reporting an error otherwise
		std::shared_ptr<ASTNodeBase> Coerce(std::shared_ptr<ASTNodeBase> node, std::shared_ptr<Type> target);
		bool IsImplicitlyConvertible(std::shared_ptr<Type> from, std::shared_ptr<Type> to, bool fromLiteral);
		
		void VisitBinaryExprArithmetic(std::shared_ptr<ASTBinaryExpression> binaryExpr, SemaContext context);	
		std::shared_ptr<ASTNodeBase> VisitBinaryExprMemberAccess(std::shared_ptr<ASTBinaryExpression> binaryExpr, SemaContext context);	
		std::shared_ptr<ASTNodeBase> VisitBinaryExprBoolean(std::shared_ptr<ASTBinaryExpression> binaryExpr, SemaContext context);
		
		bool IsNodeValue(std::shared_ptr<ASTNodeBase> node);
		std::shared_ptr<Type> GetTypeFromNode(std::shared_ptr<ASTNodeBase> node);
		
		void ConstructSymbol(std::shared_ptr<Symbol> symbol, std::shared_ptr<ASTNodeBase> clonnedNode);
		void ChangeNameOfNode(llvm::StringRef newName, std::shared_ptr<ASTNodeBase> clonnedNode);
		
		std::pair<std::optional<SymbolEntry>, size_t> LookupSymbol(llvm::StringRef name);
		std::shared_ptr<Symbol> InstantiateFromValues(std::shared_ptr<ASTVariable> target, std::shared_ptr<Symbol> genericSymbol, size_t scopeIndex, 
													  llvm::ArrayRef<std::shared_ptr<ASTNodeBase>> values);

		std::shared_ptr<Symbol> SolveConstraints(llvm::StringRef name, std::shared_ptr<Symbol> genericSymbol, size_t scopeIndex, llvm::ArrayRef<Symbol> substitutedArgs);

    private:
		std::vector<SymbolTable> m_ScopeStack;
		std::unordered_map<ASTNodeBase*, std::shared_ptr<Symbol>> m_PendingInstances;
		std::unordered_map<Symbol*, int64_t> m_ConstantValues; // consts whose value is a known integer
		std::unordered_set<Symbol*> m_ConstSymbols;
		std::unordered_set<std::string> m_FailedDeclarations;
		std::shared_ptr<Module> m_Module;
		DiagnosticsBuilder& m_DiagBuilder;
		ConstEval m_ConstantEvaluator;
		Infer m_TypeInferEngine;
		NameMangler m_NameMangler;
		
		const std::unordered_map<std::filesystem::path, CompilationUnit>& m_CompilationUnits;
    };
}
