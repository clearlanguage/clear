#include "Sema.h"
#include "AST/ASTNode.h"
#include "Core/Log.h"
#include "Core/Operator.h"
#include "Core/Value.h"
#include "Diagnostics/Diagnostic.h"
#include "Diagnostics/DiagnosticCode.h"
#include "Diagnostics/DiagnosticsBuilder.h"
#include "Lexing/TokenDefinitions.h"
#include "Sema/SymbolTable.h"
#include "Symbols/Module.h"
#include "Symbols/Symbol.h"
#include "Symbols/Type.h"
#include "Cloner.h"

#include "Compilation/CompilationManager.h"

#include <filesystem>
#include <iterator>
#include <llvm/IR/InlineAsm.h>
#include <llvm/Support/CommandLine.h>
#include <memory>
#include <optional>

namespace clear
{
	static bool IsStorageNode(const std::shared_ptr<ASTNodeBase>& node);
	static std::shared_ptr<Type> ClassOf(std::shared_ptr<Type> type);

    Sema::Sema(std::shared_ptr<Module> clearModule, DiagnosticsBuilder& builder, const std::unordered_map<std::filesystem::path, CompilationUnit>& compilationUnits)
		: m_Module(clearModule), m_DiagBuilder(builder), m_ConstantEvaluator(clearModule), m_TypeInferEngine(clearModule), m_NameMangler(clearModule), 
		  m_CompilationUnits(compilationUnits)
	{
    }

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTBlock> ast, SemaContext context)
	{
		m_ScopeStack.emplace_back();

		if (context.GlobalState)
		{
			VisitTopLevel(ast, context);
		}
		else 
		{
			for(auto& node : ast->Children)
				node = Visit(node, context);
		}

		m_ScopeStack.pop_back();

		return ast;
	}

	void Sema::VisitTopLevel(std::shared_ptr<ASTBlock> ast, SemaContext context)
	{
		// Declarations at the top of a file can be used before the line they are written on, so they are
		// analysed in phases: names and types first, then signatures, then globals, then function bodies.
		auto kindOf = [](const std::shared_ptr<ASTNodeBase>& node) { return node ? node->GetType() : ASTNodeType::Base; };
		auto& children = ast->Children;

		for (auto& node : children)
		{
			ASTNodeType kind = kindOf(node);

			// rich enums are classes underneath, they go through the class phases below
			if (auto enumNode = std::dynamic_pointer_cast<ASTEnum>(node); enumNode && enumNode->IsRich())
			{
				if (!DeclareVariantType(enumNode))
					node = nullptr;

				continue;
			}

			if (kind == ASTNodeType::Import || kind == ASTNodeType::Enum || kind == ASTNodeType::GenericTemplate || kind == ASTNodeType::Macro)
				node = Visit(node, context);
		}

		for (auto& node : children)
		{
			if (auto classNode = std::dynamic_pointer_cast<ASTClass>(node); classNode && !DeclareClassType(classNode))
				node = nullptr;
		}

		for (auto& node : children)
		{
			if (auto classNode = std::dynamic_pointer_cast<ASTClass>(node); classNode && !DeclareClassBody(classNode, context))
				node = nullptr;

			if (auto enumNode = std::dynamic_pointer_cast<ASTEnum>(node); enumNode && enumNode->IsRich() && !DeclareVariantBody(enumNode, context))
				node = nullptr;
		}

		for (auto& node : children)
		{
			if (kindOf(node) == ASTNodeType::FunctionDecleration)
				node = Visit(node, context);
			else if (auto function = std::dynamic_pointer_cast<ASTFunctionDefinition>(node); function && !DeclareFunction(function, context))
				node = nullptr;
		}

		for (auto& node : children)
		{
			switch (kindOf(node))
			{
				case ASTNodeType::Import:
				case ASTNodeType::Enum:
				case ASTNodeType::GenericTemplate:
				case ASTNodeType::Class:
				case ASTNodeType::FunctionDefinition:
				case ASTNodeType::FunctionDecleration:
				case ASTNodeType::Macro:
				case ASTNodeType::Base:
					break;
				default:
					node = Visit(node, context);
					break;
			}
		}

		for (auto& node : children)
		{
			if (auto classNode = std::dynamic_pointer_cast<ASTClass>(node))
				DefineClass(classNode, context);
			else if (auto function = std::dynamic_pointer_cast<ASTFunctionDefinition>(node))
				DefineFunction(function, context);
			else if (auto enumNode = std::dynamic_pointer_cast<ASTEnum>(node); enumNode && enumNode->IsRich())
				DefineVariant(enumNode, context);
		}

		// failed declarations were replaced by null, drop them so code generation never sees them
		std::erase(children, nullptr);

		// the file block (imports scope + file scope): remember it for generics instantiated from other files
		if (m_ScopeStack.size() == 2)
			m_Module->GlobalScopes = m_ScopeStack;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTTypeSpecifier> type, SemaContext context)
	{	
		if (!type->TypeResolver)
		{
			if (!type->IsVariadic)
				Report(DiagnosticCode_ExpectedType, Token(TokenType::Identifier, type->GetName()));
			
			return type;
		}

		if (auto resolved = Visit(type->TypeResolver, context)) type->TypeResolver = resolved;
		type->ResolvedType = GetTypeFromNode(type->TypeResolver);

		if (!type->ResolvedType)
			Report(DiagnosticCode_ExpectedType, GetNodeLocation(type->TypeResolver));

		return type;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTVariableDeclaration> decl, SemaContext context)
	{
		auto result = VisitDeclaration(decl, context);

		if (!result)
			m_FailedDeclarations.insert(decl->GetName().GetData());

		return result;
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitDeclaration(std::shared_ptr<ASTVariableDeclaration> decl, SemaContext context)
	{
		if (decl->TypeResolver)
		{
			if (auto resolved = Visit(decl->TypeResolver, context)) decl->TypeResolver = resolved;
			decl->ResolvedType = GetTypeFromNode(decl->TypeResolver);
			
			if (!decl->ResolvedType)
			{
				Report(DiagnosticCode_ExpectedType, GetNodeLocation(decl->TypeResolver));
				return nullptr;
			}

			context.ValueReq = ValueRequired::RValue;
			if (decl->Initializer)
			{
				context.ExpectedType = decl->ResolvedType;
				decl->Initializer = Visit(decl->Initializer, context);

				if (!decl->Initializer)
					return nullptr; // already reported
			}
			else if (!decl->IsParameter)
			{
				// `let x: int` starts at zero (a class starts with its field defaults), never with garbage
				if (decl->ResolvedType->IsClass())
				{
					auto target = std::make_shared<ASTVariable>(decl->GetName());
					target->Variable = std::make_shared<Symbol>(Symbol::CreateType(decl->ResolvedType));

					auto initial = std::make_shared<ASTStructExpr>();
					initial->Location = decl->GetName();
					initial->TargetType = target;
					decl->Initializer = CompleteStructValues(initial);
				}
				else
				{
					decl->Initializer = std::make_shared<ASTZero>(decl->ResolvedType);
				}
			}
		}
		else 
		{
			if (!decl->Initializer)
			{
				Report(DiagnosticCode_NeedsTypeOrValue, decl->GetName());
				return nullptr;
			}
			
			context.ValueReq = ValueRequired::RValue;
			decl->Initializer = Visit(decl->Initializer, context);

			if (!decl->Initializer)
				return nullptr; // already reported

			decl->ResolvedType = m_TypeInferEngine.InferTypeFromNode(decl->Initializer);

			if (!decl->ResolvedType || decl->ResolvedType->Get()->isVoidTy())
			{
				Report(DiagnosticCode_NeedsTypeOrValue, decl->GetName());
				return nullptr;
			}
		}


		if (decl->TypeResolver && decl->Initializer)
			decl->Initializer = Coerce(decl->Initializer, decl->ResolvedType);

		auto symbol = m_ScopeStack.back().InsertEmpty(decl->GetName().GetData(), SymbolEntryType::Variable);
	
		if (symbol.has_value())
		{
			*symbol.value() = Symbol::CreateValue(nullptr, decl->ResolvedType);
			decl->Variable = symbol.value();

			if (decl->IsConst)
			{
				m_ConstSymbols.insert(decl->Variable.get());

				if (auto value = EvaluateInteger(decl->Initializer); value && decl->ResolvedType->IsIntegral())
				{
					m_ConstantValues[decl->Variable.get()] = *value;
					decl->Initializer = std::make_shared<ASTConstantValue>(*value, decl->ResolvedType);
				}
			}

			if (context.GlobalState)
				m_Module->ExposeSymbol(decl->GetName().GetData(), decl->Variable);

			return decl;
		}
			
		Report(DiagnosticCode_RedefinedIdentifier, decl->GetName());
		return nullptr;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTVariable> variable, SemaContext context)
	{
		if (variable->Variable)
			return variable;

		std::optional<SymbolEntry> symbol;
		size_t scopeIndex = (size_t)-1;	

		for (int64_t i = (int64_t)m_ScopeStack.size() - 1; i >= 0; i--)
		{
			symbol = m_ScopeStack[i].Get(variable->GetName().GetData());
			scopeIndex = (size_t)i;

			if (symbol.has_value())
				break;
		}
		
		if (!symbol.has_value())
		{
			auto sym = LookupInModules(variable->GetName().GetData());
			
			if (sym.has_value())
				symbol = SymbolEntry { SymbolEntryType::None, sym.value() };
		}
		
		if (variable->GetName().GetData() == "Self")
		{
			if (context.TypeHint)
				symbol = { SymbolEntryType::None, std::make_shared<Symbol>(Symbol::CreateType(context.TypeHint)) };
		}
		else if (!symbol.has_value() && variable->GetName().GetData() == "self")
		{
			if (context.TypeHint)
				symbol = { SymbolEntryType::None, std::make_shared<Symbol>(Symbol::CreateType(context.TypeHint)) };
		}

		if (!symbol.has_value())
		{
			// a declaration that already failed should not cause a second error at every use
			if (!m_FailedDeclarations.contains(variable->GetName().GetData()))
				Report(DiagnosticCode_UndeclaredIdentifier, variable->GetName());

			return nullptr;
		}

		if (symbol.value().Symbol->Kind == SymbolKind::GenericTemplate && context.AllowGenericInferenceFromArgs && context.CallsiteArgs.size() > 0)
		{
			llvm::SmallVector<Symbol> transformed(context.CallsiteArgs.size());
			std::transform(context.CallsiteArgs.begin(), context.CallsiteArgs.end(), transformed.begin(), [](auto type) { return Symbol::CreateType(type); });
			symbol.value().Symbol = SolveConstraints(variable->GetName().GetData(), symbol.value().Symbol, scopeIndex, transformed);

			if (!symbol.value().Symbol)
				return nullptr;
		}
		
		variable->Variable = symbol.value().Symbol;

		// a function used as a value (not called) is its address
		if (context.ValueReq == ValueRequired::RValue && variable->Variable->Kind == SymbolKind::Function)
		{
			auto function = variable->Variable->GetFunctionSymbol().FunctionNode;

			if (function && function->SignatureResolved)
			{
				auto reference = std::make_shared<ASTFunctionRef>();
				reference->Location = variable->GetName();
				reference->Function = variable->Variable;
				reference->FunctionTy = FunctionTypeOf(function);
				return reference;
			}
		}

		// reading a const with a known integer value becomes the value itself
		if (context.ValueReq == ValueRequired::RValue)
		{
			if (auto it = m_ConstantValues.find(variable->Variable.get()); it != m_ConstantValues.end())
			{
				auto constant = std::make_shared<ASTConstantValue>(it->second, variable->Variable->GetType());
				constant->Location = variable->GetName();
				return constant;
			}
		}

		if (context.ValueReq == ValueRequired::RValue && symbol.value().Type == SymbolEntryType::Variable)
		{
			auto loadNode = std::make_shared<ASTLoad>();
			loadNode->Operand = variable;

			return loadNode;
		}
		
		return variable;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTNodeBase> ast, SemaContext context)
    {
		if (!ast) return nullptr;

    	switch (ast->GetType()) 
		{
    		case ASTNodeType::FunctionCall:				return Visit(std::dynamic_pointer_cast<ASTFunctionCall>(ast), context);
    		case ASTNodeType::Variable:					return Visit(std::dynamic_pointer_cast<ASTVariable>(ast), context);
    		case ASTNodeType::TypeSpecifier:			return Visit(std::dynamic_pointer_cast<ASTTypeSpecifier>(ast), context);
    		case ASTNodeType::Block:					return Visit(std::dynamic_pointer_cast<ASTBlock>(ast), context);
    		case ASTNodeType::VariableDecleration:		return Visit(std::dynamic_pointer_cast<ASTVariableDeclaration>(ast), context);
			case ASTNodeType::AssignmentOperator:		return Visit(std::dynamic_pointer_cast<ASTAssignmentOperator>(ast), context);
			case ASTNodeType::FunctionDefinition:		return Visit(std::dynamic_pointer_cast<ASTFunctionDefinition>(ast), context);
			case ASTNodeType::ReturnStatement:			return Visit(std::dynamic_pointer_cast<ASTReturn>(ast), context);
			case ASTNodeType::BinaryExpression:			return Visit(std::dynamic_pointer_cast<ASTBinaryExpression>(ast), context);
			case ASTNodeType::Literal:					return Visit(std::dynamic_pointer_cast<ASTNodeLiteral>(ast), context);
			case ASTNodeType::UnaryExpression:			return Visit(std::dynamic_pointer_cast<ASTUnaryExpression>(ast), context);
			case ASTNodeType::FunctionDecleration:		return Visit(std::dynamic_pointer_cast<ASTFunctionDeclaration>(ast), context);
			case ASTNodeType::Class:					return Visit(std::dynamic_pointer_cast<ASTClass>(ast), context);
			case ASTNodeType::IfExpression:				return Visit(std::dynamic_pointer_cast<ASTIfExpression>(ast), context);
			case ASTNodeType::WhileLoop:				return Visit(std::dynamic_pointer_cast<ASTWhileExpression>(ast), context);
			case ASTNodeType::ForLoop:					return Visit(std::dynamic_pointer_cast<ASTForExpression>(ast), context);
			case ASTNodeType::Enum:						return Visit(std::dynamic_pointer_cast<ASTEnum>(ast), context);
			case ASTNodeType::Switch:					return Visit(std::dynamic_pointer_cast<ASTSwitch>(ast), context);
			case ASTNodeType::Defer:					return Visit(std::dynamic_pointer_cast<ASTDefer>(ast), context);
			case ASTNodeType::ConstantValue:			return ast;
			case ASTNodeType::Zero:						return ast;
			case ASTNodeType::Assert:					return Visit(std::dynamic_pointer_cast<ASTAssert>(ast), context);
			case ASTNodeType::Contains:					return ast;
			case ASTNodeType::Intrinsic:
			{
				auto intrinsic = std::dynamic_pointer_cast<ASTIntrinsic>(ast);

				if (intrinsic->Unanalysed)
				{
					intrinsic->Unanalysed = false;
					SemaContext valueContext = context;
					valueContext.ValueReq = ValueRequired::RValue;

					for (auto& argument : intrinsic->Arguments)
					{
						argument = Visit(argument, valueContext);

						if (!argument)
							return nullptr;
					}
				}

				return ast;
			}
			case ASTNodeType::Yield:					return Visit(std::dynamic_pointer_cast<ASTYield>(ast), context);
			case ASTNodeType::Await:					return Visit(std::dynamic_pointer_cast<ASTAwait>(ast), context);
			case ASTNodeType::TupleGet:					return ast;
			case ASTNodeType::FunctionRef:				return ast;
			case ASTNodeType::VTableRef:				return ast;
			case ASTNodeType::VariantConstruct:			return ast;
			case ASTNodeType::VariantField:				return ast;
			case ASTNodeType::VariantTag:				return ast;
			case ASTNodeType::OptionalUnwrap:			return ast;
			case ASTNodeType::OptionalValueOr:			return ast;
			case ASTNodeType::UnionConstruct:			return ast;
			case ASTNodeType::TypeLiteral:				return ast;
			case ASTNodeType::Lambda:					return Visit(std::dynamic_pointer_cast<ASTLambda>(ast), context);
			case ASTNodeType::FunctionTypeExpr:			return Visit(std::dynamic_pointer_cast<ASTFunctionTypeExpr>(ast), context);
			case ASTNodeType::TupleExpr:				return Visit(std::dynamic_pointer_cast<ASTTupleExpr>(ast), context);
			case ASTNodeType::Destructure:				return Visit(std::dynamic_pointer_cast<ASTDestructure>(ast), context);
			case ASTNodeType::Sequence:
			{
				auto& children = std::dynamic_pointer_cast<ASTSequence>(ast)->Children;
				for (auto& child : children)
					child = Visit(child, context);
				std::erase(children, nullptr);
				return ast;
			}
			case ASTNodeType::Macro:					return Visit(std::dynamic_pointer_cast<ASTMacro>(ast), context);
			case ASTNodeType::MacroCall:				return ExpandMacro(std::dynamic_pointer_cast<ASTMacroCall>(ast), context);
			case ASTNodeType::Slot:						return ast;
			case ASTNodeType::Construct:				return ast;
			case ASTNodeType::Load:						return ast; // already analysed (shared default values)
			case ASTNodeType::StructExpr:				return Visit(std::dynamic_pointer_cast<ASTStructExpr>(ast), context);
			case ASTNodeType::GenericTemplate:			return Visit(std::dynamic_pointer_cast<ASTGenericTemplate>(ast), context);
			case ASTNodeType::Subscript:				return Visit(std::dynamic_pointer_cast<ASTSubscript>(ast), context);
			case ASTNodeType::ArrayType:				return Visit(std::dynamic_pointer_cast<ASTArrayType>(ast), context);
			case ASTNodeType::ListExpr:					return Visit(std::dynamic_pointer_cast<ASTListExpr>(ast), context);
			case ASTNodeType::Import:					return Visit(std::dynamic_pointer_cast<ASTImport>(ast), context);
			case ASTNodeType::TernaryExpression:		return Visit(std::dynamic_pointer_cast<ASTTernaryExpression>(ast), context);
			case ASTNodeType::CastExpr:					return Visit(std::dynamic_pointer_cast<ASTCastExpr>(ast), context);
			case ASTNodeType::SizeofExpr:				return Visit(std::dynamic_pointer_cast<ASTSizeofExpr>(ast), context);
			case ASTNodeType::IsExpr:					return Visit(std::dynamic_pointer_cast<ASTIsExpr>(ast), context);
			case ASTNodeType::LoopControlFlow:			return Visit(std::dynamic_pointer_cast<ASTLoopControlFlow>(ast), context);
			case ASTNodeType::DefaultInitializer:		return ast;
    		default:	
    			break;
    	}

		CLEAR_UNREACHABLE("Unhandled ASTNodeType");
		return nullptr;
    }

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTFunctionDefinition> func, SemaContext context)
	{	
		if (!func->SignatureResolved && !DeclareFunction(func, context))
			return func;

		DefineFunction(func, context);
		return func;
	}

	bool Sema::DeclareFunction(std::shared_ptr<ASTFunctionDefinition> func, SemaContext context)
	{
		func->SignatureResolved = true;
		context.GlobalState = false;

		// parameters live in their own scope while the signature is resolved, DefineFunction re-adds them for the body
		m_ScopeStack.emplace_back();

		for (auto arg : func->Arguments)
		{
			if (arg)	
			{
				arg->IsParameter = true;

				// `b: int = 2`: the default is evaluated at each call that leaves b out, not in the callee
				if (arg->Initializer && !arg->DefaultValue)
				{
					arg->DefaultValue = arg->Initializer;
					arg->Initializer = nullptr;
				}

				Visit(arg, context);

				if (arg->DefaultValue && arg->ResolvedType)
				{
					// like Python, a default is evaluated outside the function: it cannot see the parameters
					SymbolTable parameters = std::move(m_ScopeStack.back());
					m_ScopeStack.pop_back();

					SemaContext valueContext = context;
					valueContext.ValueReq = ValueRequired::RValue;
					arg->DefaultValue = Coerce(Visit(arg->DefaultValue, valueContext), arg->ResolvedType);

					m_ScopeStack.push_back(std::move(parameters));
				}
			}
		}
		
		if (func->ReturnType)
		{
			if (auto resolved = Visit(func->ReturnType, context)) func->ReturnType = resolved;
			func->ReturnTypeVal = GetTypeFromNode(func->ReturnType);

			if (!func->ReturnTypeVal)
			{
				Report(DiagnosticCode_ExpectedType, GetNodeLocation(func->ReturnType));
				m_ScopeStack.pop_back();
				return false;
			}
		}

		// async function f() -> T  returns a Task[T];  a function returning Generator[T] is a generator
		if (func->IsAsync)
		{
			func->CoroutineKind = 2;
			func->CoroutineValue = func->ReturnTypeVal && func->ReturnTypeVal->Get() && !func->ReturnTypeVal->Get()->isVoidTy() ? func->ReturnTypeVal : nullptr;
			func->ReturnTypeVal = m_Module->GetTypeRegistry()->GetCoroutineOf(true, func->CoroutineValue);
		}
		else if (auto coroutine = std::dynamic_pointer_cast<CoroutineType>(func->ReturnTypeVal); coroutine && coroutine->GetKind() == CoroutineType::Kind::Generator)
		{
			func->CoroutineKind = 1;
			func->CoroutineValue = coroutine->GetValueType();
		}

		if (context.TypeHint)
			func->SetName(std::format("{}.{}", context.TypeHint->GetHash(), func->GetName()));

		//TODO: temporary, until we have the clear runtime make a main function we will have to ignore mangling for main
		std::string mangledName = func->GetName() != "main" ? m_NameMangler.MangleFunctionFromNode(func) : func->GetName();
		std::optional<std::shared_ptr<Symbol>> symbol;
		
		if (func->IsGenericInstance)
		{
			symbol = func->FunctionSymbol;
		}
		else if (func->FunctionSymbol)
		{
			bool success = m_ScopeStack[m_ScopeStack.size() - 2].Insert(func->GetName(), SymbolEntryType::Function, func->FunctionSymbol);
			symbol = success ? std::optional(func->FunctionSymbol) : std::nullopt;
		}	
		else 
		{
			symbol = m_ScopeStack[m_ScopeStack.size() - 2].InsertEmpty(func->GetName(), SymbolEntryType::Function);
			if (symbol)
				*symbol.value() = Symbol::CreateFunction(nullptr);
		}

		m_ScopeStack.pop_back();

		if (!symbol.has_value())
		{
			Report(DiagnosticCode_RedefinedIdentifier, func->GetNameToken());
			return false;
		}
		
		if (!func->IsGenericInstance)
			m_Module->ExposeSymbol(func->GetName(), symbol.value());

		func->SetName(mangledName);

		std::shared_ptr<Symbol> fnSymbolPtr = symbol.value();
		
		{
			FunctionSymbol& functionSymbol = fnSymbolPtr->GetFunctionSymbol();
			functionSymbol.FunctionNode = func;
			
			//TODO: temporary again
			if (mangledName == "main")
				functionSymbol.FunctionNode->Linkage = llvm::Function::ExternalLinkage;
		}

		func->FunctionSymbol = fnSymbolPtr;
		func->SourceModule = m_Module;	

		return true;
	}

	void Sema::DefineFunction(std::shared_ptr<ASTFunctionDefinition> func, SemaContext context)
	{
		if (func->BodyResolved)
			return;

		func->BodyResolved = true;

		context.GlobalState = false;
		context.ReturnType = func->ReturnTypeVal;
		context.InLoop = false;
		context.CoroutineKind = func->CoroutineKind;
		context.CoroutineValue = func->CoroutineValue;

		// a generator only yields (`return` just ends it), a task's `return` gives its result
		if (func->CoroutineKind == 1)
			context.ReturnType = nullptr;
		else if (func->CoroutineKind == 2)
			context.ReturnType = func->CoroutineValue;
		context.InferReturnFor = func->InferReturnType ? func.get() : nullptr;
		context.ExpectedType = nullptr;

		m_ScopeStack.emplace_back();

		for (auto arg : func->Arguments)
		{
			if (arg && arg->Variable)
				m_ScopeStack.back().Insert(arg->GetName().GetData(), SymbolEntryType::Variable, arg->Variable);
		}

		Visit(func->CodeBlock, context);	
		m_ScopeStack.pop_back();

		// every path through a function with a return type must return a value (main may end and return 0, like C)
		bool returnsValue = func->CoroutineKind ? (func->CoroutineKind == 2 && func->CoroutineValue != nullptr) 
												: func->ReturnTypeVal && func->ReturnTypeVal->Get() && !func->ReturnTypeVal->Get()->isVoidTy();
		bool isMain = func->GetNameToken().GetData() == "main" && !context.TypeHint;

		if (returnsValue && !isMain && !AlwaysReturns(func->CodeBlock))
			Report(DiagnosticCode_MissingReturn, func->GetNameToken());
	}

	static bool ContainsBreak(const std::shared_ptr<ASTNodeBase>& node)
	{
		// a break that leaves *this* loop (breaks inside nested loops belong to those)
		if (!node)
			return false;

		switch (node->GetType())
		{
			case ASTNodeType::LoopControlFlow:
				return std::dynamic_pointer_cast<ASTLoopControlFlow>(node)->GetToken().GetData() == "break";
			case ASTNodeType::Block:
			{
				for (auto& child : std::dynamic_pointer_cast<ASTBlock>(node)->Children)
					if (ContainsBreak(child)) return true;
				return false;
			}
			case ASTNodeType::IfExpression:
			{
				auto ifExpr = std::dynamic_pointer_cast<ASTIfExpression>(node);
				for (auto& block : ifExpr->ConditionalBlocks)
					if (ContainsBreak(block.CodeBlock)) return true;
				return ContainsBreak(ifExpr->ElseBlock);
			}
			case ASTNodeType::Switch:
			{
				auto switchNode = std::dynamic_pointer_cast<ASTSwitch>(node);
				for (auto& switchCase : switchNode->Cases)
					if (ContainsBreak(switchCase.CodeBlock)) return true;
				return ContainsBreak(switchNode->DefaultCaseCodeBlock);
			}
			default:
				return false;
		}
	}

	bool Sema::AlwaysReturns(const std::shared_ptr<ASTNodeBase>& node)
	{
		if (!node)
			return false;

		switch (node->GetType())
		{
			case ASTNodeType::ReturnStatement:
				return true;
			case ASTNodeType::Block:
			{
				for (auto& child : std::dynamic_pointer_cast<ASTBlock>(node)->Children)
					if (AlwaysReturns(child)) return true;
				return false;
			}
			case ASTNodeType::IfExpression:
			{
				auto ifExpr = std::dynamic_pointer_cast<ASTIfExpression>(node);

				if (!ifExpr->ElseBlock)
					return false;

				for (auto& block : ifExpr->ConditionalBlocks)
					if (!AlwaysReturns(block.CodeBlock)) return false;

				return AlwaysReturns(ifExpr->ElseBlock);
			}
			case ASTNodeType::Switch:
			{
				auto switchNode = std::dynamic_pointer_cast<ASTSwitch>(node);

				if (!switchNode->DefaultCaseCodeBlock && !switchNode->IsExhaustive)
					return false;

				for (auto& switchCase : switchNode->Cases)
					if (!AlwaysReturns(switchCase.CodeBlock)) return false;

				return !switchNode->DefaultCaseCodeBlock || AlwaysReturns(switchNode->DefaultCaseCodeBlock);
			}
			case ASTNodeType::Sequence:
			{
				for (auto& child : std::dynamic_pointer_cast<ASTSequence>(node)->Children)
					if (AlwaysReturns(child)) return true;
				return false;
			}
			case ASTNodeType::WhileLoop:
			{
				// `while true:` without a break never falls through
				auto whileLoop = std::dynamic_pointer_cast<ASTWhileExpression>(node);
				auto value = EvaluateInteger(whileLoop->WhileBlock.Condition);
				return value && *value != 0 && !ContainsBreak(whileLoop->WhileBlock.CodeBlock);
			}
			default:
				return false;
		}
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTFunctionCall> funcCall, SemaContext context)
	{
		context.ValueReq = ValueRequired::RValue;

		// f(a, b = 3): `name = value` arguments are keyword arguments, matched to parameters by name in CheckCall
		{
			llvm::SmallVector<std::shared_ptr<ASTNodeBase>> positional;

			for (auto& arg : funcCall->Arguments)
			{
				auto assignment = std::dynamic_pointer_cast<ASTAssignmentOperator>(arg);
				auto name = assignment ? std::dynamic_pointer_cast<ASTVariable>(assignment->Storage) : nullptr;

				if (assignment && name && assignment->GetAssignType() == AssignmentOperatorType::Normal)
				{
					funcCall->KeywordArguments.push_back({ name->GetName(), assignment->Value });
					continue;
				}

				if (!funcCall->KeywordArguments.empty())
				{
					Report(DiagnosticCode_PositionalAfterKeyword, GetNodeLocation(arg));
					return nullptr;
				}

				positional.push_back(arg);
			}

			funcCall->Arguments = positional;

			for (auto& [name, value] : funcCall->KeywordArguments)
			{
				SemaContext valueContext = context;
				valueContext.ValueReq = ValueRequired::RValue;
				value = Visit(value, valueContext);

				if (!value)
					return nullptr;
			}
		}

		// f(values...): a tuple or fixed array becomes one argument per element
		{
			llvm::SmallVector<std::shared_ptr<ASTNodeBase>> expanded;

			for (auto& arg : funcCall->Arguments)
			{
				auto unpack = std::dynamic_pointer_cast<ASTUnaryExpression>(arg);

				if (!unpack || unpack->GetOperatorType() != OperatorType::Ellipsis)
				{
					expanded.push_back(arg);
					continue;
				}

				SemaContext storageContext = context;
				storageContext.ValueReq = ValueRequired::LValue;
				auto source = Visit(unpack->Operand, storageContext);

				if (!source)
					return nullptr;

				auto type = m_TypeInferEngine.InferTypeFromNode(source);
				Token location = GetNodeLocation(source);

				if (!type || (!type->IsTuple() && !type->IsArray()) || !IsStorageNode(source))
				{
					location.SetData(GetDisplayName(type));
					Report(DiagnosticCode_CannotUnpack, location);
					return nullptr;
				}

				size_t count = type->IsTuple() ? type->As<TupleType>()->GetElements().size() : type->As<ArrayType>()->GetArraySize();
				auto int64Type = m_Module->Lookup("int64").value()->GetType();

				for (size_t i = 0; i < count; i++)
				{
					if (type->IsTuple())
					{
						auto get = std::make_shared<ASTTupleGet>();
						get->Location = location;
						get->Tuple = source;
						get->TupleIsStorage = true;
						get->Index = i;
						get->TupleTy = type;
						expanded.push_back(get);
					}
					else
					{
						auto element = std::make_shared<ASTSubscript>();
						element->Location = location;
						element->Target = source;
						element->Meaning = SubscriptSemantic::ArrayIndex;
						element->SubscriptArgs.push_back(std::make_shared<ASTConstantValue>((int64_t)i, int64Type));

						auto load = std::make_shared<ASTLoad>();
						load->Operand = element;
						expanded.push_back(load);
					}
				}
			}

			funcCall->Arguments = expanded;
		}

		// len(x) is built in unless the program defines its own len
		if (auto callee = std::dynamic_pointer_cast<ASTVariable>(funcCall->Callee); callee && !callee->Variable && callee->GetName().GetData() == "len")
		{
			if (!LookupSymbol("len").first)
				return VisitLen(funcCall, context);
		}

		// hash(x) is built in unless the program defines its own hash
		if (auto callee = std::dynamic_pointer_cast<ASTVariable>(funcCall->Callee); callee && !callee->Variable && callee->GetName().GetData() == "hash")
		{
			if (!LookupSymbol("hash").first)
				return VisitHash(funcCall, context);
		}

		// print(...) is built in unless the program defines its own print
		if (auto callee = std::dynamic_pointer_cast<ASTVariable>(funcCall->Callee); callee && !callee->Variable && callee->GetName().GetData() == "print")
		{
			if (!LookupSymbol("print").first)
			{
				funcCall->IsBuiltinPrint = true;

				for (auto& arg : funcCall->Arguments)
				{
					arg = Visit(arg, context);

					if (!arg)
						return nullptr;

					// a class with __str__ prints as whatever that returns
					auto type = m_TypeInferEngine.InferTypeFromNode(arg);

					if (auto classType = ClassOf(type); classType && classType->As<ClassType>()->MemberFunctions.contains("__str__"))
					{
						EnsureDefined(classType->As<ClassType>()->MemberFunctions.at("__str__")->GetFunctionSymbol().FunctionNode);
						arg = CallMethod(arg, type, "__str__", {}, GetNodeLocation(arg));

						if (!arg)
							return nullptr;
					}
				}

				return funcCall;
			}
		}
		
		for (auto& arg : funcCall->Arguments)
		{
			// a lambda without parameter types is analysed once the parameter it goes to is known (in CheckCall)
			if (auto lambda = std::dynamic_pointer_cast<ASTLambda>(arg); lambda && std::any_of(lambda->Parameters.begin(), lambda->Parameters.end(), [](auto& p) { return !p->TypeResolver; }))
			{
				context.CallsiteArgs.push_back(nullptr);
				continue;
			}

			arg = Visit(arg, context);

			if (!arg)
				return nullptr;

			context.CallsiteArgs.push_back(m_TypeInferEngine.InferTypeFromNode(arg));
		}
		
		// Shape.Circle(2.0): a rich enum case with data;  opt.value_or(d)
		if (auto member = std::dynamic_pointer_cast<ASTBinaryExpression>(funcCall->Callee); member && member->GetExpression() == OperatorType::Dot)
		{
			auto left = std::dynamic_pointer_cast<ASTVariable>(member->LeftSide);
			auto right = std::dynamic_pointer_cast<ASTVariable>(member->RightSide);

			if (left && right && !left->Variable)
			{
				auto [entry, scopeIndex] = LookupSymbol(left->GetName().GetData());

				if (entry && entry->Symbol->Kind == SymbolKind::Type && entry->Symbol->GetType()->IsClass() && entry->Symbol->GetType()->As<ClassType>()->IsVariant)
				{
					auto variantType = entry->Symbol->GetType();
					auto index = variantType->As<ClassType>()->FindCase(right->GetName().GetData());

					if (index)
						return BuildVariantConstruct(variantType, *index, funcCall->Arguments, funcCall->KeywordArguments, right->GetName());
				}
			}

			if (right && right->GetName().GetData() == "value_or")
			{
				SemaContext storageContext = context;
				storageContext.ValueReq = ValueRequired::LValue;
				auto subject = Visit(member->LeftSide, storageContext);

				if (!subject)
					return nullptr;

				auto subjectType = m_TypeInferEngine.InferTypeFromNode(subject);

				if (subjectType && subjectType->IsClass() && subjectType->As<ClassType>()->IsOptional)
				{
					if (funcCall->Arguments.size() != 1)
					{
						Token where = right->GetName();
						where.SetData("value_or’ expects 1 argument (the value to use when there is none");
						m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_WrongArgumentCount, 8);
						return nullptr;
					}

					auto classType = subjectType->As<ClassType>();
					auto valueType = classType->Cases[classType->FindCase("some").value()].Fields[0].second;

					auto valueOr = std::make_shared<ASTOptionalValueOr>();
					valueOr->Location = right->GetName();
					valueOr->Subject = AsValue(subject);
					valueOr->Default = Coerce(funcCall->Arguments[0], valueType);
					valueOr->OptionalTy = subjectType;
					return valueOr;
				}

				member->LeftSide = subject;
			}
		}

		// Box(7) on a generic class: infer the type arguments from the values, like Box { 7 }
		if (auto var = std::dynamic_pointer_cast<ASTVariable>(funcCall->Callee); var && !var->Variable)
		{
			auto [entry, scopeIndex] = LookupSymbol(var->GetName().GetData());

			if (entry && entry->Symbol->Kind == SymbolKind::GenericTemplate)
			{
				auto generic = std::dynamic_pointer_cast<ASTGenericTemplate>(entry->Symbol->GetGenericTemplate().GenericTemplate);
				auto classNode = generic ? std::dynamic_pointer_cast<ASTClass>(generic->TemplateNode) : nullptr;
				bool hasInit = classNode && std::any_of(classNode->MemberFunctions.begin(), classNode->MemberFunctions.end(), 
														[](auto& fn) { return fn->GetName() == "__init__"; });

				if (classNode && !hasInit)
				{
					var->Variable = InstantiateFromValues(var, entry->Symbol, scopeIndex, funcCall->Arguments);

					if (!var->Variable)
						return nullptr;
				}

				// max(3, 9) on `function max[T](a: T, b: T)`: T comes from the arguments
				if (generic && generic->TemplateNode->GetType() == ASTNodeType::FunctionDefinition)
				{
					var->Variable = InstantiateFromValues(var, entry->Symbol, scopeIndex, funcCall->Arguments);

					if (!var->Variable)
						return nullptr;
				}
			}
		}

		// the callee is a name or a member, not a value that should be loaded
		SemaContext calleeContext = context;
		calleeContext.ValueReq = ValueRequired::Any;

		// super.method(...) calls the base class's version with the same self
		if (auto member = std::dynamic_pointer_cast<ASTBinaryExpression>(funcCall->Callee); member && member->GetExpression() == OperatorType::Dot)
		{
			if (auto left = std::dynamic_pointer_cast<ASTVariable>(member->LeftSide); left && left->GetName().GetData() == "super")
				return VisitSuperCall(funcCall, context);
		}

		funcCall->Callee = Visit(funcCall->Callee, calleeContext);

		if (!funcCall->Callee)
			return nullptr;

		// max[float64](1, 2): the explicitly instantiated function is called like any other
		// List[int]() constructs the explicitly instantiated class
		if (auto subscript = std::dynamic_pointer_cast<ASTSubscript>(funcCall->Callee); 
			subscript && subscript->Meaning == SubscriptSemantic::Generic && subscript->GeneratedType && subscript->GeneratedType->Kind == SymbolKind::Type)
		{
			auto target = std::dynamic_pointer_cast<ASTVariable>(subscript->Target);
			auto callee = std::make_shared<ASTVariable>(target ? target->GetName() : Token());
			callee->Variable = subscript->GeneratedType;
			funcCall->Callee = callee;
		}

		if (auto subscript = std::dynamic_pointer_cast<ASTSubscript>(funcCall->Callee); 
			subscript && subscript->Meaning == SubscriptSemantic::Generic && subscript->GeneratedType && subscript->GeneratedType->Kind == SymbolKind::Function)
		{
			auto target = std::dynamic_pointer_cast<ASTVariable>(subscript->Target);
			auto callee = std::make_shared<ASTVariable>(target ? target->GetName() : Token());
			callee->Variable = subscript->GeneratedType;
			funcCall->Callee = callee;
		}

		// methods of Generator[T] and Task[T] handles
		if (auto member = std::dynamic_pointer_cast<ASTBinaryExpression>(funcCall->Callee); member && member->GetExpression() == OperatorType::Dot)
		{
			auto left = std::dynamic_pointer_cast<ASTVariable>(member->LeftSide);
			bool isModule = left && left->Variable && left->Variable->Kind == SymbolKind::Module;
			auto objectType = isModule ? nullptr : m_TypeInferEngine.InferTypeFromNode(member->LeftSide);
			auto name = std::dynamic_pointer_cast<ASTVariable>(member->RightSide);

			if (objectType && std::dynamic_pointer_cast<CoroutineType>(objectType) && name)
				return CoroutineMethod(funcCall, member->LeftSide, objectType, name->GetName());
		}

		// calling a value: a function pointer, or an object with __call__ (closures are such objects)
		{
			bool isFunctionName = false;

			if (auto var = std::dynamic_pointer_cast<ASTVariable>(funcCall->Callee); var && var->Variable && 
				(var->Variable->Kind == SymbolKind::Function || var->Variable->Kind == SymbolKind::Type))
				isFunctionName = true;

			if (auto member = std::dynamic_pointer_cast<ASTBinaryExpression>(funcCall->Callee); member && member->GetExpression() == OperatorType::Dot)
			{
				// module.function(...) always names a function
				auto left = std::dynamic_pointer_cast<ASTVariable>(member->LeftSide);
				bool isModule = left && left->Variable && left->Variable->Kind == SymbolKind::Module;

				// obj.method(...) is a method call unless the member is a field holding a function
				auto objectType = isModule ? nullptr : m_TypeInferEngine.InferTypeFromNode(member->LeftSide);
				auto classType = ClassOf(objectType);
				auto name = std::dynamic_pointer_cast<ASTVariable>(member->RightSide);

				isFunctionName = isModule || !classType || !name || classType->As<ClassType>()->MemberFunctions.contains(name->GetName().GetData());
			}

			if (!isFunctionName)
			{
				auto calleeType = m_TypeInferEngine.InferTypeFromNode(funcCall->Callee);

				if (calleeType && calleeType->IsFunction())
				{
					funcCall->Callee = AsValue(funcCall->Callee);
					funcCall->IndirectType = calleeType;
					return CheckIndirectCall(funcCall);
				}

				if (auto classType = ClassOf(calleeType); classType && classType->As<ClassType>()->MemberFunctions.contains("__call__"))
				{
					EnsureDefined(classType->As<ClassType>()->MemberFunctions.at("__call__")->GetFunctionSymbol().FunctionNode);
					std::vector<std::shared_ptr<ASTNodeBase>> arguments(funcCall->Arguments.begin(), funcCall->Arguments.end());
					return CallMethod(funcCall->Callee, calleeType, "__call__", arguments, GetNodeLocation(funcCall->Callee));
				}
			}
		}

		// Point(1, 2) constructs a value of the class
		if (auto var = std::dynamic_pointer_cast<ASTVariable>(funcCall->Callee); 
			var && var->Variable && var->Variable->Kind == SymbolKind::Type && var->Variable->GetType()->IsClass())
		{
			return BuildConstruction(funcCall, var);
		}

		return CheckCall(funcCall);
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTMacro> macro, SemaContext context)
	{
		auto symbol = std::make_shared<Symbol>(Symbol { .Kind = SymbolKind::Macro, .Data = GenericTemplateSymbol { .GenericTemplate = macro } });

		if (!m_ScopeStack.back().Insert(macro->Name.GetData(), SymbolEntryType::None, symbol))
		{
			Report(DiagnosticCode_RedefinedIdentifier, macro->Name);
			return nullptr;
		}

		if (context.GlobalState)
			m_Module->ExposeSymbol(macro->Name.GetData(), symbol);

		return macro;
	}

	static bool IsStatementNode(const std::shared_ptr<ASTNodeBase>& node)
	{
		switch (node->GetType())
		{
			case ASTNodeType::VariableDecleration:
			case ASTNodeType::AssignmentOperator:
			case ASTNodeType::IfExpression:
			case ASTNodeType::WhileLoop:
			case ASTNodeType::ForLoop:
			case ASTNodeType::ReturnStatement:
			case ASTNodeType::Switch:
			case ASTNodeType::Defer:
			case ASTNodeType::Assert:
			case ASTNodeType::LoopControlFlow:
			case ASTNodeType::Destructure:
				return true;
			default:
				return false;
		}
	}

	std::shared_ptr<ASTNodeBase> Sema::ExpandMacro(std::shared_ptr<ASTMacroCall> call, SemaContext context)
	{
		auto [entry, scopeIndex] = LookupSymbol(call->Name.GetData());

		if (!entry || entry->Symbol->Kind != SymbolKind::Macro)
		{
			Report(DiagnosticCode_UndeclaredIdentifier, call->Name);
			return nullptr;
		}

		auto macro = std::dynamic_pointer_cast<ASTMacro>(entry->Symbol->GetGenericTemplate().GenericTemplate);

		if (call->Arguments.size() != macro->Parameters.size())
		{
			Token where = call->Name;
			where.SetData(std::format("{}!’ expects {} argument{}, but {} {} given", call->Name.GetData(), macro->Parameters.size(), 
									  macro->Parameters.size() == 1 ? "" : "s", call->Arguments.size(), call->Arguments.size() == 1 ? "was" : "were"));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_WrongArgumentCount, call->Name.GetData().size());
			return nullptr;
		}

		if (m_MacroDepth >= 64)
		{
			Report(DiagnosticCode_MacroTooDeep, call->Name);
			return nullptr;
		}

		// paste the body: parameters become the caller's syntax, the body's own locals get names nobody else can write
		Cloner cloner;
		cloner.DestinationModule = m_Module;
		cloner.HygieneSuffix = std::format(".m{}", m_MacroCounter++);

		for (size_t i = 0; i < macro->Parameters.size(); i++)
			cloner.ExpressionMap[macro->Parameters[i]] = call->Arguments[i];

		auto body = std::dynamic_pointer_cast<ASTBlock>(cloner.Clone(macro->Body));

		m_MacroDepth++;
		std::shared_ptr<ASTNodeBase> result;

		// a body that is one expression is an expression macro: square!(x) has a value
		if (body->Children.size() == 1 && !IsStatementNode(body->Children[0]))
		{
			result = Visit(body->Children[0], context);
		}
		else
		{
			auto sequence = std::make_shared<ASTSequence>();
			sequence->Location = call->Location;
			sequence->Children.assign(body->Children.begin(), body->Children.end());
			result = Visit(sequence, context);
		}

		m_MacroDepth--;
		return result;
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitSuperCall(std::shared_ptr<ASTFunctionCall> funcCall, SemaContext context)
	{
		auto member = std::dynamic_pointer_cast<ASTBinaryExpression>(funcCall->Callee);
		auto superToken = std::dynamic_pointer_cast<ASTVariable>(member->LeftSide)->GetName();
		auto name = std::dynamic_pointer_cast<ASTVariable>(member->RightSide);

		auto self = std::make_shared<ASTVariable>(Token(TokenType::Identifier, "self", superToken.GetSourceFile(), superToken.LineNumber, superToken.ColumnNumber));
		auto [entry, scopeIndex] = LookupSymbol("self");
		auto selfType = entry && entry->Symbol->Kind == SymbolKind::Value ? entry->Symbol->GetType() : nullptr;
		auto classType = ClassOf(selfType);
		auto base = classType ? classType->As<ClassType>()->Base : nullptr;

		if (!base || !name)
		{
			Report(DiagnosticCode_NoSuperclass, superToken);
			return nullptr;
		}

		auto method = base->MemberFunctions.find(name->GetName().GetData());

		if (method == base->MemberFunctions.end())
		{
			Report(DiagnosticCode_UnknownMember, name->GetName());
			return nullptr;
		}

		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;

		auto selfValue = Visit(self, valueContext);

		if (!selfValue)
			return nullptr;

		auto callee = std::make_shared<ASTVariable>(name->GetName());
		callee->Variable = method->second;

		// a plain (non-virtual) call: super.sound() must not dispatch back to the override
		auto call = std::make_shared<ASTFunctionCall>();
		call->Location = funcCall->Location;
		call->Callee = callee;
		call->Arguments.push_back(selfValue);
		call->Arguments.append(funcCall->Arguments.begin(), funcCall->Arguments.end());
		call->KeywordArguments = funcCall->KeywordArguments;

		EnsureDefined(method->second->GetFunctionSymbol().FunctionNode);
		return CheckCall(call);
	}

	std::shared_ptr<ASTNodeBase> Sema::CheckCall(std::shared_ptr<ASTFunctionCall> funcCall)
	{
		std::shared_ptr<ASTFunctionDefinition> function;
		std::shared_ptr<ClassType> methodClass; // the receiver's static class, for virtual dispatch
		bool isMethod = false;
		Token location = GetNodeLocation(funcCall->Callee);

		if (auto var = std::dynamic_pointer_cast<ASTVariable>(funcCall->Callee))
		{
			if (!var->Variable)
				return funcCall;

			if (var->Variable->Kind != SymbolKind::Function)
			{
				Report(DiagnosticCode_NotCallable, var->GetName());
				return nullptr;
			}

			function = var->Variable->GetFunctionSymbol().FunctionNode;
		}
		else if (auto member = std::dynamic_pointer_cast<ASTBinaryExpression>(funcCall->Callee); member && member->GetExpression() == OperatorType::Dot)
		{
			auto name = std::dynamic_pointer_cast<ASTVariable>(member->RightSide);
			auto left = std::dynamic_pointer_cast<ASTVariable>(member->LeftSide);

			if (!name)
				return funcCall;

			location = name->GetName();

			if (left && left->Variable && left->Variable->Kind == SymbolKind::Module)
			{
				// module.function(...)
				auto& exposed = left->Variable->GetModule()->GetExposedSymbols();
				auto it = exposed.find(name->GetName().GetData());

				if (it != exposed.end() && it->second->Kind == SymbolKind::Function)
					function = it->second->GetFunctionSymbol().FunctionNode;
			}
			else
			{
				// object.method(...): the object is passed as the hidden first argument
				auto objectType = m_TypeInferEngine.InferTypeFromNode(member->LeftSide);

				while (objectType && objectType->IsPointer())
					objectType = objectType->As<PointerType>()->GetBaseType();

				if (objectType && objectType->IsClass())
				{
					auto symbol = objectType->As<ClassType>()->GetMember(name->GetName().GetData());

					if (symbol && symbol.value()->Kind == SymbolKind::Function)
					{
						function = symbol.value()->GetFunctionSymbol().FunctionNode;
						methodClass = objectType->As<ClassType>();
						isMethod = true;
					}
					else if (symbol)
					{
						Report(DiagnosticCode_NotCallable, name->GetName());
						return nullptr;
					}
				}
			}
		}

		if (!function)
			return funcCall;

		EnsureDefined(function);

		size_t offset = isMethod ? 1 : 0;
		size_t expected = function->Arguments.size() >= offset ? function->Arguments.size() - offset : 0;

		// place keyword arguments by name, then fill what is still missing from parameter defaults
		if (!funcCall->KeywordArguments.empty() || funcCall->Arguments.size() < expected)
		{
			std::vector<std::shared_ptr<ASTNodeBase>> slots(std::max(expected, funcCall->Arguments.size()));

			for (size_t i = 0; i < funcCall->Arguments.size(); i++)
				slots[i] = funcCall->Arguments[i];

			for (auto& [name, value] : funcCall->KeywordArguments)
			{
				auto it = std::find_if(function->Arguments.begin() + offset, function->Arguments.end(), 
									   [&](auto& parameter) { return parameter && parameter->GetName().GetData() == name.GetData(); });

				if (it == function->Arguments.end())
				{
					Token where = name;
					where.SetData(std::format("{}’ is not a parameter of ‘{}", name.GetData(), location.GetData()));
					m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_UnknownKeyword, name.GetData().size());
					return nullptr;
				}

				size_t index = std::distance(function->Arguments.begin(), it) - offset;

				if (slots[index])
				{
					Token where = name;
					where.SetData(std::format("{}’ is given more than once (‘{}", name.GetData(), name.GetData()));
					m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_UnknownKeyword, name.GetData().size());
					return nullptr;
				}

				slots[index] = value;
			}

			for (size_t i = 0; i < expected; i++)
			{
				if (!slots[i] && function->Arguments[i + offset]->DefaultValue)
					slots[i] = function->Arguments[i + offset]->DefaultValue;
			}

			// the arguments now are positional, up to the last one that was given
			while (!slots.empty() && !slots.back())
				slots.pop_back();

			if (std::find(slots.begin(), slots.end(), nullptr) != slots.end())
			{
				size_t missing = std::distance(slots.begin(), std::find(slots.begin(), slots.end(), nullptr));
				Token where = location;
				where.SetData(std::format("{}’ is missing a value for ‘{}", location.GetData(), function->Arguments[missing + offset]->GetName().GetData()));
				m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_WrongArgumentCount, std::max<size_t>(location.GetData().size(), 1));
				return nullptr;
			}

			funcCall->Arguments.assign(slots.begin(), slots.end());
			funcCall->KeywordArguments.clear();
		}

		size_t given = funcCall->Arguments.size();

		if (function->IsVariadic ? given < expected : given != expected)
		{
			Token where = location;
			where.SetData(std::format("{}’ expects {}{} argument{}, but {} {} given", where.GetData(), function->IsVariadic ? "at least " : "", 
									  expected, expected == 1 ? "" : "s", given, given == 1 ? "was" : "were"));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_WrongArgumentCount, std::max<size_t>(location.GetData().size(), 1));
			return nullptr;
		}

		for (size_t i = 0; i < expected; i++)
		{
			auto parameterType = function->Arguments[i + offset] ? function->Arguments[i + offset]->ResolvedType : nullptr;

			if (funcCall->Arguments[i]->GetType() == ASTNodeType::Lambda)
			{
				funcCall->Arguments[i] = Visit(funcCall->Arguments[i], SemaContext { .ValueReq = ValueRequired::RValue, .GlobalState = false, .ExpectedType = parameterType });

				if (!funcCall->Arguments[i])
					return nullptr;
			}

			funcCall->Arguments[i] = Coerce(funcCall->Arguments[i], parameterType);
		}

		// a virtual method runs the receiver's own version, looked up in its vtable at run time
		if (methodClass && function->IsVirtual)
		{
			auto slot = std::find(methodClass->VirtualNames.begin(), methodClass->VirtualNames.end(), function->GetNameToken().GetData());

			if (slot != methodClass->VirtualNames.end())
				funcCall->VirtualSlot = std::distance(methodClass->VirtualNames.begin(), slot);
		}

		return funcCall;
	}

	std::shared_ptr<ASTNodeBase> Sema::CheckIndirectCall(std::shared_ptr<ASTFunctionCall> funcCall)
	{
		auto functionType = funcCall->IndirectType->As<FunctionPointerType>();
		auto& parameters = functionType->GetParameters();
		Token location = GetNodeLocation(funcCall->Callee);

		if (!funcCall->KeywordArguments.empty() || funcCall->Arguments.size() != parameters.size())
		{
			location.SetData(std::format("{}’ expects {} argument{}, but {} {} given (function values take no keyword arguments", location.GetData(), parameters.size(), 
										 parameters.size() == 1 ? "" : "s", funcCall->Arguments.size(), funcCall->Arguments.size() == 1 ? "was" : "were"));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_WrongArgumentCount, 1);
			return nullptr;
		}

		for (size_t i = 0; i < parameters.size(); i++)
		{
			if (funcCall->Arguments[i]->GetType() == ASTNodeType::Lambda)
			{
				funcCall->Arguments[i] = Visit(funcCall->Arguments[i], SemaContext { .ValueReq = ValueRequired::RValue, .GlobalState = false, .ExpectedType = parameters[i] });

				if (!funcCall->Arguments[i])
					return nullptr;
			}

			funcCall->Arguments[i] = Coerce(funcCall->Arguments[i], parameters[i]);
		}

		return funcCall;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTReturn> returnStatement, SemaContext context)
	{
		context.ValueReq = ValueRequired::RValue;

		bool hadValue = returnStatement->ReturnValue != nullptr;
		returnStatement->ReturnValue = Visit(returnStatement->ReturnValue, context);

		if (hadValue && !returnStatement->ReturnValue)
			return nullptr; // the value itself was already reported

		// a lambda's return type is whatever its body produces
		if (context.InferReturnFor && returnStatement->ReturnValue && !context.InferReturnFor->ReturnTypeVal)
		{
			context.InferReturnFor->ReturnTypeVal = m_TypeInferEngine.InferTypeFromNode(returnStatement->ReturnValue);

			if (context.InferReturnFor->ReturnTypeVal && context.InferReturnFor->ReturnTypeVal->Get()->isVoidTy())
				context.InferReturnFor->ReturnTypeVal = nullptr;

			context.ReturnType = context.InferReturnFor->ReturnTypeVal;
		}

		bool returnsValue = context.ReturnType && context.ReturnType->Get() && !context.ReturnType->Get()->isVoidTy();

		// lambda (i: int): print(i)  returns nothing: run the body, then return
		if (context.InferReturnFor && !returnsValue && returnStatement->ReturnValue)
		{
			auto valueType = m_TypeInferEngine.InferTypeFromNode(returnStatement->ReturnValue);

			if (!valueType || (valueType->Get() && valueType->Get()->isVoidTy()))
			{
				auto block = std::make_shared<ASTBlock>();
				block->Children.push_back(returnStatement->ReturnValue);
				returnStatement->ReturnValue = nullptr;
				block->Children.push_back(returnStatement);
				return block;
			}
		}

		if (returnsValue && !returnStatement->ReturnValue)
			Report(DiagnosticCode_MissingReturnValue, returnStatement->Location);
		else if (!returnsValue && returnStatement->ReturnValue)
			Report(DiagnosticCode_ReturnTypeMismatch, GetNodeLocation(returnStatement->ReturnValue));
		else if (returnsValue)
			returnStatement->ReturnValue = Coerce(returnStatement->ReturnValue, context.ReturnType);

		return returnStatement;
	}
	
	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTBinaryExpression> binaryExpression, SemaContext context)
	{	
		switch (binaryExpression->GetExpression()) 
		{
			case OperatorType::Add:
			case OperatorType::Sub:
			case OperatorType::Div:
			case OperatorType::Mul:
			case OperatorType::Mod:
			case OperatorType::Power:
			case OperatorType::BitwiseAnd:
			case OperatorType::BitwiseOr:
			case OperatorType::BitwiseXor:
			case OperatorType::LeftShift:
			case OperatorType::RightShift:
			{
				return VisitBinaryExprArithmetic(binaryExpression, context);
			}
			case OperatorType::In:
			case OperatorType::NotIn:
			{
				return VisitMembership(binaryExpression, context);
			}
			case OperatorType::And:
			case OperatorType::Or:
			case OperatorType::GreaterThan:
			case OperatorType::GreaterThanEqual:
			case OperatorType::LessThan:
			case OperatorType::LessThanEqual:
			case OperatorType::IsEqual:
			case OperatorType::NotEqual:
			{
				return VisitBinaryExprBoolean(binaryExpression, context);
			};
			case OperatorType::Dot:
			{
				return VisitBinaryExprMemberAccess(binaryExpression, context);
			}
			default:
			{
				Report(DiagnosticCode_InvalidOperator, Token());
				return nullptr;
			}
		}

		return binaryExpression;
		
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTNodeLiteral> literal, SemaContext context)
	{
		return literal;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTAssignmentOperator> assignmentOp, SemaContext context)
	{
		context.ValueReq = ValueRequired::LValue;

		SemaContext storageContext = context;
		storageContext.AssignmentTarget = true;
		assignmentOp->Storage = Visit(assignmentOp->Storage, storageContext);
		// auto type = m_TypeInferEngine.InferTypeFromNode(assignmentOp->Storage);
		//if (type->IsConst() || type->As<PointerType>()->GetBaseType()->IsConst()) {
		//	CLEAR_LOG_ERROR("WRITING TO CONST BAD!!");
			//Report(DiagnosticCode_AssignConst, Token());
		//}
	
		context.ValueReq = ValueRequired::RValue;

		if (assignmentOp->Storage && assignmentOp->Storage->GetType() != ASTNodeType::FunctionCall)
			context.ExpectedType = m_TypeInferEngine.InferTypeFromNode(assignmentOp->Storage);

		assignmentOp->Value = Visit(assignmentOp->Value, context);

		if (!assignmentOp->Storage || !assignmentOp->Value)
			return assignmentOp;

		if (auto var = std::dynamic_pointer_cast<ASTVariable>(assignmentOp->Storage); var && m_ConstSymbols.contains(var->Variable.get()))
		{
			Report(DiagnosticCode_AssignToConst, var->GetName());
			return nullptr;
		}

		// the value must convert to the type being stored into (for `x += v` too, so `count += 0.5` on an int is an error)
		if (assignmentOp->Storage->GetType() != ASTNodeType::FunctionCall)
		{
			std::shared_ptr<Type> storageType = m_TypeInferEngine.InferTypeFromNode(assignmentOp->Storage);

			// pointer += n is pointer arithmetic, not a conversion
			if (storageType && !(storageType->IsPointer() && assignmentOp->GetAssignType() != AssignmentOperatorType::Normal))
				assignmentOp->Value = Coerce(assignmentOp->Value, storageType);
		}

		// obj.name = v on a property calls its setter
		if (auto getter = std::dynamic_pointer_cast<ASTFunctionCall>(assignmentOp->Storage); getter && !getter->PropertyName.empty())
			return VisitPropertyAssign(assignmentOp, getter);

    	if (assignmentOp->Storage->GetType() == ASTNodeType::FunctionCall) 
		{
    		auto funcCallNode = std::dynamic_pointer_cast<ASTFunctionCall>(assignmentOp->Storage);
    		auto clsType = funcCallNode->ClassType;

			// only `obj[i] = v` on a class is rewritten, assigning to any other call result is an error
			if (!clsType)
			{
				Report(DiagnosticCode_AssignToRValue, GetNodeLocation(assignmentOp->Storage));
				return nullptr;
			}

			if (!clsType->MemberFunctions.contains("__setitem__"))
			{
				Report(DiagnosticCode_MissingIndexOverload, GetNodeLocation(assignmentOp->Storage));
				return nullptr;
			}

			auto setFunc = clsType->MemberFunctions.at("__setitem__");
			EnsureDefined(setFunc->GetFunctionSymbol().FunctionNode);

			// obj[k] += v  is  obj.__setitem__(k, obj.__getitem__(k) + v)
			std::shared_ptr<ASTNodeBase> current = funcCallNode;

			if (assignmentOp->GetAssignType() != AssignmentOperatorType::Normal)
			{
				auto getter = std::make_shared<ASTFunctionCall>();
				getter->Location = funcCallNode->Location;
				getter->Callee = funcCallNode->Callee;
				getter->Arguments = funcCallNode->Arguments;
				current = CheckCall(getter);

				if (!current)
					return nullptr;
			}

			auto value = CompoundValue(assignmentOp->GetAssignType(), current, assignmentOp->Value);

			if (!value)
				return nullptr;

    		auto funcCall = std::make_shared<ASTFunctionCall>();
			funcCall->Location = funcCallNode->Location;
    		funcCall->Arguments = funcCallNode->Arguments;
    		funcCall->Arguments.push_back(value);

    		auto var = std::make_shared<ASTVariable>(Token(TokenType::Identifier, "__setitem__", funcCall->Location.GetSourceFile(), funcCall->Location.LineNumber, funcCall->Location.ColumnNumber));
    		var->Variable = setFunc;

    		funcCall->Callee = var;
    		return CheckCall(funcCall);


    	}

		return assignmentOp;
	}

	std::shared_ptr<ASTNodeBase> Sema::CompoundValue(AssignmentOperatorType assignType, std::shared_ptr<ASTNodeBase> current, std::shared_ptr<ASTNodeBase> value)
	{
		static const std::unordered_map<AssignmentOperatorType, OperatorType> compound = {
			{ AssignmentOperatorType::Add, OperatorType::Add }, { AssignmentOperatorType::Sub, OperatorType::Sub },
			{ AssignmentOperatorType::Mul, OperatorType::Mul }, { AssignmentOperatorType::Div, OperatorType::Div },
			{ AssignmentOperatorType::Mod, OperatorType::Mod }, { AssignmentOperatorType::BitAnd, OperatorType::BitwiseAnd },
			{ AssignmentOperatorType::BitOr, OperatorType::BitwiseOr }, { AssignmentOperatorType::BitXor, OperatorType::BitwiseXor },
			{ AssignmentOperatorType::Shl, OperatorType::LeftShift }, { AssignmentOperatorType::Shr, OperatorType::RightShift },
		};

		auto op = compound.find(assignType);

		if (op == compound.end())
			return value;

		auto binary = std::make_shared<ASTBinaryExpression>(op->second);
		binary->LeftSide = current;
		binary->RightSide = value;

		if (auto overload = TryOperatorOverload(binary))
			return overload.value();

		if (!CheckOperands(binary))
			return nullptr;

		binary->ResultantType = m_TypeInferEngine.InferTypeFromNode(binary);
		return binary;
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitPropertyAssign(std::shared_ptr<ASTAssignmentOperator> assignmentOp, std::shared_ptr<ASTFunctionCall> getter)
	{
		auto self = getter->Arguments[0];
		auto classType = ClassOf(m_TypeInferEngine.InferTypeFromNode(self));
		auto setter = classType ? classType->As<ClassType>()->MemberFunctions.find("__set_" + getter->PropertyName) : decltype(classType->As<ClassType>()->MemberFunctions.end()){};

		if (!classType || setter == classType->As<ClassType>()->MemberFunctions.end())
		{
			Token location = GetNodeLocation(getter->Callee);
			location.SetData(getter->PropertyName);
			Report(DiagnosticCode_PropertyNotSettable, location);
			return nullptr;
		}

		// obj.name += v  is  obj.name = obj.name + v
		auto value = CompoundValue(assignmentOp->GetAssignType(), getter, assignmentOp->Value);

		if (!value)
			return nullptr;

		EnsureDefined(setter->second->GetFunctionSymbol().FunctionNode);

		auto callee = std::make_shared<ASTVariable>(GetNodeLocation(getter->Callee));
		callee->Variable = setter->second;

		auto call = std::make_shared<ASTFunctionCall>();
		call->Location = getter->Location;
		call->Callee = callee;
		call->Arguments = { self, value };
		return CheckCall(call);
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTUnaryExpression> unaryExpr, SemaContext context)
	{
		switch (unaryExpr->GetOperatorType())
		{
			case OperatorType::PostIncrement: // increment and decrement need to operate on an lvalue and always return an rvalueSema.cpp
			case OperatorType::PostDecrement:
			case OperatorType::Ellipsis:
			case OperatorType::Increment: 
			case OperatorType::Decrement:
			case OperatorType::Address:
			{
				SemaContext storageContext = context;
				storageContext.ValueReq = ValueRequired::LValue;
				unaryExpr->Operand = Visit(unaryExpr->Operand, storageContext);

				if (!unaryExpr->Operand)
					return nullptr;

				bool modifies = unaryExpr->GetOperatorType() != OperatorType::Address;

				if (auto var = std::dynamic_pointer_cast<ASTVariable>(unaryExpr->Operand); modifies && var && m_ConstSymbols.contains(var->Variable.get()))
				{
					Report(DiagnosticCode_AssignToConst, var->GetName());
					return nullptr;
				}

				break;
			}
			case OperatorType::Dereference:
			{
				if (auto literal = std::dynamic_pointer_cast<ASTNodeLiteral>(unaryExpr->Operand); literal && literal->GetData().GetData() == "null")
				{
					Report(DiagnosticCode_NullDereference, literal->GetData());
					return nullptr;
				}

				// `*p` as a storage location is simply the pointer value held in p
				if (context.ValueReq == ValueRequired::LValue)
				{
					SemaContext valueContext = context;
					valueContext.ValueReq = ValueRequired::RValue;
					unaryExpr->Operand = Visit(unaryExpr->Operand, valueContext);
					unaryExpr->IsStorage = true;
					return unaryExpr;
				}

				unaryExpr->Operand = Visit(unaryExpr->Operand, context);
				break;
			}
			default:
			{
				unaryExpr->Operand = Visit(unaryExpr->Operand, context);
				break;
			}
		}

		return unaryExpr;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTFunctionDeclaration> decl, SemaContext context)
	{
		size_t k = 0;

		for (auto arg : decl->Arguments)
		{
			if (arg->IsVariadic)
			{
				if (k + 1 != decl->Arguments.size())
					Report(DiagnosticCode_VariadicNotLast, Token());
			}
			
			Visit(arg);
			k++;
		}
		
		decl->ReturnType = m_Module->Lookup("void").value()->GetType();

		if (decl->ReturnTypeNode)
		{
			decl->ReturnTypeNode = Visit(decl->ReturnTypeNode);
			decl->ReturnType = GetTypeFromNode(decl->ReturnTypeNode);
		}
			
		std::shared_ptr<Symbol> symbol = std::make_shared<Symbol>(Symbol::CreateFunction(std::make_shared<ASTFunctionDefinition>("")));

		auto& function = symbol->GetFunctionSymbol();
		function.FunctionNode->ReturnTypeVal = decl->ReturnType->Get()->isVoidTy() ? nullptr : decl->ReturnType;
		function.FunctionNode->SetName(decl->GetName());

		for (auto& arg : decl->Arguments)
		{
			if (arg->IsVariadic)
			{
				function.FunctionNode->IsVariadic = true;
				break;
			}

			auto parameter = std::make_shared<ASTVariableDeclaration>(Token(TokenType::Identifier, arg->GetName()));
			parameter->ResolvedType = arg->ResolvedType;
			function.FunctionNode->Arguments.push_back(parameter);
		}

		bool success = m_ScopeStack.back().Insert(decl->GetName(), SymbolEntryType::FunctionDeclaration, symbol);

		if (!success)
		{
			Report(DiagnosticCode_RedefinedIdentifier, Token(TokenType::Identifier, decl->GetName()));
			return decl;
		}

		decl->DeclSymbol = symbol;
		m_Module->ExposeSymbol(decl->GetName(), symbol);
		return decl;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTClass> classExpr, SemaContext context) 
	{
		if (!DeclareClassType(classExpr) || !DeclareClassBody(classExpr, context))
			return nullptr;

		if (classExpr->LazyMethods)
		{
			for (auto& method : classExpr->MemberFunctions)
				m_LazyBodies[method.get()] = LazyBody { m_ScopeStack, m_LookupModule, classExpr->ClassTy };

			// the vtable refers to every virtual method, so those are always needed
			for (auto& method : classExpr->MemberFunctions)
			{
				if (method->IsVirtual)
					EnsureDefined(method);
			}

			return classExpr;
		}

		DefineClass(classExpr, context);
		return classExpr;
	}

	void Sema::EnsureDefined(std::shared_ptr<ASTFunctionDefinition> function)
	{
		if (!function || function->BodyResolved)
			return;

		auto it = m_LazyBodies.find(function.get());

		if (it == m_LazyBodies.end())
			return;

		LazyBody body = std::move(it->second);
		m_LazyBodies.erase(it);

		// analyse the body with the names that were visible where the class was instantiated
		std::vector<SymbolTable> callerScopes = std::move(m_ScopeStack);
		auto previousLookup = m_LookupModule;

		m_ScopeStack = std::move(body.Scopes);
		m_LookupModule = body.LookupModule;

		DefineFunction(function, SemaContext { .TypeHint = body.ClassTy });

		m_ScopeStack = std::move(callerScopes);
		m_LookupModule = previousLookup;
	}

	bool Sema::DeclareClassType(std::shared_ptr<ASTClass> classExpr)
	{
		if (classExpr->ClassTy)
			return true;

		if (m_Module->GetTypeRegistry()->GetType(classExpr->GetName()))
		{
			Report(DiagnosticCode_RedefinedIdentifier, Token(TokenType::Identifier, classExpr->GetName()));
			return false;
		}

		// the (still empty) type exists from here on, so classes can refer to each other in any order
		auto classTy = m_Module->GetTypeRegistry()->CreateType<ClassType>(classExpr->GetName(), classExpr->GetName(), *m_Module->GetContext());
		classExpr->ClassTy = classTy;
		classTy->IsTrait = classExpr->IsTrait;
		m_ClassNodes[classTy.get()] = classExpr;

		// a generic instance being created: make the type visible now so it can name itself (e.g. `self: *Box[T]`)
		if (auto it = m_PendingInstances.find(classExpr.get()); it != m_PendingInstances.end())
			*it->second->GetGeneric().GeneratedSymbol = Symbol::CreateType(classTy);

		m_Module->ExposeSymbol(classExpr->GetName(), std::make_shared<Symbol>(Symbol::CreateType(classTy)));
		return true;
	}

	bool Sema::DeclareClassBody(std::shared_ptr<ASTClass> classExpr, SemaContext context)
	{
		if (m_ClassesInProgress.contains(classExpr.get()))
		{
			Report(DiagnosticCode_InheritanceCycle, classExpr->Location.GetData().empty() ? Token(TokenType::Identifier, classExpr->GetName()) : classExpr->Location);
			return false;
		}

		if (classExpr->BodyDeclared)
			return true;

		classExpr->BodyDeclared = true;
		m_ClassesInProgress.insert(classExpr.get());
		bool success = DeclareClassBodyNow(classExpr, context);
		m_ClassesInProgress.erase(classExpr.get());

		return success;
	}

	bool Sema::DeclareClassBodyNow(std::shared_ptr<ASTClass> classExpr, SemaContext context)
	{
		auto classTy = classExpr->ClassTy->As<ClassType>();
		std::vector<std::pair<std::string, std::shared_ptr<Symbol>>> members;
		std::vector<std::shared_ptr<ASTNodeBase>> defaults;
		std::shared_ptr<ClassType> base;
		Token location = classExpr->Location.GetData().empty() ? Token(TokenType::Identifier, classExpr->GetName()) : classExpr->Location;

		// class Dog(Animal, Named): at most one base class, any number of traits
		for (auto& baseNode : classExpr->Bases)
		{
			SemaContext typeContext = context;
			typeContext.ValueReq = ValueRequired::Any;
			baseNode = Visit(baseNode, typeContext);

			auto type = baseNode ? GetTypeFromNode(baseNode) : nullptr;
			auto baseClass = type && type->IsClass() ? type->As<ClassType>() : nullptr;

			if (!baseClass || baseClass->IsVariant || baseClass->IsUnion || baseClass == classTy)
			{
				Token where = baseNode ? GetNodeLocation(baseNode) : location;
				Report(baseClass == classTy ? DiagnosticCode_InheritanceCycle : DiagnosticCode_InvalidBase, where);
				return false;
			}

			// the base's own body (fields, methods, vtable) must be known before it is copied in
			if (auto it = m_ClassNodes.find(baseClass.get()); it != m_ClassNodes.end() && !DeclareClassBody(it->second, context))
				return false;

			if (baseClass->IsTrait)
			{
				classTy->Traits.push_back(baseClass);
				continue;
			}

			if (base || classExpr->IsTrait || classExpr->IsUnion)
			{
				Report(DiagnosticCode_InvalidBase, GetNodeLocation(baseNode));
				return false;
			}

			base = baseClass;
		}

		// the base's fields come first, in the same order, so a *Dog can be used wherever a *Animal is expected
		if (base)
		{
			classTy->Base = base;
			size_t index = 0;

			for (const auto& [name, type] : base->GetMemberValues())
			{
				members.emplace_back(name, std::make_shared<Symbol>(Symbol::CreateType(type)));
				defaults.push_back(index < base->MemberDefaults.size() ? base->MemberDefaults[index] : nullptr);
				index++;
			}
		}

		for (auto node : classExpr->Members) 
		{
			Visit(node, context);

			if (!node->ResolvedType)
				return false;

			if (std::any_of(members.begin(), members.end(), [&](auto& member) { return member.first == node->GetName(); }))
			{
				Report(DiagnosticCode_RedefinedIdentifier, Token(TokenType::Identifier, node->GetName()));
				return false;
			}

			members.emplace_back(node->GetName(), std::make_shared<Symbol>(Symbol::CreateType(node->ResolvedType)));
		}

		for (size_t i = 0; i < classExpr->DefaultValues.size(); i++)
		{
			auto& node = classExpr->DefaultValues[i];

			if (node)
			{
				SemaContext valueContext = context;
				valueContext.ValueReq = ValueRequired::RValue;
				node = Visit(node, valueContext);
				node = Coerce(node, classExpr->Members[i]->ResolvedType);
			}

			defaults.push_back(node);
		}

		std::unordered_set<std::string> ownMethods;

		for (auto node : classExpr->MemberFunctions)
		{
			auto functionSymbol = std::make_shared<Symbol>(Symbol::CreateFunction(node));
			members.emplace_back(node->GetName(), functionSymbol);
			node->FunctionSymbol = functionSymbol;
			ownMethods.insert(node->GetName());
		}

		// methods the class does not define itself are the base's (they take a *Base, which a *Derived converts to)
		if (base)
		{
			for (auto& [name, symbol] : base->MemberFunctions)
			{
				if (!ownMethods.contains(name))
					members.emplace_back(name, symbol);
			}
		}

		// a class that inherits or is inherited from dispatches every method on the object's real type:
		// the table starts as the base's, overriding methods replace their slot, new methods add one
		bool inHierarchy = !classExpr->IsTrait && !classExpr->IsUnion &&
						   (base || BaseClassNames.contains(classExpr->GetName()) || BaseClassNames.contains(classExpr->TemplateName));

		if (inHierarchy)
		{
			for (auto node : classExpr->MemberFunctions)
				node->IsVirtual = node->GetName() != "__init__";
		}

		if (base)
		{
			classTy->VirtualNames = base->VirtualNames;
			classTy->VTable = base->VTable;
		}

		for (auto node : classExpr->MemberFunctions)
		{
			auto slot = std::find(classTy->VirtualNames.begin(), classTy->VirtualNames.end(), node->GetName());

			if (slot != classTy->VirtualNames.end())
			{
				node->IsVirtual = true;
				classTy->VTable[std::distance(classTy->VirtualNames.begin(), slot)] = node->FunctionSymbol;
			}
			else if (node->IsVirtual && !classExpr->IsTrait)
			{
				classTy->VirtualNames.push_back(node->GetName());
				classTy->VTable.push_back(node->FunctionSymbol);
			}
		}

		classTy->HasVTable = inHierarchy || !classTy->VirtualNames.empty();

		if (classTy->HasVTable)
		{
			// the hidden first field points at the class's table, every way of building a value fills it in
			if (!base || !base->HasVTable)
			{
				if (base)
				{
					Report(DiagnosticCode_VirtualNeedsTable, location);
					return false;
				}

				auto bytePointer = m_Module->GetTypeRegistry()->GetPointerTo(m_Module->Lookup("int8").value()->GetType());
				members.insert(members.begin(), { "__vtable", std::make_shared<Symbol>(Symbol::CreateType(bytePointer)) });
				defaults.insert(defaults.begin(), nullptr);
			}

			auto table = std::make_shared<ASTVTableRef>();
			table->ClassTy = classTy;
			table->PointerTy = members[0].second->GetType();
			defaults[0] = table;
		}

		classTy->MemberDefaults = defaults;

		if (classExpr->IsUnion)
			classTy->SetUnionBody(members);
		else
			classTy->SetBody(members);

		context.TypeHint = classTy;
		
		for (auto node : classExpr->MemberFunctions)
			DeclareFunction(node, context);

		for (auto& trait : classTy->Traits)
		{
			if (!CheckTrait(classTy, trait, location))
				return false;
		}

		return true;
	}

	std::shared_ptr<ClassType> Sema::FindTrait(const std::string& name, std::shared_ptr<Module> home)
	{
		std::shared_ptr<Symbol> symbol;

		if (home && home != m_Module)
		{
			auto& exposed = home->GetExposedSymbols();
			if (auto it = exposed.find(name); it != exposed.end())
				symbol = it->second;
		}

		if (!symbol)
		{
			auto [entry, scopeIndex] = LookupSymbol(name);
			if (entry)
				symbol = entry->Symbol;
		}

		if (!symbol || symbol->Kind != SymbolKind::Type || !symbol->GetType()->IsClass() || !symbol->GetType()->As<ClassType>()->IsTrait)
			return nullptr;

		return symbol->GetType()->As<ClassType>();
	}

	bool Sema::CheckTrait(std::shared_ptr<ClassType> classTy, std::shared_ptr<ClassType> trait, const Token& location)
	{
		// a parameter typed with the trait (or *Trait) stands for the class (or *Class)
		auto translate = [&](std::shared_ptr<Type> type) -> std::shared_ptr<Type>
		{
			if (!type)
				return type;

			if (type == trait)
				return classTy;

			if (type->IsPointer() && type->As<PointerType>()->GetBaseType() == trait)
				return m_Module->GetTypeRegistry()->GetPointerTo(classTy);

			return type;
		};

		auto describe = [&](std::shared_ptr<ASTFunctionDefinition> function, const std::string& name)
		{
			std::string text = name + "(self";

			for (size_t i = 1; i < function->Arguments.size(); i++)
				text += std::format(", {}", GetDisplayName(translate(function->Arguments[i]->ResolvedType)));

			text += ")";

			if (function->ReturnTypeVal)
				text += " -> " + GetDisplayName(translate(function->ReturnTypeVal));

			return text;
		};

		for (auto& [name, symbol] : trait->MemberFunctions)
		{
			auto required = symbol->GetFunctionSymbol().FunctionNode;
			auto found = classTy->MemberFunctions.find(name);
			bool matches = found != classTy->MemberFunctions.end();

			if (matches)
			{
				auto actual = found->second->GetFunctionSymbol().FunctionNode;
				matches = actual && actual->Arguments.size() == required->Arguments.size() && translate(required->ReturnTypeVal) == actual->ReturnTypeVal;

				for (size_t i = 1; matches && i < required->Arguments.size(); i++)
					matches = translate(required->Arguments[i]->ResolvedType) == actual->Arguments[i]->ResolvedType;
			}

			if (!matches)
			{
				Token where = location;
				where.SetData(std::format("‘{}’ needs ‘{}’ to satisfy ‘{}’", classTy->GetHash(), describe(required, std::string(name)), trait->GetHash()));
				m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_TraitNotSatisfied, std::max<size_t>(location.GetData().size(), 1));
				return false;
			}
		}

		return true;
	}

	void Sema::DefineClass(std::shared_ptr<ASTClass> classExpr, SemaContext context)
	{
		// a trait only has signatures, there is nothing to analyse or generate
		if (classExpr->IsTrait)
			return;

		context.TypeHint = classExpr->ClassTy;

		for (auto node : classExpr->MemberFunctions)
		{
			if (node->SignatureResolved && node->FunctionSymbol)
				DefineFunction(node, context);
		}
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTIfExpression> ifExpr, SemaContext context)
	{
		SemaContext conditionContext = context;
		conditionContext.ValueReq = ValueRequired::RValue;

		for (auto& conditionalBlock : ifExpr->ConditionalBlocks)
		{
			conditionalBlock.Condition = Visit(conditionalBlock.Condition, conditionContext);
			Visit(conditionalBlock.CodeBlock, context);
		}
		
		if (ifExpr->ElseBlock)
			Visit(ifExpr->ElseBlock, context);

		return ifExpr;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTImport> importExpr, SemaContext context)
	{
		// the loader already resolved the import to an absolute path
		auto it = m_CompilationUnits.find(importExpr->Filepath);
		if (it == m_CompilationUnits.end())
		{
			Report(DiagnosticCode_ImportNotFound, Token(TokenType::String, importExpr->Filepath.string()));
			return nullptr;
		}
		
		if (!importExpr->Namespace.empty())
		{
			m_Module->InsertModule(importExpr->Namespace, it->second.CompilationModule);
			m_ScopeStack.front().Insert(importExpr->Namespace, SymbolEntryType::None, std::make_shared<Symbol>(Symbol::CreateModule(it->second.CompilationModule)));
			return importExpr;
		}
		
		for (const auto& [symbolName, exposedSymbol] : it->second.CompilationModule->GetExposedSymbols())
		{
			// imported globals are variables like local ones: reading them loads the value
			// the outermost scope holds imports, so a file's own definitions shadow imported names (like Python)
			SymbolEntryType entryType = exposedSymbol->Kind == SymbolKind::Value ? SymbolEntryType::Variable : SymbolEntryType::None;
			m_ScopeStack.front().Insert(symbolName, entryType, exposedSymbol);
		}
		
		return importExpr;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTWhileExpression> whileExpr, SemaContext context)
	{
		context.ValueReq = ValueRequired::RValue;
		whileExpr->WhileBlock.Condition = Visit(whileExpr->WhileBlock.Condition, context);

		context.ValueReq = ValueRequired::Any;
		context.InLoop = true;
		Visit(whileExpr->WhileBlock.CodeBlock, context);
		
		return whileExpr;
	}
	

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTForExpression> forExpr, SemaContext context)
	{
		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;

		if (forExpr->Iterable)
		{
			// kept unanalysed in case the loop is rewritten for a class (analysis changes nodes in place)
			Cloner cloner;
			cloner.DestinationModule = m_Module;
			auto pristineIterable = cloner.Clone(forExpr->Iterable);

			// arrays are iterated in place, so analyse the storage rather than a copy
			SemaContext storageContext = context;
			storageContext.ValueReq = ValueRequired::LValue;
			forExpr->Iterable = Visit(forExpr->Iterable, storageContext);

			if (!forExpr->Iterable)
				return nullptr;

			forExpr->IterableType = m_TypeInferEngine.InferTypeFromNode(forExpr->Iterable);

			bool isClassPointer = forExpr->IterableType && forExpr->IterableType->IsPointer() && forExpr->IterableType->As<PointerType>()->GetBaseType() &&
								  forExpr->IterableType->As<PointerType>()->GetBaseType()->IsClass();

			if (forExpr->IterableType && (forExpr->IterableType->IsClass() || isClassPointer))
				return Visit(LowerClassIteration(forExpr, pristineIterable), context);

			// for x in generator:   ->   let g = generator;  defer destroy(g);  while advance(g): let x = value(g); body
			if (auto generator = std::dynamic_pointer_cast<CoroutineType>(forExpr->IterableType); generator && generator->GetKind() == CoroutineType::Kind::Generator)
			{
				static size_t s_GeneratorCounter = 0;
				Token location = forExpr->Location;
				auto token = [&](const std::string& text) { return Token(TokenType::Identifier, text, location.GetSourceFile(), location.LineNumber, location.ColumnNumber); };
				std::string handleName = std::format("__generator_{}", s_GeneratorCounter++);

				auto intrinsic = [&](const std::string& name, std::shared_ptr<Type> result)
				{
					auto node = std::make_shared<ASTIntrinsic>(name, result);
					node->Location = location;
					node->Unanalysed = true;
					node->Arguments.push_back(std::make_shared<ASTVariable>(token(handleName)));
					return node;
				};

				auto handle = std::make_shared<ASTVariableDeclaration>(token(handleName));
				handle->Location = location;
				handle->Initializer = pristineIterable;

				auto destroy = std::make_shared<ASTDefer>();
				destroy->Expr = intrinsic("coro_destroy", nullptr);

				auto element = std::make_shared<ASTVariableDeclaration>(forExpr->VariableName);
				element->Location = forExpr->VariableName;
				element->Initializer = intrinsic("coro_value", generator->GetValueType());

				auto loop = std::make_shared<ASTWhileExpression>();
				loop->WhileBlock.Condition = intrinsic("coro_advance", Symbol::GetBooleanType(m_Module).GetType());
				loop->WhileBlock.CodeBlock = std::make_shared<ASTBlock>();
				loop->WhileBlock.CodeBlock->Children.push_back(element);
				loop->WhileBlock.CodeBlock->Children.insert(loop->WhileBlock.CodeBlock->Children.end(), forExpr->CodeBlock->Children.begin(), forExpr->CodeBlock->Children.end());

				auto block = std::make_shared<ASTBlock>();
				block->Children = { handle, destroy, loop };
				return Visit(block, context);
			}

			if (!forExpr->IterableType || !forExpr->IterableType->IsArray())
			{
				Report(DiagnosticCode_NotIterable, GetNodeLocation(forExpr->Iterable));
				return nullptr;
			}

			forExpr->VariableType = forExpr->IterableType->As<ArrayType>()->GetBaseType();
		}
		else
		{
			forExpr->Start = Visit(forExpr->Start, valueContext);
			forExpr->End = Visit(forExpr->End, valueContext);

			if (!forExpr->Start || !forExpr->End)
				return nullptr;

			auto startType = m_TypeInferEngine.InferTypeFromNode(forExpr->Start);
			auto endType = m_TypeInferEngine.InferTypeFromNode(forExpr->End);

			if (!startType || !endType || !startType->IsIntegral() || !endType->IsIntegral())
			{
				Report(DiagnosticCode_InvalidForLoop, forExpr->Location);
				return nullptr;
			}

			// the loop variable takes the wider of the two bounds
			forExpr->VariableType = m_TypeInferEngine.GetCommonType(startType, endType);
			forExpr->Start = Coerce(forExpr->Start, forExpr->VariableType);
			forExpr->End = Coerce(forExpr->End, forExpr->VariableType);
		}

		m_ScopeStack.emplace_back();

		auto symbol = m_ScopeStack.back().InsertEmpty(forExpr->VariableName.GetData(), SymbolEntryType::Variable);
		*symbol.value() = Symbol::CreateValue(nullptr, forExpr->VariableType);
		forExpr->Variable = symbol.value();

		SemaContext bodyContext = context;
		bodyContext.ValueReq = ValueRequired::Any;
		bodyContext.InLoop = true;
		Visit(forExpr->CodeBlock, bodyContext);

		m_ScopeStack.pop_back();
		return forExpr;
	}

	std::shared_ptr<ASTNodeBase> Sema::LowerClassIteration(std::shared_ptr<ASTForExpression> forExpr, std::shared_ptr<ASTNodeBase> iterable)
	{
		// for x in items:            ->   let it = &items            (or the value itself if items is a temporary)
		//     body                         for i in 0..it.__len__():
		//                                      let x = it.__getitem__(i)
		//                                      body
		bool throughPointer = forExpr->IterableType->IsPointer();
		auto classType = (throughPointer ? forExpr->IterableType->As<PointerType>()->GetBaseType() : forExpr->IterableType)->As<ClassType>();

		// operator iterate: for x in obj  ->  for x in obj.iterate()  (a generator)
		if (classType->MemberFunctions.contains("__iter__"))
		{
			auto access = std::make_shared<ASTBinaryExpression>(OperatorType::Dot);
			access->Location = forExpr->Location;
			access->LeftSide = iterable;
			access->RightSide = std::make_shared<ASTVariable>(Token(TokenType::Identifier, "__iter__", forExpr->Location.GetSourceFile(), forExpr->Location.LineNumber, forExpr->Location.ColumnNumber));

			auto call = std::make_shared<ASTFunctionCall>();
			call->Location = forExpr->Location;
			call->Callee = access;

			auto loop = std::make_shared<ASTForExpression>();
			loop->Location = forExpr->Location;
			loop->VariableName = forExpr->VariableName;
			loop->Iterable = call;
			loop->CodeBlock = forExpr->CodeBlock;
			return loop;
		}

		bool slotted = false;

		if (!classType->MemberFunctions.contains("__len__") || !classType->MemberFunctions.contains("__getitem__"))
		{
			Token location = GetNodeLocation(iterable);
			location.SetData(classType->GetHash());
			Report(DiagnosticCode_NotIterable, location);
			return nullptr;
		}

		static size_t s_LoopCounter = 0;
		size_t id = s_LoopCounter++;

		Token location = forExpr->Location;
		auto token = [&](TokenType type, const std::string& text) { return Token(type, text, location.GetSourceFile(), location.LineNumber, location.ColumnNumber); };
		auto name = [&](const std::string& text) { return std::make_shared<ASTVariable>(token(TokenType::Identifier, text)); };

		auto method = [&](const std::string& target, const std::string& methodName, std::vector<std::shared_ptr<ASTNodeBase>> arguments)
		{
			auto access = std::make_shared<ASTBinaryExpression>(OperatorType::Dot);
			access->Location = location;
			access->LeftSide = name(target);
			access->RightSide = name(methodName);

			auto call = std::make_shared<ASTFunctionCall>();
			call->Location = location;
			call->Callee = access;
			call->Arguments.append(arguments.begin(), arguments.end());
			return call;
		};

		std::string iterableName = std::format("__for_iterable_{}", id);
		std::string indexName = std::format("__for_index_{}", id);

		// variables, fields and dereferences are iterated in place, anything else is evaluated once into a local
		bool isStorage = iterable->GetType() == ASTNodeType::Variable || iterable->GetType() == ASTNodeType::Subscript ||
						 (iterable->GetType() == ASTNodeType::BinaryExpression && std::dynamic_pointer_cast<ASTBinaryExpression>(iterable)->GetExpression() == OperatorType::Dot) ||
						 (iterable->GetType() == ASTNodeType::UnaryExpression && std::dynamic_pointer_cast<ASTUnaryExpression>(iterable)->GetOperatorType() == OperatorType::Dereference);

		auto iterableDecl = std::make_shared<ASTVariableDeclaration>(token(TokenType::Identifier, iterableName));
		iterableDecl->Location = location;

		if (throughPointer)
		{
			// already a pointer to the object, iterate through it
			iterableDecl->Initializer = iterable;
		}
		else if (isStorage)
		{
			auto address = std::make_shared<ASTUnaryExpression>(OperatorType::Address);
			address->Location = location;
			address->Operand = iterable;
			iterableDecl->Initializer = address;
		}
		else
		{
			iterableDecl->Initializer = iterable;
		}

		auto loop = std::make_shared<ASTForExpression>();
		loop->Location = location;
		loop->VariableName = token(TokenType::Identifier, indexName);
		loop->Start = std::make_shared<ASTNodeLiteral>(token(TokenType::Number, "0"));
		loop->End = method(iterableName, slotted ? "__slots__" : "__len__", {});

		auto elementDecl = std::make_shared<ASTVariableDeclaration>(forExpr->VariableName);
		elementDecl->Location = forExpr->VariableName;
		elementDecl->Initializer = method(iterableName, slotted ? "__at__" : "__getitem__", { name(indexName) });

		loop->CodeBlock = std::make_shared<ASTBlock>();

		if (slotted)
		{
			// if not it.__used__(i): continue
			auto unused = std::make_shared<ASTUnaryExpression>(OperatorType::Not);
			unused->Location = location;
			unused->Operand = method(iterableName, "__used__", { name(indexName) });

			auto skip = std::make_shared<ASTBlock>();
			skip->Children.push_back(std::make_shared<ASTLoopControlFlow>("continue", token(TokenType::Identifier, "continue")));

			auto check = std::make_shared<ASTIfExpression>();
			check->ConditionalBlocks.push_back({ unused, skip });
			loop->CodeBlock->Children.push_back(check);
		}

		loop->CodeBlock->Children.push_back(elementDecl);
		loop->CodeBlock->Children.insert(loop->CodeBlock->Children.end(), forExpr->CodeBlock->Children.begin(), forExpr->CodeBlock->Children.end());

		auto block = std::make_shared<ASTBlock>();
		block->Children = { iterableDecl, loop };
		return block;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTEnum> enumNode, SemaContext context)
	{
		const std::string& name = enumNode->Name.GetData();

		if (enumNode->IsRich())
		{
			if (!DeclareVariantType(enumNode) || !DeclareVariantBody(enumNode, context))
				return nullptr;

			DefineVariant(enumNode, context);
			return enumNode;
		}

		if (m_Module->GetTypeRegistry()->GetType(name))
		{
			Report(DiagnosticCode_RedefinedIdentifier, enumNode->Name);
			return nullptr;
		}

		auto underlying = m_Module->Lookup("int32").value()->GetType();
		auto enumType = m_Module->GetTypeRegistry()->CreateType<EnumType>(name, name, underlying);

		int64_t next = 0;

		for (auto& [memberName, valueNode] : enumNode->Members)
		{
			if (valueNode)
			{
				valueNode = Visit(valueNode, SemaContext { .ValueReq = ValueRequired::RValue });
				auto value = EvaluateInteger(valueNode);

				if (!value)
				{
					Report(DiagnosticCode_InvalidEnumValue, memberName);
					continue;
				}

				next = *value;
			}

			if (!enumType->AddValue(memberName.GetData(), next))
				Report(DiagnosticCode_RedefinedIdentifier, memberName);

			next++;
		}

		enumNode->EnumTy = enumType;
		m_Module->ExposeSymbol(name, std::make_shared<Symbol>(Symbol::CreateType(enumType)));

		return enumNode;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTSwitch> switchNode, SemaContext context)
	{
		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;

		switchNode->Value = Visit(switchNode->Value, valueContext);

		if (!switchNode->Value)
			return nullptr;

		std::shared_ptr<Type> valueType = m_TypeInferEngine.InferTypeFromNode(switchNode->Value);

		if (valueType && valueType->IsClass() && valueType->As<ClassType>()->IsVariant)
			return LowerVariantSwitch(switchNode, valueType, context);

		if (!valueType || !valueType->IsIntegral())
		{
			Report(DiagnosticCode_SwitchNotIntegral, GetNodeLocation(switchNode->Value));
			return nullptr;
		}

		std::unordered_set<int64_t> seen;

		for (auto& switchCase : switchNode->Cases)
		{
			for (auto& value : switchCase.Values)
			{
				value = Visit(value, valueContext);

				if (!value)
					continue;

				// enums only match their own members, plain integers match any integer constant
				if (valueType->IsEnum())
					value = Coerce(value, valueType);

				auto constant = EvaluateInteger(value);

				if (!constant)
				{
					Report(DiagnosticCode_NonConstantCase, GetNodeLocation(value));
					continue;
				}

				if (!seen.insert(*constant).second)
					Report(DiagnosticCode_DuplicateCase, GetNodeLocation(value));

				switchCase.Constants.push_back(*constant);
			}

			Visit(switchCase.CodeBlock, context);
		}

		if (switchNode->DefaultCaseCodeBlock)
			Visit(switchNode->DefaultCaseCodeBlock, context);

		// a switch over an enum without default must handle every member
		if (valueType->IsEnum() && !switchNode->DefaultCaseCodeBlock)
		{
			std::string missing;

			for (const auto& [member, value] : std::dynamic_pointer_cast<EnumType>(valueType)->GetValues())
			{
				if (!seen.contains(value))
					missing += (missing.empty() ? "" : ", ") + member;
			}

			if (!missing.empty())
			{
				Token location = switchNode->Location;
				location.SetData(missing);
				Report(DiagnosticCode_SwitchNotExhaustive, location);
			}

			switchNode->IsExhaustive = missing.empty();
		}

		return switchNode;
	}

	static bool IsStorageNode(const std::shared_ptr<ASTNodeBase>& node)
	{
		switch (node->GetType())
		{
			case ASTNodeType::Variable:
			case ASTNodeType::Subscript:
				return true;
			case ASTNodeType::BinaryExpression:
				return std::dynamic_pointer_cast<ASTBinaryExpression>(node)->GetExpression() == OperatorType::Dot;
			case ASTNodeType::UnaryExpression:
				return std::dynamic_pointer_cast<ASTUnaryExpression>(node)->IsStorage;
			default:
				return false;
		}
	}

	std::shared_ptr<ASTNodeBase> Sema::AsValue(std::shared_ptr<ASTNodeBase> node)
	{
		// a node analysed as storage (an address) read as a value
		if (!IsStorageNode(node))
			return node;

		auto load = std::make_shared<ASTLoad>();
		load->Operand = node;
		return load;
	}

	std::shared_ptr<ASTNodeBase> Sema::CallMethod(std::shared_ptr<ASTNodeBase> object, std::shared_ptr<Type> objectType, const std::string& name, 
												   std::vector<std::shared_ptr<ASTNodeBase>> arguments, const Token& location)
	{
		// object is storage (analysed as an lvalue): pass its address, or the pointer it holds
		std::shared_ptr<ClassType> classType;
		std::shared_ptr<ASTNodeBase> self;

		if (objectType->IsClass())
		{
			classType = objectType->As<ClassType>();

			// a loaded variable or field: pass the address of where it lives
			if (auto load = std::dynamic_pointer_cast<ASTLoad>(object); load && IsStorageNode(load->Operand))
				object = load->Operand;

			if (IsStorageNode(object))
			{
				auto address = std::make_shared<ASTUnaryExpression>(OperatorType::Address);
				address->Operand = object;
				address->Location = location;
				self = address;
			}
			else
			{
				self = AddressOf(object);
			}
		}
		else
		{
			classType = objectType->As<PointerType>()->GetBaseType()->As<ClassType>();
			self = AsValue(object);
		}

		auto method = classType->MemberFunctions.find(name);

		if (method == classType->MemberFunctions.end())
			return nullptr;

		auto callee = std::make_shared<ASTVariable>(Token(TokenType::Identifier, name, location.GetSourceFile(), location.LineNumber, location.ColumnNumber));
		callee->Variable = method->second;

		auto call = std::make_shared<ASTFunctionCall>();
		call->Location = location;
		call->Callee = callee;
		call->Arguments.push_back(self);
		call->Arguments.append(arguments.begin(), arguments.end());

		return CheckCall(call);
	}

	static std::shared_ptr<Type> ClassOf(std::shared_ptr<Type> type)
	{
		if (!type)
			return nullptr;

		if (type->IsClass())
			return type;

		if (type->IsPointer() && type->As<PointerType>()->GetBaseType() && type->As<PointerType>()->GetBaseType()->IsClass())
			return type->As<PointerType>()->GetBaseType();

		return nullptr;
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitLen(std::shared_ptr<ASTFunctionCall> funcCall, SemaContext context)
	{
		Token location = GetNodeLocation(funcCall->Callee);

		if (funcCall->Arguments.size() != 1)
		{
			location.SetData(std::format("len’ expects 1 argument, but {} {} given", funcCall->Arguments.size(), funcCall->Arguments.size() == 1 ? "was" : "were"));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_WrongArgumentCount, 3);
			return nullptr;
		}

		SemaContext storageContext = context;
		storageContext.ValueReq = ValueRequired::LValue;

		auto argument = Visit(funcCall->Arguments[0], storageContext);

		if (!argument)
			return nullptr;

		auto type = m_TypeInferEngine.InferTypeFromNode(argument);
		auto int64Type = m_Module->Lookup("int64").value()->GetType();

		if (type && type->IsArray())
			return std::make_shared<ASTConstantValue>((int64_t)type->As<ArrayType>()->GetArraySize(), int64Type);

		if (type && type->GetHash() == "str")
		{
			auto intrinsic = std::make_shared<ASTIntrinsic>("strlen", int64Type);
			intrinsic->Location = location;
			intrinsic->Arguments.push_back(AsValue(argument));
			return intrinsic;
		}

		if (auto classType = ClassOf(type); classType && classType->As<ClassType>()->MemberFunctions.contains("__len__"))
		{
			EnsureDefined(classType->As<ClassType>()->MemberFunctions.at("__len__")->GetFunctionSymbol().FunctionNode);
			return CallMethod(argument, type, "__len__", {}, location);
		}

		Token where = GetNodeLocation(argument);
		where.SetData(GetDisplayName(type));
		Report(DiagnosticCode_NoLength, where);
		return nullptr;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTYield> yield, SemaContext context)
	{
		if (context.CoroutineKind != 1)
		{
			Report(DiagnosticCode_YieldOutsideGenerator, yield->Location);
			return nullptr;
		}

		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;
		valueContext.ExpectedType = context.CoroutineValue;
		yield->Value = Visit(yield->Value, valueContext);

		if (!yield->Value)
			return nullptr;

		yield->Value = Coerce(yield->Value, context.CoroutineValue);
		return yield;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTAwait> await, SemaContext context)
	{
		if (context.CoroutineKind != 2)
		{
			Report(DiagnosticCode_AwaitOutsideAsync, await->Location);
			return nullptr;
		}

		if (await->IsPause)
			return await;

		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;
		await->Operand = Visit(await->Operand, valueContext);

		if (!await->Operand)
			return nullptr;

		auto task = std::dynamic_pointer_cast<CoroutineType>(m_TypeInferEngine.InferTypeFromNode(await->Operand));

		if (!task || task->GetKind() != CoroutineType::Kind::Task)
		{
			Token where = GetNodeLocation(await->Operand);
			where.SetData(GetDisplayName(m_TypeInferEngine.InferTypeFromNode(await->Operand)));
			Report(DiagnosticCode_NotAwaitable, where);
			return nullptr;
		}

		await->ValueType = task->GetValueType();
		return await;
	}

	std::shared_ptr<ASTNodeBase> Sema::CoroutineMethod(std::shared_ptr<ASTFunctionCall> funcCall, std::shared_ptr<ASTNodeBase> object, std::shared_ptr<Type> type, const Token& name)
	{
		auto coroutine = type->As<CoroutineType>();
		bool isTask = coroutine->GetKind() == CoroutineType::Kind::Task;
		auto boolType = Symbol::GetBooleanType(m_Module).GetType();
		const std::string& method = name.GetData();

		// task.run():       run to the end and give the result (then the task is gone)
		// x.resume():       run until the next suspension (yield, pause or the end); true when finished
		// gen.advance():    resume, true when a new value is ready
		// x.done(), x.value() / task.result(), x.free()
		struct Entry { const char* Intrinsic; std::shared_ptr<Type> Result; bool TaskOnly; bool GeneratorOnly; };

		Entry entry {};
		if (method == "run")                         entry = { "task_run", coroutine->GetValueType(), true, false };
		else if (method == "resume")                 entry = { "coro_resume", boolType, false, false };
		else if (method == "advance")                entry = { "coro_advance", boolType, false, true };
		else if (method == "done")                   entry = { "coro_done", boolType, false, false };
		else if (method == "value" || method == "result") entry = { "coro_value", coroutine->GetValueType(), false, false };
		else if (method == "free")                   entry = { "coro_destroy", nullptr, false, false };

		if (!entry.Intrinsic || (entry.TaskOnly && !isTask) || (entry.GeneratorOnly && isTask) || 
			!funcCall->Arguments.empty() || !funcCall->KeywordArguments.empty() ||
			((method == "value" || method == "result") && !coroutine->GetValueType()))
		{
			Token where = name;
			where.SetData(std::format("{}’ of ‘{}", method, GetDisplayName(type)));
			Report(DiagnosticCode_UnknownMember, where);
			return nullptr;
		}

		auto intrinsic = std::make_shared<ASTIntrinsic>(entry.Intrinsic, entry.Result);
		intrinsic->Location = name;
		intrinsic->Arguments.push_back(AsValue(object));
		return intrinsic;
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitHash(std::shared_ptr<ASTFunctionCall> funcCall, SemaContext context)
	{
		Token location = GetNodeLocation(funcCall->Callee);

		if (funcCall->Arguments.size() != 1)
		{
			location.SetData(std::format("hash’ expects 1 argument, but {} {} given", funcCall->Arguments.size(), funcCall->Arguments.size() == 1 ? "was" : "were"));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_WrongArgumentCount, 4);
			return nullptr;
		}

		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;

		auto argument = Visit(funcCall->Arguments[0], valueContext);

		if (!argument)
			return nullptr;

		auto type = m_TypeInferEngine.InferTypeFromNode(argument);
		auto uint64Type = m_Module->Lookup("uint64").value()->GetType();

		// a class decides how it is hashed
		if (auto classType = ClassOf(type); classType && !type->IsPointer() && classType->As<ClassType>()->MemberFunctions.contains("__hash__"))
		{
			EnsureDefined(classType->As<ClassType>()->MemberFunctions.at("__hash__")->GetFunctionSymbol().FunctionNode);
			return CallMethod(argument, type, "__hash__", {}, location);
		}

		bool isString = type && type->GetHash() == "str";
		bool isScalar = type && (type->IsIntegral() || type->IsFloatingPoint() || type->IsPointer() || type->IsEnum());

		if (!isString && !isScalar)
		{
			Token where = GetNodeLocation(argument);
			where.SetData(GetDisplayName(type));
			Report(DiagnosticCode_NotHashable, where);
			return nullptr;
		}

		auto intrinsic = std::make_shared<ASTIntrinsic>(isString ? "hash_str" : "hash_int", uint64Type);
		intrinsic->Location = location;
		intrinsic->Arguments.push_back(argument);
		return intrinsic;
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitMembership(std::shared_ptr<ASTBinaryExpression> expr, SemaContext context)
	{
		bool negate = expr->GetExpression() == OperatorType::NotIn;

		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;
		SemaContext storageContext = context;
		storageContext.ValueReq = ValueRequired::LValue;

		auto needle = Visit(expr->LeftSide, valueContext);
		auto haystack = Visit(expr->RightSide, storageContext);

		if (!needle || !haystack)
			return nullptr;

		auto type = m_TypeInferEngine.InferTypeFromNode(haystack);
		auto boolType = Symbol::GetBooleanType(m_Module).GetType();
		std::shared_ptr<ASTNodeBase> result;

		if (type && type->IsArray())
		{
			auto contains = std::make_shared<ASTContains>();
			contains->Location = expr->Location;
			contains->Needle = Coerce(needle, type->As<ArrayType>()->GetBaseType());
			contains->Haystack = haystack;
			contains->ArrayTy = type;
			contains->Negate = negate;
			return contains;
		}

		if (type && type->GetHash() == "str")
		{
			// "lo" in "hello" looks for a substring
			auto intrinsic = std::make_shared<ASTIntrinsic>("str_contains", boolType);
			intrinsic->Location = expr->Location;
			intrinsic->Arguments = { Coerce(needle, type), AsValue(haystack) };
			result = intrinsic;
		}
		else if (auto classType = ClassOf(type); classType && classType->As<ClassType>()->MemberFunctions.contains("__contains__"))
		{
			EnsureDefined(classType->As<ClassType>()->MemberFunctions.at("__contains__")->GetFunctionSymbol().FunctionNode);
			result = CallMethod(haystack, type, "__contains__", { needle }, expr->Location);

			if (!result)
				return nullptr;
		}
		else
		{
			Token location = expr->Location;
			location.SetData(std::format("{} in {}", GetDisplayName(m_TypeInferEngine.InferTypeFromNode(needle)), GetDisplayName(type)));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_InvalidOperands, 2);
			return nullptr;
		}

		if (!negate)
			return result;

		auto notNode = std::make_shared<ASTUnaryExpression>(OperatorType::Not);
		notNode->Operand = result;
		notNode->Location = expr->Location;
		return notNode;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTTupleExpr> tuple, SemaContext context)
	{
		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;

		std::vector<std::shared_ptr<Type>> types;
		bool allTypes = true;

		for (auto& value : tuple->Values)
		{
			value = Visit(value, valueContext);

			if (!value)
				return nullptr;

			auto asType = GetTypeFromNode(value);
			allTypes = allTypes && asType;
			types.push_back(asType ? asType : m_TypeInferEngine.InferTypeFromNode(value));
		}

		// (int, str) names a tuple type, (1, "a") is a tuple value
		tuple->IsType = allTypes;
		tuple->TupleTy = m_Module->GetTypeRegistry()->GetTupleFrom(types);

		if (!tuple->TupleTy)
		{
			Report(DiagnosticCode_NeedsTypeOrValue, GetNodeLocation(tuple));
			return nullptr;
		}

		return tuple;
	}

	std::shared_ptr<Type> Sema::FunctionTypeOf(const std::shared_ptr<ASTFunctionDefinition>& function)
	{
		std::vector<std::shared_ptr<Type>> parameters;

		for (auto& argument : function->Arguments)
			parameters.push_back(argument ? argument->ResolvedType : nullptr);

		return m_Module->GetTypeRegistry()->GetFunctionFrom(parameters, function->ReturnTypeVal);
	}

	void Sema::DeclareInGlobalScope(const std::function<void()>& declare)
	{
		// lambdas become top-level functions/classes: analyse them with only the file's global scopes
		constexpr size_t globalScopes = 2;

		if (m_ScopeStack.size() <= globalScopes)
		{
			declare();
			return;
		}

		std::vector<SymbolTable> locals(std::make_move_iterator(m_ScopeStack.begin() + globalScopes), std::make_move_iterator(m_ScopeStack.end()));
		m_ScopeStack.resize(globalScopes);

		declare();

		m_ScopeStack.insert(m_ScopeStack.end(), std::make_move_iterator(locals.begin()), std::make_move_iterator(locals.end()));
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTFunctionTypeExpr> type, SemaContext context)
	{
		std::vector<std::shared_ptr<Type>> parameters;

		for (auto& parameter : type->Parameters)
		{
			parameter = Visit(parameter, context);
			auto resolved = parameter ? GetTypeFromNode(parameter) : nullptr;

			if (!resolved)
			{
				Report(DiagnosticCode_ExpectedType, GetNodeLocation(parameter ? parameter : type));
				return nullptr;
			}

			parameters.push_back(resolved);
		}

		std::shared_ptr<Type> returnType;

		if (type->ReturnType)
		{
			type->ReturnType = Visit(type->ReturnType, context);
			returnType = type->ReturnType ? GetTypeFromNode(type->ReturnType) : nullptr;

			if (!returnType)
			{
				Report(DiagnosticCode_ExpectedType, GetNodeLocation(type));
				return nullptr;
			}
		}

		type->ResolvedType = m_Module->GetTypeRegistry()->GetFunctionFrom(parameters, returnType);
		return type;
	}

	static void CollectNames(const std::shared_ptr<ASTNodeBase>& node, std::vector<Token>& names)
	{
		// every name a lambda body mentions (the field after `.` is not a name of its own)
		if (!node)
			return;

		switch (node->GetType())
		{
			case ASTNodeType::Variable: names.push_back(std::dynamic_pointer_cast<ASTVariable>(node)->GetName()); break;
			case ASTNodeType::BinaryExpression:
			{
				auto binary = std::dynamic_pointer_cast<ASTBinaryExpression>(node);
				CollectNames(binary->LeftSide, names);
				if (binary->GetExpression() != OperatorType::Dot)
					CollectNames(binary->RightSide, names);
				break;
			}
			case ASTNodeType::UnaryExpression: CollectNames(std::dynamic_pointer_cast<ASTUnaryExpression>(node)->Operand, names); break;
			case ASTNodeType::FunctionCall:
			{
				auto call = std::dynamic_pointer_cast<ASTFunctionCall>(node);
				CollectNames(call->Callee, names);
				for (auto& arg : call->Arguments) CollectNames(arg, names);
				break;
			}
			case ASTNodeType::Subscript:
			{
				auto subscript = std::dynamic_pointer_cast<ASTSubscript>(node);
				CollectNames(subscript->Target, names);
				for (auto& arg : subscript->SubscriptArgs) CollectNames(arg, names);
				break;
			}
			case ASTNodeType::TernaryExpression:
			{
				auto ternary = std::dynamic_pointer_cast<ASTTernaryExpression>(node);
				CollectNames(ternary->Condition, names);
				CollectNames(ternary->Truthy, names);
				CollectNames(ternary->Falsy, names);
				break;
			}
			case ASTNodeType::CastExpr: CollectNames(std::dynamic_pointer_cast<ASTCastExpr>(node)->Object, names); break;
			case ASTNodeType::StructExpr:
			{
				auto structExpr = std::dynamic_pointer_cast<ASTStructExpr>(node);
				for (auto& value : structExpr->Values) CollectNames(value, names);
				break;
			}
			case ASTNodeType::TupleExpr:
			{
				for (auto& value : std::dynamic_pointer_cast<ASTTupleExpr>(node)->Values) CollectNames(value, names);
				break;
			}
			case ASTNodeType::ListExpr:
			{
				for (auto& value : std::dynamic_pointer_cast<ASTListExpr>(node)->Values) CollectNames(value, names);
				break;
			}
			case ASTNodeType::Lambda:
			{
				// names of a nested lambda that are not its own parameters are free here too
				auto lambda = std::dynamic_pointer_cast<ASTLambda>(node);
				std::vector<Token> inner;
				CollectNames(lambda->Body, inner);

				for (auto& name : inner)
				{
					bool isParameter = std::any_of(lambda->Parameters.begin(), lambda->Parameters.end(), [&](auto& p) { return p->GetName().GetData() == name.GetData(); });
					if (!isParameter) names.push_back(name);
				}
				break;
			}
			default: break;
		}
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTLambda> lambda, SemaContext context)
	{
		static size_t s_LambdaCounter = 0;
		size_t id = s_LambdaCounter++;
		Token location = lambda->Location;
		auto token = [&](const std::string& text) { return Token(TokenType::Identifier, text, location.GetSourceFile(), location.LineNumber, location.ColumnNumber); };

		// parameter types: written out, or taken from the function type the lambda is converted to
		auto expected = context.ExpectedType && context.ExpectedType->IsFunction() ? context.ExpectedType->As<FunctionPointerType>() : nullptr;
		std::vector<std::shared_ptr<Type>> parameterTypes;

		for (size_t i = 0; i < lambda->Parameters.size(); i++)
		{
			auto& parameter = lambda->Parameters[i];
			std::shared_ptr<Type> type;

			if (parameter->TypeResolver)
			{
				Visit(parameter->TypeResolver, context);
				type = GetTypeFromNode(parameter->TypeResolver);
			}
			else if (expected && expected->GetParameters().size() == lambda->Parameters.size())
			{
				type = expected->GetParameters()[i];
			}

			if (!type)
			{
				Report(DiagnosticCode_LambdaNeedsTypes, parameter->GetName());
				return nullptr;
			}

			parameterTypes.push_back(type);
		}

		std::shared_ptr<Type> declaredReturn;

		if (lambda->ReturnType)
		{
			Visit(lambda->ReturnType, context);
			declaredReturn = GetTypeFromNode(lambda->ReturnType);
		}
		else if (expected && expected->GetReturnType())
		{
			declaredReturn = expected->GetReturnType();
		}

		// local variables the body uses are captured (copied) into a closure object
		std::vector<Token> names;
		CollectNames(lambda->Body, names);

		std::vector<std::pair<Token, std::shared_ptr<Type>>> captures;
		constexpr size_t globalScopes = 2;

		for (auto& name : names)
		{
			bool isParameter = std::any_of(lambda->Parameters.begin(), lambda->Parameters.end(), [&](auto& p) { return p->GetName().GetData() == name.GetData(); });
			bool alreadyCaptured = std::any_of(captures.begin(), captures.end(), [&](auto& c) { return c.first.GetData() == name.GetData(); });

			if (isParameter || alreadyCaptured)
				continue;

			auto [entry, scopeIndex] = LookupSymbol(name.GetData());

			if (entry && entry->Type == SymbolEntryType::Variable && scopeIndex >= globalScopes && entry->Symbol->Kind == SymbolKind::Value)
			{
				auto type = entry->Symbol->GetType();

				if (entry->Symbol->GetLLVMValue() && type->IsPointer())
					type = type->As<PointerType>()->GetBaseType();

				captures.push_back({ name, type });
			}
		}

		auto makeParameters = [&](std::shared_ptr<ASTFunctionDefinition> function)
		{
			for (size_t i = 0; i < lambda->Parameters.size(); i++)
			{
				auto parameter = std::make_shared<ASTVariableDeclaration>(lambda->Parameters[i]->GetName());
				parameter->TypeResolver = std::make_shared<ASTTypeLiteral>(parameterTypes[i]);
				function->Arguments.push_back(parameter);
			}

			if (declaredReturn)
				function->ReturnType = std::make_shared<ASTTypeLiteral>(declaredReturn);
			else
				function->InferReturnType = true;
		};

		auto body = std::make_shared<ASTBlock>();
		auto returnStatement = std::make_shared<ASTReturn>();
		returnStatement->Location = location;
		returnStatement->ReturnValue = lambda->Body;

		if (captures.empty())
		{
			// nothing captured: an ordinary function, used through its address
			auto function = std::make_shared<ASTFunctionDefinition>(std::format("__lambda_{}", id));
			function->SetNameToken(token(function->GetName()));
			function->Location = location;
			makeParameters(function);
			body->Children.push_back(returnStatement);
			function->CodeBlock = body;

			bool success = false;
			DeclareInGlobalScope([&]()
			{
				success = DeclareFunction(function, SemaContext { .GlobalState = false });

				if (success)
					DefineFunction(function, SemaContext { .GlobalState = false });
			});

			if (!success)
				return nullptr;

			auto reference = std::make_shared<ASTFunctionRef>();
			reference->Location = location;
			reference->Function = function->FunctionSymbol;
			reference->FunctionTy = FunctionTypeOf(function);
			return reference;
		}

		// captures: a small class holding copies of them, called through __call__
		auto closure = std::make_shared<ASTClass>(std::format("__closure_{}", id));
		closure->Location = location;

		for (auto& [name, type] : captures)
		{
			auto member = std::make_shared<ASTTypeSpecifier>(name.GetData());
			member->TypeResolver = std::make_shared<ASTTypeLiteral>(type);
			closure->Members.push_back(member);
			closure->DefaultValues.push_back(nullptr);
		}

		bool success = false;
		DeclareInGlobalScope([&]() { success = DeclareClassType(closure); });

		if (!success)
			return nullptr;

		auto call = std::make_shared<ASTFunctionDefinition>("__call__");
		call->SetNameToken(token("__call__"));
		call->Location = location;

		auto self = std::make_shared<ASTVariableDeclaration>(token("self"));
		self->TypeResolver = std::make_shared<ASTTypeLiteral>(m_Module->GetTypeRegistry()->GetPointerTo(closure->ClassTy));
		call->Arguments.push_back(self);
		makeParameters(call);

		// inside __call__ each captured name is a local copied from the closure
		for (auto& [name, type] : captures)
		{
			auto access = std::make_shared<ASTBinaryExpression>(OperatorType::Dot);
			access->Location = name;
			access->LeftSide = std::make_shared<ASTVariable>(token("self"));
			access->RightSide = std::make_shared<ASTVariable>(name);

			auto local = std::make_shared<ASTVariableDeclaration>(name);
			local->Initializer = access;
			body->Children.push_back(local);
		}

		body->Children.push_back(returnStatement);
		call->CodeBlock = body;
		closure->MemberFunctions.push_back(call);

		DeclareInGlobalScope([&]()
		{
			success = DeclareClassBody(closure, SemaContext { .GlobalState = false });

			if (success)
				DefineClass(closure, SemaContext { .GlobalState = false });
		});

		if (!success)
			return nullptr;

		// the lambda's value: the closure built from the current values of the captured variables
		auto target = std::make_shared<ASTVariable>(token(closure->GetName()));
		target->Variable = std::make_shared<Symbol>(Symbol::CreateType(closure->ClassTy));

		auto value = std::make_shared<ASTStructExpr>();
		value->Location = location;
		value->TargetType = target;

		for (auto& [name, type] : captures)
			value->Values.push_back(std::make_shared<ASTVariable>(name));

		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;
		valueContext.ExpectedType = nullptr;
		return Visit(value, valueContext);
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTDestructure> destructure, SemaContext context)
	{
		// evaluate the right side once into a hidden tuple, then hand out its elements:
		//     let __destructure = value
		//     target0 = __destructure[0]   (or `let name0 = ...`)
		static size_t s_Counter = 0;
		Token location = destructure->Location;
		std::string hiddenName = std::format("__destructure_{}", s_Counter++);
		Token hiddenToken(TokenType::Identifier, hiddenName, location.GetSourceFile(), location.LineNumber, location.ColumnNumber);

		auto hidden = std::make_shared<ASTVariableDeclaration>(hiddenToken);
		hidden->Location = location;
		hidden->Initializer = destructure->Value;

		auto sequence = std::make_shared<ASTSequence>();
		sequence->Location = location;

		auto visitedHidden = Visit(hidden, context);

		if (!visitedHidden)
			return nullptr;

		sequence->Children.push_back(visitedHidden);

		auto valueType = hidden->ResolvedType;
		size_t count = destructure->Targets.size();
		size_t available = valueType->IsTuple() ? valueType->As<TupleType>()->GetElements().size() 
						 : valueType->IsArray() ? valueType->As<ArrayType>()->GetArraySize() : 0;

		if (available != count)
		{
			Token where = GetNodeLocation(destructure->Value);
			where.SetData(std::format("{}’ into {} names", GetDisplayName(valueType), count));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_DestructureMismatch, 1);
			return nullptr;
		}

		for (size_t i = 0; i < count; i++)
		{
			auto element = std::make_shared<ASTSubscript>();
			element->Location = location;
			element->Target = std::make_shared<ASTVariable>(hiddenToken);
			element->SubscriptArgs.push_back(std::make_shared<ASTNodeLiteral>(Token(TokenType::Number, std::to_string(i), location.GetSourceFile(), location.LineNumber, location.ColumnNumber)));

			std::shared_ptr<ASTNodeBase> statement;

			if (destructure->IsDeclaration)
			{
				auto name = std::dynamic_pointer_cast<ASTVariable>(destructure->Targets[i]);
				auto decl = std::make_shared<ASTVariableDeclaration>(name->GetName());
				decl->Location = name->GetName();
				decl->Initializer = element;
				statement = decl;
			}
			else
			{
				auto assignment = std::make_shared<ASTAssignmentOperator>(AssignmentOperatorType::Normal);
				assignment->Location = location;
				assignment->Storage = destructure->Targets[i];
				assignment->Value = element;
				statement = assignment;
			}

			statement = Visit(statement, context);

			if (!statement)
				return nullptr;

			sequence->Children.push_back(statement);
		}

		return sequence;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTAssert> assertNode, SemaContext context)
	{
		context.ValueReq = ValueRequired::RValue;
		assertNode->Condition = Visit(assertNode->Condition, context);

		if (!assertNode->Condition)
			return nullptr;

		if (assertNode->Message)
		{
			assertNode->Message = Visit(assertNode->Message, context);
			auto strType = m_Module->Lookup("str").value()->GetType();
			assertNode->Message = Coerce(assertNode->Message, strType);
		}

		return assertNode;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTDefer> deferNode, SemaContext context)
	{
		if (context.GlobalState)
		{
			Report(DiagnosticCode_DeferOutsideFunction, deferNode->Location);
			return nullptr;
		}

		context.ValueReq = ValueRequired::Any;
		deferNode->Expr = Visit(deferNode->Expr, context);

		return deferNode->Expr ? deferNode : nullptr;
	}

	std::optional<int64_t> Sema::EvaluateInteger(std::shared_ptr<ASTNodeBase> node)
	{
		if (!node)
			return std::nullopt;

		switch (node->GetType())
		{
			case ASTNodeType::Literal:
			{
				const Token& token = std::dynamic_pointer_cast<ASTNodeLiteral>(node)->GetData();

				if (token.IsType(TokenType::Char))
					return (int64_t)(uint8_t)token.AsChar();

				if (token.GetData() == "true")  return 1;
				if (token.GetData() == "false") return 0;

				if (token.IsType(TokenType::Number) && token.GetData().find_first_of(".eE") == std::string::npos)
				{
					try { return (int64_t)std::stoull(token.GetData()); }
					catch (...) { return std::nullopt; }
				}

				return std::nullopt;
			}
			case ASTNodeType::ConstantValue:
			{
				return std::dynamic_pointer_cast<ASTConstantValue>(node)->Value;
			}
			case ASTNodeType::SizeofExpr:
			{
				return (int64_t)std::dynamic_pointer_cast<ASTSizeofExpr>(node)->Size;
			}
			case ASTNodeType::Load:
			{
				return EvaluateInteger(std::dynamic_pointer_cast<ASTLoad>(node)->Operand);
			}
			case ASTNodeType::Variable:
			{
				auto var = std::dynamic_pointer_cast<ASTVariable>(node);
				auto it = var->Variable ? m_ConstantValues.find(var->Variable.get()) : m_ConstantValues.end();

				if (it == m_ConstantValues.end())
					return std::nullopt;

				return it->second;
			}
			case ASTNodeType::CastExpr:
			{
				return EvaluateInteger(std::dynamic_pointer_cast<ASTCastExpr>(node)->Object);
			}
			case ASTNodeType::UnaryExpression:
			{
				auto unary = std::dynamic_pointer_cast<ASTUnaryExpression>(node);
				auto value = EvaluateInteger(unary->Operand);

				if (!value)
					return std::nullopt;

				switch (unary->GetOperatorType())
				{
					case OperatorType::Negation:	return -*value;
					case OperatorType::BitwiseNot:	return ~*value;
					case OperatorType::Not:			return (int64_t)(*value == 0);
					default:						return std::nullopt;
				}
			}
			case ASTNodeType::BinaryExpression:
			{
				auto binary = std::dynamic_pointer_cast<ASTBinaryExpression>(node);
				auto lhs = EvaluateInteger(binary->LeftSide);
				auto rhs = EvaluateInteger(binary->RightSide);

				if (!lhs || !rhs)
					return std::nullopt;

				switch (binary->GetExpression())
				{
					case OperatorType::Add:			return *lhs + *rhs;
					case OperatorType::Sub:			return *lhs - *rhs;
					case OperatorType::Mul:			return *lhs * *rhs;
					case OperatorType::Div:			return *rhs == 0 ? std::nullopt : std::optional(*lhs / *rhs);
					case OperatorType::Mod:			return *rhs == 0 ? std::nullopt : std::optional(*lhs % *rhs);
					case OperatorType::BitwiseAnd:	return *lhs & *rhs;
					case OperatorType::BitwiseOr:	return *lhs | *rhs;
					case OperatorType::BitwiseXor:	return *lhs ^ *rhs;
					case OperatorType::LeftShift:	return *lhs << *rhs;
					case OperatorType::RightShift:	return *lhs >> *rhs;
					default:						return std::nullopt;
				}
			}
			default:
				return std::nullopt;
		}
	}

	static bool IsNumericLiteral(const std::shared_ptr<ASTNodeBase>& node);

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTTernaryExpression> ternaryExpr, SemaContext context)
	{
		context.ValueReq = ValueRequired::RValue;

		ternaryExpr->Condition = Visit(ternaryExpr->Condition, context);
		ternaryExpr->Truthy = Visit(ternaryExpr->Truthy, context);
		ternaryExpr->Falsy = Visit(ternaryExpr->Falsy, context);

		if (!ternaryExpr->Condition || !ternaryExpr->Truthy || !ternaryExpr->Falsy)
			return nullptr;

		// both branches meet in one type: the wider of the two (a literal adapts to the other side)
		auto truthyType = m_TypeInferEngine.InferTypeFromNode(ternaryExpr->Truthy);
		auto falsyType = m_TypeInferEngine.InferTypeFromNode(ternaryExpr->Falsy);

		if (truthyType && falsyType && truthyType != falsyType)
		{
			std::shared_ptr<Type> common;

			bool truthyAdapts = (IsNumericLiteral(ternaryExpr->Truthy) && IsImplicitlyConvertible(truthyType, falsyType, true)) || IsConstantThatFits(ternaryExpr->Truthy, falsyType);
			bool falsyAdapts = (IsNumericLiteral(ternaryExpr->Falsy) && IsImplicitlyConvertible(falsyType, truthyType, true)) || IsConstantThatFits(ternaryExpr->Falsy, truthyType);

			if (truthyAdapts)
				common = falsyType;
			else if (falsyAdapts)
				common = truthyType;
			else if ((truthyType->IsIntegral() || truthyType->IsFloatingPoint()) && (falsyType->IsIntegral() || falsyType->IsFloatingPoint()))
				common = m_TypeInferEngine.GetCommonType(truthyType, falsyType);
			else
				common = IsImplicitlyConvertible(falsyType, truthyType, false) ? truthyType : falsyType;

			ternaryExpr->Truthy = Coerce(ternaryExpr->Truthy, common);
			ternaryExpr->Falsy = Coerce(ternaryExpr->Falsy, common);
		}

		return ternaryExpr;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTCastExpr> castExpr, SemaContext context)
	{
		// casts inserted by Coerce are already complete
		if (castExpr->TargetType && !castExpr->TypeNode)
			return castExpr;

		castExpr->Object = Visit(castExpr->Object, context);
		castExpr->TypeNode = Visit(castExpr->TypeNode, context);
		castExpr->TargetType = GetTypeFromNode(castExpr->TypeNode);

		return castExpr;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTSizeofExpr> sizeofExpr, SemaContext context)
	{
		sizeofExpr->Object = Visit(sizeofExpr->Object, context);
		sizeofExpr->Size = m_TypeInferEngine.InferTypeFromNode(sizeofExpr->Object)->GetSizeInBytes(*m_Module->GetModule());
		
		return sizeofExpr;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTIsExpr> isExpr, SemaContext context)
	{
		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;
		isExpr->Object = Visit(isExpr->Object, valueContext);

		if (!isExpr->Object)
			return nullptr;

		// `shape is Shape.Circle`, `x is none`: compare which case the value holds
		auto objectType = m_TypeInferEngine.InferTypeFromNode(isExpr->Object);

		if (objectType && objectType->IsClass() && objectType->As<ClassType>()->IsVariant)
		{
			auto classType = objectType->As<ClassType>();
			std::string caseName;

			if (auto literal = std::dynamic_pointer_cast<ASTNodeLiteral>(isExpr->TypeNode); literal && literal->GetData().GetData() == "none")
				caseName = "none";
			else if (auto var = std::dynamic_pointer_cast<ASTVariable>(isExpr->TypeNode))
				caseName = var->GetName().GetData();
			else if (auto member = std::dynamic_pointer_cast<ASTBinaryExpression>(isExpr->TypeNode); member && member->GetExpression() == OperatorType::Dot)
			{
				if (auto name = std::dynamic_pointer_cast<ASTVariable>(member->RightSide))
					caseName = name->GetName().GetData();
			}

			auto index = classType->FindCase(caseName);

			if (!index)
			{
				Token location = GetNodeLocation(isExpr->TypeNode);
				location.SetData(std::format("{}’ is not a case of ‘{}", caseName, GetDisplayName(objectType)));
				m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_UnknownCase, std::max<size_t>(caseName.size(), 1));
				return nullptr;
			}

			auto int32Type = m_Module->Lookup("int32").value()->GetType();
			auto tag = std::make_shared<ASTVariantTag>();
			tag->Subject = isExpr->Object;
			tag->TagType = int32Type;

			auto compare = std::make_shared<ASTBinaryExpression>(isExpr->Negate ? OperatorType::NotEqual : OperatorType::IsEqual);
			compare->Location = isExpr->Location;
			compare->LeftSide = tag;
			compare->RightSide = std::make_shared<ASTConstantValue>((int64_t)*index, int32Type);
			compare->ResultantType = Symbol::GetBooleanType(m_Module).GetType();
			return compare;
		}

		isExpr->TypeNode = Visit(isExpr->TypeNode, context);
		isExpr->AreTypesSame = (objectType == GetTypeFromNode(isExpr->TypeNode)) != isExpr->Negate;

		return isExpr;
	}


	std::shared_ptr<Type> Sema::GetOptionalType(std::shared_ptr<Type> valueType)
	{
		// ?T is the same type everywhere (like pointers), a rich enum with the cases none and some(value: T)
		static std::map<Type*, std::shared_ptr<Type>> s_Optionals;
		auto& slot = s_Optionals[valueType.get()];

		if (!slot)
		{
			auto optional = std::make_shared<ClassType>("?" + valueType->GetHash(), *m_Module->GetContext());
			optional->IsOptional = true;

			ClassType::VariantCase none { .Name = "none" };
			ClassType::VariantCase some { .Name = "some", .Fields = { { "value", valueType } } };
			optional->SetVariantBody({ none, some }, {});

			slot = optional;
		}

		return slot;
	}

	bool Sema::DeclareVariantType(std::shared_ptr<ASTEnum> enumNode)
	{
		if (enumNode->VariantTy)
			return true;

		const std::string& name = enumNode->Name.GetData();

		if (m_Module->GetTypeRegistry()->GetType(name))
		{
			Report(DiagnosticCode_RedefinedIdentifier, enumNode->Name);
			return false;
		}

		auto classType = m_Module->GetTypeRegistry()->CreateType<ClassType>(name, name, *m_Module->GetContext());
		classType->IsVariant = true;
		enumNode->VariantTy = classType;

		m_Module->ExposeSymbol(name, std::make_shared<Symbol>(Symbol::CreateType(classType)));
		return true;
	}

	bool Sema::DeclareVariantBody(std::shared_ptr<ASTEnum> enumNode, SemaContext context)
	{
		auto classType = enumNode->VariantTy->As<ClassType>();

		if (!classType->Cases.empty())
			return true; // already declared

		if (enumNode->Members.empty())
		{
			Report(DiagnosticCode_ExpectedIdentifier, enumNode->Name);
			return false;
		}

		std::vector<ClassType::VariantCase> cases;

		for (size_t i = 0; i < enumNode->Members.size(); i++)
		{
			ClassType::VariantCase variantCase { .Name = enumNode->Members[i].first.GetData() };

			for (auto& field : enumNode->Payloads[i])
			{
				if (field->TypeResolver)
					Visit(field->TypeResolver, context);

				auto fieldType = field->TypeResolver ? GetTypeFromNode(field->TypeResolver) : nullptr;

				if (!fieldType)
				{
					Report(DiagnosticCode_ExpectedType, field->TypeResolver ? GetNodeLocation(field->TypeResolver) : field->GetName());
					return false;
				}

				variantCase.Fields.push_back({ field->GetName().GetData(), fieldType });
			}

			if (std::any_of(cases.begin(), cases.end(), [&](auto& c) { return c.Name == variantCase.Name; }))
			{
				Report(DiagnosticCode_RedefinedIdentifier, enumNode->Members[i].first);
				return false;
			}

			cases.push_back(variantCase);
		}

		std::vector<std::pair<std::string, std::shared_ptr<Symbol>>> methods;

		for (auto& method : enumNode->Methods)
		{
			auto functionSymbol = std::make_shared<Symbol>(Symbol::CreateFunction(method));
			methods.emplace_back(method->GetName(), functionSymbol);
			method->FunctionSymbol = functionSymbol;
		}

		classType->SetVariantBody(cases, methods);

		context.TypeHint = classType;

		for (auto& method : enumNode->Methods)
			DeclareFunction(method, context);

		return true;
	}

	void Sema::DefineVariant(std::shared_ptr<ASTEnum> enumNode, SemaContext context)
	{
		context.TypeHint = enumNode->VariantTy;

		for (auto& method : enumNode->Methods)
		{
			if (method->SignatureResolved && method->FunctionSymbol)
				DefineFunction(method, context);
		}
	}

	std::shared_ptr<ASTNodeBase> Sema::BuildVariantConstruct(std::shared_ptr<Type> variantType, size_t caseIndex, llvm::ArrayRef<std::shared_ptr<ASTNodeBase>> arguments, 
															  const std::vector<std::pair<Token, std::shared_ptr<ASTNodeBase>>>& keywords, const Token& location)
	{
		auto classType = variantType->As<ClassType>();
		auto& variantCase = classType->Cases[caseIndex];

		// keyword arguments name the case's fields: Shape.Rect(height = 2, width = 1)
		std::vector<std::shared_ptr<ASTNodeBase>> values(arguments.begin(), arguments.end());
		values.resize(std::max(values.size(), variantCase.Fields.size()));

		for (auto& [name, value] : keywords)
		{
			auto it = std::find_if(variantCase.Fields.begin(), variantCase.Fields.end(), [&](auto& field) { return field.first == name.GetData(); });

			if (it == variantCase.Fields.end() || values[std::distance(variantCase.Fields.begin(), it)])
			{
				Token where = name;
				where.SetData(std::format("{}’ is not a field of {}.{} (or is given twice", name.GetData(), classType->GetHash(), variantCase.Name));
				m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_UnknownKeyword, name.GetData().size());
				return nullptr;
			}

			values[std::distance(variantCase.Fields.begin(), it)] = value;
		}

		if (values.size() != variantCase.Fields.size() || std::find(values.begin(), values.end(), nullptr) != values.end())
		{
			Token where = location;
			size_t given = arguments.size() + keywords.size();
			where.SetData(std::format("{}.{}’ expects {} value{}, but {} {} given", classType->GetHash(), variantCase.Name, variantCase.Fields.size(), 
									  variantCase.Fields.size() == 1 ? "" : "s", given, given == 1 ? "was" : "were"));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_WrongArgumentCount, std::max<size_t>(location.GetData().size(), 1));
			return nullptr;
		}

		auto construct = std::make_shared<ASTVariantConstruct>();
		construct->Location = location;
		construct->VariantTy = variantType;
		construct->CaseIndex = caseIndex;

		for (size_t i = 0; i < values.size(); i++)
			construct->Values.push_back(Coerce(values[i], variantCase.Fields[i].second));

		return construct;
	}

	std::shared_ptr<ASTNodeBase> Sema::LowerVariantSwitch(std::shared_ptr<ASTSwitch> switchNode, std::shared_ptr<Type> variantType, SemaContext context)
	{
		// switch shape:                      let __match = shape
		//     case Circle(r): body     ->     switch __match.tag:
		//                                          case 0: let r = <Circle field 0 of __match>; body
		static size_t s_MatchCounter = 0;
		auto classType = variantType->As<ClassType>();
		Token location = switchNode->Location;
		Token subjectToken(TokenType::Identifier, std::format("__match_{}", s_MatchCounter++), location.GetSourceFile(), location.LineNumber, location.ColumnNumber);

		auto subjectSymbol = m_ScopeStack.back().InsertEmpty(subjectToken.GetData(), SymbolEntryType::Variable).value();
		*subjectSymbol = Symbol::CreateValue(nullptr, variantType);

		auto subjectDecl = std::make_shared<ASTVariableDeclaration>(subjectToken);
		subjectDecl->Location = location;
		subjectDecl->Initializer = switchNode->Value;
		subjectDecl->ResolvedType = variantType;
		subjectDecl->Variable = subjectSymbol;

		auto subjectStorage = [&]()
		{
			auto var = std::make_shared<ASTVariable>(subjectToken);
			var->Variable = subjectSymbol;
			return var;
		};

		auto subjectValue = std::make_shared<ASTLoad>();
		subjectValue->Operand = subjectStorage();

		auto tag = std::make_shared<ASTVariantTag>();
		tag->Subject = subjectValue;
		tag->TagType = m_Module->Lookup("int32").value()->GetType();
		switchNode->Value = tag;

		std::unordered_set<int64_t> seen;

		for (auto& switchCase : switchNode->Cases)
		{
			bool singlePattern = switchCase.Values.size() == 1;

			for (auto& pattern : switchCase.Values)
			{
				// Shape.Circle(r) / Circle(r) / Shape.Empty / Empty / none
				std::string caseName;
				std::vector<std::shared_ptr<ASTNodeBase>> bindings;
				bool hasBindings = false;
				std::shared_ptr<ASTNodeBase> namePart = pattern;

				if (auto call = std::dynamic_pointer_cast<ASTFunctionCall>(pattern))
				{
					namePart = call->Callee;
					bindings.assign(call->Arguments.begin(), call->Arguments.end());
					hasBindings = true;
				}

				if (auto literal = std::dynamic_pointer_cast<ASTNodeLiteral>(namePart); literal && literal->GetData().GetData() == "none")
					caseName = "none";
				else if (auto var = std::dynamic_pointer_cast<ASTVariable>(namePart))
					caseName = var->GetName().GetData();
				else if (auto member = std::dynamic_pointer_cast<ASTBinaryExpression>(namePart); member && member->GetExpression() == OperatorType::Dot)
				{
					if (auto name = std::dynamic_pointer_cast<ASTVariable>(member->RightSide))
						caseName = name->GetName().GetData();
				}

				auto index = classType->FindCase(caseName);
				Token patternLocation = GetNodeLocation(pattern);

				if (!index)
				{
					patternLocation.SetData(std::format("{}’ is not a case of ‘{}", caseName.empty() ? patternLocation.GetData() : caseName, GetDisplayName(variantType)));
					m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, patternLocation, DiagnosticCode_UnknownCase, 1);
					return nullptr;
				}

				if (!seen.insert((int64_t)*index).second)
					Report(DiagnosticCode_DuplicateCase, patternLocation);

				switchCase.Constants.push_back((int64_t)*index);

				auto& fields = classType->Cases[*index].Fields;

				if (hasBindings && (!singlePattern || bindings.size() != fields.size()))
				{
					patternLocation.SetData(std::format("{}’ has {} field{}, the pattern binds {} (a pattern with names must be alone in its case", 
														caseName, fields.size(), fields.size() == 1 ? "" : "s", bindings.size()));
					m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, patternLocation, DiagnosticCode_DestructureMismatch, 1);
					return nullptr;
				}

				// each name becomes a local holding that field, `_` skips a field
				std::vector<std::shared_ptr<ASTNodeBase>> declarations;

				for (size_t i = 0; i < bindings.size(); i++)
				{
					auto name = std::dynamic_pointer_cast<ASTVariable>(bindings[i]);

					if (!name)
					{
						Report(DiagnosticCode_ExpectedIdentifier, GetNodeLocation(bindings[i]));
						return nullptr;
					}

					if (name->GetName().GetData() == "_")
						continue;

					auto field = std::make_shared<ASTVariantField>();
					field->Subject = subjectStorage();
					field->VariantTy = variantType;
					field->CaseIndex = *index;
					field->FieldIndex = i;

					auto declaration = std::make_shared<ASTVariableDeclaration>(name->GetName());
					declaration->Location = name->GetName();
					declaration->Initializer = field;
					declarations.push_back(declaration);
				}

				switchCase.CodeBlock->Children.insert(switchCase.CodeBlock->Children.begin(), declarations.begin(), declarations.end());
			}

			Visit(switchCase.CodeBlock, context);
		}

		if (switchNode->DefaultCaseCodeBlock)
		{
			Visit(switchNode->DefaultCaseCodeBlock, context);
		}
		else
		{
			std::string missing;

			for (size_t i = 0; i < classType->Cases.size(); i++)
			{
				if (!seen.contains((int64_t)i))
					missing += (missing.empty() ? "" : ", ") + classType->Cases[i].Name;
			}

			if (!missing.empty())
			{
				location.SetData(missing);
				Report(DiagnosticCode_SwitchNotExhaustive, location);
			}

			switchNode->IsExhaustive = missing.empty();
		}

		auto sequence = std::make_shared<ASTSequence>();
		sequence->Location = location;
		sequence->Children = { subjectDecl, switchNode };
		return sequence;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTLoopControlFlow> controlFlow, SemaContext context)
	{
		if (!context.InLoop)
			Report(DiagnosticCode_LoopControlOutsideLoop, controlFlow->GetToken());

		return controlFlow;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTStructExpr> structExpr, SemaContext context)
	{	
		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;
		valueContext.CallsiteArgs.clear();

		for (auto& value : structExpr->Values)
		{
			// lambdas wait for the field type (see CompleteStructValues)
			if (value->GetType() != ASTNodeType::Lambda)
				value = Visit(value, valueContext);

			if (!value)
				return nullptr;
		}

		// `Box { 7 }` where Box is generic: work out the type arguments from the field values
		if (auto var = std::dynamic_pointer_cast<ASTVariable>(structExpr->TargetType); var && !var->Variable)
		{
			auto [entry, scopeIndex] = LookupSymbol(var->GetName().GetData());

			if (entry && entry->Symbol->Kind == SymbolKind::GenericTemplate)
			{
				auto generated = InstantiateFromValues(var, entry->Symbol, scopeIndex, structExpr->Values);

				if (!generated)
					return nullptr;

				var->Variable = generated;
				return CompleteStructValues(structExpr);
			}
		}

		SemaContext typeContext = context;
		typeContext.AllowGenericInferenceFromArgs = false;
		typeContext.CallsiteArgs.clear();
		structExpr->TargetType = Visit(structExpr->TargetType, typeContext);

		if (!structExpr->TargetType)
			return nullptr;
		
		return CompleteStructValues(structExpr);
	}

	std::shared_ptr<ASTNodeBase> Sema::CompleteStructValues(std::shared_ptr<ASTStructExpr> structExpr)
	{
		auto type = GetTypeFromNode(structExpr->TargetType);

		if (!type && structExpr->TargetType->GetType() == ASTNodeType::Variable)
		{
			auto var = std::dynamic_pointer_cast<ASTVariable>(structExpr->TargetType);
			type = var->Variable && var->Variable->Kind == SymbolKind::Type ? var->Variable->GetType() : nullptr;
		}

		if (!type || !type->IsClass())
		{
			Report(DiagnosticCode_ExpectedType, GetNodeLocation(structExpr->TargetType));
			return nullptr;
		}

		auto classType = type->As<ClassType>();
		auto& members = classType->GetMemberValues();

		// rich enums start as their first case, unions as all zero bytes
		if (classType->IsVariant || classType->IsUnion)
		{
			if (!structExpr->Values.empty())
			{
				Report(classType->IsUnion ? DiagnosticCode_UnionNeedsField : DiagnosticCode_NeedsCaseValues, GetNodeLocation(structExpr->TargetType));
				return nullptr;
			}

			return std::make_shared<ASTZero>(type);
		}

		// the hidden vtable field is never written by hand: values start at the first real field
		if (classType->HasVTable && (structExpr->Values.empty() || structExpr->Values[0]->GetType() != ASTNodeType::VTableRef))
			structExpr->Values.insert(structExpr->Values.begin(), classType->MemberDefaults[0]);

		if (structExpr->Values.size() > members.size())
		{
			Token location = GetNodeLocation(structExpr->TargetType);
			size_t hidden = classType->HasVTable ? 1 : 0, fields = members.size() - hidden, given = structExpr->Values.size() - hidden;
			location.SetData(std::format("{}’ has {} field{}, but {} value{} given", classType->GetHash(), fields, fields == 1 ? "" : "s", 
										 given, given == 1 ? " was" : "s were"));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_TooManyValues, classType->GetHash().size());
			return nullptr;
		}

		size_t index = 0;
		for (const auto& [name, memberType] : members)
		{
			if (index < structExpr->Values.size())
			{
				// a lambda takes its parameter types from the field it is stored in
				if (structExpr->Values[index]->GetType() == ASTNodeType::Lambda)
				{
					structExpr->Values[index] = Visit(structExpr->Values[index], SemaContext { .ValueReq = ValueRequired::RValue, .GlobalState = false, .ExpectedType = memberType });

					if (!structExpr->Values[index])
						return nullptr;
				}

				structExpr->Values[index] = Coerce(structExpr->Values[index], memberType);
			}
			else
			{
				// fields that were not given take their default, or zero
				auto defaultValue = index < classType->MemberDefaults.size() ? classType->MemberDefaults[index] : nullptr;
				structExpr->Values.push_back(defaultValue ? defaultValue : std::make_shared<ASTZero>(memberType));
			}

			index++;
		}

		return structExpr;
	}

	std::shared_ptr<ASTNodeBase> Sema::BuildConstruction(std::shared_ptr<ASTFunctionCall> funcCall, std::shared_ptr<ASTVariable> target)
	{
		auto classType = target->Variable->GetType()->As<ClassType>();
		auto init = classType->MemberFunctions.find("__init__");

		// Number(f = 1.5): a union is built with at most one field set
		if (classType->IsUnion)
		{
			auto construct = std::make_shared<ASTUnionConstruct>();
			construct->Location = funcCall->Location;
			construct->UnionTy = classType;

			if (funcCall->Arguments.size() + funcCall->KeywordArguments.size() > 1)
			{
				Report(DiagnosticCode_UnionNeedsField, target->GetName());
				return nullptr;
			}

			if (!funcCall->KeywordArguments.empty())
			{
				auto& [name, value] = funcCall->KeywordArguments[0];
				auto member = classType->GetMember(name.GetData());

				if (!member || member.value()->Kind != SymbolKind::Type)
				{
					Report(DiagnosticCode_UnknownMember, name);
					return nullptr;
				}

				construct->FieldTy = member.value()->GetType();
				construct->Value = Coerce(value, construct->FieldTy);
			}
			else if (!funcCall->Arguments.empty())
			{
				construct->FieldTy = classType->GetMemberValueByIndex(0).value()->GetType();
				construct->Value = Coerce(funcCall->Arguments[0], construct->FieldTy);
			}

			return construct;
		}

		if (init == classType->MemberFunctions.end())
		{
			// no __init__: Point(1, 2) fills the fields in order, exactly like Point { 1, 2 }
			auto structExpr = std::make_shared<ASTStructExpr>();
			structExpr->Location = funcCall->Location;
			structExpr->TargetType = target;
			structExpr->Values.assign(funcCall->Arguments.begin(), funcCall->Arguments.end());

			// Point(y = 2, x = 1): keyword arguments name fields, the others keep their defaults
			if (!funcCall->KeywordArguments.empty())
			{
				auto& members = classType->GetMemberValues();
				structExpr->Values.resize(members.size());

				for (auto& [name, value] : funcCall->KeywordArguments)
				{
					auto index = classType->GetMemberValueIndex(name.GetData());

					if (!index || structExpr->Values[*index])
					{
						Token where = name;
						where.SetData(std::format("{}’ is not a field of ‘{}’ (or is given twice", name.GetData(), classType->GetHash()));
						m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_UnknownKeyword, name.GetData().size());
						return nullptr;
					}

					structExpr->Values[*index] = value;
				}

				// fields left out use their default (or zero)
				for (size_t i = 0; i < structExpr->Values.size(); i++)
				{
					if (!structExpr->Values[i])
					{
						auto defaultValue = i < classType->MemberDefaults.size() ? classType->MemberDefaults[i] : nullptr;
						structExpr->Values[i] = defaultValue ? defaultValue : std::make_shared<ASTZero>(classType->GetMemberValueByIndex(i).value()->GetType());
					}
				}
			}

			return CompleteStructValues(structExpr);
		}

		// default-initialize, then run __init__(&object, args...)
		auto initial = std::make_shared<ASTStructExpr>();
		initial->TargetType = target;

		auto construct = std::make_shared<ASTConstruct>();
		construct->Location = funcCall->Location;
		construct->ClassTy = classType;
		construct->Initial = CompleteStructValues(initial);
		construct->Self = std::make_shared<ASTSlot>(m_Module->GetTypeRegistry()->GetPointerTo(classType));

		auto callee = std::make_shared<ASTVariable>(Token(TokenType::Identifier, "__init__", target->GetName().GetSourceFile(), target->GetName().LineNumber, target->GetName().ColumnNumber));
		callee->Variable = init->second;

		construct->InitCall = std::make_shared<ASTFunctionCall>();
		construct->InitCall->Location = funcCall->Location;
		construct->InitCall->Callee = callee;
		construct->InitCall->Arguments.push_back(construct->Self);
		construct->InitCall->Arguments.append(funcCall->Arguments.begin(), funcCall->Arguments.end());

		if (!construct->Initial || !CheckCall(construct->InitCall))
			return nullptr;

		return construct;
	}

	std::optional<std::shared_ptr<Symbol>> Sema::LookupInModules(llvm::StringRef name)
	{
		if (m_LookupModule && m_LookupModule != m_Module)
		{
			if (auto symbol = m_LookupModule->Lookup(name))
				return symbol;
		}

		return m_Module->Lookup(name);
	}

	std::pair<std::optional<SymbolEntry>, size_t> Sema::LookupSymbol(llvm::StringRef name)
	{
		for (int64_t i = (int64_t)m_ScopeStack.size() - 1; i >= 0; i--)
		{
			if (auto entry = m_ScopeStack[i].Get(name))
				return { entry, (size_t)i };
		}

		if (auto symbol = LookupInModules(name))
			return { SymbolEntry { SymbolEntryType::None, symbol.value() }, 0 };

		return { std::nullopt, 0 };
	}

	static bool BindGenericType(std::shared_ptr<ASTNodeBase> pattern, std::shared_ptr<Type> actual, 
								llvm::ArrayRef<std::string> names, std::unordered_map<std::string, std::shared_ptr<Type>>& bindings)
	{
		if (!pattern || !actual)
			return false;

		if (auto var = std::dynamic_pointer_cast<ASTVariable>(pattern))
		{
			const std::string& name = var->GetName().GetData();

			if (std::find(names.begin(), names.end(), name) == names.end())
				return false;

			bindings.try_emplace(name, actual);
			return true;
		}

		// List[T] matched against an instance of List binds T to its type arguments
		if (auto subscript = std::dynamic_pointer_cast<ASTSubscript>(pattern); subscript && actual->IsClass())
		{
			auto target = std::dynamic_pointer_cast<ASTVariable>(subscript->Target);
			auto classType = actual->As<ClassType>();

			if (!target || target->GetName().GetData() != classType->GenericOrigin || subscript->SubscriptArgs.size() != classType->GenericArguments.size())
				return false;

			bool bound = false;

			for (size_t i = 0; i < subscript->SubscriptArgs.size(); i++)
				bound |= BindGenericType(subscript->SubscriptArgs[i], classType->GenericArguments[i], names, bindings);

			return bound;
		}

		// [N; T] matched against an array binds T to the element type
		if (auto array = std::dynamic_pointer_cast<ASTArrayType>(pattern); array && actual->IsArray())
			return BindGenericType(array->TypeNode, actual->As<ArrayType>()->GetBaseType(), names, bindings);

		// *T matched against a pointer binds T to the pointee
		if (auto unary = std::dynamic_pointer_cast<ASTUnaryExpression>(pattern); unary && unary->GetOperatorType() == OperatorType::Dereference && actual->IsPointer())
			return BindGenericType(unary->Operand, actual->As<PointerType>()->GetBaseType(), names, bindings);

		return false;
	}

	std::shared_ptr<Symbol> Sema::InstantiateFromValues(std::shared_ptr<ASTVariable> target, std::shared_ptr<Symbol> genericSymbol, size_t scopeIndex, 
														  llvm::ArrayRef<std::shared_ptr<ASTNodeBase>> values)
	{
		auto generic = std::dynamic_pointer_cast<ASTGenericTemplate>(genericSymbol->GetGenericTemplate().GenericTemplate);

		// what each value is matched against: class fields in order, or function parameters in order
		std::vector<std::shared_ptr<ASTNodeBase>> patterns;

		if (auto classNode = std::dynamic_pointer_cast<ASTClass>(generic->TemplateNode))
		{
			for (auto& member : classNode->Members)
				patterns.push_back(member->TypeResolver);
		}
		else if (auto function = std::dynamic_pointer_cast<ASTFunctionDefinition>(generic->TemplateNode))
		{
			for (auto& argument : function->Arguments)
				patterns.push_back(argument ? argument->TypeResolver : nullptr);
		}

		std::unordered_map<std::string, std::shared_ptr<Type>> bindings;

		for (size_t i = 0; i < values.size() && i < patterns.size(); i++)
		{
			if (values[i])
				BindGenericType(patterns[i], m_TypeInferEngine.InferTypeFromNode(values[i]), generic->GenericTypeNames, bindings);
		}

		llvm::SmallVector<Symbol> arguments;

		for (const auto& name : generic->GenericTypeNames)
		{
			auto it = bindings.find(name);

			if (it == bindings.end())
			{
				// a type parameter that no field value determines, e.g. Box { } with no values
				Report(DiagnosticCode_CannotInferGeneric, target->GetName());
				return nullptr;
			}

			arguments.push_back(Symbol::CreateType(it->second));
		}

		return SolveConstraints(target->GetName().GetData(), genericSymbol, scopeIndex, arguments);
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTGenericTemplate> generic, SemaContext context)
	{
		generic->HomeModule = m_Module;

		bool success = m_ScopeStack.back().Insert(generic->GetName(), SymbolEntryType::GenericTemplate, std::make_shared<Symbol>(Symbol::CreateGenericTemplate(generic)));
			
		if (!success)
			Report(DiagnosticCode_RedefinedIdentifier, Token(TokenType::Identifier, generic->GetName()));
		
		m_Module->ExposeSymbol(generic->GetName(), m_ScopeStack.back().Get(generic->GetName()).value().Symbol);
		return generic;	
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTSubscript> subscript, SemaContext context)
	{
		// Generator[T] and Task[T] are built in (unless the program has its own)
		if (auto target = std::dynamic_pointer_cast<ASTVariable>(subscript->Target); target && !target->Variable &&
			(target->GetName().GetData() == "Generator" || target->GetName().GetData() == "Task") && !LookupSymbol(target->GetName().GetData()).first)
		{
			if (subscript->SubscriptArgs.size() != 1)
			{
				Report(DiagnosticCode_ExpectedType, target->GetName());
				return nullptr;
			}

			auto argument = Visit(subscript->SubscriptArgs[0], SemaContext { .ValueReq = ValueRequired::Any, .TypeHint = context.TypeHint });
			auto valueType = argument ? GetTypeFromNode(argument) : nullptr;
			bool isNone = !valueType && argument && argument->GetType() == ASTNodeType::Variable && std::dynamic_pointer_cast<ASTVariable>(argument)->GetName().GetData() == "none";

			if (!valueType && !isNone)
			{
				Report(DiagnosticCode_ExpectedType, GetNodeLocation(subscript->SubscriptArgs[0]));
				return nullptr;
			}

			if (valueType && valueType->GetHash() == "none")
				valueType = nullptr;

			auto literal = std::make_shared<ASTTypeLiteral>(m_Module->GetTypeRegistry()->GetCoroutineOf(target->GetName().GetData() == "Task", valueType));
			literal->Location = target->GetName();
			return literal;
		}

		subscript->Target = Visit(subscript->Target, { .ValueReq = ValueRequired::LValue, .TypeHint = context.TypeHint, .AllowGenericInferenceFromArgs = false });
		subscript->Meaning = SubscriptSemantic::Generic;

		if (!subscript->Target)
			return nullptr;

		if (IsNodeValue(subscript->Target))
		{
			subscript->Meaning = SubscriptSemantic::ArrayIndex;	
		}

		for (auto& arg : subscript->SubscriptArgs)
		{
			arg = Visit(arg, { .ValueReq = ValueRequired::RValue });
		}

		if (subscript->Meaning == SubscriptSemantic::ArrayIndex)
		{
			auto targetType = m_TypeInferEngine.InferTypeFromNode(subscript->Target);

			if (!targetType)
				return nullptr;

			// f()[i]: index the computed pointer (or array) directly
			if (!IsStorageNode(subscript->Target) && !targetType->IsClass())
				subscript->TargetIsValue = true;

			// a class, or a pointer to a class, that defines indexing: obj[i] calls obj.__getitem__(i)
			std::shared_ptr<ClassType> clsType;
			std::shared_ptr<ASTNodeBase> self = subscript->Target;

			if (targetType->IsClass())
			{
				clsType = targetType->As<ClassType>();

				if (IsStorageNode(subscript->Target))
				{
					// the target is the object's storage, pass its address as self
					auto address = std::make_shared<ASTUnaryExpression>(OperatorType::Address);
					address->Operand = subscript->Target;
					address->Location = GetNodeLocation(subscript->Target);
					self = address;
				}
				else
				{
					// a computed object (grid[i][j], make_list()[0]): index a temporary copy of it
					self = AddressOf(subscript->Target);
				}
			}
			else if (targetType->IsPointer())
			{
				auto pointee = targetType->As<PointerType>()->GetBaseType();

				if (pointee && pointee->IsClass() && (pointee->As<ClassType>()->MemberFunctions.contains("__getitem__") || 
													  pointee->As<ClassType>()->MemberFunctions.contains("__setitem__")))
				{
					clsType = pointee->As<ClassType>();

					// the pointer value is the object's address
					auto load = std::make_shared<ASTLoad>();
					load->Operand = subscript->Target;
					self = load;
				}
			}

			if (clsType)
			{
				bool hasGet = clsType->MemberFunctions.contains("__getitem__");
				bool hasSet = clsType->MemberFunctions.contains("__setitem__");

				// other lvalue uses (len(obj[i]), obj[i].method()) only need the value __getitem__ returns
				bool assignTarget = context.AssignmentTarget && context.ValueReq == ValueRequired::LValue;

				if (!hasGet && !(hasSet && assignTarget))
				{
					Report(DiagnosticCode_MissingIndexOverload, GetNodeLocation(subscript->Target));
					return nullptr;
				}

				auto funcCall = std::make_shared<ASTFunctionCall>();
				funcCall->Location = GetNodeLocation(subscript->Target);
				funcCall->ClassType = clsType;
				funcCall->Arguments.push_back(self);
				funcCall->Arguments.append(subscript->SubscriptArgs.begin(), subscript->SubscriptArgs.end());

				auto var = std::make_shared<ASTVariable>(Token(TokenType::Identifier, "__getitem__", funcCall->Location.GetSourceFile(), funcCall->Location.LineNumber, funcCall->Location.ColumnNumber));
				funcCall->Callee = var;

				if (!hasGet)
					return funcCall; // only used as the target of an assignment, which calls __setitem__

				var->Variable = clsType->MemberFunctions.at("__getitem__");

				// an assignment rewrites this call into __setitem__, its arguments are checked there
				if (assignTarget && hasSet)
					return funcCall;

				return CheckCall(funcCall);
			}

			// tuple[i]: the index must be a constant
			if (targetType->IsTuple())
			{
				auto tupleType = targetType->As<TupleType>();
				auto value = subscript->SubscriptArgs.size() == 1 ? EvaluateInteger(subscript->SubscriptArgs[0]) : std::nullopt;

				if (!value || *value < 0 || (size_t)*value >= tupleType->GetElements().size())
				{
					Token location = GetNodeLocation(subscript->SubscriptArgs.empty() ? subscript->Target : subscript->SubscriptArgs[0]);
					location.SetData(std::format("{}’ (a tuple of {} needs a constant index 0 to {}", location.GetData(), tupleType->GetElements().size(), tupleType->GetElements().size() - 1));
					m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_IndexOutOfRange, 1);
					return nullptr;
				}

				auto get = std::make_shared<ASTTupleGet>();
				get->Location = GetNodeLocation(subscript->Target);
				get->Tuple = subscript->Target;
				get->TupleIsStorage = IsStorageNode(subscript->Target);
				get->WantAddress = context.ValueReq == ValueRequired::LValue;
				get->Index = (size_t)*value;
				get->TupleTy = tupleType;
				return get;
			}

			// a constant index into a fixed array is checked right here
			if (targetType->IsArray() && subscript->SubscriptArgs.size() >= 1)
			{
				auto value = EvaluateInteger(subscript->SubscriptArgs[0]);
				size_t size = targetType->As<ArrayType>()->GetArraySize();

				if (value && (*value < 0 || (uint64_t)*value >= size))
				{
					Token location = GetNodeLocation(subscript->SubscriptArgs[0]);
					location.SetData(std::format("{}’ is outside an array of {} (valid: 0 to {}", *value, size, size - 1));
					m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_IndexOutOfRange, std::to_string(*value).size());
					return nullptr;
				}
			}

			for (auto& index : subscript->SubscriptArgs)
			{
				auto indexType = m_TypeInferEngine.InferTypeFromNode(index);

				if (!indexType || !indexType->IsIntegral() || indexType->IsEnum() || indexType->Get()->isIntegerTy(1))
				{
					Token location = GetNodeLocation(index);
					location.SetData(std::format("{}’ is not an integer (‘{}", GetDisplayName(indexType), location.GetData()));
					m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_IndexNotInteger, 1);
					return nullptr;
				}
			}
			if (context.ValueReq == ValueRequired::RValue)
			{
				std::shared_ptr<ASTLoad> load = std::make_shared<ASTLoad>();
				load->Operand = subscript;
				return load;
			}
		} 
		else 
		{
			std::shared_ptr<ASTVariable> var = std::dynamic_pointer_cast<ASTVariable>(subscript->Target);
			CLEAR_VERIFY(var, "");
				
			std::shared_ptr<Symbol> genericSym;
			size_t scopeIndex = (size_t)-1;

			for (size_t i = m_ScopeStack.size(); i-- > 0; )
			{
				auto& table = m_ScopeStack[i];

				if (auto entry = table.Get(var->GetName().GetData()))
				{
					genericSym = entry.value().Symbol;
					scopeIndex = i; 
					break;
				}
			}

			if (!genericSym)
			{
				Report(DiagnosticCode_UndeclaredIdentifier, var->GetName());
				return nullptr;
			}
			
			llvm::SmallVector<Symbol> substitutedArgs;

			for (auto node : subscript->SubscriptArgs)
			{
				if (auto ty = GetTypeFromNode(node))
				{
					substitutedArgs.push_back(Symbol::CreateType(ty));
				}
				else 
				{
					m_ConstantEvaluator.Evaluate(node);
					substitutedArgs.push_back(Symbol::CreateValue(m_ConstantEvaluator.CurrentValue, m_TypeInferEngine.InferTypeFromNode(node)));
					m_ConstantEvaluator.CurrentValue = nullptr;
				}
			}

			subscript->GeneratedType = SolveConstraints(var->GetName().GetData(), genericSym, scopeIndex, substitutedArgs);

			if (!subscript->GeneratedType)
				return nullptr;
		}

		return subscript;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTArrayType> arrayType, SemaContext context)
	{
		arrayType->SizeNode = Visit(arrayType->SizeNode, context);
		arrayType->TypeNode = Visit(arrayType->TypeNode, context);

		std::shared_ptr<Type> baseTy = GetTypeFromNode(arrayType->TypeNode);

		if (!baseTy)
		{
			Report(DiagnosticCode_ExpectedType, GetNodeLocation(arrayType->TypeNode));
			return nullptr;
		}

		int64_t size = EvaluateInteger(arrayType->SizeNode).value_or(0);
			
		if (size <= 0)
		{
			Report(DiagnosticCode_InvalidArraySize, Token());
			return nullptr;
		}

		arrayType->GeneratedArrayType = m_Module->GetTypeRegistry()->GetArrayFrom(baseTy, (size_t)size);
		return arrayType;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTListExpr> listExpr, SemaContext context)
	{
		context.ValueReq = ValueRequired::RValue;
		for (auto& value : listExpr->Values)
		{
			value = Visit(value, context);

			if (!value)
				return nullptr;
		}

		// `{}` has no type of its own, it takes the type of whatever it initializes (see Coerce)
		if (listExpr->Values.empty())
			return listExpr;
		
		std::shared_ptr<Type> targetBaseType = m_TypeInferEngine.InferTypeFromNode(listExpr->Values[0]);

		for (size_t i = 1; i < listExpr->Values.size(); i++)
		{
			//TODO: insert cast expr if types not same
		}

		listExpr->ListType = m_Module->GetTypeRegistry()->GetArrayFrom(targetBaseType, listExpr->Values.size());
		return listExpr;
	}

	bool Sema::IsImplicitlyConvertible(std::shared_ptr<Type> from, std::shared_ptr<Type> to, bool fromLiteral)
	{
		if (from == to || from->GetHash() == to->GetHash())
			return true;

		llvm::Type* src = from->Get();
		llvm::Type* dst = to->Get();

		if (!src || !dst)
			return false;

		// enums never mix with plain integers (or other enums) without `as`
		if (from->IsEnum() || to->IsEnum())
			return false;

		// none only becomes an optional (handled in Coerce), never a plain value
		if (from->GetHash() == "none" || to->GetHash() == "none")
			return false;

		// pointers: null converts to anything, otherwise the pointee must match (or be opaque)
		if (src->isPointerTy() && dst->isPointerTy())
		{
			if (!from->IsPointer() || !to->IsPointer())
				return true;

			auto fromBase = from->As<PointerType>()->GetBaseType();
			auto toBase = to->As<PointerType>()->GetBaseType();

			// a *Dog is a *Animal: the base's fields are at the start of the derived class
			if (fromBase && toBase && fromBase->IsClass() && toBase->IsClass() && fromBase->As<ClassType>()->DerivesFrom(toBase->As<ClassType>()))
				return true;

			// str and *int8 point at the same thing
			return fromBase == toBase || !fromBase || !toBase || fromBase->Get()->isVoidTy() || toBase->Get()->isVoidTy();
		}

		bool srcInt = src->isIntegerTy(), dstInt = dst->isIntegerTy();
		bool srcFloat = src->isFloatingPointTy(), dstFloat = dst->isFloatingPointTy();

		// a numeric literal can become any numeric type (`let x: uint8 = 5`, `let f: float32 = 2.5`)
		if (fromLiteral && (srcInt || srcFloat) && (dstInt || dstFloat) && !dst->isIntegerTy(1))
			return !(srcFloat && dstInt);

		if (srcInt && dstInt)
		{
			unsigned srcBits = src->getIntegerBitWidth(), dstBits = dst->getIntegerBitWidth();

			if (dstBits == 1)
				return srcBits == 1;

			if (srcBits == 1 || srcBits == dstBits)
				return true; // bool -> int, or the same width with different signedness

			if (dstBits > srcBits)
				return from->IsSigned() == to->IsSigned() || !from->IsSigned(); // uint8 -> int16 is fine, int8 -> uint16 is not

			return false;
		}

		if (srcInt && dstFloat)
		{
			unsigned bits = src->getIntegerBitWidth();
			return dst->isDoubleTy() ? bits <= 32 : bits <= 16;
		}

		if (srcFloat && dstFloat)
			return dst->getPrimitiveSizeInBits() > src->getPrimitiveSizeInBits();

		return false;
	}

	static bool IsNumericLiteral(const std::shared_ptr<ASTNodeBase>& node)
	{
		if (auto literal = std::dynamic_pointer_cast<ASTNodeLiteral>(node))
			return literal->GetData().IsType(TokenType::Number) || literal->GetData().IsType(TokenType::Char);

		if (auto unary = std::dynamic_pointer_cast<ASTUnaryExpression>(node); unary && unary->GetOperatorType() == OperatorType::Negation)
			return IsNumericLiteral(unary->Operand);

		return false;
	}

	bool Sema::IsConstantThatFits(std::shared_ptr<ASTNodeBase> node, std::shared_ptr<Type> target)
	{
		// a compile-time integer behaves like a literal as long as the target can hold it exactly
		auto source = m_TypeInferEngine.InferTypeFromNode(node);

		if (!source || source->IsEnum() || !source->IsIntegral() || source->GetHash() == "bool")
			return false;

		auto value = EvaluateInteger(node);

		if (!value || !target->Get())
			return false;

		if (target->Get()->isFloatingPointTy())
			return true;

		if (!target->IsIntegral() || target->Get()->isIntegerTy(1))
			return false;

		unsigned bits = target->Get()->getIntegerBitWidth();

		if (target->IsSigned())
		{
			if (bits >= 64) return true;
			int64_t limit = (int64_t)1 << (bits - 1);
			return *value >= -limit && *value < limit;
		}

		if (*value < 0)
			return false;

		return bits >= 64 || (uint64_t)*value < ((uint64_t)1 << bits);
	}

	std::shared_ptr<ASTNodeBase> Sema::Coerce(std::shared_ptr<ASTNodeBase> node, std::shared_ptr<Type> target)
	{
		if (!node || !target)
			return node;

		// ?T accepts none and anything that converts to T
		if (target->IsClass() && target->As<ClassType>()->IsOptional)
		{
			auto optional = target->As<ClassType>();
			auto valueSource = m_TypeInferEngine.InferTypeFromNode(node);

			if (valueSource == target)
				return node;

			if (valueSource && valueSource->GetHash() == "none")
				return BuildVariantConstruct(target, optional->FindCase("none").value(), {}, {}, GetNodeLocation(node));

			auto valueType = optional->Cases[optional->FindCase("some").value()].Fields[0].second;
			auto converted = Coerce(node, valueType);
			return BuildVariantConstruct(target, optional->FindCase("some").value(), { converted }, {}, GetNodeLocation(node));
		}

		// a tuple literal converts element by element
		if (auto tuple = std::dynamic_pointer_cast<ASTTupleExpr>(node); tuple && !tuple->IsType && target->IsTuple())
		{
			auto& elements = target->As<TupleType>()->GetElements();

			if (elements.size() != tuple->Values.size())
			{
				Token location = GetNodeLocation(node);
				location.SetData(std::format("{}’ to ‘{}", GetDisplayName(tuple->TupleTy), GetDisplayName(target)));
				m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_ImplicitConversion, 1);
				return node;
			}

			for (size_t i = 0; i < elements.size(); i++)
				tuple->Values[i] = Coerce(tuple->Values[i], elements[i]);

			tuple->TupleTy = target;
			return tuple;
		}

		// an array literal for a declared array: convert each element, missing elements are zero (`{}` is all zeros)
		if (auto list = std::dynamic_pointer_cast<ASTListExpr>(node); list && target->IsArray())
		{
			auto arrayType = target->As<ArrayType>();

			if (list->Values.size() > arrayType->GetArraySize())
			{
				Token location = GetNodeLocation(node);
				location.SetData(std::format("{} values do not fit in ‘{}", list->Values.size(), GetDisplayName(target)));
				m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_ImplicitConversion, 1);
				return node;
			}

			for (auto& value : list->Values)
				value = Coerce(value, arrayType->GetBaseType());

			while (list->Values.size() < arrayType->GetArraySize())
				list->Values.push_back(std::make_shared<ASTZero>(arrayType->GetBaseType()));

			list->ListType = target;
			return list;
		}

		std::shared_ptr<Type> source = m_TypeInferEngine.InferTypeFromNode(node);

		if (!source || source == target)
			return node;

		// a literal adapts to the target type only if its value fits (`let x: uint8 = 300` is an error)
		bool isLiteral = IsNumericLiteral(node);
		bool literalIsInteger = isLiteral && source->IsIntegral() && target->IsIntegral() && !target->IsEnum();

		if (literalIsInteger && !IsConstantThatFits(node, target))
		{
			Token location = GetNodeLocation(node);
			auto value = EvaluateInteger(node);
			location.SetData(std::format("{}’ does not fit in ‘{}", value ? std::to_string(*value) : location.GetData(), GetDisplayName(target)));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_LiteralOutOfRange, 1);
			return node;
		}

		// a lambda that captures variables is an object, not a plain function
		if (target->IsFunction() && source->IsClass() && source->GetHash().starts_with("__closure_"))
		{
			Report(DiagnosticCode_ClosureNotFunction, GetNodeLocation(node));
			return node;
		}

		if (!IsImplicitlyConvertible(source, target, isLiteral || IsConstantThatFits(node, target)))
		{
			Token location = GetNodeLocation(node);
			size_t width = std::max<size_t>(location.GetData().size(), 1);

			location.SetData(std::format("{}’ to ‘{}", GetDisplayName(source), GetDisplayName(target)));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_ImplicitConversion, width);
			return node;
		}

		if (source->Get() == target->Get())
			return node; // nothing to do at the machine level

		std::shared_ptr<ASTCastExpr> cast = std::make_shared<ASTCastExpr>();
		cast->Object = node;
		cast->TargetType = target;
		cast->Location = GetNodeLocation(node);

		return cast;
	}

	void Sema::Report(DiagnosticCode code, Token token)
	{
		// TODO: change CodeGeneration to Semanatic Analysis
		m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, token, code);
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitBinaryExprArithmetic(std::shared_ptr<ASTBinaryExpression> binaryExpression, SemaContext context)
	{
		context.ValueReq = ValueRequired::RValue;

		binaryExpression->LeftSide = Visit(binaryExpression->LeftSide, context);
		binaryExpression->RightSide = Visit(binaryExpression->RightSide, context);

		if (!binaryExpression->LeftSide || !binaryExpression->RightSide)
			return nullptr;

		if (auto overload = TryOperatorOverload(binaryExpression))
			return overload.value();

		if (!CheckOperands(binaryExpression))
			return nullptr;
		
		binaryExpression->ResultantType = m_TypeInferEngine.InferTypeFromNode(binaryExpression);
		return binaryExpression;
	}

	static const char* GetDunderName(OperatorType op)
	{
		switch (op)
		{
			case OperatorType::Add:					return "__add__";
			case OperatorType::Sub:					return "__sub__";
			case OperatorType::Mul:					return "__mul__";
			case OperatorType::Div:					return "__div__";
			case OperatorType::Mod:					return "__mod__";
			case OperatorType::Power:				return "__pow__";
			case OperatorType::IsEqual:				return "__eq__";
			case OperatorType::NotEqual:			return "__ne__";
			case OperatorType::LessThan:			return "__lt__";
			case OperatorType::LessThanEqual:		return "__le__";
			case OperatorType::GreaterThan:			return "__gt__";
			case OperatorType::GreaterThanEqual:	return "__ge__";
			default:								return nullptr;
		}
	}

	static const char* GetOperatorSpelling(OperatorType op)
	{
		switch (op)
		{
			case OperatorType::Add: return "+";   case OperatorType::Sub: return "-";
			case OperatorType::Mul: return "*";   case OperatorType::Div: return "/";
			case OperatorType::Mod: return "%";   case OperatorType::IsEqual: return "==";
			case OperatorType::Power: return "**";   case OperatorType::In: return "in";   case OperatorType::NotIn: return "not in";
			case OperatorType::NotEqual: return "!=";   case OperatorType::LessThan: return "<";
			case OperatorType::LessThanEqual: return "<=";   case OperatorType::GreaterThan: return ">";
			case OperatorType::GreaterThanEqual: return ">=";   case OperatorType::BitwiseAnd: return "&";
			case OperatorType::BitwiseOr: return "|";   case OperatorType::BitwiseXor: return "^";
			case OperatorType::LeftShift: return "<<";   case OperatorType::RightShift: return ">>";
			case OperatorType::And: return "and";   case OperatorType::Or: return "or";
			default: return "?";
		}
	}

	std::shared_ptr<ASTNodeBase> Sema::AddressOf(std::shared_ptr<ASTNodeBase> node)
	{
		// a loaded variable/field already has an address, anything else gets a temporary
		if (auto load = std::dynamic_pointer_cast<ASTLoad>(node))
			return load->Operand;

		auto temporary = std::make_shared<ASTTemporary>();
		temporary->Operand = node;
		temporary->ValueType = m_TypeInferEngine.InferTypeFromNode(node);
		temporary->Location = GetNodeLocation(node);
		return temporary;
	}

	std::optional<std::shared_ptr<ASTNodeBase>> Sema::TryOperatorOverload(std::shared_ptr<ASTBinaryExpression> expr)
	{
		auto lhsType = m_TypeInferEngine.InferTypeFromNode(expr->LeftSide);

		if (!lhsType || !lhsType->IsClass())
			return std::nullopt;

		auto classType = lhsType->As<ClassType>();
		const char* name = GetDunderName(expr->GetExpression());
		bool negate = false;

		auto findMethod = [&](const char* methodName) -> std::shared_ptr<Symbol>
		{
			if (!methodName) return nullptr;
			auto it = classType->MemberFunctions.find(methodName);
			return it == classType->MemberFunctions.end() ? nullptr : it->second;
		};

		std::shared_ptr<Symbol> method = findMethod(name);

		// a != b falls back to not (a == b)
		if (!method && expr->GetExpression() == OperatorType::NotEqual)
		{
			method = findMethod("__eq__");
			negate = true;
		}

		Token location = expr->Location;

		if (!method)
		{
			location.SetData(std::format("{}’ has no {} method for ‘{}", classType->GetHash(), name ? name : "operator", GetOperatorSpelling(expr->GetExpression())));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_MissingOperatorOverload, 1);
			return std::shared_ptr<ASTNodeBase>(nullptr);
		}

		auto function = method->GetFunctionSymbol().FunctionNode;
		EnsureDefined(function);

		if (function->Arguments.size() != 2)
		{
			location.SetData(std::format("{}.{}", classType->GetHash(), negate ? "__eq__" : name));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_BadOperatorSignature, 1);
			return std::shared_ptr<ASTNodeBase>(nullptr);
		}

		auto callee = std::make_shared<ASTVariable>(Token(TokenType::Identifier, negate ? "__eq__" : name, location.GetSourceFile(), location.LineNumber, location.ColumnNumber));
		callee->Variable = method;

		auto call = std::make_shared<ASTFunctionCall>();
		call->Location = location;
		call->Callee = callee;
		call->Arguments.push_back(AddressOf(expr->LeftSide));

		// the other operand is passed the way the method declares it: by value or by pointer
		auto otherType = function->Arguments[1]->ResolvedType;
		auto rhsType = m_TypeInferEngine.InferTypeFromNode(expr->RightSide);

		if (otherType && otherType->IsPointer() && rhsType && rhsType->IsClass())
			call->Arguments.push_back(AddressOf(expr->RightSide));
		else
			call->Arguments.push_back(Coerce(expr->RightSide, otherType));

		if (!negate)
			return std::shared_ptr<ASTNodeBase>(call);

		auto notNode = std::make_shared<ASTUnaryExpression>(OperatorType::Not);
		notNode->Operand = call;
		notNode->Location = location;
		return std::shared_ptr<ASTNodeBase>(notNode);
	}

	bool Sema::CheckOperands(std::shared_ptr<ASTBinaryExpression> expr)
	{
		auto lhs = m_TypeInferEngine.InferTypeFromNode(expr->LeftSide);
		auto rhs = m_TypeInferEngine.InferTypeFromNode(expr->RightSide);

		if (!lhs || !rhs)
			return false;

		auto isBool    = [](std::shared_ptr<Type> t) { return t->Get()->isIntegerTy(1); };
		auto isNumber  = [&](std::shared_ptr<Type> t) { return (t->IsIntegral() || t->IsFloatingPoint()) && !t->IsEnum() && !isBool(t); };
		auto isInteger = [&](std::shared_ptr<Type> t) { return t->IsIntegral() && !t->IsEnum() && !isBool(t); };
		auto isPointer = [](std::shared_ptr<Type> t) { return t->Get()->isPointerTy(); };
		auto truthy    = [&](std::shared_ptr<Type> t) { return t->IsIntegral() || t->IsFloatingPoint() || isPointer(t); };

		bool valid = false;

		switch (expr->GetExpression())
		{
			case OperatorType::Add:
			case OperatorType::Sub:
				valid = (isNumber(lhs) && isNumber(rhs)) || (isPointer(lhs) && isInteger(rhs));
				break;
			case OperatorType::Mul:
			case OperatorType::Div:
			case OperatorType::Mod:
			case OperatorType::Power:
				valid = isNumber(lhs) && isNumber(rhs);

				if (valid && (expr->GetExpression() == OperatorType::Div || expr->GetExpression() == OperatorType::Mod) && isInteger(lhs) && isInteger(rhs))
				{
					if (auto divisor = EvaluateInteger(expr->RightSide); divisor && *divisor == 0)
					{
						Report(DiagnosticCode_DivisionByZero, GetNodeLocation(expr->RightSide));
						return false;
					}
				}
				break;
			case OperatorType::BitwiseAnd:
			case OperatorType::BitwiseOr:
			case OperatorType::BitwiseXor:
				valid = (isInteger(lhs) && isInteger(rhs)) || (isBool(lhs) && isBool(rhs));
				break;
			case OperatorType::LeftShift:
			case OperatorType::RightShift:
				valid = isInteger(lhs) && isInteger(rhs);

				if (valid)
				{
					auto amount = EvaluateInteger(expr->RightSide);
					unsigned bits = lhs->Get()->getIntegerBitWidth();

					if (amount && (*amount < 0 || *amount >= (int64_t)bits))
					{
						Token location = GetNodeLocation(expr->RightSide);
						location.SetData(std::format("{}’ for a {} bit value (valid: 0 to {}", *amount, bits, bits - 1));
						m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_ShiftOutOfRange, 1);
						return false;
					}
				}
				break;
			case OperatorType::IsEqual:
			case OperatorType::NotEqual:
				valid = (isNumber(lhs) && isNumber(rhs)) || (isPointer(lhs) && isPointer(rhs)) ||
						(isBool(lhs) && isBool(rhs)) || (lhs->IsEnum() && lhs->GetHash() == rhs->GetHash());
				break;
			case OperatorType::LessThan:
			case OperatorType::LessThanEqual:
			case OperatorType::GreaterThan:
			case OperatorType::GreaterThanEqual:
				valid = (isNumber(lhs) && isNumber(rhs)) || (isPointer(lhs) && isPointer(rhs)) ||
						(lhs->IsEnum() && lhs->GetHash() == rhs->GetHash());
				break;
			case OperatorType::And:
			case OperatorType::Or:
				valid = truthy(lhs) && truthy(rhs);
				break;
			default:
				valid = true;
				break;
		}

		if (!valid)
		{
			Token location = expr->Location;
			location.SetData(std::format("{} {} {}", GetDisplayName(lhs), GetOperatorSpelling(expr->GetExpression()), GetDisplayName(rhs)));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_InvalidOperands, 1);
		}

		return valid;
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitBinaryExprMemberAccess(std::shared_ptr<ASTBinaryExpression> binaryExpr, SemaContext context)
	{
		bool insertLoad = context.ValueReq == ValueRequired::RValue;

		context.ValueReq = ValueRequired::LValue;
		binaryExpr->LeftSide = Visit(binaryExpr->LeftSide, context);

		if (std::shared_ptr<ASTVariable> var = std::dynamic_pointer_cast<ASTVariable>(binaryExpr->LeftSide); var && var->Variable->Kind == SymbolKind::Module)
		{
			std::shared_ptr<ASTVariable> member = std::dynamic_pointer_cast<ASTVariable>(binaryExpr->RightSide);
			std::shared_ptr<Symbol> symbol = var->Variable->GetModule()->GetExposedSymbols().at(member->GetName().GetData());
			binaryExpr->ResultantType = symbol->GetType();
			
			return binaryExpr;
		}
		
		// Shape.Empty: a rich enum case without data
		if (std::shared_ptr<ASTVariable> var = std::dynamic_pointer_cast<ASTVariable>(binaryExpr->LeftSide); 
			var && var->Variable && var->Variable->Kind == SymbolKind::Type && var->Variable->GetType()->IsClass() && var->Variable->GetType()->As<ClassType>()->IsVariant)
		{
			auto variantType = var->Variable->GetType();
			auto member = std::dynamic_pointer_cast<ASTVariable>(binaryExpr->RightSide);
			auto index = member ? variantType->As<ClassType>()->FindCase(member->GetName().GetData()) : std::nullopt;

			if (!index)
			{
				Report(DiagnosticCode_UnknownMember, member ? member->GetName() : GetNodeLocation(binaryExpr->RightSide));
				return nullptr;
			}

			if (!variantType->As<ClassType>()->Cases[*index].Fields.empty())
			{
				Token location = member->GetName();
				location.SetData(std::format("{}.{}", variantType->GetHash(), member->GetName().GetData()));
				Report(DiagnosticCode_NeedsCaseValues, location);
				return nullptr;
			}

			return BuildVariantConstruct(variantType, *index, {}, {}, member->GetName());
		}

		// Color.Red
		if (std::shared_ptr<ASTVariable> var = std::dynamic_pointer_cast<ASTVariable>(binaryExpr->LeftSide); 
			var && var->Variable && var->Variable->Kind == SymbolKind::Type && var->Variable->GetType()->IsEnum())
		{
			auto enumType = std::dynamic_pointer_cast<EnumType>(var->Variable->GetType());
			auto member = std::dynamic_pointer_cast<ASTVariable>(binaryExpr->RightSide);
			auto value = member ? enumType->GetValue(member->GetName().GetData()) : std::nullopt;

			if (!value)
			{
				Report(DiagnosticCode_UnknownMember, member ? member->GetName() : GetNodeLocation(binaryExpr->RightSide));
				return nullptr;
			}

			auto constant = std::make_shared<ASTConstantValue>(*value, enumType);
			constant->Location = var->GetName();
			return constant;
		}

		std::shared_ptr<Type> lhsType = m_TypeInferEngine.InferTypeFromNode(binaryExpr->LeftSide);

		if (!lhsType)
			return binaryExpr;

		// task.run(), gen.advance() ...: the call that follows handles these
		if (std::dynamic_pointer_cast<CoroutineType>(lhsType))
			return binaryExpr;

		if (!lhsType->IsPointer() && !lhsType->IsClass())
		{
			Report(DiagnosticCode_InvalidMemberAccess, GetNodeLocation(binaryExpr->RightSide));
			return nullptr;
		}

		// optional.value
		if (lhsType->IsClass() && lhsType->As<ClassType>()->IsOptional)
		{
			auto member = std::dynamic_pointer_cast<ASTVariable>(binaryExpr->RightSide);

			if (!member || member->GetName().GetData() != "value")
			{
				Report(DiagnosticCode_UnknownMember, member ? member->GetName() : GetNodeLocation(binaryExpr->RightSide));
				return nullptr;
			}

			auto unwrap = std::make_shared<ASTOptionalUnwrap>();
			unwrap->Location = member->GetName();
			unwrap->Subject = AsValue(binaryExpr->LeftSide);
			unwrap->OptionalTy = lhsType;
			return unwrap;
		}

		while (lhsType->IsPointer())
			lhsType = lhsType->As<PointerType>()->GetBaseType();

		if (binaryExpr->RightSide->GetType() != ASTNodeType::Variable)
		{
			Report(DiagnosticCode_InvalidMemberAccess, Token());
			return nullptr;
		}
		
		std::shared_ptr<ASTVariable> member = std::dynamic_pointer_cast<ASTVariable>(binaryExpr->RightSide);
		std::optional<std::shared_ptr<Symbol>> memberSymbol = lhsType->As<ClassType>()->GetMember(
			member->GetName().GetData()
		);

		if (!memberSymbol)
		{
			Report(DiagnosticCode_UnknownMember, member->GetName());
			return nullptr;
		}

		// obj.area where area is a property: call its getter
		if (memberSymbol.value()->Kind == SymbolKind::Function && memberSymbol.value()->GetFunctionSymbol().FunctionNode->IsProperty)
		{
			auto getterFunction = memberSymbol.value()->GetFunctionSymbol().FunctionNode;
			EnsureDefined(getterFunction);

			auto result = CallMethod(binaryExpr->LeftSide, m_TypeInferEngine.InferTypeFromNode(binaryExpr->LeftSide), member->GetName().GetData(), {}, member->GetName());

			if (auto call = std::dynamic_pointer_cast<ASTFunctionCall>(result))
				call->PropertyName = member->GetName().GetData();

			return result;
		}

		binaryExpr->ResultantType = memberSymbol.value()->GetType(); 
		// TODO: check if has function, public and private members etc...
		
		if (insertLoad)
		{
			std::shared_ptr<ASTLoad> loadOp = std::make_shared<ASTLoad>();
			loadOp->Operand = binaryExpr;
			return loadOp;
		}

		return binaryExpr;
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitBinaryExprBoolean(std::shared_ptr<ASTBinaryExpression> binaryExpr, SemaContext context)
	{
		context.ValueReq = ValueRequired::RValue;

		binaryExpr->LeftSide = Visit(binaryExpr->LeftSide, context);
		binaryExpr->RightSide = Visit(binaryExpr->RightSide, context);

		if (!binaryExpr->LeftSide || !binaryExpr->RightSide)
			return nullptr;

		if (auto overload = TryOperatorOverload(binaryExpr))
			return overload.value();

		if (!CheckOperands(binaryExpr))
			return nullptr;

		binaryExpr->ResultantType = Symbol::GetBooleanType(m_Module).GetType();
		return binaryExpr;
	}

	bool Sema::IsNodeValue(std::shared_ptr<ASTNodeBase> node)
	{
		//TODO: may not always be the case as we may allow nested types in the future
		switch (node->GetType())
		{
			case ASTNodeType::Literal:
			{
				return true;
			}
			case ASTNodeType::Variable:
			{
				std::shared_ptr<ASTVariable> variable = std::dynamic_pointer_cast<ASTVariable>(node);
				return variable->Variable->Kind == SymbolKind::Value || variable->Variable->Kind == SymbolKind::Function;
			}
			case ASTNodeType::BinaryExpression:
			{
				std::shared_ptr<ASTBinaryExpression> binaryExpr = std::dynamic_pointer_cast<ASTBinaryExpression>(node);
				return true;
			}
			case ASTNodeType::Subscript:
			{
				std::shared_ptr<ASTSubscript> subscript = std::dynamic_pointer_cast<ASTSubscript>(node);
				return subscript->Meaning == SubscriptSemantic::ArrayIndex;
			}
			case ASTNodeType::Load:
			{
				return true;
			}
			case ASTNodeType::UnaryExpression:
			{
				return true;
			}
			case ASTNodeType::TypeLiteral:
			case ASTNodeType::ArrayType:
			case ASTNodeType::FunctionTypeExpr:
			case ASTNodeType::TypeSpecifier:
			case ASTNodeType::GenericTemplate:
				return false;
			case ASTNodeType::TupleExpr:
				return !std::dynamic_pointer_cast<ASTTupleExpr>(node)->IsType;
			default:
			{
				// calls, casts, constants...: anything else computes a value
				return true;
			}
		}
	
		CLEAR_UNREACHABLE("unimplemented");
		return false;
	}

	std::shared_ptr<Type> Sema::GetTypeFromNode(std::shared_ptr<ASTNodeBase> node)
	{
		switch (node->GetType())
		{
			case ASTNodeType::Variable:
			{
				std::shared_ptr<ASTVariable> var = std::dynamic_pointer_cast<ASTVariable>(node);
				auto sym = var->Variable ? std::optional(var->Variable) : LookupInModules(var->GetName().GetData());

				if (!sym || sym.value()->Kind != SymbolKind::Type)
					return nullptr;

				return sym.value()->GetType();
			}

			case ASTNodeType::UnaryExpression:
			{
				std::shared_ptr<ASTUnaryExpression> unary = std::dynamic_pointer_cast<ASTUnaryExpression>(node);

				if (unary->GetOperatorType() == OperatorType::Dereference)
				{
					std::shared_ptr<Type> base = GetTypeFromNode(unary->Operand);
					return base ? m_Module->GetTypeRegistry()->GetPointerTo(base) : nullptr;
				}

				if (unary->GetOperatorType() == OperatorType::Optional)
				{
					std::shared_ptr<Type> base = GetTypeFromNode(unary->Operand);
					return base ? GetOptionalType(base) : nullptr;
				}
			}
			case ASTNodeType::Subscript:
			{
				std::shared_ptr<ASTSubscript> subscript = std::dynamic_pointer_cast<ASTSubscript>(node);
				
				if (subscript->GeneratedType && subscript->GeneratedType->Kind != SymbolKind::None)
				{
					return subscript->GeneratedType->GetType();
				}
				
				return GetTypeFromNode(subscript->Target);
			}
			case ASTNodeType::ArrayType:
			{
				std::shared_ptr<ASTArrayType> arrayType = std::dynamic_pointer_cast<ASTArrayType>(node);
				return arrayType->GeneratedArrayType;
			}
			case ASTNodeType::TupleExpr:
			{
				auto tuple = std::dynamic_pointer_cast<ASTTupleExpr>(node);
				return tuple->IsType ? tuple->TupleTy : nullptr;
			}
			case ASTNodeType::FunctionTypeExpr:	return std::dynamic_pointer_cast<ASTFunctionTypeExpr>(node)->ResolvedType;
			case ASTNodeType::TypeLiteral:		return std::dynamic_pointer_cast<ASTTypeLiteral>(node)->ResolvedType;
			default:
			{
				break;
			}
		}

		return nullptr;
	}

	void Sema::ConstructSymbol(std::shared_ptr<Symbol> symbol, std::shared_ptr<ASTNodeBase> clonnedNode)
	{
		CLEAR_VERIFY(symbol->Kind == SymbolKind::Generic, "");

		switch (clonnedNode->GetType())
		{
			case ASTNodeType::Class:
			{
				*symbol->GetGeneric().GeneratedSymbol = Symbol::CreateType(std::dynamic_pointer_cast<ASTClass>(clonnedNode)->ClassTy); 
				break;
			}
			case ASTNodeType::FunctionDefinition:
			{
				symbol->GetGeneric().GeneratedSymbol = std::dynamic_pointer_cast<ASTFunctionDefinition>(clonnedNode)->FunctionSymbol; 
				break;
			}
			default:
			{
				CLEAR_UNREACHABLE("Unimplemented");
				break;
			}
		}
	}

	void Sema::ChangeNameOfNode(llvm::StringRef newName, std::shared_ptr<ASTNodeBase> clonnedNode)
	{
		switch (clonnedNode->GetType())
		{
			case ASTNodeType::Class:
			{
				std::dynamic_pointer_cast<ASTClass>(clonnedNode)->SetName(newName); 
				break;
			}
			case ASTNodeType::FunctionDefinition:
			{
				std::dynamic_pointer_cast<ASTFunctionDefinition>(clonnedNode)->SetName(std::string(newName)); 
				break;
			}
			default:
			{
				CLEAR_UNREACHABLE("Unimplemented");
				break;
			}
		}
	}

	
	std::shared_ptr<Symbol> Sema::SolveConstraints(llvm::StringRef name, std::shared_ptr<Symbol> genericSymbol, size_t scopeIndex, llvm::ArrayRef<Symbol> substitutedArgs)
	{
		std::string instanceName = m_NameMangler.MangleGeneric(name, substitutedArgs);
		std::shared_ptr<Symbol> instanceSymbol;

		// one instance per set of type arguments, wherever in the file it is asked for
		if (auto it = m_GenericInstances.find(instanceName); it != m_GenericInstances.end())
			return it->second->GetGeneric().GeneratedSymbol;

		for (auto rit = m_ScopeStack.rbegin(); rit != m_ScopeStack.rend(); rit++)
		{
			if (auto entry = rit->Get(instanceName))
			{
				instanceSymbol = entry.value().Symbol;
				break;
			}
		}

		if (instanceSymbol)
			return instanceSymbol->GetGeneric().GeneratedSymbol;

		GenericTemplateSymbol genericTemplate = genericSymbol->GetGenericTemplate();
		std::shared_ptr<ASTGenericTemplate> node = std::dynamic_pointer_cast<ASTGenericTemplate>(genericTemplate.GenericTemplate);

		// [T: Shape]: checked once, when the instance is first asked for
		for (size_t i = 0; i < substitutedArgs.size() && i < node->Constraints.size(); i++)
		{
			const Token& constraint = node->Constraints[i];

			if (constraint.GetData().empty())
				continue;

			auto trait = FindTrait(constraint.GetData(), node->HomeModule);

			if (!trait)
			{
				Report(DiagnosticCode_ExpectedType, constraint);
				return nullptr;
			}

			auto argument = substitutedArgs[i].Kind == SymbolKind::Type ? substitutedArgs[i].GetType() : nullptr;
			auto argumentClass = ClassOf(argument);

			if (!argumentClass || !argumentClass->As<ClassType>()->Satisfies(trait))
			{
				Token where = constraint;
				where.SetData(std::format("‘{}’ does not satisfy ‘{}’ (needed by ‘{}’). Declare it as class {}({})", 
										  argument ? GetDisplayName(argument) : "?", trait->GetHash(), std::string(name), argument ? GetDisplayName(argument) : "?", trait->GetHash()));
				m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_TraitNotSatisfied, constraint.GetData().size());
				return nullptr;
			}
		}
		
		Cloner cloner;
		cloner.DestinationModule = m_Module;
		
		for (size_t i = 0; i < substitutedArgs.size(); i++)
		{
			cloner.SubstitutionMap[node->GenericTypeNames[i]] = substitutedArgs[i]; 
		}
		
		std::shared_ptr<ASTNodeBase> clonned = cloner.Clone(node->TemplateNode);
		ChangeNameOfNode(instanceName, clonned);

		instanceSymbol = std::make_shared<Symbol>(Symbol::CreateGeneric(clonned));
		m_GenericInstances[instanceName] = instanceSymbol;

		// methods of generic classes are only analysed when used, so List[T] can hold types that lack some operations
		if (auto classNode = std::dynamic_pointer_cast<ASTClass>(clonned))
		{
			classNode->LazyMethods = true;
			classNode->TemplateName = std::string(name);
		}
		bool success = m_ScopeStack[scopeIndex].Insert(instanceName, SymbolEntryType::None, instanceSymbol);
		CLEAR_VERIFY(success, ""); //TODO Report(...)

		// a generic function's symbol exists before its body is analysed, so it can call itself
		if (auto function = std::dynamic_pointer_cast<ASTFunctionDefinition>(clonned))
		{
			function->IsGenericInstance = true;
			instanceSymbol->GetGeneric().GeneratedSymbol = function->FunctionSymbol;
		}

		auto pendingNode = clonned.get();
		m_PendingInstances[pendingNode] = instanceSymbol;

		if (node->HomeModule && node->HomeModule != m_Module)
		{
			// a template from another file: analyse it with that file's names, as if it were written there
			std::vector<SymbolTable> callerScopes = std::move(m_ScopeStack);
			m_ScopeStack = node->HomeModule->GlobalScopes;

			if (m_ScopeStack.empty())
				m_ScopeStack.emplace_back();

			m_ScopeStack.back().Insert(instanceName, SymbolEntryType::None, instanceSymbol);

			auto previousLookup = m_LookupModule;
			m_LookupModule = node->HomeModule;

			clonned = Visit(clonned);

			m_LookupModule = previousLookup;
			m_ScopeStack = std::move(callerScopes);
		}
		else
		{
			// analyse the instance where the template was declared: it must not see the caller's local variables
			std::vector<SymbolTable> callerScopes(std::make_move_iterator(m_ScopeStack.begin() + scopeIndex + 1), std::make_move_iterator(m_ScopeStack.end()));
			m_ScopeStack.resize(scopeIndex + 1);

			clonned = Visit(clonned);

			m_ScopeStack.insert(m_ScopeStack.end(), std::make_move_iterator(callerScopes.begin()), std::make_move_iterator(callerScopes.end()));
		}

		m_PendingInstances.erase(pendingNode);

		if (!clonned)
			return nullptr;

		if (auto classNode = std::dynamic_pointer_cast<ASTClass>(clonned); classNode && classNode->ClassTy)
		{
			auto classType = classNode->ClassTy->As<ClassType>();
			classType->GenericOrigin = node->GetName();

			for (const auto& argument : substitutedArgs)
				classType->GenericArguments.push_back(argument.Kind == SymbolKind::Type ? argument.GetType() : nullptr);
		}

		ConstructSymbol(instanceSymbol, clonned);

		return instanceSymbol->GetGeneric().GeneratedSymbol;
	}
}
