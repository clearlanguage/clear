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

			if (kind == ASTNodeType::Import || kind == ASTNodeType::Enum || kind == ASTNodeType::GenericTemplate)
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
		}

		// failed declarations were replaced by null, drop them so code generation never sees them
		std::erase(children, nullptr);
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTTypeSpecifier> type, SemaContext context)
	{	
		if (!type->TypeResolver)
		{
			if (!type->IsVariadic)
				Report(DiagnosticCode_ExpectedType, Token(TokenType::Identifier, type->GetName()));
			
			return type;
		}

		Visit(type->TypeResolver, context);
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
			Visit(decl->TypeResolver, context);
			decl->ResolvedType = GetTypeFromNode(decl->TypeResolver);
			
			if (!decl->ResolvedType)
			{
				Report(DiagnosticCode_ExpectedType, GetNodeLocation(decl->TypeResolver));
				return nullptr;
			}

			context.ValueReq = ValueRequired::RValue;
			if (decl->Initializer)
			{
				decl->Initializer = Visit(decl->Initializer, context);

				if (!decl->Initializer)
					return nullptr; // already reported
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
			auto sym = m_Module->Lookup(variable->GetName().GetData());
			
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
		}
		
		variable->Variable = symbol.value().Symbol;

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
				Visit(arg, context);
		}
		
		if (func->ReturnType)
		{
			Visit(func->ReturnType, context);
			func->ReturnTypeVal = GetTypeFromNode(func->ReturnType);

			if (!func->ReturnTypeVal)
			{
				Report(DiagnosticCode_ExpectedType, GetNodeLocation(func->ReturnType));
				m_ScopeStack.pop_back();
				return false;
			}
		}

		if (context.TypeHint)
			func->SetName(std::format("{}.{}", context.TypeHint->GetHash(), func->GetName()));

		//TODO: temporary, until we have the clear runtime make a main function we will have to ignore mangling for main
		std::string mangledName = func->GetName() != "main" ? m_NameMangler.MangleFunctionFromNode(func) : func->GetName();
		std::optional<std::shared_ptr<Symbol>> symbol;
		
		if (func->FunctionSymbol)
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

		m_ScopeStack.emplace_back();

		for (auto arg : func->Arguments)
		{
			if (arg && arg->Variable)
				m_ScopeStack.back().Insert(arg->GetName().GetData(), SymbolEntryType::Variable, arg->Variable);
		}

		Visit(func->CodeBlock, context);	
		m_ScopeStack.pop_back();
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTFunctionCall> funcCall, SemaContext context)
	{
		context.ValueReq = ValueRequired::RValue;

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
				}

				return funcCall;
			}
		}
		
		for (auto& arg : funcCall->Arguments)
		{
			arg = Visit(arg, context);

			if (!arg)
				return nullptr;

			context.CallsiteArgs.push_back(m_TypeInferEngine.InferTypeFromNode(arg));
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
			}
		}

		// the callee is a name or a member, not a value that should be loaded
		SemaContext calleeContext = context;
		calleeContext.ValueReq = ValueRequired::Any;
		funcCall->Callee = Visit(funcCall->Callee, calleeContext);

		if (!funcCall->Callee)
			return nullptr;

		// Point(1, 2) constructs a value of the class
		if (auto var = std::dynamic_pointer_cast<ASTVariable>(funcCall->Callee); 
			var && var->Variable && var->Variable->Kind == SymbolKind::Type && var->Variable->GetType()->IsClass())
		{
			return BuildConstruction(funcCall, var);
		}

		return CheckCall(funcCall);
	}

	std::shared_ptr<ASTNodeBase> Sema::CheckCall(std::shared_ptr<ASTFunctionCall> funcCall)
	{
		std::shared_ptr<ASTFunctionDefinition> function;
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

		size_t offset = isMethod ? 1 : 0;
		size_t expected = function->Arguments.size() >= offset ? function->Arguments.size() - offset : 0;
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
			funcCall->Arguments[i] = Coerce(funcCall->Arguments[i], parameterType);
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

		bool returnsValue = context.ReturnType && context.ReturnType->Get() && !context.ReturnType->Get()->isVoidTy();

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
			case OperatorType::BitwiseAnd:
			case OperatorType::BitwiseOr:
			case OperatorType::BitwiseXor:
			case OperatorType::LeftShift:
			case OperatorType::RightShift:
			{
				return VisitBinaryExprArithmetic(binaryExpression, context);
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
		assignmentOp->Storage = Visit(assignmentOp->Storage, context);
		// auto type = m_TypeInferEngine.InferTypeFromNode(assignmentOp->Storage);
		//if (type->IsConst() || type->As<PointerType>()->GetBaseType()->IsConst()) {
		//	CLEAR_LOG_ERROR("WRITING TO CONST BAD!!");
			//Report(DiagnosticCode_AssignConst, Token());
		//}
	
		context.ValueReq = ValueRequired::RValue;
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

			if (!clsType->MemberFunctions.contains("__setitem__") || assignmentOp->GetAssignType() != AssignmentOperatorType::Normal)
			{
				Report(DiagnosticCode_MissingIndexOverload, GetNodeLocation(assignmentOp->Storage));
				return nullptr;
			}

			auto setFunc = clsType->MemberFunctions.at("__setitem__");

    		auto funcCall = std::make_shared<ASTFunctionCall>( );
    		funcCall->Callee = setFunc->GetFunctionSymbol().FunctionNode;
    		funcCall->Arguments = funcCallNode->Arguments;
    		funcCall->Arguments.push_back(assignmentOp->Value);

    		auto var = std::make_shared<ASTVariable>(Token{});
    		var->Variable = setFunc;

    		funcCall->Callee = var;
    		return funcCall;


    	}

		return assignmentOp;
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
		return decl;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTClass> classExpr, SemaContext context) 
	{
		if (!DeclareClassType(classExpr) || !DeclareClassBody(classExpr, context))
			return nullptr;

		DefineClass(classExpr, context);
		return classExpr;
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

		// a generic instance being created: make the type visible now so it can name itself (e.g. `self: *Box[T]`)
		if (auto it = m_PendingInstances.find(classExpr.get()); it != m_PendingInstances.end())
			*it->second->GetGeneric().GeneratedSymbol = Symbol::CreateType(classTy);

		m_Module->ExposeSymbol(classExpr->GetName(), std::make_shared<Symbol>(Symbol::CreateType(classTy)));
		return true;
	}

	bool Sema::DeclareClassBody(std::shared_ptr<ASTClass> classExpr, SemaContext context)
	{
		if (classExpr->BodyDeclared)
			return true;

		classExpr->BodyDeclared = true;

		auto classTy = classExpr->ClassTy->As<ClassType>();
		std::vector<std::pair<std::string, std::shared_ptr<Symbol>>> members;

		for (auto node : classExpr->Members) 
		{
			Visit(node, context);

			if (!node->ResolvedType)
				return false;

			members.emplace_back(node->GetName(), std::make_shared<Symbol>(Symbol::CreateType(node->ResolvedType)));
		}
		
		classTy->MemberDefaults.clear();

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

			classTy->MemberDefaults.push_back(node);
		}

		for (auto node : classExpr->MemberFunctions)
		{
			auto functionSymbol = std::make_shared<Symbol>(Symbol::CreateFunction(node));
			members.emplace_back(node->GetName(), functionSymbol);
			node->FunctionSymbol = functionSymbol;
		}
		
		classTy->SetBody(members);

		context.TypeHint = classTy;
		
		for (auto node : classExpr->MemberFunctions)
			DeclareFunction(node, context);

		return true;
	}

	void Sema::DefineClass(std::shared_ptr<ASTClass> classExpr, SemaContext context)
	{
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
		std::filesystem::path parent = m_Module->GetPath().parent_path();
		std::filesystem::path absolute = std::filesystem::absolute(parent / importExpr->Filepath);
		
		auto it = m_CompilationUnits.find(absolute);
		if (it == m_CompilationUnits.end())
		{
			Report(DiagnosticCode_ImportNotFound, Token(TokenType::String, importExpr->Filepath.string()));
			return nullptr;
		}
		
		if (!importExpr->Namespace.empty())
		{
			m_Module->InsertModule(importExpr->Namespace, it->second.CompilationModule);
			return importExpr;
		}
		
		for (const auto& [symbolName, exposedSymbol] : it->second.CompilationModule->GetExposedSymbols())
		{
			m_ScopeStack.back().Insert(symbolName, SymbolEntryType::None, exposedSymbol);
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
			// arrays are iterated in place, so analyse the storage rather than a copy
			SemaContext storageContext = context;
			storageContext.ValueReq = ValueRequired::LValue;
			forExpr->Iterable = Visit(forExpr->Iterable, storageContext);

			if (!forExpr->Iterable)
				return nullptr;

			forExpr->IterableType = m_TypeInferEngine.InferTypeFromNode(forExpr->Iterable);

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

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTEnum> enumNode, SemaContext context)
	{
		const std::string& name = enumNode->Name.GetData();

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

		return switchNode;
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

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTTernaryExpression> ternaryExpr, SemaContext context)
	{
		context.ValueReq = ValueRequired::RValue;

		ternaryExpr->Condition = Visit(ternaryExpr->Condition, context);
		ternaryExpr->Truthy = Visit(ternaryExpr->Truthy, context);
		ternaryExpr->Falsy = Visit(ternaryExpr->Falsy, context);

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
		isExpr->Object = Visit(isExpr->Object, context);
		isExpr->TypeNode = Visit(isExpr->TypeNode, context);
		isExpr->AreTypesSame = m_TypeInferEngine.InferTypeFromNode(isExpr->Object) == GetTypeFromNode(isExpr->TypeNode);

		return isExpr;
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
			value = Visit(value, valueContext);

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

		if (structExpr->Values.size() > members.size())
		{
			Token location = GetNodeLocation(structExpr->TargetType);
			location.SetData(std::format("{}’ has {} field{}, but {} value{} given", classType->GetHash(), members.size(), members.size() == 1 ? "" : "s", 
										 structExpr->Values.size(), structExpr->Values.size() == 1 ? " was" : "s were"));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_TooManyValues, classType->GetHash().size());
			return nullptr;
		}

		size_t index = 0;
		for (const auto& [name, memberType] : members)
		{
			if (index < structExpr->Values.size())
			{
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

		if (init == classType->MemberFunctions.end())
		{
			// no __init__: Point(1, 2) fills the fields in order, exactly like Point { 1, 2 }
			auto structExpr = std::make_shared<ASTStructExpr>();
			structExpr->Location = funcCall->Location;
			structExpr->TargetType = target;
			structExpr->Values.assign(funcCall->Arguments.begin(), funcCall->Arguments.end());

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

	std::pair<std::optional<SymbolEntry>, size_t> Sema::LookupSymbol(llvm::StringRef name)
	{
		for (int64_t i = (int64_t)m_ScopeStack.size() - 1; i >= 0; i--)
		{
			if (auto entry = m_ScopeStack[i].Get(name))
				return { entry, (size_t)i };
		}

		if (auto symbol = m_Module->Lookup(name))
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
		auto classNode = std::dynamic_pointer_cast<ASTClass>(generic->TemplateNode);

		if (!classNode)
		{
			Report(DiagnosticCode_ExpectedType, target->GetName());
			return nullptr;
		}

		std::unordered_map<std::string, std::shared_ptr<Type>> bindings;

		for (size_t i = 0; i < values.size() && i < classNode->Members.size(); i++)
		{
			if (values[i])
				BindGenericType(classNode->Members[i]->TypeResolver, m_TypeInferEngine.InferTypeFromNode(values[i]), generic->GenericTypeNames, bindings);
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
		bool success = m_ScopeStack.back().Insert(generic->GetName(), SymbolEntryType::GenericTemplate, std::make_shared<Symbol>(Symbol::CreateGenericTemplate(generic)));
			
		if (!success)
			Report(DiagnosticCode_RedefinedIdentifier, Token(TokenType::Identifier, generic->GetName()));
		
		m_Module->ExposeSymbol(generic->GetName(), m_ScopeStack.back().Get(generic->GetName()).value().Symbol);
		return generic;	
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTSubscript> subscript, SemaContext context)
	{
		subscript->Target = Visit(subscript->Target, { .ValueReq = ValueRequired::LValue, .TypeHint = context.TypeHint, .AllowGenericInferenceFromArgs = false });
		subscript->Meaning = SubscriptSemantic::Generic;

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
			//TODO: check all values are ints and cast if needed
			auto targetType = m_TypeInferEngine.InferTypeFromNode(subscript->Target);
			if (targetType->IsClass())
			{
				auto clsType = std::dynamic_pointer_cast<ClassType>(targetType);

				if (!clsType->MemberFunctions.contains("__getitem__"))
				{
					Report(DiagnosticCode_MissingIndexOverload, GetNodeLocation(subscript->Target));
					return nullptr;
				}

				auto memberFunc = clsType->MemberFunctions.at("__getitem__");
				auto funcCall = std::make_shared<ASTFunctionCall>( );
				funcCall->Callee = memberFunc->GetFunctionSymbol().FunctionNode;
				funcCall->ClassType = clsType;
				funcCall->Arguments.push_back(subscript->Target );
				funcCall->Arguments.insert(
					funcCall->Arguments.end(),
					subscript->SubscriptArgs.begin(),
					subscript->SubscriptArgs.end()
				);

				auto var = std::make_shared<ASTVariable>(Token{});
				var->Variable = memberFunc;

				funcCall->Callee = var;
				return funcCall;

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
			value = Visit(value, context);
		
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

		// pointers: null converts to anything, otherwise the pointee must match (or be opaque)
		if (src->isPointerTy() && dst->isPointerTy())
		{
			if (!from->IsPointer() || !to->IsPointer())
				return true;

			auto fromBase = from->As<PointerType>()->GetBaseType();
			auto toBase = to->As<PointerType>()->GetBaseType();

			return !fromBase || !toBase || fromBase->Get()->isVoidTy() || toBase->Get()->isVoidTy();
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

		std::shared_ptr<Type> source = m_TypeInferEngine.InferTypeFromNode(node);

		if (!source || source == target)
			return node;

		if (!IsImplicitlyConvertible(source, target, IsNumericLiteral(node) || IsConstantThatFits(node, target)))
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
				valid = isNumber(lhs) && isNumber(rhs);
				break;
			case OperatorType::BitwiseAnd:
			case OperatorType::BitwiseOr:
			case OperatorType::BitwiseXor:
				valid = (isInteger(lhs) && isInteger(rhs)) || (isBool(lhs) && isBool(rhs));
				break;
			case OperatorType::LeftShift:
			case OperatorType::RightShift:
				valid = isInteger(lhs) && isInteger(rhs);
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

		if (!lhsType->IsPointer() && !lhsType->IsClass())
		{
			Report(DiagnosticCode_InvalidMemberAccess, GetNodeLocation(binaryExpr->RightSide));
			return nullptr;
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
			default:
			{
				break;
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
				auto sym = var->Variable ? std::optional(var->Variable) : m_Module->Lookup(var->GetName().GetData());

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
		
		Cloner cloner;
		cloner.DestinationModule = m_Module;
		
		for (size_t i = 0; i < substitutedArgs.size(); i++)
		{
			cloner.SubstitutionMap[node->GenericTypeNames[i]] = substitutedArgs[i]; 
		}
		
		std::shared_ptr<ASTNodeBase> clonned = cloner.Clone(node->TemplateNode);
		ChangeNameOfNode(instanceName, clonned);

		instanceSymbol = std::make_shared<Symbol>(Symbol::CreateGeneric(clonned));
		bool success = m_ScopeStack[scopeIndex].Insert(instanceName, SymbolEntryType::None, instanceSymbol);
		CLEAR_VERIFY(success, ""); //TODO Report(...)

		m_PendingInstances[clonned.get()] = instanceSymbol;
		clonned = Visit(clonned);
		m_PendingInstances.erase(clonned.get());

		ConstructSymbol(instanceSymbol, clonned);

		return instanceSymbol->GetGeneric().GeneratedSymbol;
	}
}
