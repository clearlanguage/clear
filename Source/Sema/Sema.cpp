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
		
		for(auto& node : ast->Children)
			node = Visit(node, context);

		m_ScopeStack.pop_back();

		return ast;
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

		return type;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTVariableDeclaration> decl, SemaContext context)
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
		bool globalState = context.GlobalState;
		context.GlobalState = false;

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
				return func;
			}
		}

		context.ReturnType = func->ReturnTypeVal;
		context.InLoop = false;
			
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

		if (!symbol.has_value())
		{
			Report(DiagnosticCode_RedefinedIdentifier, func->GetNameToken());
			m_ScopeStack.pop_back();
			return func;
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

		Visit(func->CodeBlock, context);	
		
		m_ScopeStack.pop_back();
		

		return func;
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
			context.CallsiteArgs.push_back(m_TypeInferEngine.InferTypeFromNode(arg));
		}
		
		Visit(funcCall->Callee, context);
		return funcCall;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTReturn> returnStatement, SemaContext context)
	{
		context.ValueReq = ValueRequired::RValue;
		returnStatement->ReturnValue = Visit(returnStatement->ReturnValue, context);

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
				VisitBinaryExprArithmetic(binaryExpression, context);
				break;
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
		function.FunctionNode->ReturnTypeVal = decl->ReturnType;

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
		auto classTy = m_Module->GetTypeRegistry()->CreateType<ClassType>(classExpr->GetName(), classExpr->GetName(), *m_Module->GetContext());
		std::vector<std::pair<std::string, std::shared_ptr<Symbol>>> members;

		for (auto node : classExpr->Members) 
		{
			Visit(node, context);
			members.emplace_back(node->GetName(), std::make_shared<Symbol>(Symbol::CreateType(node->ResolvedType)));
		}
		
		for (auto node : classExpr->DefaultValues)
		{
			if (node)
				Visit(node, context);
		}

		for (auto node : classExpr->MemberFunctions)
		{
			auto functionSymbol = std::make_shared<Symbol>(Symbol::CreateFunction(node));
			members.emplace_back(node->GetName(), functionSymbol);
			node->FunctionSymbol = functionSymbol;
		}
		
		classTy->SetBody(members);
		classExpr->ClassTy = classTy;

		// a generic instance being created: make the type visible now so its own methods can name it (e.g. `self: *Box[T]`)
		if (auto it = m_PendingInstances.find(classExpr.get()); it != m_PendingInstances.end())
			*it->second->GetGeneric().GeneratedSymbol = Symbol::CreateType(classTy);
		context.TypeHint = classTy;
		
		for (auto node : classExpr->MemberFunctions)
		{
			Visit(node, context);
		}
	
	
		m_Module->ExposeSymbol(classExpr->GetName(), std::make_shared<Symbol>(Symbol::CreateType(classTy)));
		return classExpr;
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
				return structExpr;
			}
		}

		SemaContext typeContext = context;
		typeContext.AllowGenericInferenceFromArgs = false;
		typeContext.CallsiteArgs.clear();
		structExpr->TargetType = Visit(structExpr->TargetType, typeContext);
		
		return structExpr;
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

	std::shared_ptr<ASTNodeBase> Sema::Coerce(std::shared_ptr<ASTNodeBase> node, std::shared_ptr<Type> target)
	{
		if (!node || !target)
			return node;

		std::shared_ptr<Type> source = m_TypeInferEngine.InferTypeFromNode(node);

		if (!source || source == target)
			return node;

		if (!IsImplicitlyConvertible(source, target, IsNumericLiteral(node)))
		{
			Token location = GetNodeLocation(node);
			size_t width = std::max<size_t>(location.GetData().size(), 1);

			location.SetData(std::format("{}’ to ‘{}", source->GetHash(), target->GetHash()));
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

	void Sema::VisitBinaryExprArithmetic(std::shared_ptr<ASTBinaryExpression> binaryExpression, SemaContext context)
	{
		context.ValueReq = ValueRequired::RValue;

		binaryExpression->LeftSide = Visit(binaryExpression->LeftSide, context);
		binaryExpression->RightSide = Visit(binaryExpression->RightSide, context);
		
		binaryExpression->ResultantType = m_TypeInferEngine.InferTypeFromNode(binaryExpression);

		// TODO: check if types are compatible and perform casting if needed
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
