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
				Report(DiagnosticCode_None, Token());
			
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
			
			context.ValueReq = ValueRequired::RValue;
			if (decl->Initializer)
				decl->Initializer = Visit(decl->Initializer, context);
		}
		else 
		{
			if (!decl->Initializer)
			{
				//DiagnosticCode_NeedsValueWithoutTypeAnnotation
				Report(DiagnosticCode_None, Token());
				return nullptr;
			}
			
			context.ValueReq = ValueRequired::RValue;
			decl->Initializer = Visit(decl->Initializer, context);
			decl->ResolvedType = m_TypeInferEngine.InferTypeFromNode(decl->Initializer);
		}


		std::shared_ptr<Type> inferredType = decl->Initializer ? m_TypeInferEngine.InferTypeFromNode(decl->Initializer) : nullptr;
		
		// TODO if inferred type and constructed type are not the same then insert a cast
		auto symbol = m_ScopeStack.back().InsertEmpty(decl->GetName().GetData(), SymbolEntryType::Variable);
	
		if (symbol.has_value())
		{
			*symbol.value() = Symbol::CreateValue(nullptr, decl->ResolvedType);
			decl->Variable = symbol.value();
			return decl;
		}
			
		// DiagnosticCode_AlreadyDefinedVariable
		Report(DiagnosticCode_None, decl->GetName());

		if (context.GlobalState)
			m_Module->ExposeSymbol(decl->GetName().GetData(), symbol.value());
		
		return decl;
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
			// DiagnosticCode_UndefinedVariable
			Report(DiagnosticCode_None, variable->GetName());
			return nullptr;
		}

		if (symbol.value().Symbol->Kind == SymbolKind::GenericTemplate && context.AllowGenericInferenceFromArgs && context.CallsiteArgs.size() > 0)
		{
			llvm::SmallVector<Symbol> transformed(context.CallsiteArgs.size());
			std::transform(context.CallsiteArgs.begin(), context.CallsiteArgs.end(), transformed.begin(), [](auto type) { return Symbol::CreateType(type); });
			symbol.value().Symbol = SolveConstraints(variable->GetName().GetData(), symbol.value().Symbol, scopeIndex, transformed);
		}
		
		variable->Variable = symbol.value().Symbol;

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
			CLEAR_VERIFY(func->ReturnTypeVal, "");
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

		if (!symbol.has_value())
		{
			// DiagnosticCode_AlreadyDefinedFunction
			Report(DiagnosticCode_None, Token());
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
			{
				VisitBinaryExprArithmetic(binaryExpression, context);
				break;
			}
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
				CLEAR_UNREACHABLE("unimplemented");
				break;
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

    	if (assignmentOp->Storage->GetType() == ASTNodeType::FunctionCall) {
    		auto funcCallNode = std::dynamic_pointer_cast<ASTFunctionCall>(assignmentOp->Storage);
    		auto clsType = funcCallNode->ClassType;
    		CLEAR_VERIFY(clsType->MemberFunctions.contains("operator_set"),"Class does not have array index overload ",clsType->GetHash())
			auto setFunc = clsType->MemberFunctions.at("operator_set");

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
				unaryExpr->Operand = Visit(unaryExpr->Operand, { ValueRequired::LValue });
				break;
			}
			case OperatorType::Dereference:
			{
				unaryExpr->Operand = Visit(unaryExpr->Operand, context);
				
				if (context.ValueReq == ValueRequired::LValue)
					return unaryExpr->Operand;
					
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
					Report(DiagnosticCode_None, Token()); // DiagnosticCode_VariadicArgsMustAtTheEnd
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
			// DiagnosticCode_AlreadyDefinedSymbol
			Report(DiagnosticCode_None, Token());
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
		for (auto conditionalBlock : ifExpr->ConditionalBlocks)
		{
			Visit(conditionalBlock.Condition, SemaContext { .ValueReq = ValueRequired::RValue });
			Visit(conditionalBlock.CodeBlock, context);
		}
		
		if (ifExpr->ElseBlock)
			Visit(ifExpr->ElseBlock);

		return ifExpr;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTImport> importExpr, SemaContext context)
	{
		std::filesystem::path parent = m_Module->GetPath().parent_path();
		std::filesystem::path absolute = std::filesystem::absolute(parent / importExpr->Filepath);
		
		auto it = m_CompilationUnits.find(absolute);
		if (it == m_CompilationUnits.end())
		{
			Report(DiagnosticCode_None, Token()); // not found file path
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
		whileExpr->WhileBlock.Condition = Visit(whileExpr->WhileBlock.Condition, context);
		Visit(whileExpr->WhileBlock.CodeBlock, context);
		
		return whileExpr;
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

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTStructExpr> structExpr, SemaContext context)
	{	
		Visit(structExpr->TargetType, context);

		for (auto& value : structExpr->Values)
		{
			context.ValueReq = ValueRequired::RValue;
			value = Visit(value, context);
		}
		
		return structExpr;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTGenericTemplate> generic, SemaContext context)
	{
		bool success = m_ScopeStack.back().Insert(generic->GetName(), SymbolEntryType::GenericTemplate, std::make_shared<Symbol>(Symbol::CreateGenericTemplate(generic)));
			
		if (!success)
			Report(DiagnosticCode_None, Token());
		
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
				CLEAR_VERIFY(clsType->MemberFunctions.contains("operator_get"),"Class does not have array index overload ",clsType->GetHash())
				auto memberFunc = clsType->MemberFunctions.at("operator_get");
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
				//DiagnosticCode_UndeclaredIdentifier
				Report(DiagnosticCode_None, var->GetName());
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

		m_ConstantEvaluator.Evaluate(arrayType->SizeNode);
		std::shared_ptr<Type> baseTy = GetTypeFromNode(arrayType->TypeNode);
		
		int64_t size = m_ConstantEvaluator.GetValue<int64_t>();
		m_ConstantEvaluator.CurrentValue = nullptr;
			
		if (size <= 0)
		{
			Report(DiagnosticCode_None, Token());
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
		
		std::shared_ptr<Type> lhsType = m_TypeInferEngine.InferTypeFromNode(binaryExpr->LeftSide);

		if (!lhsType)
			return binaryExpr;

		while (lhsType->IsPointer())
			lhsType = lhsType->As<PointerType>()->GetBaseType();

		if (binaryExpr->RightSide->GetType() != ASTNodeType::Variable)
		{
			// DiagnosticCode_InvalidAccess
			Report(DiagnosticCode_None, Token());
			return nullptr;
		}
		
		std::shared_ptr<ASTVariable> member = std::dynamic_pointer_cast<ASTVariable>(binaryExpr->RightSide);
		std::optional<std::shared_ptr<Symbol>> memberSymbol = lhsType->As<ClassType>()->GetMember(
			member->GetName().GetData()
		);

		if (!memberSymbol)
		{
			// DiagnosticCode_MissingMember
			Report(DiagnosticCode_None, Token());
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

		binaryExpr->ResultantType = m_Module->GetTypeRegistry()->GetPointerTo(binaryExpr->ResultantType);
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

		clonned = Visit(clonned);
		ConstructSymbol(instanceSymbol, clonned);

		return instanceSymbol->GetGeneric().GeneratedSymbol;
	}
}
