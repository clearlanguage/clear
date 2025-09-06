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

#include <iterator>
#include <llvm/IR/InlineAsm.h>
#include <llvm/Support/CommandLine.h>
#include <memory>
#include <optional>

namespace clear
{
    Sema::Sema(std::shared_ptr<Module> clearModule, DiagnosticsBuilder& builder)
		: m_Module(clearModule), m_DiagBuilder(builder), m_ConstantEvaluator(clearModule), m_TypeInferEngine(clearModule), m_NameMangler(clearModule)
	{
    }

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTBlock> ast, SemaContext context)
	{
		m_ScopeStack.emplace_back();
		
		for(auto node : ast->Children)
			node = Visit(node, context);

		m_ScopeStack.pop_back();

		return ast;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTType> type, SemaContext context)
	{
		type->ConstructedType = ConstructType(type, context.TypeHint).value_or(Symbol());	
		return type;
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

			if (decl->Initializer)
				decl->Initializer = Visit(decl->Initializer, { .ValueReq = ValueRequired::RValue } );
		}
		else 
		{
			if (!decl->Initializer)
			{
				//DiagnosticCode_NeedsValueWithoutTypeAnnotation
				Report(DiagnosticCode_None, Token());
				return nullptr;
			}

			decl->Initializer = Visit(decl->Initializer, { .ValueReq = ValueRequired::RValue });
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
		return decl;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTVariable> variable, SemaContext context)
	{
		if (variable->Variable)
			return variable;

		std::optional<SymbolEntry> symbol;

		for (auto it = m_ScopeStack.rbegin(); it != m_ScopeStack.rend(); it++)
		{
			symbol = it->Get(variable->GetName().GetData());
			
			if (symbol.has_value())
				break;
		}
		
		if (!symbol.has_value())
		{
			std::optional<Symbol> sym = m_Module->Lookup(variable->GetName().GetData());
			
			if (sym.has_value())
				symbol = SymbolEntry { SymbolEntryType::None, std::make_shared<Symbol>(sym.value()) };
		}
		
		if (variable->GetName().GetData() == "self" || variable->GetName().GetData() == "Self")
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
		
		variable->Variable = symbol.value().Symbol;

		if (context.ValueReq == ValueRequired::RValue && symbol.value().Type == SymbolEntryType::Variable)
		{
			auto loadNode = std::make_shared<ASTLoad>();
			loadNode->Operand = variable;

			return loadNode;
		}
		
		return variable;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTExpression> expr, SemaContext context)
	{
		expr->RootExpr = Visit(expr->RootExpr, context);
		return expr;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTNodeBase> ast, SemaContext context)
    {
    	switch (ast->GetType()) 
		{
    		case ASTNodeType::FunctionCall:				return Visit(std::dynamic_pointer_cast<ASTFunctionCall>(ast), context);
    		case ASTNodeType::Variable:					return Visit(std::dynamic_pointer_cast<ASTVariable>(ast), context);
    		case ASTNodeType::TypeResolver:				return Visit(std::dynamic_pointer_cast<ASTType>(ast), context);
    		case ASTNodeType::TypeSpecifier:			return Visit(std::dynamic_pointer_cast<ASTTypeSpecifier>(ast), context);
    		case ASTNodeType::Block:					return Visit(std::dynamic_pointer_cast<ASTBlock>(ast), context);
    		case ASTNodeType::VariableDecleration:		return Visit(std::dynamic_pointer_cast<ASTVariableDeclaration>(ast), context);
			case ASTNodeType::Expression:				return Visit(std::dynamic_pointer_cast<ASTExpression>(ast), context);
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
			case ASTNodeType::DefaultInitializer:		return ast;
    		default:	
    			break;
    	}

		CLEAR_UNREACHABLE("Unhandled ASTNodeType");
		return nullptr;
    }

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTFunctionDefinition> func, SemaContext context)
	{	
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
			arg = Visit(arg, context);
		
		Visit(funcCall->Callee, context);
		return funcCall;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTReturn> returnStatement, SemaContext context)
	{
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

		auto type = m_TypeInferEngine.InferTypeFromNode(assignmentOp->Storage);

		//if (type->IsConst() || type->As<PointerType>()->GetBaseType()->IsConst()) {
		//	CLEAR_LOG_ERROR("WRITING TO CONST BAD!!");
			//Report(DiagnosticCode_AssignConst, Token());
		//}
	
		context.ValueReq = ValueRequired::RValue;
		assignmentOp->Value = Visit(assignmentOp->Value, context);

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
		
		if (decl->ReturnType)
			Visit(decl->ReturnType);
		
		auto symbol = m_ScopeStack.back().InsertEmpty(decl->GetName(), SymbolEntryType::FunctionDeclaration);

		if (!symbol.has_value())
		{
			// DiagnosticCode_AlreadyDefinedSymbol
			Report(DiagnosticCode_None, Token());
			return decl;
		}

		decl->DeclSymbol = symbol.value();
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

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTWhileExpression> whileExpr, SemaContext context)
	{
		whileExpr->WhileBlock.Condition = Visit(whileExpr->WhileBlock.Condition, context);
		Visit(whileExpr->WhileBlock.CodeBlock, context);
		
		return whileExpr;
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
		
		return generic;	
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTSubscript> subscript, SemaContext context)
	{
		subscript->Target = Visit(subscript->Target, { .ValueReq = ValueRequired::LValue });
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

			std::optional<size_t> optIndex;

			for (size_t i = m_ScopeStack.size(); i-- > 0; )
			{
				auto& table = m_ScopeStack[i];

				if (auto entry = table.Get(var->GetName().GetData()))
				{
					genericSym = entry.value().Symbol;
					optIndex = i; 
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

			std::string name = m_NameMangler.MangleGeneric(var->GetName().GetData(), substitutedArgs);
			std::shared_ptr<Symbol> sym;

			for (auto rit = m_ScopeStack.rbegin(); rit != m_ScopeStack.rend(); rit++)
			{
				if (auto entry = rit->Get(name))
				{
					sym = entry.value().Symbol;
					break;
			    }
			}

			if (!sym)
			{
				GenericTemplateSymbol genericTemplate = genericSym->GetGenericTemplate();
				std::shared_ptr<ASTGenericTemplate> node = std::dynamic_pointer_cast<ASTGenericTemplate>(genericTemplate.GenericTemplate);
				
				Cloner cloner;
				cloner.DestinationModule = m_Module;
				
				for (size_t i = 0; i < substitutedArgs.size(); i++)
				{
					cloner.SubstitutionMap[node->GenericTypeNames[i]] = substitutedArgs[i]; 
				}
				
				std::shared_ptr<ASTNodeBase> clonned = cloner.Clone(node->TemplateNode);
				ChangeNameOfNode(name, clonned);

				sym = std::make_shared<Symbol>(Symbol::CreateGeneric(clonned));
				bool success = m_ScopeStack[optIndex.value()].Insert(name, SymbolEntryType::None, sym);
				CLEAR_VERIFY(success, "");

				clonned = Visit(clonned);
				ConstructSymbol(sym, clonned);
			}

			GenericSymbol generic = sym->GetGeneric();		
			subscript->GeneratedType = generic.GeneratedSymbol;
		}

		return subscript;
	}

	void Sema::Report(DiagnosticCode code, Token token)
	{
		// TODO: change CodeGeneration to Semanatic Analysis
		m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, token, code);
	}

	std::optional<Symbol> Sema::ConstructType(std::shared_ptr<ASTType> type, std::shared_ptr<Type> selfType)
	{
		std::vector<Token> tokens = std::move(type->TakeTokens());
		
		auto ConstructArray = [&](auto begin, size_t& childIndex) -> std::optional<Symbol>
	 		{
				if (childIndex + 1 >= type->Children.size())
				{
					// DiagnosticCode_ArrayExpectedSizeAndType
					Report(DiagnosticCode_None, *begin);
					return std::nullopt;
				}

				m_ConstantEvaluator.Evaluate(type->Children[childIndex++]);
				
				if (!m_ConstantEvaluator.CurrentValue)
				{
					// DiagnosticCode_ArraySizeEvaluationFailed
					Report(DiagnosticCode_None, *begin);
					return std::nullopt;
				}

				int64_t size = m_ConstantEvaluator.GetValue<int64_t>();
				m_ConstantEvaluator.CurrentValue = nullptr;	

				if (size == 0)
				{
					// DiagnosticCode_ArraySizeOutOfRange
					Report(DiagnosticCode_None, *begin);
					return std::nullopt;
				}
				
				if (auto astType = std::dynamic_pointer_cast<ASTType>(type->Children[childIndex++]))
				{
					std::shared_ptr<Type> baseTy = ConstructType(astType, selfType)
						.and_then([](const Symbol& type)
							{
								return std::optional(type.GetType());
							})
						.value_or(nullptr);
				

					if (!baseTy)
						return std::nullopt;

					return Symbol::CreateType(m_Module->GetTypeRegistry()->GetArrayFrom(baseTy, size));			
				}
				
				// DiagnosticCode_ArrayExpectedType
				Report(DiagnosticCode_None, *begin);
				return {};

			};

		// TODO: add all the diagnostic codes
		auto pivot = std::find_if(tokens.begin(), tokens.end(), [](const Token& other)
						  {
								return( other.GetType() == TokenType::Identifier ||
									   other.GetType() == TokenType::Keyword ||
									   other.GetType() == TokenType::LeftBracket
										) and other.GetData() != "const";
						  });
			
		if (pivot == tokens.end())
		{
			// DiagnosticCode_MissingBaseType
			Report(DiagnosticCode_None, tokens.back());
			return std::nullopt;
		}

		auto curr = pivot;
		size_t childrenIndex = 0;	

		Symbol baseType;

		if (curr->GetType() == TokenType::LeftBracket)
		{
			baseType = ConstructArray(pivot, childrenIndex).value_or(Symbol());
			
			if (baseType.Kind == SymbolKind::None)
				return std::nullopt;
		}	
		else 
		{
			if (curr->GetData() == "self")
			{
				if (!selfType)
				{
					// DiagnosticCode_CannotUseSelfWhenNotInClassArg
					Report(DiagnosticCode_None, *curr);
					return std::nullopt;
				}

				baseType = Symbol::CreateType(selfType);
			}
			else 
			{
				baseType = m_Module->Lookup(curr->GetData()).value();
			}

			if (baseType.Kind == SymbolKind::None)
			{
				// DiagnosticCode_UndeclaredIdentifier
				Report(DiagnosticCode_None, *curr);
				return std::nullopt;
			}
		}
		
		if (baseType.Kind == SymbolKind::ClassTemplate)
		{
			CLEAR_UNREACHABLE("TODO");
		}
		
		std::shared_ptr<Type> createdType = baseType.GetType();

		if (pivot == tokens.begin())
			return Symbol::CreateType(createdType);
		
		curr--;
		
		for(;; curr--)
		{
			if(curr->IsType(TokenType::Star))
			{
				createdType = m_Module->GetTypeRegistry()->GetPointerTo(createdType);
			}
			else if (curr->GetData() == "const")
			{
				createdType = m_Module->GetTypeRegistry()->GetConstFrom(createdType);
			}
			else 
			{
				// DiagnosticCode_UnexepctedTokenInType (should be handled by parser but check for it here anyways)
				Report(DiagnosticCode_None, *curr);
				return std::nullopt;
			}

			if (curr == tokens.begin())
				break;
		}

		return Symbol::CreateType(createdType);	
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
		
		std::shared_ptr<Type> lhsType = m_TypeInferEngine.InferTypeFromNode(binaryExpr->LeftSide);
		CLEAR_VERIFY(lhsType, "");

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
				std::optional<Symbol> sym = var->Variable ? std::optional(*var->Variable) : m_Module->Lookup(var->GetName().GetData());

				if (!sym || sym->Kind != SymbolKind::Type)
					return nullptr;

				return sym->GetType();
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
}
