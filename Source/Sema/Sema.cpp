#include "Sema.h"
#include "AST/ASTNode.h"
#include "Core/Log.h"
#include "Core/Operator.h"
#include "Core/Value.h"
#include "Diagnostics/Diagnostic.h"
#include "Diagnostics/DiagnosticCode.h"
#include "Diagnostics/DiagnosticsBuilder.h"
#include "Sema/SymbolTable.h"
#include "Symbols/Module.h"
#include "Symbols/Symbol.h"
#include "Symbols/Type.h"

#include <iterator>
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
		return type;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTVariableDeclaration> decl, SemaContext context)
	{
		if (decl->TypeResolver)
		{
			Visit(decl->TypeResolver, context);

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

			decl->TypeResolver = std::make_shared<ASTType>();
			decl->TypeResolver->ConstructedType = Symbol::CreateType(m_TypeInferEngine.InferTypeFromNode(decl->Initializer));
		}


		std::shared_ptr<ASTType> type = std::dynamic_pointer_cast<ASTType>(decl->TypeResolver);
		
		std::shared_ptr<Type> inferredType = decl->Initializer ? m_TypeInferEngine.InferTypeFromNode(decl->Initializer) : nullptr;
		
		if (type->ConstructedType.Kind == SymbolKind::InferType)
		{
			if (!inferredType)
			{
				// DiagnosticCode_InferredDeclarationMustHaveValue
				Report(DiagnosticCode_None, Token());
				return decl;
			}

			auto inferTypeInfo = type->ConstructedType.GetInferType();
	
			type->ConstructedType = Symbol::CreateType(inferredType);

			if(inferTypeInfo.IsConst)
			{
				type->ConstructedType = Symbol::CreateType(m_Module->GetTypeRegistry()->GetConstFrom(type->ConstructedType.GetType()));
			}
		}
		
		// TODO if inferred type and constructed type are not the same then insert a cast
		
		auto symbol = m_ScopeStack.back().InsertEmpty(decl->GetName().GetData(), SymbolEntryType::Variable);
	
		if (symbol.has_value())
		{
			*symbol.value() = Symbol::CreateValue(nullptr, type->ConstructedType.GetType());
			decl->Variable = symbol.value();
			return decl;
		}
			
		// DiagnosticCode_AlreadyDefinedVariable
		Report(DiagnosticCode_None, decl->GetName());
		return decl;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTVariable> variable, SemaContext context)
	{
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
    		default:	
				CLEAR_UNREACHABLE("Unhandled ASTNodeType");
    			break;
    	}

		return nullptr;
    }

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTFunctionDefinition> func, SemaContext context)
	{	
		m_ScopeStack.emplace_back();

		bool isGeneric = func->IsVariadic;
		 	
		if (func->GenericTypes.size() > 0)
		{
			//TODO: generic function so delay instantiation until function call
			isGeneric = true;
			return func;
		}

		for (auto arg : func->Arguments)
		{
			if (arg)	
				Visit(arg, context);
		}
		
		if (func->ReturnType)
			Visit(func->ReturnType, context);
		
		//TODO: temporary, until we have the clear runtime make a main function we will have to ignore mangling for main
		std::string mangledName = func->GetName() != "main" ? m_NameMangler.MangleFunctionFromNode(func) : func->GetName();
		std::optional<std::shared_ptr<Symbol>> symbol;
		
		if (func->FunctionSymbol)
		{
			bool success = m_ScopeStack.back().Insert(func->GetName(), isGeneric ? SymbolEntryType::GenericFunction : SymbolEntryType::Function, func->FunctionSymbol);
			symbol = success ? std::optional(func->FunctionSymbol) : std::nullopt;
		}	
		else 
		{
			symbol = m_ScopeStack.back().InsertEmpty(func->GetName(), isGeneric ? SymbolEntryType::GenericFunction : SymbolEntryType::Function);
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

		context.ValueReq = ValueRequired::RValue;
		assignmentOp->Value = Visit(assignmentOp->Value, context);

		return assignmentOp;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTUnaryExpression> unaryExpr, SemaContext context)
	{
		switch (unaryExpr->GetOperatorType())
		{
			case OperatorType::PostIncrement: // increment and decrement need to operate on an lvalue and always return an rvalue
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
			members.emplace_back(node->GetName(), std::make_shared<Symbol>(node->TypeResolver->ConstructedType));
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
			Visit(conditionalBlock.Condition, context);
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
				if (childIndex + 2 >= type->Children.size())
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

				if (size <= 0)
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
								return other.GetType() == TokenType::Identifier || 
									   other.GetType() == TokenType::Keyword || 
									   other.GetType() == TokenType::RightBracket;
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

		if (curr->GetType() == TokenType::RightBracket)
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

		if (binaryExpr->RightSide->GetType() != ASTNodeType::Member)
		{
			// DiagnosticCode_InvalidAccess
			Report(DiagnosticCode_None, Token());
			return nullptr;
		}
		
		std::shared_ptr<ASTMember> member = std::dynamic_pointer_cast<ASTMember>(binaryExpr->RightSide);
		
		if (lhsType->IsPointer())
			lhsType = lhsType->As<PointerType>()->GetBaseType();
		
		std::optional<std::shared_ptr<Symbol>> memberSymbol = lhsType->As<ClassType>()->GetMember(member->GetName());

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
}
