#include "Sema.h"
#include "Core/Log.h"
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
			Visit(node, context);

		m_ScopeStack.pop_back();

		return ast;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTType> type, SemaContext context)
	{
		type->ConstructedType = ConstructType(type).value_or(Symbol());	
		return type;
	}
	
	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTTypeSpecifier> type, SemaContext context)
	{
		Visit(type->TypeResolver, context);
		return type;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTVariableDeclaration> decl, SemaContext context)
	{
		Visit(decl->TypeResolver, context);
			
		context.ValueReq = ValueRequired::RValue;

		if (decl->Initializer)
			Visit(decl->Initializer, context);

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
			// DiagnosticCode_UndefinedVariable
			Report(DiagnosticCode_None, variable->GetName());
			return nullptr;
		}
		
		variable->Variable = symbol.value().Symbol;
		
		if (context.ValueReq == ValueRequired::RValue && symbol.value().Type == SymbolEntryType::Variable)
		{
			auto loadNode = std::make_shared<ASTUnaryExpression>(OperatorType::Dereference);
			loadNode->Operand = variable;

			return loadNode;
		}
		
		return variable;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTExpression> expr, SemaContext context)
	{
		Visit(expr->RootExpr, context);
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
    		default:
				CLEAR_UNREACHABLE("Unhandled ASTNodeType");
    			break;
    	}

		return nullptr;
    }

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTFunctionDefinition> func, SemaContext context)
	{	
		if (func->GenericTypes.size() > 0)
		{
			//TODO: generic function so delay instantiation until function call
			return func;
		}

		for (auto arg : func->Arguments)
			Visit(arg, context);
		
		if (func->ReturnType)
			Visit(func->ReturnType, context);
	
		std::string mangledName = m_NameMangler.MangleFunctionFromNode(func);
		auto symbol = m_ScopeStack.back().InsertEmpty(func->GetName(), SymbolEntryType::Function);
	
		if (!symbol.has_value())
		{
			// DiagnosticCode_AlreadyDefinedFunction
			Report(DiagnosticCode_None, Token());
			m_ScopeStack.pop_back();
			return func;
		}
		
		func->SetName(mangledName);

		auto fnSymbolPtr = symbol.value();
		*fnSymbolPtr = Symbol::CreateFunction(nullptr);
		
		{
			FunctionSymbol& functionSymbol = fnSymbolPtr->GetFunctionSymbol();
			functionSymbol.FunctionNode = func;
		}

		func->FunctionSymbol = fnSymbolPtr;
		func->SourceModule = m_Module;	

		Visit(func->CodeBlock, context);	
		return func;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTFunctionCall> funcCall, SemaContext context)
	{
		for (auto arg : funcCall->Arguments)
			Visit(arg, context);
		
		Visit(funcCall->Callee, context);
		return funcCall;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTReturn> returnStatement, SemaContext context)
	{
		Visit(returnStatement->ReturnValue, context);
		//TODO: add type checking
		return returnStatement;
	}
	
	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTBinaryExpression> binaryExpression, SemaContext context)
	{	
		context.ValueReq = ValueRequired::RValue;

		binaryExpression->LeftSide = Visit(binaryExpression->LeftSide, context);
		binaryExpression->RightSide = Visit(binaryExpression->RightSide, context);
			
		// TODO: check if types are compatible and perform casting if needed
		return binaryExpression;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTNodeLiteral> literal, SemaContext context)
	{
		return literal;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTAssignmentOperator> assignmentOp, SemaContext context)
	{
		context.ValueReq = ValueRequired::LValue;
		Visit(assignmentOp->Storage);

		context.ValueReq = ValueRequired::RValue;
		Visit(assignmentOp->Value);

		return assignmentOp;
	}

	void Sema::Report(DiagnosticCode code, Token token)
	{
		// TODO: change CodeGeneration to Semanatic Analysis
		m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, token, code);
	}

	std::optional<Symbol> Sema::ConstructType(std::shared_ptr<ASTType> type)
	{
		std::vector<Token> tokens = std::move(type->TakeTokens());
		
		if (tokens[0].GetData() == "let")
			return Symbol::CreateInferType(false);
		
		if (tokens[0].GetData() == "const" && tokens.size() == 1)
			return Symbol::CreateInferType(true);

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
					std::shared_ptr<Type> baseTy = ConstructType(astType)
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
			baseType = m_Module->Lookup(curr->GetData());

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
}
