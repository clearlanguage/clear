#include "Sema.h"
#include "Core/Log.h"
#include "Diagnostics/Diagnostic.h"
#include "Diagnostics/DiagnosticCode.h"
#include "Diagnostics/DiagnosticsBuilder.h"
#include "Symbols/Module.h"
#include "Symbols/Symbol.h"

#include <memory>
#include <optional>

namespace clear
{
    Sema::Sema(std::shared_ptr<Module> clearModule, DiagnosticsBuilder& builder)
		: m_Module(clearModule), m_DiagBuilder(builder), m_ConstantEvaluator(clearModule), m_TypeInferEngine(clearModule)
	{
    }

	void Sema::Visit(std::shared_ptr<ASTBlock> ast)
	{
		m_ScopeStack.emplace_back();
		
		for(auto node : ast->Children)
			node->Accept(*this);

		m_ScopeStack.pop_back();
	}

	void Sema::Visit(std::shared_ptr<ASTType> type)
	{
		type->ConstructedType = ConstructType(type).value_or(Symbol());	
	}

	void Sema::Visit(std::shared_ptr<ASTVariableDeclaration> decl)
	{
		decl->TypeResolver->Accept(*this);
		decl->Initializer->Accept(*this);

		std::shared_ptr<ASTType> type = std::dynamic_pointer_cast<ASTType>(decl->TypeResolver);
		
		std::shared_ptr<Type> inferredType = m_TypeInferEngine.InferTypeFromNode(decl->Initializer);
		
		if (type->ConstructedType.Kind == SymbolKind::InferType)
		{
			auto inferTypeInfo = type->ConstructedType.GetInferType();
	
			type->ConstructedType = Symbol::CreateType(inferredType);

			if(inferTypeInfo.IsConst)
			{
				type->ConstructedType = Symbol::CreateType(m_Module->GetTypeRegistry()->GetConstFrom(type->ConstructedType.GetType()));
			}
		}
		
		// TODO if inferred type and constructed type are not the same then insert a cast
		
		auto symbol = m_ScopeStack.back().InsertEmpty(decl->GetName().GetData());
		
		if (symbol.has_value())
		{
			decl->Variable = symbol.value();
			return;
		}
			
		// DiagnosticCode_AlreadyDefinedVariable
		Report(DiagnosticCode_None, decl->GetName());
	}

	void Sema::Visit(std::shared_ptr<ASTVariable> variable)
	{
		std::optional<std::shared_ptr<Symbol>> symbol;

		for (auto it = m_ScopeStack.rbegin(); it != m_ScopeStack.rend(); it++)
		{
			symbol = m_ScopeStack.back().Get(variable->GetName().GetData());
			
			if (symbol.has_value())
				break;
		}

		if (symbol.has_value())
		{
			variable->Variable = symbol.value();
			return;
		}
			
		// DiagnosticCode_UndefinedVariable
		Report(DiagnosticCode_None, variable->GetName());
	}

	void Sema::Visit(std::shared_ptr<ASTNodeBase> ast)
	{
		ast->Accept(*this);
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
