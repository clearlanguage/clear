#include "Sema.h"
#include "Core/Log.h"
#include "Diagnostics/Diagnostic.h"
#include "Diagnostics/DiagnosticCode.h"
#include "Diagnostics/DiagnosticsBuilder.h"
#include "Symbols/Module.h"
#include "Symbols/Symbol.h"

#include <optional>

namespace clear
{
    Sema::Sema(std::shared_ptr<Module> clearModule, DiagnosticsBuilder& builder)
		: m_Module(clearModule), m_DiagBuilder(builder), m_ConstantEvaluator(clearModule)
	{
    }

	void Sema::Visit(std::shared_ptr<ASTBlock> ast)
	{
		m_ScopeStack.push_back(std::make_shared<SymbolTable>());
		
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
		
	}

	void Sema::Report(DiagnosticCode code, Token token)
	{
		// TODO: change CodeGeneration to Semanatic Analysis
		m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, token, code);
	}

	std::optional<Symbol> Sema::ConstructType(std::shared_ptr<ASTType> type)
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
