#include "Sema.h"
#include "AST/ASTNode.h"
#include "Core/Log.h"
#include "Core/CrashHandler.h"
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
	static bool IsNumericLiteral(const std::shared_ptr<ASTNodeBase>& node);
	static std::optional<size_t> FindTypeCase(const std::shared_ptr<ClassType>& variant, const std::shared_ptr<Type>& type);
	static bool IsFreshValue(const std::shared_ptr<ASTNodeBase>& node);
	static bool IsOptionalType(const std::shared_ptr<Type>& type);
	static std::shared_ptr<Type> OptionalValueType(const std::shared_ptr<Type>& optional);
	static std::shared_ptr<ASTVariable> RootVariable(std::shared_ptr<ASTNodeBase> node, bool* throughCall);
	static bool BindGenericType(std::shared_ptr<ASTNodeBase> pattern, std::shared_ptr<Type> actual, 
								llvm::ArrayRef<std::string> names, std::unordered_map<std::string, std::shared_ptr<Type>>& bindings);
	static std::shared_ptr<Type> ClassOf(std::shared_ptr<Type> type);
	static void DispatchOnObject(const std::shared_ptr<ASTNodeBase>& node, const std::shared_ptr<ClassType>& classType);

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
			m_BlockCandidates.push_back(m_Copies.Candidates.size());
			struct PopBlock { std::vector<size_t>& Stack; ~PopBlock() { Stack.pop_back(); } } popBlock { m_BlockCandidates };

			for (size_t i = 0; i < ast->Children.size(); i++)
			{
				size_t firstCandidate = m_Copies.Candidates.size();
				ast->Children[i] = Visit(ast->Children[i], context);
				CheckStatementUses(ast->Children[i], firstCandidate);

				// `if not r: return` above: the rest of the block sees r as its value, in a scope of its own (r is
				// already declared in this one)
				if (auto narrowed = std::exchange(m_NarrowAfter, {}); !narrowed.empty() && i + 1 < ast->Children.size())
				{
					auto rest = std::make_shared<ASTBlock>();
					rest->Location = narrowed.front()->Location;
					rest->Children.insert(rest->Children.end(), narrowed.begin(), narrowed.end());
					rest->Children.insert(rest->Children.end(), ast->Children.begin() + i + 1, ast->Children.end());
					ast->Children.resize(i + 1);
					ast->Children.push_back(rest);
				}
			}
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

		// const N = 8 (a number worked out from literals and other names): known before any type uses it,
		// so [N; int] works in fields and parameters
		std::function<bool(const std::shared_ptr<ASTNodeBase>&)> simple = [&](const std::shared_ptr<ASTNodeBase>& value) -> bool
		{
			if (!value)
				return false;

			switch (value->GetType())
			{
				case ASTNodeType::Literal:
				case ASTNodeType::Variable:
					return true;
				case ASTNodeType::BinaryExpression:
				{
					auto binary = std::dynamic_pointer_cast<ASTBinaryExpression>(value);
					return binary->GetExpression() != OperatorType::Dot && simple(binary->LeftSide) && simple(binary->RightSide);
				}
				case ASTNodeType::UnaryExpression:
					return simple(std::dynamic_pointer_cast<ASTUnaryExpression>(value)->Operand);
				default:
					return false;
			}
		};

		std::unordered_set<ASTNodeBase*> early;

		for (auto& node : children)
		{
			if (auto decl = std::dynamic_pointer_cast<ASTVariableDeclaration>(node); decl && decl->IsConst && simple(decl->Initializer))
			{
				node = Visit(node, context);
				early.insert(node.get());
			}
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
			if (early.contains(node.get()))
				continue;

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
				case ASTNodeType::VariableDecleration:
				case ASTNodeType::Destructure:
				case ASTNodeType::Block:     // declarations the parser grouped together
				case ASTNodeType::Sequence:
					node = Visit(node, context);
					break;
				default:
					// print("hi") at the top of a file: there is nowhere for it to run
					Report(DiagnosticCode_TopLevelStatement, GetNodeLocation(node));
					node = nullptr;
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

		size_t errorsBefore = m_DiagBuilder.ErrorCount();
		if (auto resolved = Visit(type->TypeResolver, context)) type->TypeResolver = resolved;
		type->ResolvedType = GetTypeFromNode(type->TypeResolver);

		if (!type->ResolvedType && m_DiagBuilder.ErrorCount() == errorsBefore) // (else already reported: an unknown name)
			Report(DiagnosticCode_ExpectedType, GetNodeLocation(type->TypeResolver));

		return type;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTVariableDeclaration> decl, SemaContext context)
	{
		auto result = VisitDeclaration(decl, context);

		if (!result)
			m_FailedDeclarations.insert(decl->GetName().GetData());
		else if (auto narrowing = m_NarrowingDeclarations.find(decl.get()); narrowing != m_NarrowingDeclarations.end() && decl->Variable)
			m_NarrowedNames[decl->Variable.get()] = narrowing->second;

		return result;
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitDeclaration(std::shared_ptr<ASTVariableDeclaration> decl, SemaContext context)
	{
		if (decl->TypeResolver)
		{
			size_t errorsBefore = m_DiagBuilder.ErrorCount();
			if (auto resolved = Visit(decl->TypeResolver, context)) decl->TypeResolver = resolved;
			decl->ResolvedType = GetTypeFromNode(decl->TypeResolver);
			
			if (!decl->ResolvedType)
			{
				if (m_DiagBuilder.ErrorCount() == errorsBefore) // (else already reported: an unknown name)
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
			auto written = decl->Initializer;
			decl->Initializer = Visit(decl->Initializer, context);

			if (!decl->Initializer)
				return nullptr; // already reported

			decl->ResolvedType = m_TypeInferEngine.InferTypeFromNode(decl->Initializer);

			// let r = xs.remove(0): a call that returns nothing
			if (auto call = std::dynamic_pointer_cast<ASTFunctionCall>(written); call && (!decl->ResolvedType || decl->ResolvedType->Get()->isVoidTy()))
			{
				auto callee = std::dynamic_pointer_cast<ASTVariable>(call->Callee);

				if (auto member = std::dynamic_pointer_cast<ASTBinaryExpression>(call->Callee); member && !callee)
					callee = std::dynamic_pointer_cast<ASTVariable>(member->RightSide);

				Token where = callee ? callee->GetName() : GetNodeLocation(written);
				Report(DiagnosticCode_NoValue, where);
				return nullptr;
			}

			// an alias names the place a pointer points at, it holds no value of its own
			if (decl->IsAlias && decl->ResolvedType && decl->ResolvedType->IsPointer())
				decl->ResolvedType = decl->ResolvedType->As<PointerType>()->GetBaseType();

			if (!decl->ResolvedType || decl->ResolvedType->Get()->isVoidTy())
			{
				Report(DiagnosticCode_NeedsTypeOrValue, decl->GetName());
				return nullptr;
			}

			if (!decl->IsAlias)
				decl->Initializer = TakeOwnership(decl->Initializer, decl->ResolvedType);
		}


		if (decl->TypeResolver && decl->Initializer)
			decl->Initializer = Coerce(decl->Initializer, decl->ResolvedType);

		auto symbol = m_ScopeStack.back().InsertEmpty(decl->GetName().GetData(), SymbolEntryType::Variable);
	
		if (symbol.has_value())
		{
			*symbol.value() = Symbol::CreateValue(nullptr, decl->ResolvedType);
			decl->Variable = symbol.value();

			// owning values can be moved out of locals (and parameters), not out of anything else
			if (!context.GlobalState && !decl->IsAlias)
			{
				m_LocalVariables.insert(decl->Variable.get());
				m_LocalDeclarations[decl->Variable.get()] = decl.get();
				m_LocalOrder[decl->Variable.get()] = m_LocalCounter++;
				m_Moved.erase(decl->Variable.get());
				NoteElementPointer(decl);

				// p = &a.items[0], v: str = name, part = xs[1:3]: what they look into is not moved away later
				if (decl->ResolvedType && (decl->ResolvedType->IsPointer() || std::dynamic_pointer_cast<SliceType>(decl->ResolvedType)))
					NeverMove(decl->Initializer);

				// let w = words[i]: a copy that may turn out to be needless (if w is only read; see FinishCopies)
				if (auto copy = std::dynamic_pointer_cast<ASTCopy>(decl->Initializer))
				{
					auto root = RootVariable(copy->Value, nullptr);
					auto rootType = root && root->Variable ? root->Variable->GetType() : nullptr;

					if (root && root->Variable && m_LocalVariables.contains(root->Variable.get()) && rootType && !rootType->IsPointer() && AddressOfRead(copy->Value))
						m_Copies.Views.push_back({ decl, copy, decl->Variable.get(), root->Variable.get(), m_Copies.Clock });
				}
			}

			// `if q:` names the value inside the optional q (see NarrowedDeclaration)
			if (auto field = std::dynamic_pointer_cast<ASTVariantField>(decl->Initializer); field && decl->IsAlias && !context.GlobalState)
			{
				auto subject = std::dynamic_pointer_cast<ASTVariable>(field->Subject);

				if (subject && subject->Variable && m_LocalVariables.contains(subject->Variable.get()) && field->VariantTy && field->VariantTy->IsClass() &&
					field->VariantTy->As<ClassType>()->IsOptional)
					m_NarrowedAliases[decl->Variable.get()] = subject;
			}

			if (decl->IsConst)
			{
				m_ConstSymbols.insert(decl->Variable.get());

				if (auto value = EvaluateInteger(decl->Initializer); value && decl->ResolvedType->IsIntegral())
				{
					m_ConstantValues[decl->Variable.get()] = *value;
					m_Module->ConstantValues[decl->Variable.get()] = { *value, decl->ResolvedType };
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
		// helper() when both `import "ma"` and `import "mb"` define one (a definition in this file would win)
		if (auto ambiguous = m_AmbiguousImports.find(variable->GetName().GetData()); ambiguous != m_AmbiguousImports.end())
		{
			bool definedHere = false;

			for (size_t i = 1; i < m_ScopeStack.size() && !definedHere; i++)
				definedHere = m_ScopeStack[i].Get(ambiguous->first).has_value();

			if (!definedHere)
			{
				Token where = variable->GetName();
				where.SetData(std::format("{}’ is defined by both ‘{}’ and ‘{}", ambiguous->first, ambiguous->second.first, ambiguous->second.second));
				Report(DiagnosticCode_AmbiguousImport, where, variable->GetName().GetData().size());
				return nullptr;
			}
		}

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

		if (variable.get() != m_Reinitialised)
		{
			CheckNotMoved(variable);
			CheckStalePointer(variable);
		}

		NoteUse(variable, context.ValueReq);

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
			std::shared_ptr<Type> constantType;

			if (auto known = KnownConstant(variable->Variable.get(), &constantType))
			{
				auto constant = std::make_shared<ASTConstantValue>(*known, constantType ? constantType : variable->Variable->GetType());
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
		if (ast)
		{
			NoteProgress("checking", ast->Location.GetSourceFile(), ast->Location.LineNumber, ast->Location.ColumnNumber);

			// remember the program's own line (not the standard library's), for notes on errors inside the library
			static const std::string standard = std::filesystem::weakly_canonical(std::filesystem::path(CLEAR_STANDARD_DIR)).string();
			const auto& file = ast->Location.GetSourceFile();

			if (!file.empty() && !file.string().starts_with(standard))
				m_DiagBuilder.SetUserLine(&ast->Location);
		}

		// "only read" applies to x and x.field.field, not to anything inside a call or another expression
		bool chain = ast && (ast->GetType() == ASTNodeType::Variable || 
							 (ast->GetType() == ASTNodeType::BinaryExpression && std::dynamic_pointer_cast<ASTBinaryExpression>(ast)->GetExpression() == OperatorType::Dot));
		struct ReadingGuard { bool& Flag; bool Saved; ~ReadingGuard() { Flag = Saved; } } readingGuard { m_ReadingUse, m_ReadingUse };

		if (!chain)
			m_ReadingUse = false;

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
			case ASTNodeType::Move:						return ast;
			case ASTNodeType::Copy:						return ast;
			case ASTNodeType::Once:						return ast;
			case ASTNodeType::SliceExpr:				return VisitSlice(std::dynamic_pointer_cast<ASTSliceExpr>(ast), context);
			case ASTNodeType::Destroy:					return ast;
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

				if (!Visit(arg, context) && arg->TypeResolver && !arg->ResolvedType)
				{
					m_ScopeStack.pop_back(); // its type is unknown (reported): the function can't be declared
					return false;
				}

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
			size_t errorsBefore = m_DiagBuilder.ErrorCount();
			if (auto resolved = Visit(func->ReturnType, context)) func->ReturnType = resolved;
			func->ReturnTypeVal = GetTypeFromNode(func->ReturnType);

			if (!func->ReturnTypeVal)
			{
				if (m_DiagBuilder.ErrorCount() == errorsBefore) // (else already reported: an unknown name)
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

		// a body can be analysed in the middle of another one (generics, lambdas): its moves are its own
		MovedSet outerMoved = std::exchange(m_Moved, {});
		bool outerUnreachable = std::exchange(m_Unreachable, false);
		auto outerLoops = std::exchange(m_LoopMoves, {});
		auto outerCopies = std::exchange(m_Copies, {});
		size_t errorsBefore = m_DiagBuilder.ErrorCount();

		Visit(func->CodeBlock, context);	
		m_ScopeStack.pop_back();
		bool bodyHadErrors = m_DiagBuilder.ErrorCount() != errorsBefore;

		FinishCopies();
		m_Copies = std::move(outerCopies);

		m_Moved = std::move(outerMoved);
		m_Unreachable = outerUnreachable;
		m_LoopMoves = std::move(outerLoops);

		// every path through a function with a return type must return a value (main may end and return 0, like C)
		bool returnsValue = func->CoroutineKind ? (func->CoroutineKind == 2 && func->CoroutineValue != nullptr) 
												: func->ReturnTypeVal && func->ReturnTypeVal->Get() && !func->ReturnTypeVal->Get()->isVoidTy();
		bool isMain = func->GetNameToken().GetData() == "main" && !context.TypeHint;

		// (not after an error in the body: a statement that failed may well have been the return)
		if (returnsValue && !isMain && !bodyHadErrors && !AlwaysReturns(func->CodeBlock))
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

		// user?.greet(): only called when user holds a value
		if (auto chain = std::dynamic_pointer_cast<ASTBinaryExpression>(funcCall->Callee); chain && chain->GetExpression() == OperatorType::OptionalDot)
			return VisitOptionalChain(chain, context, funcCall);

		// m.Pt(1, 2), m.make(): a class or function of a module imported `as m` is called like one written here
		if (auto access = std::dynamic_pointer_cast<ASTBinaryExpression>(funcCall->Callee); access && access->GetExpression() == OperatorType::Dot)
		{
			if (auto member = std::dynamic_pointer_cast<ASTVariable>(ModuleMember(access)))
			{
				// a generic function: made for these arguments, like max(3, 9)
				if (member->Variable->Kind == SymbolKind::GenericTemplate)
				{
					auto generic = std::dynamic_pointer_cast<ASTGenericTemplate>(member->Variable->GetGenericTemplate().GenericTemplate);

					if (generic && generic->TemplateNode->GetType() == ASTNodeType::FunctionDefinition)
					{
						for (auto& argument : funcCall->Arguments)
						{
							if (!(argument = Visit(argument, context)))
								return nullptr;
						}

						member->Variable = InstantiateFromValues(member, member->Variable, 0, funcCall->Arguments);

						if (!member->Variable)
							return nullptr;
					}
				}

				funcCall->Callee = member;
			}
		}

		// xs.push(v), xs.remove(i) ...: these can move or free the items, so pointers into xs go stale
		if (auto member = std::dynamic_pointer_cast<ASTBinaryExpression>(funcCall->Callee); member && member->GetExpression() == OperatorType::Dot)
		{
			static const std::unordered_set<std::string> changing = { "push", "pop", "insert", "remove", "clear", "reserve", "resize", "grow", "extend", "append", "free", "take" };

			if (auto method = std::dynamic_pointer_cast<ASTVariable>(member->RightSide); method && changing.contains(method->GetName().GetData()))
				NoteContainerChange(member->LeftSide, method->GetName());
		}

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

		// pointer(s): the address of a slice's (or str's) first item, unchecked (for library code)
		if (auto callee = std::dynamic_pointer_cast<ASTVariable>(funcCall->Callee); callee && !callee->Variable && 
			callee->GetName().GetData() == "pointer" && !LookupSymbol("pointer").first && funcCall->Arguments.size() == 1)
		{
			SemaContext valueContext = context;
			valueContext.ValueReq = ValueRequired::RValue;
			auto value = Visit(funcCall->Arguments[0], valueContext);
			auto slice = value ? std::dynamic_pointer_cast<SliceType>(m_TypeInferEngine.InferTypeFromNode(value)) : nullptr;

			if (!slice)
			{
				Report(DiagnosticCode_ExpectedType, value ? GetNodeLocation(value) : callee->GetName());
				return nullptr;
			}

			return SliceIntrinsic("slice_data", m_Module->GetTypeRegistry()->GetPointerTo(slice->GetBaseType()), { value }, callee->GetName());
		}

		// view(p, n): the slice of n items starting at p (for containers that manage raw memory)
		if (auto callee = std::dynamic_pointer_cast<ASTVariable>(funcCall->Callee); callee && !callee->Variable && 
			callee->GetName().GetData() == "view" && !LookupSymbol("view").first)
		{
			if (funcCall->Arguments.size() != 2)
			{
				Report(DiagnosticCode_WrongArgumentCount, callee->GetName());
				return nullptr;
			}

			SemaContext valueContext = context;
			valueContext.ValueReq = ValueRequired::RValue;
			auto pointer = Visit(funcCall->Arguments[0], valueContext);
			auto length = Visit(funcCall->Arguments[1], valueContext);
			auto pointerType = pointer ? m_TypeInferEngine.InferTypeFromNode(pointer) : nullptr;

			if (!length || !pointerType || !pointerType->IsPointer() || !pointerType->As<PointerType>()->GetBaseType())
			{
				Report(DiagnosticCode_ExpectedType, pointer ? GetNodeLocation(pointer) : callee->GetName());
				return nullptr;
			}

			auto slice = m_Module->GetTypeRegistry()->GetSliceOf(pointerType->As<PointerType>()->GetBaseType());
			return SliceIntrinsic("make_slice", slice, { pointer, Coerce(length, m_Module->Lookup("int64").value()->GetType()) }, callee->GetName());
		}

		// place(p, v): put v into raw memory that holds no value yet (unlike *p = v, nothing is cleaned up first)
		if (auto callee = std::dynamic_pointer_cast<ASTVariable>(funcCall->Callee); callee && !callee->Variable && callee->GetName().GetData() == "place" &&
			!LookupSymbol("place").first)
		{
			if (funcCall->Arguments.size() != 2)
			{
				Report(DiagnosticCode_WrongArgumentCount, callee->GetName());
				return nullptr;
			}

			auto target = std::make_shared<ASTUnaryExpression>(OperatorType::Dereference);
			target->Location = callee->GetName();
			target->Operand = funcCall->Arguments[0];

			auto assignment = std::make_shared<ASTAssignmentOperator>(AssignmentOperatorType::Normal);
			assignment->Location = callee->GetName();
			assignment->Storage = target;
			assignment->Value = funcCall->Arguments[1];

			auto result = Visit(assignment, context);

			if (auto placed = std::dynamic_pointer_cast<ASTAssignmentOperator>(result))
				placed->DestroyOld = false;

			return result;
		}

		// destroy(p): clean up *p now;  take(p): hand over the value at p (raw memory a container manages)
		if (auto callee = std::dynamic_pointer_cast<ASTVariable>(funcCall->Callee); callee && !callee->Variable && 
			(callee->GetName().GetData() == "destroy" || callee->GetName().GetData() == "take" || callee->GetName().GetData() == "clone") && 
			!LookupSymbol(callee->GetName().GetData()).first)
		{
			if (funcCall->Arguments.size() != 1)
			{
				Report(DiagnosticCode_WrongArgumentCount, callee->GetName());
				return nullptr;
			}

			SemaContext valueContext = context;
			valueContext.ValueReq = ValueRequired::RValue;
			auto pointer = Visit(funcCall->Arguments[0], valueContext);
			auto pointerType = pointer ? m_TypeInferEngine.InferTypeFromNode(pointer) : nullptr;

			if (!pointerType || !pointerType->IsPointer() || !pointerType->As<PointerType>()->GetBaseType())
			{
				Report(DiagnosticCode_ExpectedType, pointer ? GetNodeLocation(pointer) : callee->GetName());
				return nullptr;
			}

			auto valueType = pointerType->As<PointerType>()->GetBaseType();

			if (callee->GetName().GetData() == "destroy")
			{
				auto destroy = std::make_shared<ASTDestroy>();
				destroy->Location = callee->GetName();
				destroy->Pointer = pointer;
				destroy->ValueType = valueType;
				return destroy;
			}

			if (callee->GetName().GetData() == "clone" && !IsCopyable(valueType))
			{
				Token where = callee->GetName();
				where.SetData(GetDisplayName(valueType));
				Report(DiagnosticCode_CannotCopyOwning, where);
				return nullptr;
			}

			if (callee->GetName().GetData() == "clone")
				EnsureCopyDefined(valueType);

			auto take = std::make_shared<ASTIntrinsic>(callee->GetName().GetData(), valueType);
			take->Location = callee->GetName();
			take->Arguments.push_back(pointer);
			return take;
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

				// print("a", end = "") is not Python's print: say so instead of ignoring it
				if (!funcCall->KeywordArguments.empty())
				{
					auto& [name, value] = funcCall->KeywordArguments[0];
					Token where = name;
					where.SetData(std::format("{}’ is not something print takes: it prints its values with spaces between them and a line break after. "
											  "Without the line break: print_text(text), after import ‘io", name.GetData()));
					m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_UnknownKeyword, name.GetData().size());
					return nullptr;
				}

				for (auto& arg : funcCall->Arguments)
				{
					arg = Visit(arg, context);

					if (!arg)
						return nullptr;

					// a class with __str__ prints as whatever that returns
					auto type = m_TypeInferEngine.InferTypeFromNode(arg);
					std::unordered_set<Type*> printed;
					EnsurePrintable(type, printed);

					// print(make_name()): the new value lives until the end of the block, then is cleaned up
					if (IsOwning(type) && IsFreshValue(arg))
					{
						auto load = std::make_shared<ASTLoad>();
						load->Operand = AddressOf(arg);
						arg = load;
					}

					if (auto classType = ClassOf(type); classType && classType->As<ClassType>()->MemberFunctions.contains("__str__"))
					{
						EnsureDefined(classType->As<ClassType>()->MemberFunctions.at("__str__")->GetFunctionSymbol().FunctionNode);
						arg = CallMethod(arg, type, "__str__", {}, GetNodeLocation(arg));

						// operator str giving a String: printed, then cleaned up at the end of the block
						if (arg && IsOwning(m_TypeInferEngine.InferTypeFromNode(arg)) && IsFreshValue(arg))
						{
							auto load = std::make_shared<ASTLoad>();
							load->Operand = AddressOf(arg);
							arg = load;
						}

						if (!arg)
							return nullptr;
					}
				}

				return funcCall;
			}
		}
		
		// generic f(x: T, g: F): the type parameters plain arguments go to (an untyped lambda passed as an F is a type of its own)
		std::vector<bool> toTypeParameter(funcCall->Arguments.size(), false);

		if (auto callee = std::dynamic_pointer_cast<ASTVariable>(funcCall->Callee); callee && !callee->Variable)
		{
			auto [entry, scope] = LookupSymbol(callee->GetName().GetData());
			auto generic = entry && entry->Symbol->Kind == SymbolKind::GenericTemplate ? std::dynamic_pointer_cast<ASTGenericTemplate>(entry->Symbol->GetGenericTemplate().GenericTemplate) : nullptr;
			auto function = generic ? std::dynamic_pointer_cast<ASTFunctionDefinition>(generic->TemplateNode) : nullptr;

			for (size_t i = 0; function && i < funcCall->Arguments.size() && i < function->Arguments.size(); i++)
			{
				auto typeName = std::dynamic_pointer_cast<ASTVariable>(function->Arguments[i]->TypeResolver);
				toTypeParameter[i] = typeName && std::find(generic->GenericTypeNames.begin(), generic->GenericTypeNames.end(), typeName->GetName().GetData()) != generic->GenericTypeNames.end();
			}
		}

		for (size_t i = 0; i < funcCall->Arguments.size(); i++)
		{
			auto& arg = funcCall->Arguments[i];

			// a lambda without parameter types is analysed once the parameter it goes to is known (in CheckCall)
			if (auto lambda = std::dynamic_pointer_cast<ASTLambda>(arg); lambda && !toTypeParameter[i] && 
				std::any_of(lambda->Parameters.begin(), lambda->Parameters.end(), [](auto& p) { return !p->TypeResolver; }))
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

			// the type named on the left: Shape, or sh.Shape through an import alias
			std::shared_ptr<Symbol> typeSymbol;

			if (left && right && !left->Variable)
			{
				if (auto [entry, scopeIndex] = LookupSymbol(left->GetName().GetData()); entry)
					typeSymbol = entry->Symbol;
			}
			else if (auto access = std::dynamic_pointer_cast<ASTBinaryExpression>(member->LeftSide); right && access && access->GetExpression() == OperatorType::Dot)
			{
				if (auto aliased = std::dynamic_pointer_cast<ASTVariable>(ModuleMember(access)))
					typeSymbol = aliased->Variable;
			}

			if (typeSymbol && typeSymbol->Kind == SymbolKind::Type && typeSymbol->GetType()->IsClass() && typeSymbol->GetType()->As<ClassType>()->IsVariant)
			{
				auto variantType = typeSymbol->GetType();
				auto index = variantType->As<ClassType>()->FindCase(right->GetName().GetData());

				if (index)
					return BuildVariantConstruct(variantType, *index, funcCall->Arguments, funcCall->KeywordArguments, right->GetName());
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
		// (m.Box(7) through an import alias: the callee already names the template)
		if (auto var = std::dynamic_pointer_cast<ASTVariable>(funcCall->Callee); var && (!var->Variable || var->Variable->Kind == SymbolKind::GenericTemplate))
		{
			auto [found, foundScope] = var->Variable ? std::pair<std::optional<SymbolEntry>, size_t>() : LookupSymbol(var->GetName().GetData());
			std::optional<SymbolEntry> entry = var->Variable ? std::optional(SymbolEntry { SymbolEntryType::None, var->Variable }) : found;
			size_t scopeIndex = var->Variable ? 0 : foundScope;

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

		// xs.map(lambda x: x * 2): a generic method is made for these arguments (it is not a member yet)
		if (auto member = std::dynamic_pointer_cast<ASTBinaryExpression>(funcCall->Callee); member && member->GetExpression() == OperatorType::Dot)
		{
			auto name = std::dynamic_pointer_cast<ASTVariable>(member->RightSide);

			if (name && m_GenericMethodNames.contains(name->GetName().GetData()))
			{
				member->LeftSide = Visit(member->LeftSide, calleeContext);

				if (!member->LeftSide)
					return nullptr;

				auto objectType = m_TypeInferEngine.InferTypeFromNode(member->LeftSide);
				auto classType = ClassOf(objectType);
				auto methods = classType ? m_GenericMethods.find(classType.get()) : m_GenericMethods.end();

				if (methods != m_GenericMethods.end() && methods->second.contains(name->GetName().GetData()) &&
					!classType->As<ClassType>()->MemberFunctions.contains(name->GetName().GetData()))
					return CallGenericMethod(funcCall, member, objectType, classType->As<ClassType>(), name->GetName().GetData());
			}
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

				if (auto classType = ClassOf(calleeType); classType && m_LambdaTemplates.contains(classType.get()))
					return CallLambdaTemplate(funcCall, calleeType, classType->As<ClassType>());

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

		// int64(x), T(0) in a generic: the value converted, like `x as int64`
		if (auto var = std::dynamic_pointer_cast<ASTVariable>(funcCall->Callee); 
			var && var->Variable && var->Variable->Kind == SymbolKind::Type && funcCall->Arguments.size() == 1 && funcCall->KeywordArguments.empty())
		{
			auto cast = std::make_shared<ASTCastExpr>();
			cast->Location = var->GetName();
			cast->Object = funcCall->Arguments[0];
			cast->TypeNode = std::make_shared<ASTTypeLiteral>(var->Variable->GetType());
			return Visit(cast, context);
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
		auto macroSymbol = call->ResolvedMacro ? call->ResolvedMacro : (entry ? entry->Symbol : nullptr);

		if (!macroSymbol || macroSymbol->Kind != SymbolKind::Macro)
		{
			Report(DiagnosticCode_UndeclaredIdentifier, call->Name);
			return nullptr;
		}

		auto macro = std::dynamic_pointer_cast<ASTMacro>(macroSymbol->GetGenericTemplate().GenericTemplate);

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
		else if (context.ValueReq == ValueRequired::RValue)
		{
			// let r = twice!("ab") with a macro made of statements: it has no value to give
			Token where = call->Name;
			size_t width = where.GetData().size() + 1;
			where.SetData(call->Name.GetData());
			Report(DiagnosticCode_MacroHasNoValue, where, width);
			result = nullptr;
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
			// P(...): counted as written, without the self that init gets
			size_t shown = funcCall->IsConstructor && expected > 0 ? expected - 1 : expected;
			size_t passed = funcCall->IsConstructor && given > 0 ? given - 1 : given;

			Token where = location;
			where.SetData(std::format("{}’ expects {}{} argument{}, but {} {} given", where.GetData(), function->IsVariadic ? "at least " : "", 
									  shown, shown == 1 ? "" : "s", passed, passed == 1 ? "was" : "were"));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_WrongArgumentCount, std::max<size_t>(location.GetData().size(), 1));
			return nullptr;
		}

		size_t firstCandidate = m_Copies.Candidates.size();
		struct Lent { Sema* S; std::shared_ptr<ASTFunctionCall> Call; size_t First; ~Lent() { S->KeepLentArguments(Call->Arguments, First); } } lent { this, funcCall, firstCandidate };

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

		// printf("%s", text): a C function's extra arguments get a str as the char* C expects
		if (function->IsVariadic && !function->CodeBlock)
		{
			auto bytes = m_Module->GetTypeRegistry()->GetPointerTo(m_Module->Lookup("int8").value()->GetType());

			for (size_t i = expected; i < funcCall->Arguments.size(); i++)
			{
				if (auto type = m_TypeInferEngine.InferTypeFromNode(funcCall->Arguments[i]); type && type->GetHash() == "str")
					funcCall->Arguments[i] = Coerce(funcCall->Arguments[i], bytes);
			}
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

		// a lambda that borrows a local cannot outlive it
		if (returnStatement->ReturnValue)
		{
			auto type = m_TypeInferEngine.InferTypeFromNode(returnStatement->ReturnValue);

			if (type && m_BorrowingClosures.contains(type.get()))
			{
				Token location = GetNodeLocation(returnStatement->ReturnValue);
				Report(DiagnosticCode_BorrowingLambdaEscapes, location, location.GetData().size());
				return nullptr;
			}
		}

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
		{
			// `return s`: s ends here anyway, so it is handed over rather than copied
			m_Returning = true;
			returnStatement->ReturnValue = Coerce(returnStatement->ReturnValue, context.ReturnType);
			m_Returning = false;
		}

		// a copy made earlier in this block whose variable is not used between it and here is the last use on
		// this path, even if the variable is used again further down (another path) or the block is in a loop
		if (!m_BlockCandidates.empty())
		{
			for (size_t i = m_BlockCandidates.back(); i < m_Copies.Candidates.size(); i++)
			{
				auto& candidate = m_Copies.Candidates[i];

				if (candidate.Valid && m_Copies.Uses[candidate.Variable] == candidate.Use)
					candidate.Final = true;
			}
		}

		m_Unreachable = true;
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
			case OperatorType::Coalesce:
				return VisitCoalesce(binaryExpression, context);
			case OperatorType::OptionalDot:
				return VisitOptionalChain(binaryExpression, context, nullptr);
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

		// m[k] = v can add an entry, which may move the others
		if (auto subscript = std::dynamic_pointer_cast<ASTSubscript>(assignmentOp->Storage))
			NoteContainerChange(subscript->Target, GetNodeLocation(subscript->Target));

		EndNarrowing(assignmentOp);

		// x = v gives x a new value (x += v reads it first)
		auto reinitialised = assignmentOp->GetAssignType() == AssignmentOperatorType::Normal ? std::dynamic_pointer_cast<ASTVariable>(assignmentOp->Storage) : nullptr;
		m_Reinitialised = reinitialised.get();
		assignmentOp->Storage = Visit(assignmentOp->Storage, storageContext);
		m_Reinitialised = nullptr;
		// auto type = m_TypeInferEngine.InferTypeFromNode(assignmentOp->Storage);
		//if (type->IsConst() || type->As<PointerType>()->GetBaseType()->IsConst()) {
		//	CLEAR_LOG_ERROR("WRITING TO CONST BAD!!");
			//Report(DiagnosticCode_AssignConst, Token());
		//}
	
		context.ValueReq = ValueRequired::RValue;

		if (assignmentOp->Storage && assignmentOp->Storage->GetType() != ASTNodeType::FunctionCall)
			context.ExpectedType = m_TypeInferEngine.InferTypeFromNode(assignmentOp->Storage);

		// ops["inc"] = lambda x: x + 1: the value takes the type operator set takes (that gives the lambda its types)
		if (auto setter = std::dynamic_pointer_cast<ASTFunctionCall>(assignmentOp->Storage); setter && setter->ClassType)
		{
			if (auto set = setter->ClassType->MemberFunctions.find("__setitem__"); set != setter->ClassType->MemberFunctions.end())
			{
				auto node = set->second->GetFunctionSymbol().FunctionNode;
				EnsureDefined(node);

				if (node && !node->Arguments.empty() && node->Arguments.back())
					context.ExpectedType = node->Arguments.back()->ResolvedType;
			}
		}

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

			// text += "!":  text = text + "!"  (a new String; the old one is cleaned up)
			if (assignmentOp->GetAssignType() == AssignmentOperatorType::Add)
			{
				if (auto text = TextConcat(AsValue(assignmentOp->Storage), assignmentOp->Value, assignmentOp->Value->Location))
				{
					assignmentOp->Value = text;
					assignmentOp->SetAssignType(AssignmentOperatorType::Normal);
				}
			}

			// replacing an owning value cleans up the old one, wherever it is (*p = v and p[i] = v too: raw memory
			// that holds no value yet is filled with place(p, v) instead)
			assignmentOp->DestroyOld = assignmentOp->GetAssignType() == AssignmentOperatorType::Normal && IsOwning(storageType);

			// a += b on a class is a = a + b with its operator add (the target is worked out once: the value
			// reads it through a slot the assignment fills with the target's address)
			bool overloaded = false;

			if (storageType && storageType->IsClass() && assignmentOp->GetAssignType() != AssignmentOperatorType::Normal &&
				assignmentOp->GetAssignType() != AssignmentOperatorType::Initialize)
			{
				auto target = std::make_shared<ASTSlot>(m_Module->GetTypeRegistry()->GetPointerTo(storageType));
				target->Location = GetNodeLocation(assignmentOp->Storage);

				auto current = std::make_shared<ASTUnaryExpression>(OperatorType::Dereference);
				current->Operand = target;
				current->Location = target->Location;

				auto value = CompoundValue(assignmentOp->GetAssignType(), current, assignmentOp->Value);

				if (!value)
					return nullptr;

				assignmentOp->CompoundTarget = target;
				assignmentOp->Value = value;
				assignmentOp->SetAssignType(AssignmentOperatorType::Normal);
				assignmentOp->DestroyOld = IsOwning(storageType);
				overloaded = true;
			}

			// pointer += n is pointer arithmetic, not a conversion
			if (storageType && !overloaded && !(storageType->IsPointer() && assignmentOp->GetAssignType() != AssignmentOperatorType::Normal))
				assignmentOp->Value = Coerce(assignmentOp->Value, storageType);

			if (reinitialised && reinitialised->Variable && !m_Unreachable)
				m_Moved.erase(reinitialised->Variable.get());

			if (reinitialised && reinitialised->Variable)
			{
				m_ElementPointers.erase(reinitialised->Variable.get());
				m_StalePointers.erase(reinitialised->Variable.get());
			}
		}

		// make().qty = 5: the change would go to a value that is thrown away straight after
		if (auto root = WrittenTemporary(assignmentOp->Storage))
		{
			Token location = GetNodeLocation(root);
			size_t width = location.GetData().size();
			location.SetData(GetDisplayName(m_TypeInferEngine.InferTypeFromNode(AsValue(root))));
			Report(DiagnosticCode_AssignToTemporary, location, width);
			return nullptr;
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

				DispatchOnObject(current, clsType);

				// a get that returns a reference: the current value is what it points at
				if (auto pointer = m_TypeInferEngine.InferTypeFromNode(current); pointer && pointer->IsPointer())
				{
					auto deref = std::make_shared<ASTUnaryExpression>(OperatorType::Dereference);
					deref->Location = funcCallNode->Location;
					deref->Operand = current;
					current = deref;
				}
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

			auto checked = CheckCall(funcCall);
			DispatchOnObject(checked, clsType);
			return checked;


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

		// text += "!"
		if (op->second == OperatorType::Add)
		{
			if (auto text = TextConcat(current, value, value->Location))
				return text;
		}

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

		auto checked = CheckCall(call);
		DispatchOnObject(checked, classType->As<ClassType>());
		return checked;
	}

	bool Sema::CheckDereference(std::shared_ptr<ASTUnaryExpression> deref)
	{
		// *n with n an int: only a pointer can be read through (a type, *int, is fine)
		auto operand = deref->Operand;

		if (!operand)
			return false;

		if (auto variable = std::dynamic_pointer_cast<ASTVariable>(operand); variable && !variable->Variable)
			return true;

		if (!IsNodeValue(operand))
			return true;

		auto type = m_TypeInferEngine.InferTypeFromNode(operand);

		if (!type || type->IsPointer() || type->Get()->isPointerTy())
			return true;

		Token location = GetNodeLocation(operand);
		size_t width = std::max<size_t>(location.GetData().size(), 1);
		auto load = std::dynamic_pointer_cast<ASTLoad>(operand);
		bool named = operand->GetType() == ASTNodeType::Variable || (load && load->Operand->GetType() == ASTNodeType::Variable);
		location.SetData(std::format("{}’ is a ‘{}", named ? location.GetData() : "the value", GetDisplayName(type)));
		Report(DiagnosticCode_NotAPointer, location, width);
		return false;
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

				if (!modifies)
					NeverMove(unaryExpr->Operand); // a pointer to it may be used after its last mention

				// &seven(), &(a + b): the value is kept in a temporary (until the end of the block) and that is pointed at
				if (!modifies && !IsStorageNode(unaryExpr->Operand) && !std::dynamic_pointer_cast<ASTTemporary>(unaryExpr->Operand))
				{
					auto type = m_TypeInferEngine.InferTypeFromNode(unaryExpr->Operand);

					if (type && !type->IsPointer() && unaryExpr->Operand->GetType() != ASTNodeType::Variable)
						return AddressOf(unaryExpr->Operand);
				}

				if (auto var = std::dynamic_pointer_cast<ASTVariable>(unaryExpr->Operand); modifies && var && m_ConstSymbols.contains(var->Variable.get()))
				{
					Report(DiagnosticCode_AssignToConst, var->GetName());
					return nullptr;
				}

				break;
			}
			case OperatorType::Dereference:
			{
				if (auto literal = std::dynamic_pointer_cast<ASTNodeLiteral>(unaryExpr->Operand); literal && literal->GetData().GetData() == "null" && !literal->GetData().IsType(TokenType::String))
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
					return CheckDereference(unaryExpr) ? unaryExpr : nullptr;
				}

				unaryExpr->Operand = Visit(unaryExpr->Operand, context);

				if (!unaryExpr->Operand)
					return nullptr; // already reported (`*Nope`: an unknown type)

				if (!CheckDereference(unaryExpr))
					return nullptr;

				break;
			}
			default:
			{
				unaryExpr->Operand = Visit(unaryExpr->Operand, context);

				if (!unaryExpr->Operand)
					return nullptr; // already reported (`?Nope`: an unknown type)

				// -v on a class: its operator negate
				if (unaryExpr->GetOperatorType() == OperatorType::Negation && unaryExpr->Operand)
				{
					auto type = m_TypeInferEngine.InferTypeFromNode(unaryExpr->Operand);

					if (type && type->IsClass())
					{
						auto classType = type->As<ClassType>();
						Token location = unaryExpr->Location.GetData().empty() ? GetNodeLocation(unaryExpr->Operand) : unaryExpr->Location;
						auto method = classType->MemberFunctions.find("__neg__");

						if (method == classType->MemberFunctions.end())
						{
							if (classType->IsOptional)
								location.SetData(std::format("‘{}’ is optional, so it may hold no value: use its value with `x ?? fallback`, `if x:` or `.value` first",
															 GetDisplayName(classType)));
							else
								location.SetData(std::format("‘{}’ has no ‘operator negate’ for ‘-’. Define it in the class: operator negate(self) -> {}",
															 GetDisplayName(classType), GetDisplayName(classType)));

							m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_MissingOperatorOverload, 1);
							return nullptr;
						}

						auto function = method->second->GetFunctionSymbol().FunctionNode;
						EnsureDefined(function);

						if (!function || function->Arguments.size() != 1)
						{
							location.SetData(std::format("‘operator negate’ of ‘{}’ must take only self: operator negate(self) -> {}",
														 GetDisplayName(classType), GetDisplayName(classType)));
							m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_MissingOperatorOverload, 1);
							return nullptr;
						}

						return CallMethod(unaryExpr->Operand, type, "__neg__", {}, location);
					}

					if (type && !(type->IsIntegral() || type->IsFloatingPoint()) || (type && (type->IsEnum() || type->Get()->isIntegerTy(1))))
					{
						Token location = unaryExpr->Location.GetData().empty() ? GetNodeLocation(unaryExpr->Operand) : unaryExpr->Location;
						location.SetData(std::format("-{}", GetDisplayName(type)));
						m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_InvalidOperands, 1);
						return nullptr;
					}
				}

				// not x with x optional: x holds no value
				if (unaryExpr->GetOperatorType() == OperatorType::Not && unaryExpr->Operand)
				{
					auto type = m_TypeInferEngine.InferTypeFromNode(unaryExpr->Operand);

					if (IsOptionalType(type))
					{
						if (OptionalValueType(type)->GetHash() == "bool")
						{
							Report(DiagnosticCode_OptionalBoolCondition, GetNodeLocation(unaryExpr->Operand));
							return nullptr;
						}

						return OptionalTest(unaryExpr->Operand, type, false);
					}
				}
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

			// a C function returning str returns char* (it becomes a str where one is expected)
			if (decl->ReturnType && decl->ReturnType->GetHash() == "str")
				decl->ReturnType = m_Module->GetTypeRegistry()->GetPointerTo(m_Module->Lookup("int8").value()->GetType());
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

			// a C function sees a str as char* (the call converts it, checking it ends with a zero)
			if (arg->ResolvedType && arg->ResolvedType->GetHash() == "str")
				arg->ResolvedType = m_Module->GetTypeRegistry()->GetPointerTo(m_Module->Lookup("int8").value()->GetType());

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

			// the vtable refers to every virtual method, and cleanup to operator destruct, so those are always needed
			for (auto& method : classExpr->MemberFunctions)
			{
				if (method->IsVirtual || method->GetName().find("__destruct__") != std::string::npos)
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

	void Sema::EnsurePrintable(const std::shared_ptr<Type>& type, std::unordered_set<Type*>& seen)
	{
		// print(list) prints each item with its operator str: the methods of a generic class are only
		// analysed when used, so these count as used
		if (!type || !seen.insert(type.get()).second)
			return;

		if (auto array = std::dynamic_pointer_cast<ArrayType>(type))
			return EnsurePrintable(array->GetBaseType(), seen);

		if (auto slice = std::dynamic_pointer_cast<SliceType>(type))
			return EnsurePrintable(slice->GetBaseType(), seen);

		if (auto tuple = std::dynamic_pointer_cast<TupleType>(type))
		{
			for (auto& element : tuple->GetElements())
				EnsurePrintable(element, seen);
			return;
		}

		if (!type->IsClass())
			return;

		auto classType = type->As<ClassType>();

		if (auto str = classType->MemberFunctions.find("__str__"); str != classType->MemberFunctions.end())
		{
			auto function = str->second->GetFunctionSymbol().FunctionNode;
			EnsureDefined(function);

			if (function)
				EnsurePrintable(function->ReturnTypeVal, seen);
			return;
		}

		for (auto& argument : classType->GenericArguments)
			EnsurePrintable(argument, seen);

		for (auto& variantCase : classType->Cases)
		{
			for (auto& field : variantCase.Fields)
				EnsurePrintable(field.second, seen);
		}

		for (const auto& [name, field] : classType->GetMemberValues())
			EnsurePrintable(field, seen);
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

		// class N: next: N would be infinitely big
		for (size_t i = 0; i < members.size(); i++)
		{
			if (members[i].second->Kind != SymbolKind::Type || !ContainsByValue(members[i].second->GetType(), classTy))
				continue;

			auto spec = std::find_if(classExpr->Members.begin(), classExpr->Members.end(), [&](auto& m) { return m->GetName() == members[i].first; });
			Token location = spec != classExpr->Members.end() && (*spec)->TypeResolver ? GetNodeLocation((*spec)->TypeResolver) : classExpr->Location;
			size_t width = location.GetData().size();
			location.SetData(GetDisplayName(classTy));
			Report(DiagnosticCode_InfiniteType, location, width);
			m_BrokenClasses.insert(classTy.get()); // its uses would only repeat the problem
			return false;
		}

		// a union does not know which field it holds, so it could never clean one up
		if (classExpr->IsUnion)
		{
			for (auto& member : classExpr->Members)
			{
				auto spec = std::dynamic_pointer_cast<ASTTypeSpecifier>(member);
				auto field = std::find_if(members.begin(), members.end(), [&](auto& m) { return spec && m.first == spec->GetName(); });

				if (field != members.end() && IsOwning(field->second->GetType()))
				{
					Token location = spec->TypeResolver ? GetNodeLocation(spec->TypeResolver) : Token();

					if (location.GetSourceFile().empty())
						location = classExpr->Location;

					size_t width = location.GetData().size();
					location.SetData(GetDisplayName(field->second->GetType()));
					Report(DiagnosticCode_UnionOwningField, location, width);
				}
			}
		}

		if (classExpr->IsUnion)
			classTy->SetUnionBody(members);
		else
			classTy->SetBody(members);

		context.TypeHint = classTy;
		
		for (auto node : classExpr->MemberFunctions)
			DeclareFunction(node, context);

		// an override is called through the base's slot (obj.speak(), print(*p), destroy(p)...), so it must
		// take and give the same types as the version it replaces
		if (base && !CheckOverrides(classExpr, base))
			return false;

		for (auto& trait : classTy->Traits)
		{
			if (!CheckTrait(classTy, trait, location))
				return false;
		}

		// function map[U](self, ...): kept as a template with what it needs to be analysed later, made per use
		for (auto& generic : classExpr->GenericMethods)
		{
			auto method = std::dynamic_pointer_cast<ASTFunctionDefinition>(generic->TemplateNode);
			generic->HomeModule = generic->HomeModule ? generic->HomeModule : m_Module;
			m_GenericMethods[classTy.get()][method->GetName()] = GenericMethod { generic, LazyBody { m_ScopeStack, m_LookupModule, classTy }, {} };
			m_GenericMethodNames.insert(method->GetName());
		}

		return true;
	}

	bool Sema::CheckOverrides(std::shared_ptr<ASTClass> classExpr, std::shared_ptr<ClassType> base)
	{
		auto same = [](const std::shared_ptr<Type>& a, const std::shared_ptr<Type>& b)
		{
			return (!a && !b) || (a && b && (a == b || a->GetHash() == b->GetHash()));
		};

		auto describe = [&](const std::shared_ptr<ASTFunctionDefinition>& function)
		{
			std::string text = "(self";

			for (size_t i = 1; i < function->Arguments.size(); i++)
				text += ", " + GetDisplayName(function->Arguments[i]->ResolvedType);

			return text + ")" + (function->ReturnTypeVal ? " -> " + GetDisplayName(function->ReturnTypeVal) : "");
		};

		// (by the names the class knows them by: a declared function's own name is its mangled one)
		for (auto& [name, symbol] : classExpr->ClassTy->As<ClassType>()->MemberFunctions)
		{
			if (symbol->Kind != SymbolKind::Function)
				continue;

			auto node = symbol->GetFunctionSymbol().FunctionNode;

			// operator copy gives a value of its own class: it is never called through a base's slot
			if (!node || !node->IsVirtual || name == "__copy__" || !node->SignatureResolved)
				continue;

			auto inherited = base->MemberFunctions.find(name);

			if (inherited == base->MemberFunctions.end() || inherited->second->Kind != SymbolKind::Function)
				continue;

			auto original = inherited->second->GetFunctionSymbol().FunctionNode;

			if (!original || !original->SignatureResolved || original == node)
				continue;

			bool matches = original->Arguments.size() == node->Arguments.size() && same(original->ReturnTypeVal, node->ReturnTypeVal);

			for (size_t i = 0; matches && i < node->Arguments.size(); i++)
			{
				auto mine = node->Arguments[i]->ResolvedType, theirs = original->Arguments[i]->ResolvedType;

				// self: a pointer to each one's own class, or each one's own value
				if (i == 0)
					matches = mine && theirs && mine->IsPointer() == theirs->IsPointer();
				else
					matches = same(mine, theirs);
			}

			if (!matches)
			{
				Token location = node->GetNameToken().GetData().empty() ? classExpr->Location : node->GetNameToken();
				size_t width = std::max<size_t>(location.GetData().size(), 1);
				location.SetData(std::format("‘{}’ in ‘{}’ is {}, but in ‘{}’ it is {}. An override must take and give the same types, because it is "
											 "called wherever the base's version is", location.GetData(), classExpr->GetName(), describe(node),
											 GetDisplayName(base), describe(original)));
				Report(DiagnosticCode_OverrideMismatch, location, width);
				return false;
			}
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

		auto branches = BeginBranches();
		std::vector<std::shared_ptr<ASTVariableDeclaration>> narrowAfter;

		for (size_t index = 0; index < ifExpr->ConditionalBlocks.size(); index++)
		{
			auto& conditionalBlock = ifExpr->ConditionalBlocks[index];

			// optional local variables known to hold a value: inside the block for `if a and b:` (each of a, b),
			// after it (or in the else) for `if not a or b is none:` (each of a, b)
			std::vector<Narrowing> inside, outside;
			CollectNarrowings(conditionalBlock.Condition, true, inside);
			CollectNarrowings(conditionalBlock.Condition, false, outside);

			m_Moved = branches.Start;
			conditionalBlock.Condition = TestCondition(Visit(conditionalBlock.Condition, conditionContext), true);
			branches.Start = m_Moved; // conditions run one after another

			for (auto& narrowing : inside)
				conditionalBlock.CodeBlock->Children.insert(conditionalBlock.CodeBlock->Children.begin(), NarrowedDeclaration(narrowing));

			BeginBranch(branches);
			Visit(conditionalBlock.CodeBlock, context);
			EndBranch(branches);

			if (!outside.empty() && index + 1 == ifExpr->ConditionalBlocks.size())
			{
				// if not r: ... else: <r has a value>;   if not r: return  <r has a value from here on>
				for (auto& narrowing : outside)
				{
					if (ifExpr->ElseBlock)
						ifExpr->ElseBlock->Children.insert(ifExpr->ElseBlock->Children.begin(), NarrowedDeclaration(narrowing));
					else if (ifExpr->ConditionalBlocks.size() == 1 && m_Unreachable)
						narrowAfter.push_back(NarrowedDeclaration(narrowing));
				}
			}
		}
		
		if (ifExpr->ElseBlock)
		{
			BeginBranch(branches);
			Visit(ifExpr->ElseBlock, context);
			EndBranch(branches);
		}

		EndBranches(branches, !ifExpr->ElseBlock);
		m_NarrowAfter = narrowAfter;
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
			std::string from = importExpr->Filepath.filename().string();

			if (m_ScopeStack.front().Insert(symbolName, entryType, exposedSymbol))
			{
				m_ImportedFrom[symbolName] = from;
				continue;
			}

			// two imports define the same name: using it without saying which is an error (see Visit(ASTVariable))
			auto existing = m_ScopeStack.front().Get(symbolName);

			if (existing && existing->Symbol != exposedSymbol && m_ImportedFrom.contains(symbolName) && m_ImportedFrom[symbolName] != from)
				m_AmbiguousImports.try_emplace(symbolName, m_ImportedFrom[symbolName], from);
		}
		
		return importExpr;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTWhileExpression> whileExpr, SemaContext context)
	{
		MovedSet beforeLoop = m_Moved;
		BeginLoop();

		context.ValueReq = ValueRequired::RValue;
		whileExpr->WhileBlock.Condition = TestCondition(Visit(whileExpr->WhileBlock.Condition, context), true);

		context.ValueReq = ValueRequired::Any;
		context.InLoop = true;
		Visit(whileExpr->WhileBlock.CodeBlock, context);

		EndLoop(beforeLoop);
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

				// a generator held in a variable is iterated in place (a later loop can carry on after a break),
				// a new one belongs to the loop and is cleaned up when it ends
				auto handle = std::make_shared<ASTVariableDeclaration>(token(handleName));
				handle->Location = location;
				handle->Initializer = pristineIterable;

				bool held = pristineIterable->GetType() == ASTNodeType::Variable ||
							(pristineIterable->GetType() == ASTNodeType::BinaryExpression && std::dynamic_pointer_cast<ASTBinaryExpression>(pristineIterable)->GetExpression() == OperatorType::Dot);

				if (held)
				{
					auto address = std::make_shared<ASTUnaryExpression>(OperatorType::Address);
					address->Location = location;
					address->Operand = pristineIterable;
					handle->Initializer = address;
					handle->IsAlias = true;
				}

				auto element = std::make_shared<ASTVariableDeclaration>(forExpr->VariableName);
				element->Location = forExpr->VariableName;
				element->Initializer = intrinsic("coro_value", generator->GetValueType());

				// objects are named in place (the value the generator holds), numbers are copied
				if (generator->GetValueType() && generator->GetValueType()->IsClass())
				{
					element->Initializer = intrinsic("coro_value_address", m_Module->GetTypeRegistry()->GetPointerTo(generator->GetValueType()));
					element->IsAlias = true;
				}

				auto loop = std::make_shared<ASTWhileExpression>();
				loop->WhileBlock.Condition = intrinsic("coro_advance", Symbol::GetBooleanType(m_Module).GetType());
				loop->WhileBlock.CodeBlock = std::make_shared<ASTBlock>();
				loop->WhileBlock.CodeBlock->Children.push_back(element);
				loop->WhileBlock.CodeBlock->Children.insert(loop->WhileBlock.CodeBlock->Children.end(), forExpr->CodeBlock->Children.begin(), forExpr->CodeBlock->Children.end());

				auto block = std::make_shared<ASTBlock>();
				block->Children = { handle, loop };
				return Visit(block, context);
			}

			// for x in slice:   ->   let s = slice;  for i in 0..len(s): let x = s[i] (the element itself if it is an object)
			if (auto slice = std::dynamic_pointer_cast<SliceType>(forExpr->IterableType))
			{
				static size_t s_SliceCounter = 0;
				size_t id = s_SliceCounter++;
				Token location = forExpr->Location;
				auto name = [&](const std::string& text) { return std::make_shared<ASTVariable>(Token(TokenType::Identifier, text, location.GetSourceFile(), location.LineNumber, location.ColumnNumber)); };
				std::string sliceName = std::format("__slice_{}", id), indexName = std::format("__slice_index_{}", id);
				auto int64Type = m_Module->Lookup("int64").value()->GetType();

				auto intrinsic = [&](const std::string& intrinsicName, std::shared_ptr<Type> result, std::vector<std::shared_ptr<ASTNodeBase>> arguments)
				{
					auto node = std::make_shared<ASTIntrinsic>(intrinsicName, result);
					node->Location = location;
					node->Unanalysed = true;
					node->Arguments.assign(arguments.begin(), arguments.end());
					return node;
				};

				auto held = std::make_shared<ASTVariableDeclaration>(name(sliceName)->GetName());
				held->Location = location;
				held->Initializer = pristineIterable;

				auto element = std::make_shared<ASTVariableDeclaration>(forExpr->VariableName);
				element->Location = forExpr->VariableName;
				auto address = intrinsic("slice_at", m_Module->GetTypeRegistry()->GetPointerTo(slice->GetBaseType()), { name(sliceName), name(indexName) });

				if (slice->GetBaseType()->IsClass() || IsOwning(slice->GetBaseType()))
				{
					element->Initializer = address;
					element->IsAlias = true;
				}
				else
				{
					auto value = std::make_shared<ASTUnaryExpression>(OperatorType::Dereference);
					value->Location = location;
					value->Operand = address;
					value->IsElement = true;
					element->Initializer = value;
				}

				auto loop = std::make_shared<ASTForExpression>();
				loop->Location = location;
				loop->VariableName = name(indexName)->GetName();
				loop->Start = std::make_shared<ASTConstantValue>((int64_t)0, int64Type);
				loop->End = intrinsic("slice_len", int64Type, { name(sliceName) });
				loop->CodeBlock = std::make_shared<ASTBlock>();
				loop->CodeBlock->Children.push_back(element);
				loop->CodeBlock->Children.insert(loop->CodeBlock->Children.end(), forExpr->CodeBlock->Children.begin(), forExpr->CodeBlock->Children.end());

				auto block = std::make_shared<ASTBlock>();
				block->Children = { held, loop };
				return Visit(block, context);
			}

			if (!forExpr->IterableType || !forExpr->IterableType->IsArray())
			{
				Report(DiagnosticCode_NotIterable, GetNodeLocation(forExpr->Iterable));
				return nullptr;
			}

			forExpr->VariableType = forExpr->IterableType->As<ArrayType>()->GetBaseType();
			forExpr->IterableIsTemporary = !IsStorageNode(forExpr->Iterable);

			// for s in [String("a"), String("b")]: the new array is kept (and cleaned up) until the end of the block,
			// its items are visited in place like those of a variable
			if (forExpr->IterableIsTemporary && IsOwning(forExpr->IterableType) && IsFreshValue(forExpr->Iterable))
			{
				forExpr->Iterable = AddressOf(forExpr->Iterable);
				forExpr->IterableIsTemporary = false;
			}
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

		MovedSet beforeLoop = m_Moved;
		BeginLoop();
		Visit(forExpr->CodeBlock, bodyContext);
		EndLoop(beforeLoop);

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

		// a get that returns a reference to an object: the loop variable *is* that object (x.qty = 1 changes the item);
		// numbers and other plain values are copied, like in Python
		// (and anything else that owns something, a Task or a Generator: a copy would be cleaned up each time round)
		if (auto getter = classType->MemberFunctions.at("__getitem__")->GetFunctionSymbol().FunctionNode; 
			getter && getter->ReturnTypeVal && getter->ReturnTypeVal->IsPointer() && 
			(getter->ReturnTypeVal->As<PointerType>()->GetBaseType()->IsClass() || IsOwning(getter->ReturnTypeVal->As<PointerType>()->GetBaseType())))
			elementDecl->IsAlias = true;
		else if (getter && getter->ReturnTypeVal && getter->ReturnTypeVal->IsPointer())
		{
			auto value = std::make_shared<ASTUnaryExpression>(OperatorType::Dereference);
			value->Location = location;
			value->Operand = elementDecl->Initializer;
			elementDecl->Initializer = value;
		}

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

		// switch on text:  if s == "one": ... else if s == "two" or s == "2": ... else: <default>   (s computed once)
		if (valueType && (valueType->GetHash() == "str" || (ClassOf(valueType) && ClassOf(valueType)->GetHash() == "String")))
		{
			auto subject = EvaluatedOnce(switchNode->Value, valueType);
			auto chain = std::make_shared<ASTIfExpression>();
			chain->Location = switchNode->Location;

			for (auto& switchCase : switchNode->Cases)
			{
				std::shared_ptr<ASTNodeBase> condition;

				for (auto& value : switchCase.Values)
				{
					auto equal = std::make_shared<ASTBinaryExpression>(OperatorType::IsEqual);
					equal->Location = GetNodeLocation(value);
					equal->LeftSide = subject;
					equal->RightSide = value;

					if (!condition)
					{
						condition = equal;
						continue;
					}

					auto either = std::make_shared<ASTBinaryExpression>(OperatorType::Or);
					either->Location = equal->Location;
					either->LeftSide = condition;
					either->RightSide = equal;
					condition = either;
				}

				if (condition)
					chain->ConditionalBlocks.push_back({ .Condition = condition, .CodeBlock = switchCase.CodeBlock });
			}

			if (chain->ConditionalBlocks.empty())
				return switchNode->DefaultCaseCodeBlock ? Visit(switchNode->DefaultCaseCodeBlock, context) : nullptr;

			chain->ElseBlock = switchNode->DefaultCaseCodeBlock;
			return Visit(chain, context);
		}

		if (!valueType || !valueType->IsIntegral())
		{
			Report(DiagnosticCode_SwitchNotIntegral, GetNodeLocation(switchNode->Value));
			return nullptr;
		}

		std::unordered_set<int64_t> seen;
		auto branches = BeginBranches();

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

			BeginBranch(branches);
			Visit(switchCase.CodeBlock, context);
			EndBranch(branches);
		}

		if (switchNode->DefaultCaseCodeBlock)
		{
			BeginBranch(branches);
			Visit(switchNode->DefaultCaseCodeBlock, context);
			EndBranch(branches);
		}

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

		EndBranches(branches, !switchNode->DefaultCaseCodeBlock && !switchNode->IsExhaustive);
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
			case ASTNodeType::TupleGet:
			{
				// t[0] analysed as an lvalue gives the element's address
				auto get = std::dynamic_pointer_cast<ASTTupleGet>(node);
				return get->WantAddress && get->TupleIsStorage;
			}
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

	// a method (an operator, a property) called with the object as its first argument runs the object's own
	// version, like obj.method() does: the call goes through the vtable slot the method has in classType
	static void DispatchOnObject(const std::shared_ptr<ASTNodeBase>& node, const std::shared_ptr<ClassType>& classType)
	{
		auto call = std::dynamic_pointer_cast<ASTFunctionCall>(node);

		if (!call || !classType || call->VirtualSlot >= 0 || call->Arguments.empty())
			return;

		auto callee = std::dynamic_pointer_cast<ASTVariable>(call->Callee);

		if (!callee || !callee->Variable || callee->Variable->Kind != SymbolKind::Function)
			return;

		auto function = callee->Variable->GetFunctionSymbol().FunctionNode;

		// (a method taking self by value gets a copy of the object, which is of the type it was called on)
		if (!function || !function->IsVirtual || function->Arguments.empty() || !function->Arguments[0]->ResolvedType || !function->Arguments[0]->ResolvedType->IsPointer())
			return;

		for (size_t i = 0; i < classType->VTable.size(); i++)
		{
			auto& entry = classType->VTable[i];

			if (entry == callee->Variable || (entry && entry->Kind == SymbolKind::Function && entry->GetFunctionSymbol().FunctionNode == function))
			{
				call->VirtualSlot = (int64_t)i;
				return;
			}
		}
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

		auto checked = CheckCall(call);
		DispatchOnObject(checked, classType);
		return checked;
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

		bool wasReading = std::exchange(m_ReadingUse, true); // len(x) only reads x
		auto argument = Visit(funcCall->Arguments[0], storageContext);
		m_ReadingUse = wasReading;

		if (!argument)
			return nullptr;

		auto type = m_TypeInferEngine.InferTypeFromNode(argument);
		auto int64Type = m_Module->Lookup("int64").value()->GetType();

		// len(a) with a: [N; T] or *[N; T]: N
		if (type && type->IsPointer() && type->As<PointerType>()->GetBaseType() && type->As<PointerType>()->GetBaseType()->IsArray())
			type = type->As<PointerType>()->GetBaseType();

		if (type && type->IsArray())
			return std::make_shared<ASTConstantValue>((int64_t)type->As<ArrayType>()->GetArraySize(), int64Type);

		if (std::dynamic_pointer_cast<SliceType>(type))
			return SliceIntrinsic("slice_len", int64Type, { AsValue(argument) }, location);

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

		// the generator owns what it yields (and cleans it up when the next value replaces it): a new value is
		// handed over, anything else is copied, so the place it came from (a local, a map's own keys) keeps its own
		m_ViewsAllowed = true;
		yield->Value = Coerce(yield->Value, context.CoroutineValue);
		m_ViewsAllowed = false;
		yield->Value = OwnedValue(yield->Value, context.CoroutineValue);
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

		// awaiting a task finishes and frees it: a variable holding it is moved from
		await->Operand = TakeOwnership(await->Operand, task);
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

		// x.free(): clean up the variable (or temporary) holding it, which then holds nothing
		if (method == "free")
		{
			std::shared_ptr<ASTNodeBase> holder = IsStorageNode(object) ? object : nullptr;

			if (auto load = std::dynamic_pointer_cast<ASTLoad>(object); load && !holder)
				holder = load->Operand;

			if (holder)
			{
				auto destroy = std::make_shared<ASTDestroy>();
				destroy->Location = name;
				destroy->Pointer = holder;
				destroy->ValueType = type;
				return destroy;
			}
		}

		auto intrinsic = std::make_shared<ASTIntrinsic>(entry.Intrinsic, entry.Result);
		intrinsic->Location = name;
		intrinsic->Arguments.push_back(AsValue(object));

		// gen.value(): the generator keeps its value, the caller gets a copy
		if ((method == "value" || method == "result") && IsOwning(entry.Result))
			return OwnedValue(intrinsic, entry.Result);

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
		intrinsic->Arguments.push_back(AsValue(argument));
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

		auto needleType = m_TypeInferEngine.InferTypeFromNode(needle);

		if (type && type->GetHash() == "str" && needleType && needleType->IsIntegral() && !needleType->IsEnum() && needleType->Get()->isIntegerTy(8))
		{
			// c in "+-" with c a byte (int8, uint8, a 'x' literal): one of those bytes
			auto intrinsic = std::make_shared<ASTIntrinsic>("str_contains_byte", boolType);
			intrinsic->Location = expr->Location;
			intrinsic->Arguments = { needle, AsValue(haystack) };
			result = intrinsic;
		}
		else if (type && type->GetHash() == "str")
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

		// the tuple owns its values: a variable's value is copied in (or moved, at its last use)
		if (!tuple->IsType)
		{
			for (size_t i = 0; i < tuple->Values.size(); i++)
				tuple->Values[i] = TakeOwnership(tuple->Values[i], types[i]);
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
				if (parameter) // (else already reported: an unknown name)
					Report(DiagnosticCode_ExpectedType, GetNodeLocation(parameter));

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
				if (type->ReturnType) // (else already reported: an unknown name)
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
				if (binary->GetExpression() != OperatorType::Dot && binary->GetExpression() != OperatorType::OptionalDot)
					CollectNames(binary->RightSide, names);
				break;
			}
			case ASTNodeType::UnaryExpression: CollectNames(std::dynamic_pointer_cast<ASTUnaryExpression>(node)->Operand, names); break;
			case ASTNodeType::MacroCall:
			{
				// twice!(a + k): what is passed in is pasted into the lambda's body
				for (auto& arg : std::dynamic_pointer_cast<ASTMacroCall>(node)->Arguments) CollectNames(arg, names);
				break;
			}
			case ASTNodeType::IsExpr: CollectNames(std::dynamic_pointer_cast<ASTIsExpr>(node)->Object, names); break;
			case ASTNodeType::SliceExpr:
			{
				auto slice = std::dynamic_pointer_cast<ASTSliceExpr>(node);
				CollectNames(slice->Target, names);
				CollectNames(slice->Start, names);
				CollectNames(slice->End, names);
				break;
			}
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

			// a function type that does not fit says what the parameters must be; with nothing to go on, the
			// types come from the calls (see CallLambdaTemplate)
			if (!type && expected)
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
				m_Copies.NeverMove.insert(MoveRoot(entry->Symbol.get())); // the lambda may run after the variable's last mention
			}
		}

		bool untyped = std::find(parameterTypes.begin(), parameterTypes.end(), nullptr) != parameterTypes.end();

		if (captures.empty() && !untyped)
		{
			// nothing captured: an ordinary function, used through its address
			auto function = std::make_shared<ASTFunctionDefinition>(std::format("__lambda_{}", id));
			function->SetNameToken(token(function->GetName()));
			function->Location = location;

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

			auto returnStatement = std::make_shared<ASTReturn>();
			returnStatement->Location = location;
			returnStatement->ReturnValue = lambda->Body;

			auto body = std::make_shared<ASTBlock>();
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

		// captures: a small class holding copies of them, called through __call__. A value that owns memory is
		// borrowed (the closure holds a pointer to the variable), or moved in with `move lambda`
		auto closure = std::make_shared<ASTClass>(std::format("__closure_{}", id));
		closure->Location = location;

		// captured values are copied when the lambda is made, like ints; one that cannot be copied (a File) is
		// borrowed instead (or moved in with `move lambda`)
		auto borrowed = [&](const std::shared_ptr<Type>& type) { return IsOwning(type) && !lambda->MovesCaptures && !IsCopyable(type); };
		bool borrows = false;

		for (auto& [name, type] : captures)
		{
			auto member = std::make_shared<ASTTypeSpecifier>(name.GetData());
			borrows = borrows || borrowed(type);
			member->TypeResolver = std::make_shared<ASTTypeLiteral>(borrowed(type) ? m_Module->GetTypeRegistry()->GetPointerTo(type) : type);
			closure->Members.push_back(member);
			closure->DefaultValues.push_back(nullptr);
		}

		bool success = false;
		DeclareInGlobalScope([&]() { success = DeclareClassType(closure); });

		if (!success)
			return nullptr;

		// lambda x: x * 2 with nothing saying what x is: __call__ is made for each set of argument types it is called with
		if (untyped)
			m_LambdaTemplates[closure->ClassTy.get()] = LambdaTemplate { lambda, captures, parameterTypes, declaredReturn, {} };
		else
			closure->MemberFunctions.push_back(BuildClosureCall("__call__", lambda, lambda->Body, closure->ClassTy, captures, parameterTypes, declaredReturn));

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
		{
			if (!borrowed(type))
			{
				value->Values.push_back(std::make_shared<ASTVariable>(name));
				continue;
			}

			auto address = std::make_shared<ASTUnaryExpression>(OperatorType::Address);
			address->Location = name;
			address->Operand = std::make_shared<ASTVariable>(name);
			value->Values.push_back(address);
		}

		if (borrows)
			m_BorrowingClosures.insert(closure->ClassTy.get());

		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;
		valueContext.ExpectedType = nullptr;

		// move lambda: the captured variables are handed over, not copied
		bool wasReturning = std::exchange(m_Returning, m_Returning || lambda->MovesCaptures);
		auto result = Visit(value, valueContext);
		m_Returning = wasReturning;
		return result;
	}

	std::shared_ptr<ASTFunctionDefinition> Sema::BuildClosureCall(const std::string& name, const std::shared_ptr<ASTLambda>& lambda, std::shared_ptr<ASTNodeBase> body,
																  std::shared_ptr<Type> closureType, const std::vector<std::pair<Token, std::shared_ptr<Type>>>& captures,
																  const std::vector<std::shared_ptr<Type>>& parameterTypes, std::shared_ptr<Type> declaredReturn)
	{
		Token location = lambda->Location;
		auto token = [&](const std::string& text) { return Token(TokenType::Identifier, text, location.GetSourceFile(), location.LineNumber, location.ColumnNumber); };

		auto call = std::make_shared<ASTFunctionDefinition>(name);
		call->SetNameToken(token(name));
		call->Location = location;

		auto self = std::make_shared<ASTVariableDeclaration>(token("self"));
		self->TypeResolver = std::make_shared<ASTTypeLiteral>(m_Module->GetTypeRegistry()->GetPointerTo(closureType));
		call->Arguments.push_back(self);

		for (size_t i = 0; i < lambda->Parameters.size(); i++)
		{
			auto parameter = std::make_shared<ASTVariableDeclaration>(lambda->Parameters[i]->GetName());
			parameter->TypeResolver = std::make_shared<ASTTypeLiteral>(parameterTypes[i]);
			call->Arguments.push_back(parameter);
		}

		if (declaredReturn)
			call->ReturnType = std::make_shared<ASTTypeLiteral>(declaredReturn);
		else
			call->InferReturnType = true;

		auto block = std::make_shared<ASTBlock>();

		// inside __call__ each captured name is a local copied from the closure (an owning value is used in place:
		// the closure holds a pointer to it when borrowed, or the value itself when it was moved in)
		for (auto& [captured, type] : captures)
		{
			auto access = std::make_shared<ASTBinaryExpression>(OperatorType::Dot);
			access->Location = captured;
			access->LeftSide = std::make_shared<ASTVariable>(token("self"));
			access->RightSide = std::make_shared<ASTVariable>(captured);

			auto local = std::make_shared<ASTVariableDeclaration>(captured);
			local->Initializer = access;

			if (IsOwning(type))
			{
				local->IsAlias = true;

				// the closure holds the value itself (copied or moved in), or a pointer to it (borrowed)
				if (lambda->MovesCaptures || IsCopyable(type))
				{
					auto address = std::make_shared<ASTUnaryExpression>(OperatorType::Address);
					address->Location = captured;
					address->Operand = access;
					local->Initializer = address;
				}
			}

			block->Children.push_back(local);
		}

		auto returnStatement = std::make_shared<ASTReturn>();
		returnStatement->Location = location;
		returnStatement->ReturnValue = body;
		block->Children.push_back(returnStatement);
		call->CodeBlock = block;
		return call;
	}

	std::shared_ptr<ASTNodeBase> Sema::CallLambdaTemplate(std::shared_ptr<ASTFunctionCall> funcCall, std::shared_ptr<Type> calleeType, std::shared_ptr<ClassType> closureType)
	{
		auto& lambdaTemplate = m_LambdaTemplates.at(closureType.get());
		auto& lambda = lambdaTemplate.Lambda;
		Token location = GetNodeLocation(funcCall->Callee);

		if (funcCall->Arguments.size() != lambda->Parameters.size())
		{
			location.SetData(std::format("lambda’ expects {} argument{}, but {} {} given", lambda->Parameters.size(), lambda->Parameters.size() == 1 ? "" : "s",
										 funcCall->Arguments.size(), funcCall->Arguments.size() == 1 ? "was" : "were"));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_WrongArgumentCount, 1);
			return nullptr;
		}

		// the parameter types are those of the arguments (or what the lambda wrote out)
		std::vector<std::shared_ptr<Type>> parameterTypes = lambdaTemplate.ParameterTypes;
		std::string key;

		for (size_t i = 0; i < parameterTypes.size(); i++)
		{
			// the arguments are already analysed by the time a call reaches here
			if (!parameterTypes[i])
			{
				parameterTypes[i] = funcCall->Arguments[i] ? m_TypeInferEngine.InferTypeFromNode(funcCall->Arguments[i]) : nullptr;

				if (!parameterTypes[i])
				{
					Report(DiagnosticCode_LambdaNeedsTypes, lambda->Parameters[i]->GetName());
					return nullptr;
				}
			}

			key += parameterTypes[i]->GetHash() + ";";
		}

		std::string name = InstantiateLambdaCall(closureType, parameterTypes, location);

		if (name.empty())
			return nullptr;

		std::vector<std::shared_ptr<ASTNodeBase>> arguments(funcCall->Arguments.begin(), funcCall->Arguments.end());
		return CallMethod(funcCall->Callee, calleeType, name, arguments, location);
	}

	std::string Sema::InstantiateLambdaCall(std::shared_ptr<ClassType> closureType, const std::vector<std::shared_ptr<Type>>& argumentTypes, const Token& location)
	{
		// the __call__ of an untyped lambda for these argument types (made once per set of types)
		auto& lambdaTemplate = m_LambdaTemplates.at(closureType.get());
		auto& lambda = lambdaTemplate.Lambda;

		if (argumentTypes.size() != lambda->Parameters.size())
		{
			Token where = location;
			where.SetData(std::format("lambda’ expects {} argument{}, but {} {} given", lambda->Parameters.size(), lambda->Parameters.size() == 1 ? "" : "s",
									  argumentTypes.size(), argumentTypes.size() == 1 ? "was" : "were"));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_WrongArgumentCount, 1);
			return "";
		}

		// a type the lambda wrote out wins over the argument's
		std::vector<std::shared_ptr<Type>> parameterTypes = lambdaTemplate.ParameterTypes;
		std::string key;

		for (size_t i = 0; i < parameterTypes.size(); i++)
		{
			if (!parameterTypes[i])
				parameterTypes[i] = argumentTypes[i];

			if (!parameterTypes[i])
				return "";

			key += parameterTypes[i]->GetHash() + ";";
		}

		if (auto existing = lambdaTemplate.Instances.find(key); existing != lambdaTemplate.Instances.end())
			return existing->second;

		std::string name = std::format("__call_{}__", lambdaTemplate.Instances.size());
		lambdaTemplate.Instances[key] = name;

		Cloner cloner;
		cloner.DestinationModule = m_Module;
		auto call = BuildClosureCall(name, lambda, cloner.Clone(lambda->Body), closureType, lambdaTemplate.Captures, parameterTypes, lambdaTemplate.DeclaredReturn);

		bool success = false;
		DeclareInGlobalScope([&]()
		{
			SemaContext methodContext { .TypeHint = closureType, .GlobalState = false };
			success = DeclareFunction(call, methodContext);

			if (success)
			{
				closureType->MemberFunctions[name] = call->FunctionSymbol;
				DefineFunction(call, methodContext);
			}
		});

		return success ? name : "";
	}

	bool Sema::BindCallable(std::shared_ptr<ASTFunctionTypeExpr> pattern, std::shared_ptr<Type> actual, llvm::ArrayRef<std::string> names,
							std::unordered_map<std::string, std::shared_ptr<Type>>& bindings, const Token& location)
	{
		// `f: function(T) -> U` given a function, a lambda or a closure: its parameter and return types bind T and U
		std::vector<std::shared_ptr<Type>> parameters;
		std::shared_ptr<Type> result;

		if (actual && actual->IsFunction())
		{
			auto function = actual->As<FunctionPointerType>();
			parameters.assign(function->GetParameters().begin(), function->GetParameters().end());
			result = function->GetReturnType();
		}
		else if (auto classType = ClassOf(actual) ? ClassOf(actual)->As<ClassType>() : nullptr)
		{
			std::string callName = "__call__";

			// an untyped lambda: made for the parameter types the pattern gives (T is known by now)
			if (m_LambdaTemplates.contains(classType.get()))
			{
				std::vector<std::shared_ptr<Type>> wanted;

				for (auto& parameter : pattern->Parameters)
				{
					auto variable = std::dynamic_pointer_cast<ASTVariable>(parameter);
					auto bound = variable ? bindings.find(variable->GetName().GetData()) : bindings.end();
					wanted.push_back(bound != bindings.end() ? bound->second : GetTypeFromNode(parameter));
				}

				callName = InstantiateLambdaCall(classType, wanted, location);
			}

			auto call = classType->MemberFunctions.find(callName);

			if (callName.empty() || call == classType->MemberFunctions.end())
				return false;

			auto node = call->second->GetFunctionSymbol().FunctionNode;
			EnsureDefined(node);

			for (size_t i = 1; i < node->Arguments.size(); i++)
				parameters.push_back(node->Arguments[i]->ResolvedType);

			result = node->ReturnTypeVal;
		}
		else
		{
			return false;
		}

		for (size_t i = 0; i < pattern->Parameters.size() && i < parameters.size(); i++)
			BindGenericType(pattern->Parameters[i], parameters[i], names, bindings);

		if (pattern->ReturnType && result)
			BindGenericType(pattern->ReturnType, result, names, bindings);

		return true;
	}

	std::shared_ptr<ASTNodeBase> Sema::CallGenericMethod(std::shared_ptr<ASTFunctionCall> funcCall, std::shared_ptr<ASTBinaryExpression> member, std::shared_ptr<Type> objectType,
														  std::shared_ptr<ClassType> classType, const std::string& name)
	{
		auto& generic = m_GenericMethods.at(classType.get()).at(name);
		auto method = std::dynamic_pointer_cast<ASTFunctionDefinition>(generic.Template->TemplateNode);
		Token location = GetNodeLocation(member->RightSide);

		size_t offset = !method->Arguments.empty() && method->Arguments[0]->GetName().GetData() == "self" ? 1 : 0;
		auto& arguments = funcCall->Arguments;

		if (arguments.size() + offset != method->Arguments.size())
		{
			location.SetData(name);
			Report(DiagnosticCode_WrongArgumentCount, location);
			return nullptr;
		}

		// lambdas without types were left for the parameter they go to: here they are their own type
		for (auto& argument : arguments)
		{
			if (argument && argument->GetType() == ASTNodeType::Lambda)
			{
				argument = Visit(argument, SemaContext { .ValueReq = ValueRequired::RValue, .GlobalState = false });

				if (!argument)
					return nullptr;
			}
		}

		// the method's own type parameters, from the arguments (plain types first, then functions, whose types may need them)
		std::unordered_map<std::string, std::shared_ptr<Type>> bindings;
		const auto& names = generic.Template->GenericTypeNames;

		for (size_t i = 0; i < arguments.size(); i++)
		{
			auto pattern = method->Arguments[i + offset]->TypeResolver;

			if (pattern && pattern->GetType() != ASTNodeType::FunctionTypeExpr)
				BindGenericType(pattern, m_TypeInferEngine.InferTypeFromNode(arguments[i]), names, bindings);
		}

		std::vector<size_t> callableArguments; // passed to `function(...)` parameters: the method takes them as they are

		for (size_t i = 0; i < arguments.size(); i++)
		{
			if (auto pattern = std::dynamic_pointer_cast<ASTFunctionTypeExpr>(method->Arguments[i + offset]->TypeResolver))
			{
				if (BindCallable(pattern, m_TypeInferEngine.InferTypeFromNode(arguments[i]), names, bindings, location))
					callableArguments.push_back(i);
			}
		}

		llvm::SmallVector<Symbol> typeArguments;
		std::string key;

		for (const auto& typeName : names)
		{
			auto bound = bindings.find(typeName);

			if (bound == bindings.end() || !bound->second)
			{
				location.SetData(name);
				Report(DiagnosticCode_CannotInferGeneric, location);
				return nullptr;
			}

			typeArguments.push_back(Symbol::CreateType(bound->second));
			key += bound->second->GetHash() + ";";
		}

		for (size_t i : callableArguments)
			key += "@" + m_TypeInferEngine.InferTypeFromNode(arguments[i])->GetHash();

		std::string instanceName;

		if (auto existing = generic.Instances.find(key); existing != generic.Instances.end())
		{
			instanceName = existing->second;
		}
		else
		{
			instanceName = std::format("{}__{}", name, generic.Instances.size());
			generic.Instances[key] = instanceName;

			Cloner cloner;
			cloner.DestinationModule = m_Module;

			for (size_t i = 0; i < names.size(); i++)
				cloner.SubstitutionMap[names[i]] = typeArguments[i];

			auto instance = cloner.CloneFunction(method);
			instance->SetName(instanceName);

			// a lambda or closure passed to `f: function(T) -> U` is taken as it is (it may hold captured values)
			for (size_t i : callableArguments)
				instance->Arguments[i + offset]->TypeResolver = std::make_shared<ASTTypeLiteral>(m_TypeInferEngine.InferTypeFromNode(arguments[i]));

			// declared and analysed where the class was, like its other methods
			std::vector<SymbolTable> callerScopes = std::exchange(m_ScopeStack, generic.Context.Scopes);
			auto callerLookup = std::exchange(m_LookupModule, generic.Context.LookupModule);
			m_ScopeStack.emplace_back();

			bool declared = DeclareFunction(instance, SemaContext { .TypeHint = classType, .GlobalState = false });

			m_ScopeStack = std::move(callerScopes);
			m_LookupModule = callerLookup;

			if (!declared)
				return nullptr;

			classType->MemberFunctions[instanceName] = instance->FunctionSymbol;
			m_LazyBodies[instance.get()] = generic.Context;
			EnsureDefined(instance);
		}

		EnsureDefined(classType->MemberFunctions.at(instanceName)->GetFunctionSymbol().FunctionNode);
		std::vector<std::shared_ptr<ASTNodeBase>> values(arguments.begin(), arguments.end());
		return CallMethod(member->LeftSide, objectType, instanceName, values, location);
	}

	// a place written without calls (xs[i + 1].name), as text: two places with the same text are the same place
	static std::optional<std::string> PlaceText(const std::shared_ptr<ASTNodeBase>& node)
	{
		if (!node)
			return std::nullopt;

		if (auto variable = std::dynamic_pointer_cast<ASTVariable>(node); variable && !variable->Variable)
			return variable->GetName().GetData();

		if (auto literal = std::dynamic_pointer_cast<ASTNodeLiteral>(node); literal && literal->GetData().IsType(TokenType::Number))
			return literal->GetData().GetData();

		if (auto subscript = std::dynamic_pointer_cast<ASTSubscript>(node))
		{
			auto text = PlaceText(subscript->Target);

			if (!text || subscript->SubscriptArgs.empty())
				return std::nullopt;

			*text += "[";

			for (auto& argument : subscript->SubscriptArgs)
			{
				auto index = PlaceText(argument);

				if (!index)
					return std::nullopt;

				*text += *index + ",";
			}

			return *text + "]";
		}

		if (auto binary = std::dynamic_pointer_cast<ASTBinaryExpression>(node))
		{
			auto op = binary->GetExpression();

			if (op != OperatorType::Dot && op != OperatorType::Add && op != OperatorType::Sub && op != OperatorType::Mul)
				return std::nullopt;

			auto left = PlaceText(binary->LeftSide);
			auto right = PlaceText(binary->RightSide);

			if (!left || !right)
				return std::nullopt;

			return std::format("({}{}{})", *left, (int)op, *right);
		}

		return std::nullopt;
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitSwap(std::shared_ptr<ASTDestructure> destructure, SemaContext context, bool& isSwap)
	{
		// xs[i], xs[j] = xs[j], xs[i]: the same places on both sides, so the values only change places. Each is taken
		// out as it is (no copy), then put into its new place (nothing to clean up: its old value was taken out)
		auto tuple = std::dynamic_pointer_cast<ASTTupleExpr>(destructure->Value);

		if (destructure->IsDeclaration || !tuple || tuple->Values.size() != destructure->Targets.size() || tuple->Values.size() < 2)
			return nullptr;

		std::vector<std::string> targets, values;

		for (size_t i = 0; i < tuple->Values.size(); i++)
		{
			auto target = PlaceText(destructure->Targets[i]);
			auto value = PlaceText(tuple->Values[i]);

			if (!target || !value)
				return nullptr;

			targets.push_back(*target);
			values.push_back(*value);
		}

		std::sort(targets.begin(), targets.end());
		std::sort(values.begin(), values.end());

		if (targets != values || std::adjacent_find(targets.begin(), targets.end()) != targets.end())
			return nullptr;

		isSwap = true;

		static size_t s_Counter = 0;
		Token location = destructure->Location;
		auto token = [&](const std::string& text) { return Token(TokenType::Identifier, text, location.GetSourceFile(), location.LineNumber, location.ColumnNumber); };

		auto call = [&](const std::string& name, std::vector<std::shared_ptr<ASTNodeBase>> arguments)
		{
			auto node = std::make_shared<ASTFunctionCall>();
			node->Location = location;
			node->Callee = std::make_shared<ASTVariable>(token(name));
			node->Arguments.append(arguments.begin(), arguments.end());
			return node;
		};

		auto address = [&](std::shared_ptr<ASTNodeBase> place)
		{
			auto node = std::make_shared<ASTUnaryExpression>(OperatorType::Address);
			node->Location = location;
			node->Operand = place;
			return node;
		};

		std::vector<std::shared_ptr<ASTNodeBase>> statements;
		std::vector<std::string> names;

		for (auto& value : tuple->Values)
		{
			names.push_back(std::format("__swap_{}", s_Counter++));
			auto held = std::make_shared<ASTVariableDeclaration>(token(names.back()));
			held->Location = location;
			held->Initializer = call("take", { address(value) });
			statements.push_back(held);
		}

		for (size_t i = 0; i < destructure->Targets.size(); i++)
			statements.push_back(call("place", { address(destructure->Targets[i]), std::make_shared<ASTVariable>(token(names[i])) }));

		// the pointers made here are used right away: they do not keep the places from being moved out later
		auto neverMove = m_Copies.NeverMove;
		auto sequence = std::make_shared<ASTSequence>();
		sequence->Location = location;

		for (auto& statement : statements)
		{
			auto visited = Visit(statement, context);

			if (!visited)
				return nullptr;

			sequence->Children.push_back(visited);
		}

		m_Copies.NeverMove = std::move(neverMove);
		return sequence;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTDestructure> destructure, SemaContext context)
	{
		bool isSwap = false;
		auto swap = VisitSwap(destructure, context, isSwap);

		if (isSwap)
			return swap;

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
		// a defer runs at the end of the block, after everything below it: what it uses is never moved away
		bool wasInDefer = std::exchange(m_Copies.InDefer, true);
		deferNode->Expr = Visit(deferNode->Expr, context);
		m_Copies.InDefer = wasInDefer;

		return deferNode->Expr ? deferNode : nullptr;
	}

	std::optional<int64_t> Sema::KnownConstant(Symbol* symbol, std::shared_ptr<Type>* type)
	{
		if (auto it = m_ConstantValues.find(symbol); it != m_ConstantValues.end())
			return it->second;

		// a const of an imported file (`import "sizes"` or `as m`: m.N)
		for (const auto& [path, unit] : m_CompilationUnits)
		{
			if (!unit.CompilationModule || unit.CompilationModule == m_Module)
				continue;

			if (auto it = unit.CompilationModule->ConstantValues.find(symbol); it != unit.CompilationModule->ConstantValues.end())
			{
				if (type)
					*type = it->second.second;

				return it->second.first;
			}
		}

		return std::nullopt;
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

				if (token.IsType(TokenType::Keyword) && token.GetData() == "true")  return 1;
				if (token.IsType(TokenType::Keyword) && token.GetData() == "false") return 0;

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
				return var->Variable ? KnownConstant(var->Variable.get()) : std::nullopt;
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
					case OperatorType::LeftShift:	return *rhs < 0 || *rhs > 63 ? std::nullopt : std::optional((int64_t)((uint64_t)*lhs << *rhs));
					case OperatorType::RightShift:	return *rhs < 0 || *rhs > 63 ? std::nullopt : std::optional(*lhs >> *rhs);
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

		if (!ternaryExpr->Condition || !ternaryExpr->Truthy || !ternaryExpr->Falsy)
			return nullptr;

		// both branches meet in one type: the wider of the two (a literal adapts to the other side)
		auto truthyType = m_TypeInferEngine.InferTypeFromNode(ternaryExpr->Truthy);
		auto falsyType = m_TypeInferEngine.InferTypeFromNode(ternaryExpr->Falsy);

		// when found use index otherwise none: an optional of the other side (or the optional the value goes into)
		bool truthyNone = truthyType && truthyType->GetHash() == "none";
		bool falsyNone = falsyType && falsyType->GetHash() == "none";

		if (truthyType && falsyType && truthyNone != falsyNone)
		{
			auto valueType = truthyNone ? falsyType : truthyType;
			bool expectsOptional = context.ExpectedType && context.ExpectedType->IsClass() && context.ExpectedType->As<ClassType>()->IsOptional;
			auto optional = expectsOptional ? context.ExpectedType : (ClassOf(valueType) && ClassOf(valueType)->As<ClassType>()->IsOptional ? valueType : GetOptionalType(valueType));

			if (optional)
			{
				ternaryExpr->Truthy = Coerce(ternaryExpr->Truthy, optional);
				ternaryExpr->Falsy = Coerce(ternaryExpr->Falsy, optional);
				return ternaryExpr;
			}
		}

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

		// when c use s otherwise String("x"): one side is new and the other is not. Each side then gives a value of
		// its own (s is copied, or moved at its last use), so the result is new either way and has one owner
		auto resultType = m_TypeInferEngine.InferTypeFromNode(ternaryExpr);

		if (IsOwning(resultType) && IsFreshValue(ternaryExpr->Truthy) != IsFreshValue(ternaryExpr->Falsy))
		{
			ternaryExpr->Truthy = TakeOwnership(ternaryExpr->Truthy, resultType);
			ternaryExpr->Falsy = TakeOwnership(ternaryExpr->Falsy, resultType);
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

		if (!castExpr->Object || !castExpr->TargetType)
			return castExpr;

		auto sourceType = m_TypeInferEngine.InferTypeFromNode(castExpr->Object);

		// text as *int8, pointer as str: the same conversions as when passing them (see Coerce)
		if (sourceType && sourceType != castExpr->TargetType && (sourceType->GetHash() == "str" || castExpr->TargetType->GetHash() == "str"))
		{
			auto converted = Coerce(AsValue(castExpr->Object), castExpr->TargetType);

			if (converted && m_TypeInferEngine.InferTypeFromNode(converted) == castExpr->TargetType)
				return converted;
		}

		// value as Number: put it in the variant
		if (castExpr->TargetType->IsClass() && castExpr->TargetType->As<ClassType>()->IsTypeVariant && sourceType != castExpr->TargetType)
			return Coerce(AsValue(castExpr->Object), castExpr->TargetType);

		// number as int: read it as that type, stopping the program if it holds another one
		if (sourceType && sourceType->IsClass() && sourceType->As<ClassType>()->IsTypeVariant && sourceType != castExpr->TargetType)
		{
			auto variant = sourceType->As<ClassType>();
			auto index = FindTypeCase(variant, castExpr->TargetType);

			if (!index)
			{
				std::string names;
				for (auto& c : variant->Cases)
					names += (names.empty() ? "" : ", ") + c.Name;

				Token location = GetNodeLocation(castExpr->Object);
				location.SetData(std::format("{}’ never holds a {}; it holds one of: {}", variant->GetHash(), GetDisplayName(castExpr->TargetType), names));
				m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_NotInVariant, 1);
				return nullptr;
			}

			auto unwrap = std::make_shared<ASTOptionalUnwrap>();
			unwrap->Location = castExpr->Location.GetData().empty() ? GetNodeLocation(castExpr->Object) : castExpr->Location;
			unwrap->Subject = AsValue(castExpr->Object);
			unwrap->OptionalTy = sourceType;
			unwrap->CaseIndex = (int64_t)*index;
			return unwrap;
		}

		// K.A as String, 5 as str: there is no such conversion
		if (sourceType && sourceType != castExpr->TargetType && !CastAllowed(sourceType, castExpr->TargetType))
		{
			Token location = castExpr->Location.GetData().empty() ? GetNodeLocation(castExpr->Object) : castExpr->Location;
			size_t width = std::max<size_t>(location.GetData().size(), 1);
			location.SetData(ConversionAdvice(sourceType, castExpr->TargetType));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_NoConversion, width);
			return nullptr;
		}

		return castExpr;
	}

	bool Sema::CastAllowed(const std::shared_ptr<Type>& from, const std::shared_ptr<Type>& to)
	{
		// what `as` can do: numbers (and bools, chars, enums) to numbers, pointers to pointers or integers and back
		if (!from || !to)
			return false;

		if (from == to || from->GetHash() == to->GetHash())
			return true;

		auto scalar = [](const std::shared_ptr<Type>& type) { return type->IsIntegral() || type->IsFloatingPoint() || type->IsEnum(); };

		if (scalar(from) && scalar(to))
			return true;

		if ((from->IsPointer() && (to->IsPointer() || to->IsIntegral())) || (to->IsPointer() && from->IsIntegral()))
			return true;

		return IsImplicitlyConvertible(from, to, false);
	}

	std::string Sema::ConversionAdvice(const std::shared_ptr<Type>& from, const std::shared_ptr<Type>& to)
	{
		std::string fromName = GetDisplayName(from), toName = GetDisplayName(to);
		bool toText = to->GetHash() == "String" || to->GetHash() == "str";

		if (toText && (from->IsIntegral() || from->IsFloatingPoint()) && !from->IsEnum())
			return std::format("A {} is not text: use from_int(x) or from_float(x) to make a String of it", fromName);

		// m[a] with a: str on a Map[String, V], f(a) taking a String...
		if (to->GetHash() == "String" && from->GetHash() == "str")
			return "A String is expected here: write String(x) to make one from a str (it allocates, so it is written out), e.g. m[String(key)] or String(key) in m";

		if (toText && from->IsEnum())
			return std::format("A {} is not text: switch over it and give each case its text", fromName);

		if (to->IsPointer() && to->As<PointerType>()->GetBaseType() == from)
			return std::format("A {} is expected here, not a {}: pass its address with &", toName, fromName);

		if (from->IsPointer() && from->As<PointerType>()->GetBaseType() == to)
			return std::format("A {} is expected here, not a pointer to one: use *pointer for the value", toName);

		return std::format("There is no conversion from ‘{}’ to ‘{}’, not even with ‘as’", fromName, toName);
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTSizeofExpr> sizeofExpr, SemaContext context)
	{
		sizeofExpr->Object = Visit(sizeofExpr->Object, context);

		if (!sizeofExpr->Object)
			return nullptr;

		// sizeof a type (?int64, [3; int8], *T) or of a value's type: the bytes one takes in memory, padding
		// included (the distance between two of them in an array)
		auto type = GetTypeFromNode(sizeofExpr->Object);

		if (!type)
			type = m_TypeInferEngine.InferTypeFromNode(sizeofExpr->Object);

		if (!type || type->GetHash() == "void")
		{
			Report(DiagnosticCode_ExpectedType, GetNodeLocation(sizeofExpr->Object));
			return nullptr;
		}

		sizeofExpr->Size = type->GetSizeInBytes(*m_Module->GetModule());
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

		// p is Shape.Circle with p a pointer: the value it points at
		if (objectType && objectType->IsPointer() && objectType->As<PointerType>()->GetBaseType() && 
			objectType->As<PointerType>()->GetBaseType()->IsClass() && objectType->As<PointerType>()->GetBaseType()->As<ClassType>()->IsVariant)
		{
			auto deref = std::make_shared<ASTUnaryExpression>(OperatorType::Dereference);
			deref->Location = isExpr->Object->Location;
			deref->Operand = isExpr->Object;
			isExpr->Object = deref;
			objectType = objectType->As<PointerType>()->GetBaseType();
		}

		if (objectType && objectType->IsClass() && objectType->As<ClassType>()->IsVariant)
		{
			auto classType = objectType->As<ClassType>();
			std::string caseName;

			if (classType->IsTypeVariant)
			{
				// number is int
				auto typeNode = Visit(isExpr->TypeNode, SemaContext { .ValueReq = ValueRequired::Any });
				auto type = typeNode ? GetTypeFromNode(typeNode) : nullptr;
				auto index = FindTypeCase(classType, type);
				caseName = index ? classType->Cases[*index].Name : (type ? GetDisplayName(type) : "?");
			}
			else if (auto literal = std::dynamic_pointer_cast<ASTNodeLiteral>(isExpr->TypeNode); literal && literal->GetData().GetData() == "none")
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

			// find(1) is not none: a new value is only looked at, it is kept (and cleaned up) at the end of the block
			if (IsOwning(objectType) && IsFreshValue(isExpr->Object))
			{
				auto load = std::make_shared<ASTLoad>();
				load->Location = isExpr->Object->Location;
				load->Operand = AddressOf(isExpr->Object);
				isExpr->Object = load;
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

				// Neg(child: Expr): an Expr inside an Expr, without end
				if (ContainsByValue(fieldType, classType))
				{
					Token location = field->TypeResolver ? GetNodeLocation(field->TypeResolver) : field->GetName();
					size_t width = location.GetData().size();
					location.SetData(GetDisplayName(classType));
					Report(DiagnosticCode_InfiniteType, location, width);
					m_BrokenClasses.insert(classType.get()); // its uses would only repeat the problem
					return false;
				}

				variantCase.Fields.push_back({ field->GetName().GetData(), fieldType });

				// variant Number: int, float64 — the case is named after its type
				if (enumNode->IsTypeVariant)
					variantCase.Name = GetDisplayName(fieldType);
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
		classType->IsTypeVariant = enumNode->IsTypeVariant;

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

		// switching on a variable (or *self) looks at it in place; a computed value is kept (and cleaned up) here
		if (auto load = std::dynamic_pointer_cast<ASTLoad>(switchNode->Value); load && IsStorageNode(load->Operand))
		{
			subjectDecl->Initializer = load->Operand;
			subjectDecl->IsAlias = true;
		}
		else if (auto deref = std::dynamic_pointer_cast<ASTUnaryExpression>(switchNode->Value); deref && deref->GetOperatorType() == OperatorType::Dereference)
		{
			deref->IsStorage = true;
			subjectDecl->IsAlias = true;
		}

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

		auto branches = BeginBranches();

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

				if (classType->IsTypeVariant)
				{
					// case int(x): the pattern names a type
					auto typeNode = Visit(namePart, SemaContext { .ValueReq = ValueRequired::Any });
					auto type = typeNode ? GetTypeFromNode(typeNode) : nullptr;
					auto typeIndex = FindTypeCase(classType, type);
					caseName = typeIndex ? classType->Cases[*typeIndex].Name : (type ? GetDisplayName(type) : "?");
				}
				else if (auto literal = std::dynamic_pointer_cast<ASTNodeLiteral>(namePart); literal && literal->GetData().GetData() == "none")
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

					// an object in the case is named in place (like a for loop over objects), numbers are copied
					if (fields[i].second->IsClass())
					{
						field->AsAddress = true;
						declaration->IsAlias = true;
					}
					declarations.push_back(declaration);
				}

				switchCase.CodeBlock->Children.insert(switchCase.CodeBlock->Children.begin(), declarations.begin(), declarations.end());
			}

			BeginBranch(branches);
			Visit(switchCase.CodeBlock, context);
			EndBranch(branches);
		}

		if (switchNode->DefaultCaseCodeBlock)
		{
			BeginBranch(branches);
			Visit(switchNode->DefaultCaseCodeBlock, context);
			EndBranch(branches);
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

		EndBranches(branches, !switchNode->DefaultCaseCodeBlock && !switchNode->IsExhaustive);

		auto sequence = std::make_shared<ASTSequence>();
		sequence->Location = location;
		sequence->Children = { subjectDecl, switchNode };
		return sequence;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTLoopControlFlow> controlFlow, SemaContext context)
	{
		if (!context.InLoop)
			Report(DiagnosticCode_LoopControlOutsideLoop, controlFlow->GetToken());

		if (!m_LoopMoves.empty() && !m_Unreachable)
		{
			auto& loop = m_LoopMoves.back();
			auto& into = controlFlow->GetToken().GetData() == "break" ? loop.AtBreak : loop.AtContinue;
			into.insert(m_Moved.begin(), m_Moved.end());
		}

		m_Unreachable = true;
		return controlFlow;
	}

	std::shared_ptr<ASTNodeBase> Sema::Visit(std::shared_ptr<ASTStructExpr> structExpr, SemaContext context)
	{	
		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;
		valueContext.CallsiteArgs.clear();

		// P { 1, y = 2 }: `name = value` sets that field (after the positional values, like keyword arguments)
		{
			std::vector<std::shared_ptr<ASTNodeBase>> positional;

			for (auto& value : structExpr->Values)
			{
				auto assignment = std::dynamic_pointer_cast<ASTAssignmentOperator>(value);
				auto name = assignment ? std::dynamic_pointer_cast<ASTVariable>(assignment->Storage) : nullptr;

				if (assignment && name && assignment->GetAssignType() == AssignmentOperatorType::Normal)
				{
					structExpr->NamedValues.push_back({ name->GetName(), assignment->Value });
					continue;
				}

				if (!structExpr->NamedValues.empty())
				{
					Report(DiagnosticCode_PositionalAfterKeyword, GetNodeLocation(value));
					return nullptr;
				}

				positional.push_back(value);
			}

			structExpr->Values = positional;
		}

		for (auto& value : structExpr->Values)
		{
			// lambdas wait for the field type (see CompleteStructValues)
			if (value->GetType() != ASTNodeType::Lambda)
				value = Visit(value, valueContext);

			if (!value)
				return nullptr;
		}

		for (auto& [name, value] : structExpr->NamedValues)
		{
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

		// Node(1, none, none) when Node itself was already reported (E104): no second error about its fields
		if (type && m_BrokenClasses.contains(type.get()))
			return nullptr;

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

		if (!structExpr->NamedValues.empty() && structExpr->Values.size() <= members.size())
		{
			// named fields go in their place; fields neither given nor named keep their default (below)
			std::vector<std::shared_ptr<ASTNodeBase>> placed(structExpr->Values.begin(), structExpr->Values.end());
			placed.resize(members.size());
			size_t hidden = classType->HasVTable ? 1 : 0;

			for (auto& [name, value] : structExpr->NamedValues)
			{
				auto index = classType->GetMemberValueIndex(name.GetData());

				if (!index || *index < hidden || placed[*index])
				{
					Token where = name;
					where.SetData(std::format("{}’ is not a field of ‘{}’ (or is given twice", name.GetData(), GetDisplayName(classType)));
					m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, where, DiagnosticCode_UnknownKeyword, name.GetData().size());
					return nullptr;
				}

				placed[*index] = value;
			}

			for (size_t i = 0; i < placed.size(); i++)
			{
				if (!placed[i])
				{
					auto defaultValue = i < classType->MemberDefaults.size() ? classType->MemberDefaults[i] : nullptr;
					placed[i] = defaultValue ? defaultValue : std::make_shared<ASTZero>(classType->GetMemberValueByIndex(i).value()->GetType());
				}
			}

			structExpr->Values = placed;
			structExpr->NamedValues.clear();
		}

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

		// Outcome(5) with `variant Outcome: int, String`: the value, put in the variant
		if (classType->IsTypeVariant && funcCall->Arguments.size() == 1 && funcCall->KeywordArguments.empty())
		{
			SemaContext valueContext { .ValueReq = ValueRequired::RValue, .GlobalState = false };
			auto value = Visit(funcCall->Arguments[0], valueContext);
			return value ? Coerce(value, classType) : nullptr;
		}

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

				// the positional values start after the hidden vtable field (indexes below count it)
				if (classType->HasVTable)
					structExpr->Values.insert(structExpr->Values.begin(), classType->MemberDefaults[0]);

				if (structExpr->Values.size() > members.size())
					return CompleteStructValues(structExpr); // reports the extra values

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

		// named after the class in messages: P(...), not __init__
		auto callee = std::make_shared<ASTVariable>(Token(TokenType::Identifier, target->GetName().GetData(), target->GetName().GetSourceFile(), target->GetName().LineNumber, target->GetName().ColumnNumber));
		callee->Variable = init->second;

		construct->InitCall = std::make_shared<ASTFunctionCall>();
		construct->InitCall->Location = funcCall->Location;
		construct->InitCall->IsConstructor = true;
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
		if (auto array = std::dynamic_pointer_cast<ASTArrayType>(pattern); array && array->SizeNode && actual->IsArray())
			return BindGenericType(array->TypeNode, actual->As<ArrayType>()->GetBaseType(), names, bindings);

		// []T matched against a slice (or an array, which becomes one)
		if (auto array = std::dynamic_pointer_cast<ASTArrayType>(pattern); array && !array->SizeNode)
		{
			if (auto slice = std::dynamic_pointer_cast<SliceType>(actual))
				return BindGenericType(array->TypeNode, slice->GetBaseType(), names, bindings);

			if (actual->IsArray())
				return BindGenericType(array->TypeNode, actual->As<ArrayType>()->GetBaseType(), names, bindings);

			// a List[String] (anything with operator slice) passed as a []T: T is what its slices hold
			if (actual->IsClass())
			{
				auto slicer = actual->As<ClassType>()->MemberFunctions.find("__slice__");
				auto node = slicer != actual->As<ClassType>()->MemberFunctions.end() ? slicer->second->GetFunctionSymbol().FunctionNode : nullptr;

				if (auto view = node ? std::dynamic_pointer_cast<SliceType>(node->ReturnTypeVal) : nullptr)
					return BindGenericType(array->TypeNode, view->GetBaseType(), names, bindings);
			}
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
		auto callables = m_CallablePatterns.find(generic.get());
		auto callableOf = [&](const std::shared_ptr<ASTNodeBase>& pattern) -> std::shared_ptr<ASTFunctionTypeExpr>
		{
			auto variable = std::dynamic_pointer_cast<ASTVariable>(pattern);

			if (!variable || callables == m_CallablePatterns.end())
				return nullptr;

			auto found = callables->second.find(variable->GetName().GetData());
			return found != callables->second.end() ? found->second : nullptr;
		};

		// plain parameters first, then functions (an untyped lambda is made for the T the others gave)
		for (size_t i = 0; i < values.size() && i < patterns.size(); i++)
		{
			if (values[i] && !callableOf(patterns[i]))
				BindGenericType(patterns[i], m_TypeInferEngine.InferTypeFromNode(values[i]), generic->GenericTypeNames, bindings);
		}

		for (size_t i = 0; i < values.size() && i < patterns.size(); i++)
		{
			if (auto callable = callableOf(patterns[i]); callable && values[i])
			{
				auto actual = m_TypeInferEngine.InferTypeFromNode(values[i]);
				BindCallable(callable, actual, generic->GenericTypeNames, bindings, GetNodeLocation(values[i]));
				bindings[std::dynamic_pointer_cast<ASTVariable>(patterns[i])->GetName().GetData()] = actual;
			}
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

		// function apply_to[T, U](x: T, f: function(T) -> U): f gets a type parameter of its own (whatever is passed:
		// a function, a lambda, a closure), and T and U come from that value's signature (see InstantiateFromValues)
		if (auto function = std::dynamic_pointer_cast<ASTFunctionDefinition>(generic->TemplateNode); function && !m_CallablePatterns.contains(generic.get()))
		{
			auto& patterns = m_CallablePatterns[generic.get()];

			for (size_t i = 0; i < function->Arguments.size(); i++)
			{
				auto& argument = function->Arguments[i];
				auto pattern = argument ? std::dynamic_pointer_cast<ASTFunctionTypeExpr>(argument->TypeResolver) : nullptr;

				if (!pattern)
					continue;

				std::string name = std::format("__callable_{}", i);
				patterns[name] = pattern;
				argument->TypeResolver = std::make_shared<ASTVariable>(Token(TokenType::Identifier, name, pattern->Location.GetSourceFile(), pattern->Location.LineNumber, pattern->Location.ColumnNumber));
				generic->GenericTypeNames.push_back(name);
				generic->Constraints.resize(generic->GenericTypeNames.size());
			}
		}

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

		// m.Box[int]: a generic class of a module imported `as m`
		if (auto access = std::dynamic_pointer_cast<ASTBinaryExpression>(subscript->Target); access && access->GetExpression() == OperatorType::Dot)
		{
			if (auto member = std::dynamic_pointer_cast<ASTVariable>(ModuleMember(access)); member && member->Variable->Kind == SymbolKind::GenericTemplate)
				subscript->Target = member;
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

			// s[i] on a slice: the element itself (checked against the length when checks are on)
			if (auto slice = std::dynamic_pointer_cast<SliceType>(targetType); slice && subscript->SubscriptArgs.size() == 1 && subscript->SubscriptArgs[0])
			{
				auto int64Type = m_Module->Lookup("int64").value()->GetType();
				auto address = SliceIntrinsic("slice_at", m_Module->GetTypeRegistry()->GetPointerTo(slice->GetBaseType()),
											  { AsValue(subscript->Target), Coerce(subscript->SubscriptArgs[0], int64Type) }, GetNodeLocation(subscript->SubscriptArgs[0]));

				auto element = std::make_shared<ASTUnaryExpression>(OperatorType::Dereference);
				element->Location = GetNodeLocation(subscript->Target);
				element->Operand = address;
				element->IsStorage = context.ValueReq == ValueRequired::LValue;
				element->IsElement = true;
				return element;
			}

			// a[i] with a: *[N; T]: the array a points at, like a *List[T] uses the list's operator get
			if (targetType->IsPointer() && targetType->As<PointerType>()->GetBaseType() && targetType->As<PointerType>()->GetBaseType()->IsArray())
			{
				auto array = std::make_shared<ASTUnaryExpression>(OperatorType::Dereference);
				array->Location = GetNodeLocation(subscript->Target);
				array->Operand = AsValue(subscript->Target);
				array->IsStorage = true;
				subscript->Target = array;
				targetType = targetType->As<PointerType>()->GetBaseType();
			}

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

				// operator get returning *T: obj[i] is the element itself (read it, assign to it, change its fields)
				auto getter = var->Variable->GetFunctionSymbol().FunctionNode;
				EnsureDefined(getter);

				if (getter && getter->ReturnTypeVal && getter->ReturnTypeVal->IsPointer())
				{
					auto call = CheckCall(funcCall);

					if (!call)
						return nullptr;

					DispatchOnObject(call, clsType);

					auto element = std::make_shared<ASTUnaryExpression>(OperatorType::Dereference);
					element->Location = funcCall->Location;
					element->Operand = call;
					element->IsStorage = context.ValueReq == ValueRequired::LValue;
					element->IsElement = true;
					return element;
				}

				auto checked = CheckCall(funcCall);
				DispatchOnObject(checked, clsType);
				return checked;
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

				// split(s)[0]: the new tuple is kept (and cleaned up) at the end of the block, the element is read from it
				bool kept = !IsStorageNode(subscript->Target) && IsFreshValue(subscript->Target) && IsOwning(tupleType);

				if (kept)
					subscript->Target = AddressOf(subscript->Target);

				auto get = std::make_shared<ASTTupleGet>();
				get->Location = GetNodeLocation(subscript->Target);
				get->Tuple = subscript->Target;
				get->TupleIsStorage = kept || IsStorageNode(subscript->Target);
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

			// m.Box[int]: not a name here, the template was found through an import alias
			if (!genericSym && var->Variable && var->Variable->Kind == SymbolKind::GenericTemplate)
			{
				genericSym = var->Variable;
				scopeIndex = 0;
			}

			if (!genericSym)
			{
				Report(DiagnosticCode_UndeclaredIdentifier, var->GetName());
				return nullptr;
			}
			
			llvm::SmallVector<Symbol> substitutedArgs;

			if (std::find(subscript->SubscriptArgs.begin(), subscript->SubscriptArgs.end(), nullptr) != subscript->SubscriptArgs.end())
				return nullptr; // an argument was already reported

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
		// []T
		if (!arrayType->SizeNode)
		{
			arrayType->TypeNode = Visit(arrayType->TypeNode, context);
			auto baseTy = arrayType->TypeNode ? GetTypeFromNode(arrayType->TypeNode) : nullptr;

			if (!baseTy)
			{
				if (arrayType->TypeNode) // (else already reported: an unknown name)
					Report(DiagnosticCode_ExpectedType, GetNodeLocation(arrayType->TypeNode));

				return nullptr;
			}

			arrayType->GeneratedArrayType = m_Module->GetTypeRegistry()->GetSliceOf(baseTy);
			return arrayType;
		}

		auto writtenSize = arrayType->SizeNode;
		arrayType->SizeNode = Visit(arrayType->SizeNode, context);
		arrayType->TypeNode = Visit(arrayType->TypeNode, context);

		if (!arrayType->SizeNode)
			return nullptr; // already reported

		std::shared_ptr<Type> baseTy = GetTypeFromNode(arrayType->TypeNode);

		if (!baseTy)
		{
			if (arrayType->TypeNode) // (else already reported: an unknown name)
				Report(DiagnosticCode_ExpectedType, GetNodeLocation(arrayType->TypeNode));

			return nullptr;
		}

		int64_t size = EvaluateInteger(arrayType->SizeNode).value_or(0);
			
		if (size <= 0)
		{
			Token where = GetNodeLocation(writtenSize);
			Report(DiagnosticCode_InvalidArraySize, where.GetSourceFile().empty() ? arrayType->Location : where);
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

		// [a, String("y")]: the array owns its items, each one is a value of its own (a is copied, or moved)
		if (IsOwning(targetBaseType))
		{
			for (auto& value : listExpr->Values)
				value = TakeOwnership(value, targetBaseType);
		}

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

			if (srcBits == 1)
				return true; // bool -> int

			// int8 -1 as uint8 is 255: a change of sign is written out with `as` (a literal that fits still adapts)
			if (srcBits == dstBits)
				return from->IsSigned() == to->IsSigned();

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

		// a literal above the int64 range (0xFFFFFFFFFFFFFFFF): its bits only fit 64 unsigned bits
		if (!source->IsSigned() && source->Get()->getIntegerBitWidth() == 64 && *value < 0)
			return !target->IsSigned() && bits == 64;

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

	// the case of a type variant that holds exactly `type`
	static std::optional<size_t> FindTypeCase(const std::shared_ptr<ClassType>& variant, const std::shared_ptr<Type>& type)
	{
		for (size_t i = 0; type && i < variant->Cases.size(); i++)
		{
			auto& caseType = variant->Cases[i].Fields[0].second;

			if (caseType == type || caseType->GetHash() == type->GetHash())
				return i;
		}

		return std::nullopt;
	}

	std::shared_ptr<ASTNodeBase> Sema::WrittenTemporary(std::shared_ptr<ASTNodeBase> storage)
	{
		// follow a.b.c / a[i].b down to what is written into: a variable or pointer is real storage,
		// a value made on the spot (a call's result, a copy from a get) is not
		auto node = storage;

		while (node)
		{
			if (auto binary = std::dynamic_pointer_cast<ASTBinaryExpression>(node); binary && binary->GetExpression() == OperatorType::Dot)
				node = binary->LeftSide;
			else if (auto subscript = std::dynamic_pointer_cast<ASTSubscript>(node); subscript && subscript->Meaning == SubscriptSemantic::ArrayIndex)
				node = subscript->Target;
			else if (auto load = std::dynamic_pointer_cast<ASTLoad>(node))
				node = load->Operand;
			else
				break;
		}

		if (!node || node == storage)
			return nullptr;

		switch (node->GetType())
		{
			case ASTNodeType::Temporary:
			{
				auto type = std::dynamic_pointer_cast<ASTTemporary>(node)->ValueType;
				return type && !type->IsPointer() ? node : nullptr;
			}
			case ASTNodeType::FunctionCall:
			case ASTNodeType::Construct:
			case ASTNodeType::StructExpr:
			{
				auto type = m_TypeInferEngine.InferTypeFromNode(node);
				return type && !type->IsPointer() ? node : nullptr;
			}
			default:
				return nullptr;
		}
	}

	static bool IsFreshValue(const std::shared_ptr<ASTNodeBase>& node)
	{
		switch (node->GetType())
		{
			case ASTNodeType::FunctionCall:
			case ASTNodeType::Construct:
			case ASTNodeType::StructExpr:
			case ASTNodeType::VariantConstruct:
			case ASTNodeType::UnionConstruct:
			case ASTNodeType::TupleExpr:
			case ASTNodeType::ListExpr:
			case ASTNodeType::Zero:
			case ASTNodeType::Move:
			case ASTNodeType::Copy:
			case ASTNodeType::ConstantValue:
			case ASTNodeType::Literal:
			case ASTNodeType::Await:
				return true;
			case ASTNodeType::Intrinsic:
				return std::dynamic_pointer_cast<ASTIntrinsic>(node)->Name == "take" || std::dynamic_pointer_cast<ASTIntrinsic>(node)->Name == "clone" ||
					   std::dynamic_pointer_cast<ASTIntrinsic>(node)->Name == "task_run";
			case ASTNodeType::OptionalUnwrap:
				return IsFreshValue(std::dynamic_pointer_cast<ASTOptionalUnwrap>(node)->Subject);
			case ASTNodeType::TernaryExpression:
			{
				auto ternary = std::dynamic_pointer_cast<ASTTernaryExpression>(node);
				return IsFreshValue(ternary->Truthy) && IsFreshValue(ternary->Falsy);
			}
			default:
				return false;
		}
	}

	bool Sema::ContainsByValue(const std::shared_ptr<Type>& type, const std::shared_ptr<Type>& target)
	{
		// whether `type` holds a `target` inside itself (not behind a pointer or a List): then target would contain
		// itself, without end
		if (!type)
			return false;

		if (type == target)
			return true;

		if (auto array = std::dynamic_pointer_cast<ArrayType>(type))
			return ContainsByValue(array->GetBaseType(), target);

		if (auto tuple = std::dynamic_pointer_cast<TupleType>(type))
		{
			for (auto& element : tuple->GetElements())
				if (ContainsByValue(element, target))
					return true;
			return false;
		}

		auto classType = std::dynamic_pointer_cast<ClassType>(type);

		if (!classType)
			return false;

		static thread_local std::unordered_set<Type*> s_Visiting;

		if (!s_Visiting.insert(type.get()).second)
			return false;

		struct Done { Type* T; ~Done() { s_Visiting.erase(T); } } done { type.get() };

		for (auto& variantCase : classType->Cases)
			for (auto& [name, field] : variantCase.Fields)
				if (ContainsByValue(field, target))
					return true;

		for (const auto& [name, field] : classType->GetMemberValues())
			if (ContainsByValue(field, target))
				return true;

		return false;
	}

	void Sema::AdaptLiterals(std::shared_ptr<ASTBinaryExpression> expr, const SemaContext& context)
	{
		// a number written out takes the type of what it is combined with, when it fits: `c >> 16` with c a uint32
		// stays unsigned, `b + 32` with b an int8 stays int8, `f * 2.0` with f a float32 stays float32.
		// Two literals take the type the result goes into (`let big: int64 = 1 << 40`).
		auto numeric = [](const std::shared_ptr<Type>& type) { return type && (type->IsIntegral() || type->IsFloatingPoint()) && !type->IsEnum() && !type->Get()->isIntegerTy(1); };
		auto fits = [&](const std::shared_ptr<ASTNodeBase>& literal, const std::shared_ptr<Type>& target)
		{
			auto type = m_TypeInferEngine.InferTypeFromNode(literal);

			if (!numeric(type) || !numeric(target) || type == target)
				return false;

			if (target->IsIntegral())
				return type->IsIntegral() && IsConstantThatFits(literal, target);

			return IsImplicitlyConvertible(type, target, true);
		};

		bool leftLiteral = IsNumericLiteral(expr->LeftSide), rightLiteral = IsNumericLiteral(expr->RightSide);
		auto leftType = m_TypeInferEngine.InferTypeFromNode(expr->LeftSide);
		auto rightType = m_TypeInferEngine.InferTypeFromNode(expr->RightSide);

		// a shift amount does not decide the type of what is shifted
		bool shift = expr->GetExpression() == OperatorType::LeftShift || expr->GetExpression() == OperatorType::RightShift;

		// the literal becomes a constant of that type (not just converted: code generation looks at its type)
		auto adapt = [&](std::shared_ptr<ASTNodeBase>& literal, const std::shared_ptr<Type>& target)
		{
			if (auto value = EvaluateInteger(literal); value && target->IsIntegral())
			{
				auto constant = std::make_shared<ASTConstantValue>(*value, target);
				constant->Location = GetNodeLocation(literal); // diagnostics about it still point at what was written
				literal = constant;
			}
			else
				literal = Coerce(literal, target);
		};

		if (leftLiteral && !rightLiteral && !shift && fits(expr->LeftSide, rightType))
			adapt(expr->LeftSide, rightType);
		else if (rightLiteral && !leftLiteral && fits(expr->RightSide, leftType))
			adapt(expr->RightSide, leftType);
		else if (leftLiteral && (rightLiteral || shift) && numeric(context.ExpectedType) && fits(expr->LeftSide, context.ExpectedType))
		{
			adapt(expr->LeftSide, context.ExpectedType);

			if (rightLiteral && !shift && fits(expr->RightSide, context.ExpectedType))
				adapt(expr->RightSide, context.ExpectedType);
		}
	}

	static bool IsOptionalType(const std::shared_ptr<Type>& type)
	{
		return type && type->IsClass() && type->As<ClassType>()->IsOptional;
	}

	static std::shared_ptr<Type> OptionalValueType(const std::shared_ptr<Type>& optional)
	{
		auto classType = optional->As<ClassType>();
		return classType->Cases[classType->FindCase("some").value()].Fields[0].second;
	}

	std::optional<Sema::Narrowing> Sema::NarrowableOptional(const std::shared_ptr<ASTNodeBase>& node)
	{
		// a plain local variable (a field or global could be changed by any call in the block)
		auto variable = std::dynamic_pointer_cast<ASTVariable>(node);

		if (!variable)
			return std::nullopt;

		auto [entry, scope] = LookupSymbol(variable->GetName().GetData());

		if (!entry || entry->Type != SymbolEntryType::Variable || !m_LocalVariables.contains(entry->Symbol.get()))
			return std::nullopt;

		auto type = entry->Symbol->GetType();

		if (!IsOptionalType(type) || OptionalValueType(type)->GetHash() == "bool")
			return std::nullopt;

		return Narrowing { entry->Symbol, variable->GetName(), type };
	}

	void Sema::CollectNarrowings(const std::shared_ptr<ASTNodeBase>& test, bool whenTrue, std::vector<Narrowing>& narrowings)
	{
		// the optional locals that hold a value when `test` turns out true (whenTrue) or false:
		// true:  a, a and b, a is not none, not (a is none)        false: not a, not a or b is none
		if (auto binary = std::dynamic_pointer_cast<ASTBinaryExpression>(test); binary && binary->GetExpression() == (whenTrue ? OperatorType::And : OperatorType::Or))
		{
			CollectNarrowings(binary->LeftSide, whenTrue, narrowings);
			CollectNarrowings(binary->RightSide, whenTrue, narrowings);
			return;
		}

		if (auto unary = std::dynamic_pointer_cast<ASTUnaryExpression>(test); unary && unary->GetOperatorType() == OperatorType::Not)
		{
			CollectNarrowings(unary->Operand, !whenTrue, narrowings);
			return;
		}

		if (auto is = std::dynamic_pointer_cast<ASTIsExpr>(test))
		{
			auto literal = std::dynamic_pointer_cast<ASTNodeLiteral>(is->TypeNode);

			if (literal && literal->GetData().GetData() == "none" && is->Negate == whenTrue)
			{
				if (auto narrowing = NarrowableOptional(is->Object))
					narrowings.push_back(*narrowing);
			}

			return;
		}

		if (whenTrue)
		{
			if (auto narrowing = NarrowableOptional(test))
				narrowings.push_back(*narrowing);
		}
	}

	bool Sema::EndNarrowing(const std::shared_ptr<ASTAssignmentOperator>& assignment)
	{
		// r = none (or another optional) inside `if r:` sets the optional itself; from here on r is the optional again
		auto target = std::dynamic_pointer_cast<ASTVariable>(assignment->Storage);

		if (!target || target->Variable || assignment->GetAssignType() != AssignmentOperatorType::Normal)
			return false;

		// what the value is, before it is analysed: none, an optional variable, or a call returning one
		auto optionalValue = [&](const std::shared_ptr<ASTNodeBase>& value) -> bool
		{
			if (auto literal = std::dynamic_pointer_cast<ASTNodeLiteral>(value))
				return literal->GetData().GetData() == "none" && !literal->GetData().IsType(TokenType::String);

			if (auto variable = std::dynamic_pointer_cast<ASTVariable>(value))
			{
				auto [entry, scope] = LookupSymbol(variable->GetName().GetData());
				return entry && entry->Type == SymbolEntryType::Variable && entry->Symbol && IsOptionalType(entry->Symbol->GetType());
			}

			if (auto call = std::dynamic_pointer_cast<ASTFunctionCall>(value))
			{
				auto callee = std::dynamic_pointer_cast<ASTVariable>(call->Callee);
				auto [entry, scope] = callee ? LookupSymbol(callee->GetName().GetData()) : std::pair<std::optional<SymbolEntry>, size_t>();

				if (entry && entry->Symbol && entry->Symbol->Kind == SymbolKind::Function)
				{
					auto function = entry->Symbol->GetFunctionSymbol().FunctionNode;
					return function && IsOptionalType(function->ReturnTypeVal);
				}
			}

			return false;
		};

		if (!optionalValue(assignment->Value))
			return false;

		for (int64_t i = (int64_t)m_ScopeStack.size() - 1; i >= 0; i--)
		{
			auto entry = m_ScopeStack[i].Get(target->GetName().GetData());

			if (!entry)
				continue;

			auto narrowed = m_NarrowedNames.find(entry->Symbol.get());

			if (narrowed == m_NarrowedNames.end())
				return false;

			m_ScopeStack[i].Set(target->GetName().GetData(), SymbolEntry { SymbolEntryType::Variable, narrowed->second.Variable });
			return true;
		}

		return false;
	}

	std::shared_ptr<ASTVariableDeclaration> Sema::NarrowedDeclaration(const Narrowing& narrowing)
	{
		// let r = <the value inside r>, named in place: changing it changes the optional
		auto subject = std::make_shared<ASTVariable>(narrowing.Name);
		subject->Variable = narrowing.Variable;

		auto field = std::make_shared<ASTVariantField>();
		field->Location = narrowing.Name;
		field->Subject = subject;
		field->VariantTy = narrowing.Optional;
		field->CaseIndex = narrowing.Optional->As<ClassType>()->FindCase("some").value();
		field->FieldIndex = 0;
		field->AsAddress = true;

		auto declaration = std::make_shared<ASTVariableDeclaration>(narrowing.Name);
		declaration->Location = narrowing.Name;
		declaration->Initializer = field;
		declaration->IsAlias = true;
		m_NarrowingDeclarations[declaration.get()] = narrowing;
		return declaration;
	}

	std::shared_ptr<ASTNodeBase> Sema::OptionalTest(std::shared_ptr<ASTNodeBase> value, std::shared_ptr<Type> optional, bool hasValue)
	{
		auto int32Type = m_Module->Lookup("int32").value()->GetType();

		auto tag = std::make_shared<ASTVariantTag>();
		tag->Location = value->Location;
		tag->Subject = value;
		tag->TagType = int32Type;

		auto compare = std::make_shared<ASTBinaryExpression>(hasValue ? OperatorType::NotEqual : OperatorType::IsEqual);
		compare->Location = value->Location;
		compare->LeftSide = tag;
		compare->RightSide = std::make_shared<ASTConstantValue>((int64_t)optional->As<ClassType>()->FindCase("none").value(), int32Type);
		compare->ResultantType = Symbol::GetBooleanType(m_Module).GetType();
		return compare;
	}

	std::shared_ptr<ASTNodeBase> Sema::TestCondition(std::shared_ptr<ASTNodeBase> condition, bool hasValue)
	{
		// an optional as a condition means "holds a value" (never "the value is true": ?bool is ambiguous)
		if (!condition)
			return nullptr;

		auto type = m_TypeInferEngine.InferTypeFromNode(condition);

		if (!IsOptionalType(type))
			return condition;

		if (OptionalValueType(type)->GetHash() == "bool")
		{
			Report(DiagnosticCode_OptionalBoolCondition, GetNodeLocation(condition));
			return condition;
		}

		return OptionalTest(condition, type, hasValue);
	}

	std::shared_ptr<ASTNodeBase> Sema::EvaluatedOnce(std::shared_ptr<ASTNodeBase> value, std::shared_ptr<Type> type)
	{
		// a new value that owns memory is kept in a temporary (cleaned up at the end of the block); what is read
		// from it is copied by whoever keeps it
		if (IsOwning(type) && IsFreshValue(value))
		{
			auto load = std::make_shared<ASTLoad>();
			load->Operand = AddressOf(value);
			value = load;
		}

		auto once = std::make_shared<ASTOnce>();
		once->Location = value->Location;
		once->Operand = value;
		return once;
	}

	std::shared_ptr<ASTNodeBase> Sema::SliceIntrinsic(const std::string& name, std::shared_ptr<Type> result, std::vector<std::shared_ptr<ASTNodeBase>> arguments, const Token& location)
	{
		auto intrinsic = std::make_shared<ASTIntrinsic>(name, result);
		intrinsic->Location = location;
		intrinsic->Arguments.assign(arguments.begin(), arguments.end());
		return intrinsic;
	}

	std::shared_ptr<ASTNodeBase> Sema::SliceOf(std::shared_ptr<ASTNodeBase> node, std::shared_ptr<Type> type, const Token& location)
	{
		// the whole of an array, or of an object with operator slice, as a []T; null if it has none
		auto int64Type = m_Module->Lookup("int64").value()->GetType();

		if (auto slice = std::dynamic_pointer_cast<SliceType>(type))
			return node;

		// where the value lives: a variable is used in place, a computed value is kept in a temporary
		auto storage = [&]() -> std::shared_ptr<ASTNodeBase>
		{
			if (auto load = std::dynamic_pointer_cast<ASTLoad>(node); load && IsStorageNode(load->Operand))
				return load->Operand;

			if (IsStorageNode(node))
				return node;

			return AddressOf(node);
		};

		auto pointee = type && type->IsPointer() ? type->As<PointerType>()->GetBaseType() : nullptr;

		if (type && (type->IsArray() || (pointee && pointee->IsArray())))
		{
			auto array = (type->IsArray() ? type : pointee)->As<ArrayType>();
			auto address = type->IsArray() ? storage() : node;
			auto slice = m_Module->GetTypeRegistry()->GetSliceOf(array->GetBaseType());
			return SliceIntrinsic("slice_of_array", slice, { address, std::make_shared<ASTConstantValue>((int64_t)array->GetArraySize(), int64Type) }, location);
		}

		auto classType = ClassOf(type) ? ClassOf(type)->As<ClassType>() : nullptr;

		if (classType && classType->MemberFunctions.contains("__slice__") && classType->MemberFunctions.contains("__len__"))
		{
			// self is passed as a pointer, computed once (it is used for the length too)
			auto self = std::make_shared<ASTOnce>();
			self->Location = location;
			self->Operand = type->IsPointer() ? node : storage();

			if (IsStorageNode(self->Operand))
			{
				auto address = std::make_shared<ASTUnaryExpression>(OperatorType::Address);
				address->Location = location;
				address->Operand = self->Operand;
				self->Operand = address;
			}

			auto selfType = m_TypeInferEngine.InferTypeFromNode(self->Operand);

			EnsureDefined(classType->MemberFunctions.at("__len__")->GetFunctionSymbol().FunctionNode);
			EnsureDefined(classType->MemberFunctions.at("__slice__")->GetFunctionSymbol().FunctionNode);

			auto length = CallMethod(self, selfType, "__len__", {}, location);
			return length ? CallMethod(self, selfType, "__slice__", { std::make_shared<ASTConstantValue>((int64_t)0, int64Type), length }, location) : nullptr;
		}

		return nullptr;
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitSlice(std::shared_ptr<ASTSliceExpr> slice, SemaContext context)
	{
		SemaContext storageContext = context;
		storageContext.ValueReq = ValueRequired::LValue;
		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;
		valueContext.ExpectedType = nullptr;

		auto target = Visit(slice->Target, storageContext);

		if (!target)
			return nullptr;

		auto int64Type = m_Module->Lookup("int64").value()->GetType();
		auto type = m_TypeInferEngine.InferTypeFromNode(target);
		Token location = slice->Location;

		std::shared_ptr<ASTNodeBase> start = slice->Start ? Coerce(Visit(slice->Start, valueContext), int64Type) : nullptr;
		std::shared_ptr<ASTNodeBase> end = slice->End ? Coerce(Visit(slice->End, valueContext), int64Type) : nullptr;

		if ((slice->Start && !start) || (slice->End && !end))
			return nullptr;

		// an object with operator slice decides itself (List: a view of its items)
		auto classType = ClassOf(type) ? ClassOf(type)->As<ClassType>() : nullptr;

		if (classType && classType->MemberFunctions.contains("__slice__"))
		{
			// self is passed as a pointer, computed once (it is used for the length too)
			auto self = std::make_shared<ASTOnce>();
			self->Location = location;
			self->Operand = type->IsPointer() ? AsValue(target) : (IsStorageNode(target) ? target : AddressOf(target));

			if (IsStorageNode(self->Operand))
			{
				auto address = std::make_shared<ASTUnaryExpression>(OperatorType::Address);
				address->Location = location;
				address->Operand = self->Operand;
				self->Operand = address;
			}

			auto selfType = m_TypeInferEngine.InferTypeFromNode(self->Operand);
			EnsureDefined(classType->MemberFunctions.at("__slice__")->GetFunctionSymbol().FunctionNode);

			if (!end)
			{
				if (!classType->MemberFunctions.contains("__len__"))
				{
					location.SetData(GetDisplayName(type));
					Report(DiagnosticCode_NotIterable, location);
					return nullptr;
				}

				EnsureDefined(classType->MemberFunctions.at("__len__")->GetFunctionSymbol().FunctionNode);
				end = CallMethod(self, selfType, "__len__", {}, location);
			}

			if (!start)
				start = std::make_shared<ASTConstantValue>((int64_t)0, int64Type);

			return end ? CallMethod(self, selfType, "__slice__", { start, end }, location) : nullptr;
		}

		auto whole = SliceOf(target, type, location);

		if (!whole)
		{
			Token where = GetNodeLocation(target);
			where.SetData(std::format("{}’ cannot be sliced (arrays, slices, and classes with operator slice can", GetDisplayName(type)));
			Report(DiagnosticCode_NotIterable, where);
			return nullptr;
		}

		if (!start && !end)
			return std::dynamic_pointer_cast<SliceType>(type) ? AsValue(whole) : whole;

		// the whole is computed once: the end defaults to its length
		auto once = std::make_shared<ASTOnce>();
		once->Location = location;
		once->Operand = std::dynamic_pointer_cast<SliceType>(type) ? AsValue(whole) : whole;
		auto sliceType = m_TypeInferEngine.InferTypeFromNode(once);

		if (!start)
			start = std::make_shared<ASTConstantValue>((int64_t)0, int64Type);

		if (!end)
			end = SliceIntrinsic("slice_len", int64Type, { once }, location);

		return SliceIntrinsic("slice_range", sliceType, { once, start, end }, location);
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitCoalesce(std::shared_ptr<ASTBinaryExpression> expr, SemaContext context)
	{
		// a ?? b   ->   when a is none use b otherwise a.value   (a is computed once, b only when needed)
		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;

		auto left = Visit(expr->LeftSide, valueContext);

		if (!left)
			return nullptr;

		auto optional = m_TypeInferEngine.InferTypeFromNode(left);

		if (!IsOptionalType(optional))
		{
			Token location = GetNodeLocation(left);
			location.SetData(std::format("??’ needs an optional on its left, this is ‘{}", optional ? GetDisplayName(optional) : "?"));
			Report(DiagnosticCode_NotOptional, location);
			return nullptr;
		}

		auto valueType = OptionalValueType(optional);
		valueContext.ExpectedType = valueType;
		auto right = Visit(expr->RightSide, valueContext);

		if (!right)
			return nullptr;

		// `a ?? b` with b optional too stays optional: `first ?? second ?? 0`
		bool optionalResult = IsOptionalType(m_TypeInferEngine.InferTypeFromNode(right));
		auto resultType = optionalResult ? optional : valueType;
		auto once = EvaluatedOnce(left, optional);

		std::shared_ptr<ASTNodeBase> held = once;

		if (!optionalResult)
		{
			auto unwrap = std::make_shared<ASTOptionalUnwrap>();
			unwrap->Location = expr->Location;
			unwrap->Subject = once;
			unwrap->OptionalTy = optional;
			held = unwrap;
		}

		auto ternary = std::make_shared<ASTTernaryExpression>();
		ternary->Location = expr->Location;
		ternary->Condition = OptionalTest(once, optional, false);
		ternary->Truthy = OwnedValue(Coerce(right, resultType), resultType);
		ternary->Falsy = OwnedValue(held, resultType);

		if (!ternary->Truthy || !ternary->Falsy)
			return nullptr;

		return ternary;
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitOptionalChain(std::shared_ptr<ASTBinaryExpression> expr, SemaContext context, std::shared_ptr<ASTFunctionCall> call)
	{
		// a?.b   ->   when a is none use none otherwise some(a.value.b)        (a is computed once)
		// a?.f() ->   the same, or just `if a: a.f()` when f returns nothing
		SemaContext valueContext = context;
		valueContext.ValueReq = ValueRequired::RValue;
		valueContext.ExpectedType = nullptr;

		auto left = Visit(expr->LeftSide, valueContext);

		if (!left)
			return nullptr;

		auto optional = m_TypeInferEngine.InferTypeFromNode(left);

		if (!IsOptionalType(optional))
		{
			Token location = GetNodeLocation(left);
			location.SetData(std::format("?.’ needs an optional on its left, this is ‘{}’ (use ‘.’", optional ? GetDisplayName(optional) : "?"));
			Report(DiagnosticCode_NotOptional, location);
			return nullptr;
		}

		auto once = EvaluatedOnce(left, optional);

		auto unwrap = std::make_shared<ASTOptionalUnwrap>();
		unwrap->Location = expr->Location;
		unwrap->Subject = once;
		unwrap->OptionalTy = optional;

		auto access = std::make_shared<ASTBinaryExpression>(OperatorType::Dot);
		access->Location = expr->Location;
		access->LeftSide = unwrap;
		access->RightSide = expr->RightSide;

		std::shared_ptr<ASTNodeBase> member = access;

		if (call)
		{
			auto method = std::make_shared<ASTFunctionCall>();
			method->Location = call->Location;
			method->Callee = access;
			method->Arguments = call->Arguments;
			method->KeywordArguments = call->KeywordArguments;
			member = method;
		}

		member = Visit(member, valueContext);

		if (!member)
			return nullptr;

		auto memberType = m_TypeInferEngine.InferTypeFromNode(member);

		if (!memberType || (memberType->Get() && memberType->Get()->isVoidTy()))
		{
			auto body = std::make_shared<ASTBlock>();
			body->Children.push_back(member);

			auto onlyIf = std::make_shared<ASTIfExpression>();
			onlyIf->ConditionalBlocks.push_back({ .Condition = OptionalTest(once, optional, true), .CodeBlock = body });
			return onlyIf;
		}

		// user?.address?.city: a member that is optional itself is not wrapped again
		auto resultType = IsOptionalType(memberType) ? memberType : GetOptionalType(memberType);

		auto none = std::make_shared<ASTNodeLiteral>(Token(TokenType::Keyword, "none", expr->Location.GetSourceFile(), expr->Location.LineNumber, expr->Location.ColumnNumber));

		auto ternary = std::make_shared<ASTTernaryExpression>();
		ternary->Location = expr->Location;
		ternary->Condition = OptionalTest(once, optional, false);
		ternary->Truthy = Coerce(Visit(none, valueContext), resultType);
		ternary->Falsy = Coerce(OwnedValue(member, memberType), resultType);

		if (!ternary->Truthy || !ternary->Falsy)
			return nullptr;

		return ternary;
	}

	std::shared_ptr<ASTNodeBase> Sema::TextConcat(std::shared_ptr<ASTNodeBase> left, std::shared_ptr<ASTNodeBase> right, const Token& location)
	{
		// "a" + "b", name + "!", "<" + name: a new String holding both (String + String has its own operator add)
		auto leftType = m_TypeInferEngine.InferTypeFromNode(left);
		auto rightType = m_TypeInferEngine.InferTypeFromNode(right);
		auto isStr = [](const std::shared_ptr<Type>& type) { return type && type->GetHash() == "str"; };
		auto isString = [](const std::shared_ptr<Type>& type) { return type && ClassOf(type) && ClassOf(type)->GetHash() == "String"; };

		if ((isStr(leftType) || isString(leftType)) && (isStr(rightType) || isString(rightType)))
			return CallLibraryFunction("concat_text", { left, right }, location);

		return nullptr;
	}

	std::shared_ptr<ASTNodeBase> Sema::CallLibraryFunction(const std::string& name, std::vector<std::shared_ptr<ASTNodeBase>> arguments, const Token& location)
	{
		// a function of the standard library the compiler calls on the program's behalf (string is always imported)
		std::shared_ptr<Symbol> function;

		if (auto [entry, scope] = LookupSymbol(name); entry && entry->Symbol->Kind == SymbolKind::Function)
			function = entry->Symbol;
		else if (auto found = LookupInModules(name); found && found.value()->Kind == SymbolKind::Function)
			function = found.value();

		if (!function)
		{
			Token where = location;
			where.SetData(name);
			Report(DiagnosticCode_UndeclaredIdentifier, where);
			return nullptr;
		}

		auto callee = std::make_shared<ASTVariable>(Token(TokenType::Identifier, name, location.GetSourceFile(), location.LineNumber, location.ColumnNumber));
		callee->Variable = function;

		auto call = std::make_shared<ASTFunctionCall>();
		call->Location = location;
		call->Callee = callee;
		call->Arguments.assign(arguments.begin(), arguments.end());
		return CheckCall(call);
	}

	std::shared_ptr<ASTNodeBase> Sema::OwnedValue(std::shared_ptr<ASTNodeBase> node, std::shared_ptr<Type> type)
	{
		// a value that will be owned (and cleaned up) by whoever receives it: new values as they are, others copied
		if (!node || !IsOwning(type) || IsFreshValue(node))
			return node;

		if (!IsCopyable(type))
		{
			Token location = GetNodeLocation(node);
			location.SetData(GetDisplayName(type));
			Report(DiagnosticCode_CannotCopyOwning, location);
			return nullptr;
		}

		EnsureCopyDefined(type);
		auto copy = std::make_shared<ASTCopy>();
		copy->Location = node->Location;
		copy->Value = node;
		copy->ValueType = type;
		m_Copies.Copies.push_back(copy);
		return copy;
	}

	// operator copy of a generic class is only analysed once something is really copied
	void Sema::EnsureCopyDefined(std::shared_ptr<Type> type)
	{
		if (!IsOwning(type))
			return;

		// a type that holds itself (through a List) is already being prepared
		static thread_local std::unordered_set<Type*> s_Preparing;

		if (!s_Preparing.insert(type.get()).second)
			return;

		struct Done { Type* T; ~Done() { s_Preparing.erase(T); } } done { type.get() };

		if (auto array = std::dynamic_pointer_cast<ArrayType>(type))
			return EnsureCopyDefined(array->GetBaseType());

		if (auto tuple = std::dynamic_pointer_cast<TupleType>(type))
		{
			for (auto& element : tuple->GetElements())
				EnsureCopyDefined(element);
			return;
		}

		if (!type->IsClass())
			return;

		auto classType = type->As<ClassType>();

		if (classType->IsVariant)
		{
			for (auto& variantCase : classType->Cases)
				for (auto& [name, fieldType] : variantCase.Fields)
					EnsureCopyDefined(fieldType);
			return;
		}

		if (auto copy = classType->MemberFunctions.find("__copy__"); copy != classType->MemberFunctions.end())
		{
			EnsureDefined(copy->second->GetFunctionSymbol().FunctionNode);
			return;
		}

		for (const auto& [name, fieldType] : classType->GetMemberValues())
			EnsureCopyDefined(fieldType);
	}

	std::shared_ptr<ASTNodeBase> Sema::TakeOwnership(std::shared_ptr<ASTNodeBase> node, std::shared_ptr<Type> type)
	{
		// an owning value has one owner: it is moved out of a local, never silently copied out of anything else
		if (!node || !IsOwning(type) || m_ViewsAllowed || IsFreshValue(node))
			return node;

		auto isLocal = [&](const std::shared_ptr<ASTNodeBase>& storage)
		{
			auto variable = std::dynamic_pointer_cast<ASTVariable>(storage);
			return variable && variable->Variable && m_LocalVariables.contains(variable->Variable.get());
		};

		auto copy = [&]()
		{
			EnsureCopyDefined(type);
			auto result = std::make_shared<ASTCopy>();
			result->Location = node->Location;
			result->Value = node;
			result->ValueType = type;
			m_Copies.Copies.push_back(result);
			return result;
		};

		bool copyable = IsCopyable(type);

		// let b = a  /  f(a): b gets its own copy and a stays as it was;  return a: a ends here, so it is moved.
		// If a turns out not to be used again, the copy becomes a move too (see FinishCopies)
		if (auto load = std::dynamic_pointer_cast<ASTLoad>(node); load && isLocal(load->Operand) && copyable && !m_Returning)
		{
			auto result = copy();
			auto variable = std::dynamic_pointer_cast<ASTVariable>(load->Operand);

			if (auto use = m_Copies.UseOf.find(variable.get()); use != m_Copies.UseOf.end())
				m_Copies.Candidates.push_back(CopyCandidate { variable->Variable.get(), use->second, result, variable, true });

			return result;
		}

		// let u = q inside `if q:` (q the value in the optional q): at its last use the optional is emptied instead
		if (auto load = std::dynamic_pointer_cast<ASTLoad>(node); load && copyable && !m_Returning)
		{
			auto variable = std::dynamic_pointer_cast<ASTVariable>(load->Operand);
			auto alias = variable && variable->Variable ? m_NarrowedAliases.find(variable->Variable.get()) : m_NarrowedAliases.end();

			if (alias != m_NarrowedAliases.end())
			{
				auto result = copy();

				if (auto use = m_Copies.UseOf.find(variable.get()); use != m_Copies.UseOf.end())
					m_Copies.Candidates.push_back(CopyCandidate { alias->second->Variable.get(), use->second, result, alias->second, true });

				return result;
			}
		}

		// a value that cannot be copied (a File, a class with its own destruct) moves out of a local, which is left empty
		if (auto load = std::dynamic_pointer_cast<ASTLoad>(node); load && isLocal(load->Operand))
		{
			if (auto variable = std::dynamic_pointer_cast<ASTVariable>(load->Operand))
				m_Copies.NotViewable.insert(variable->Variable.get());

			RecordMove(std::dynamic_pointer_cast<ASTVariable>(load->Operand));
			MarkMovedFrom(load->Operand);

			auto move = std::make_shared<ASTMove>();
			move->Location = node->Location;
			move->Storage = load->Operand;
			move->Value = node;
			move->ValueType = type;
			return move;
		}

		// let s = maybe.value of a copyable value: a copy, which becomes a move (leaving the optional none) when the
		// optional is not used again
		if (auto unwrap = std::dynamic_pointer_cast<ASTOptionalUnwrap>(node); unwrap && copyable && !m_Returning)
		{
			if (auto load = std::dynamic_pointer_cast<ASTLoad>(unwrap->Subject); load && isLocal(load->Operand))
			{
				auto result = copy();
				auto variable = std::dynamic_pointer_cast<ASTVariable>(load->Operand);

				if (auto use = m_Copies.UseOf.find(variable.get()); use != m_Copies.UseOf.end())
					m_Copies.Candidates.push_back(CopyCandidate { variable->Variable.get(), use->second, result, variable, true });

				return result;
			}
		}

		// let line = maybe.value (of something that cannot be copied): the optional is left as none
		if (auto unwrap = std::dynamic_pointer_cast<ASTOptionalUnwrap>(node); unwrap && !copyable)
		{
			if (auto load = std::dynamic_pointer_cast<ASTLoad>(unwrap->Subject); load && isLocal(load->Operand))
			{
				MarkMovedFrom(load->Operand);
				auto move = std::make_shared<ASTMove>();
				move->Location = node->Location;
				move->Storage = load->Operand;
				move->Value = node;
				move->ValueType = type;
				return move;
			}
		}

		// reading through a pointer (*p, p[i]) copies like any other read; only a value that cannot be copied is
		// handed over as it is (code managing raw memory, like List, uses take(p) to move instead)
		if (auto unary = std::dynamic_pointer_cast<ASTUnaryExpression>(node); !copyable && unary && unary->GetOperatorType() == OperatorType::Dereference && !unary->IsElement)
			return node;

		if (auto subscript = std::dynamic_pointer_cast<ASTSubscript>(node); !copyable && subscript && subscript->Meaning == SubscriptSemantic::ArrayIndex)
		{
			auto targetType = m_TypeInferEngine.InferTypeFromNode(subscript->Target);
			if (targetType && targetType->IsPointer())
				return node;
		}

		if (auto load = std::dynamic_pointer_cast<ASTLoad>(node); load && !copyable)
		{
			if (auto subscript = std::dynamic_pointer_cast<ASTSubscript>(load->Operand); subscript && subscript->Meaning == SubscriptSemantic::ArrayIndex)
			{
				auto targetType = m_TypeInferEngine.InferTypeFromNode(subscript->Target);
				if (targetType && targetType->IsPointer())
					return node;
			}

			if (auto unary = std::dynamic_pointer_cast<ASTUnaryExpression>(load->Operand); unary && unary->GetOperatorType() == OperatorType::Dereference && !unary->IsElement)
				return node;
		}

		// reading from a field, an element or another place: a copy of its own
		if (copyable)
			return copy();

		Token location = GetNodeLocation(node);
		location.SetData(GetDisplayName(type));
		Report(DiagnosticCode_CannotCopyOwning, location);
		return node;
	}

	std::shared_ptr<ASTNodeBase> Sema::Coerce(std::shared_ptr<ASTNodeBase> node, std::shared_ptr<Type> target)
	{
		if (!node || !target)
			return node;

		// let s: String = "text", greet("ada") with a String parameter: a literal becomes a String where that is the type
		if (auto literal = std::dynamic_pointer_cast<ASTNodeLiteral>(node); literal && literal->GetData().IsType(TokenType::String) && target->GetHash() == "String")
		{
			if (auto made = CallLibraryFunction("string_from_literal", { node }, literal->GetData()))
				return made;
		}

		// str <-> C strings and bytes: str -> *int8 is a pointer to its bytes (they must end with a zero),
		// *int8 -> str measures the C string, []int8 and str are the same thing seen two ways
		if (auto source = m_TypeInferEngine.InferTypeFromNode(node); source && source != target)
		{
			bool sourceStr = source->GetHash() == "str", targetStr = target->GetHash() == "str";
			auto bytes = [](const std::shared_ptr<Type>& type) { return type->IsPointer() && type->As<PointerType>()->GetBaseType() && type->As<PointerType>()->GetBaseType()->GetHash() == "int8"; };
			auto byteSlice = [](const std::shared_ptr<Type>& type) { auto slice = std::dynamic_pointer_cast<SliceType>(type); return slice && type->GetHash() != "str" && slice->GetBaseType()->GetHash() == "int8"; };

			if (sourceStr && bytes(target))
				return SliceIntrinsic("str_c", target, { node }, GetNodeLocation(node));

			if (targetStr && bytes(source))
				return SliceIntrinsic("str_from_c", target, { node }, GetNodeLocation(node));

			if ((sourceStr && byteSlice(target)) || (targetStr && byteSlice(source)))
				return SliceIntrinsic("slice_retype", target, { node }, GetNodeLocation(node));
		}

		// an array or a list (anything with operator slice) where a []T is expected: all of it, as a view
		if (auto slice = std::dynamic_pointer_cast<SliceType>(target))
		{
			auto type = m_TypeInferEngine.InferTypeFromNode(node);

			if (type && type != target && !std::dynamic_pointer_cast<SliceType>(type))
			{
				if (auto whole = SliceOf(node, type, GetNodeLocation(node)); whole && m_TypeInferEngine.InferTypeFromNode(whole) == target)
					return whole;
			}
		}

		if (IsOwning(target) && m_TypeInferEngine.InferTypeFromNode(node) == target)
			return TakeOwnership(node, target);

		// a value going into a type variant becomes the case of its type
		if (target->IsClass() && target->As<ClassType>()->IsTypeVariant)
		{
			auto variant = target->As<ClassType>();
			auto source = m_TypeInferEngine.InferTypeFromNode(node);

			if (!source || source == target)
				return node;

			auto index = FindTypeCase(variant, source);

			// no exact match: the one type it converts to (a number literal prefers its own kind: 2 -> int, 2.5 -> float)
			if (!index)
			{
				bool literal = IsNumericLiteral(node);
				std::vector<size_t> candidates;

				for (size_t i = 0; i < variant->Cases.size(); i++)
				{
					if (IsImplicitlyConvertible(source, variant->Cases[i].Fields[0].second, literal))
						candidates.push_back(i);
				}

				if (candidates.size() > 1 && literal)
				{
					std::erase_if(candidates, [&](size_t i)
					{
						auto caseType = variant->Cases[i].Fields[0].second;
						return caseType->IsFloatingPoint() != source->IsFloatingPoint() || caseType->Get()->isIntegerTy(1);
					});

					if (candidates.size() > 1)
						candidates.resize(1);
				}

				if (candidates.size() == 1)
					index = candidates[0];
			}

			if (!index)
			{
				std::string names;
				for (auto& c : variant->Cases)
					names += (names.empty() ? "" : ", ") + c.Name;

				Token location = GetNodeLocation(node);
				location.SetData(std::format("{}’ cannot go into ‘{}’, which holds one of: {}", GetDisplayName(source), variant->GetHash(), names));
				m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_NotInVariant, 1);
				return node;
			}

			return BuildVariantConstruct(target, *index, { node }, {}, GetNodeLocation(node));
		}

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

			// only suggest `as` where `as` can do it
			if (!CastAllowed(source, target))
			{
				location.SetData(ConversionAdvice(source, target));
				m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_NoConversion, width);
				return node;
			}

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

	void Sema::Report(DiagnosticCode code, Token token, size_t width)
	{
		m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, token, code, std::max<size_t>(width, 1));
	}

	void Sema::Warn(DiagnosticCode code, Token token, size_t width)
	{
		m_DiagBuilder.Report(Stage::CodeGeneration, Severity::Low, token, code, std::max<size_t>(width, 1));
	}

	void Sema::RecordMove(const std::shared_ptr<ASTVariable>& variable)
	{
		if (m_Unreachable || !variable->Variable)
			return;

		// f(a, a): the second one finds a already empty
		CheckNotMoved(variable);
		m_Moved[variable->Variable.get()] = variable->GetName();
	}

	void Sema::CheckNotMoved(const std::shared_ptr<ASTVariable>& variable)
	{
		if (m_Unreachable || m_Moved.empty() || !variable->Variable)
			return;

		auto it = m_Moved.find(variable->Variable.get());

		if (it == m_Moved.end())
			return;

		Token location = variable->GetName();
		size_t width = location.GetData().size();
		location.SetData(std::format("‘{}’ was moved on line {}", location.GetData(), it->second.LineNumber + 1));
		Report(DiagnosticCode_UseAfterMove, location, width);

		m_Moved.erase(it); // one error per move is enough
	}

	static std::shared_ptr<ASTVariable> RootVariable(std::shared_ptr<ASTNodeBase> node, bool* throughCall)
	{
		// the variable an expression like &a.items[i].field or a.get(i) starts from
		while (node)
		{
			switch (node->GetType())
			{
				case ASTNodeType::Variable:
					return std::dynamic_pointer_cast<ASTVariable>(node);
				case ASTNodeType::Load:
					node = std::dynamic_pointer_cast<ASTLoad>(node)->Operand;
					break;
				case ASTNodeType::UnaryExpression:
				{
					auto unary = std::dynamic_pointer_cast<ASTUnaryExpression>(node);

					if (unary->GetOperatorType() != OperatorType::Address && unary->GetOperatorType() != OperatorType::Dereference)
						return nullptr;

					node = unary->Operand;
					break;
				}
				case ASTNodeType::BinaryExpression:
				{
					auto binary = std::dynamic_pointer_cast<ASTBinaryExpression>(node);

					if (binary->GetExpression() != OperatorType::Dot)
						return nullptr;

					node = binary->LeftSide;
					break;
				}
				case ASTNodeType::Subscript:
					if (throughCall) *throughCall = true;
					node = std::dynamic_pointer_cast<ASTSubscript>(node)->Target;
					break;
				case ASTNodeType::Once:
					node = std::dynamic_pointer_cast<ASTOnce>(node)->Operand;
					break;
				case ASTNodeType::Intrinsic:
				{
					// slice_range(s, a, b), slice_of_array(&xs, n): a view of the first argument
					auto intrinsic = std::dynamic_pointer_cast<ASTIntrinsic>(node);

					if (intrinsic->Arguments.empty() || (intrinsic->Name != "slice_range" && intrinsic->Name != "slice_of_array" && intrinsic->Name != "slice_at"))
						return nullptr;

					if (throughCall) *throughCall = true;
					node = intrinsic->Arguments[0];
					break;
				}
				case ASTNodeType::FunctionCall:
				{
					// a method call: its first argument is the object
					auto call = std::dynamic_pointer_cast<ASTFunctionCall>(node);

					if (call->Arguments.empty())
						return nullptr;

					if (throughCall) *throughCall = true;
					node = call->Arguments[0];
					break;
				}
				default:
					return nullptr;
			}
		}

		return nullptr;
	}

	static std::string PathOf(const std::shared_ptr<ASTNodeBase>& node)
	{
		// c.sections, box.items.data: the chain of names an expression reaches its object through ("" if none)
		if (!node)
			return "";

		switch (node->GetType())
		{
			case ASTNodeType::Variable: return std::dynamic_pointer_cast<ASTVariable>(node)->GetName().GetData();
			case ASTNodeType::Load:     return PathOf(std::dynamic_pointer_cast<ASTLoad>(node)->Operand);
			case ASTNodeType::Once:     return PathOf(std::dynamic_pointer_cast<ASTOnce>(node)->Operand);
			case ASTNodeType::UnaryExpression:
			{
				auto unary = std::dynamic_pointer_cast<ASTUnaryExpression>(node);
				return unary->GetOperatorType() == OperatorType::Address || unary->GetOperatorType() == OperatorType::Dereference ? PathOf(unary->Operand) : "";
			}
			case ASTNodeType::BinaryExpression:
			{
				auto binary = std::dynamic_pointer_cast<ASTBinaryExpression>(node);
				auto field = std::dynamic_pointer_cast<ASTVariable>(binary->RightSide);

				if (binary->GetExpression() != OperatorType::Dot || !field)
					return "";

				auto left = PathOf(binary->LeftSide);
				return left.empty() ? "" : left + "." + field->GetName().GetData();
			}
			default:
				return "";
		}
	}

	static std::string ContainerPathOf(std::shared_ptr<ASTNodeBase> node)
	{
		// &c.sections[0] -> "c.sections": the collection an element pointer (or slice) points into
		while (node)
		{
			switch (node->GetType())
			{
				case ASTNodeType::FunctionCall:
				{
					auto call = std::dynamic_pointer_cast<ASTFunctionCall>(node);
					return call->Arguments.empty() ? "" : PathOf(call->Arguments[0]);
				}
				case ASTNodeType::Intrinsic:
				{
					auto intrinsic = std::dynamic_pointer_cast<ASTIntrinsic>(node);
					return intrinsic->Arguments.empty() ? "" : PathOf(intrinsic->Arguments[0]);
				}
				case ASTNodeType::Subscript:
					return PathOf(std::dynamic_pointer_cast<ASTSubscript>(node)->Target);
				case ASTNodeType::Load:
					node = std::dynamic_pointer_cast<ASTLoad>(node)->Operand;
					break;
				case ASTNodeType::Once:
					node = std::dynamic_pointer_cast<ASTOnce>(node)->Operand;
					break;
				case ASTNodeType::UnaryExpression:
					node = std::dynamic_pointer_cast<ASTUnaryExpression>(node)->Operand;
					break;
				case ASTNodeType::BinaryExpression:
					node = std::dynamic_pointer_cast<ASTBinaryExpression>(node)->LeftSide;
					break;
				default:
					return "";
			}
		}

		return "";
	}

	void Sema::NoteElementPointer(const std::shared_ptr<ASTVariableDeclaration>& decl)
	{
		bool view = decl->ResolvedType && std::dynamic_pointer_cast<SliceType>(decl->ResolvedType);

		if (!decl->Initializer || !decl->ResolvedType || (!decl->ResolvedType->IsPointer() && !view))
			return;

		// let best = xs[0] with xs: List[*Order]: the pointer stored in the item is read out (a load), it points
		// wherever it pointed before, not into xs (unlike &xs[0] or xs.slot(0))
		auto top = decl->Initializer;
		auto unaryTop = std::dynamic_pointer_cast<ASTUnaryExpression>(top);

		if (top->GetType() == ASTNodeType::Load || (unaryTop && unaryTop->GetOperatorType() == OperatorType::Dereference))
			return;

		bool throughCall = false;
		auto root = RootVariable(decl->Initializer, &throughCall);

		if (!root || !throughCall || !root->Variable || !m_LocalVariables.contains(root->Variable.get()))
			return;

		// only into something that holds items: a class (or a pointer to one), not a plain array
		auto type = root->Variable->GetType();

		if (type && type->IsPointer())
			type = type->As<PointerType>()->GetBaseType();

		if (!type || !type->IsClass())
			return;

		m_ElementPointers[decl->Variable.get()] = ElementPointer { root->Variable.get(), root->GetName().GetData(), ContainerPathOf(decl->Initializer) };
	}

	void Sema::NoteContainerChange(const std::shared_ptr<ASTNodeBase>& container, const Token& change)
	{
		if (m_ElementPointers.empty())
			return;

		auto root = RootVariable(container, nullptr);

		if (!root)
			return;

		auto [entry, scope] = LookupSymbol(root->GetName().GetData());
		Symbol* symbol = root->Variable ? root->Variable.get() : (entry ? entry->Symbol.get() : nullptr);

		// c.problems.push(x) does not move c.sections's items: the paths must overlap (one is the other or inside it)
		std::string changed = PathOf(container);
		auto overlaps = [](const std::string& a, const std::string& b)
		{
			if (a.empty() || b.empty() || a == b)
				return true;

			const std::string& shorter = a.size() < b.size() ? a : b;
			const std::string& longer = a.size() < b.size() ? b : a;
			return longer.starts_with(shorter) && longer[shorter.size()] == '.';
		};

		for (auto& [pointer, element] : m_ElementPointers)
		{
			if (element.Container == symbol && overlaps(element.ContainerPath, changed) && !m_StalePointers.contains(pointer))
				m_StalePointers[pointer] = change;
		}
	}

	void Sema::CheckStalePointer(const std::shared_ptr<ASTVariable>& variable)
	{
		if (m_StalePointers.empty() || !variable->Variable)
			return;

		auto stale = m_StalePointers.find(variable->Variable.get());

		if (stale == m_StalePointers.end())
			return;

		Token location = variable->GetName();
		size_t width = location.GetData().size();
		location.SetData(std::format("‘{}’ points into ‘{}’, which was changed by ‘{}’ on line {}", location.GetData(),
									 m_ElementPointers[variable->Variable.get()].ContainerName, stale->second.GetData(), stale->second.LineNumber + 1));
		Warn(DiagnosticCode_StaleElementPointer, location, width);

		m_StalePointers.erase(stale); // warn once
		m_ElementPointers.erase(variable->Variable.get());
	}

	Symbol* Sema::MoveRoot(Symbol* symbol)
	{
		auto alias = m_NarrowedAliases.find(symbol);
		return alias != m_NarrowedAliases.end() ? alias->second->Variable.get() : symbol;
	}

	void Sema::NoteUse(const std::shared_ptr<ASTVariable>& variable, ValueRequired valueRequired)
	{
		Symbol* symbol = MoveRoot(variable->Variable.get());

		if (!symbol || !m_LocalVariables.contains(symbol))
			return;

		m_Copies.UseOf[variable.get()] = ++m_Copies.Uses[symbol];

		if (m_Copies.InDefer)
			m_Copies.NeverMove.insert(symbol);

		// for views: when it is used, and whether the use may change it (anything but reading its value)
		size_t clock = ++m_Copies.Clock;
		m_Copies.LastUse[symbol] = clock;

		if ((valueRequired != ValueRequired::RValue && !m_ReadingUse) || variable.get() == m_Reinitialised)
			m_Copies.Writes[symbol].push_back(clock);

		if (!m_LoopMoves.empty())
		{
			auto order = m_LocalOrder.find(symbol);

			if (order != m_LocalOrder.end() && order->second < m_LoopMoves.back().FirstLocal)
				m_Copies.NotViewable.insert(symbol);
		}
	}

	std::shared_ptr<ASTNodeBase> Sema::AddressOfRead(const std::shared_ptr<ASTNodeBase>& value)
	{
		// where a value that was read lives: list[i] is *(pointer), obj.field and a[i] are loads of storage
		if (auto element = std::dynamic_pointer_cast<ASTUnaryExpression>(value); element && element->GetOperatorType() == OperatorType::Dereference && element->IsElement)
			return element->Operand;

		if (auto load = std::dynamic_pointer_cast<ASTLoad>(value); load && IsStorageNode(load->Operand))
		{
			auto address = std::make_shared<ASTUnaryExpression>(OperatorType::Address);
			address->Location = value->Location;
			address->Operand = load->Operand;
			return address;
		}

		return nullptr;
	}

	void Sema::KeepLentArguments(llvm::ArrayRef<std::shared_ptr<ASTNodeBase>> arguments, size_t firstCandidate)
	{
		// a + a, x.combine(x): self (or another argument) points at a variable this same call also gets a copy
		// of. That copy must stay a copy: moving would empty the variable self still looks at.
		std::unordered_set<Symbol*> lent;

		for (auto& argument : arguments)
		{
			auto type = argument ? m_TypeInferEngine.InferTypeFromNode(argument) : nullptr;

			// a pointer or slice into a variable, or the variable's storage itself (self is passed that way)
			if (type && (type->IsPointer() || std::dynamic_pointer_cast<SliceType>(type) || IsStorageNode(argument)))
			{
				if (auto root = RootVariable(argument, nullptr); root && root->Variable)
					lent.insert(MoveRoot(root->Variable.get()));
			}
		}

		for (size_t i = firstCandidate; i < m_Copies.Candidates.size(); i++)
		{
			if (lent.contains(m_Copies.Candidates[i].Variable))
				m_Copies.Candidates[i].Valid = false;
		}
	}

	// the nodes a statement evaluates, as code generation reaches them (a node shared by two places counts twice).
	// Nested blocks are statements of their own, and lambdas are separate functions
	template <typename F>
	static void ForEachEvaluated(const std::shared_ptr<ASTNodeBase>& node, std::unordered_set<ASTNodeBase*>& once, F&& visit)
	{
		if (!node)
			return;

		auto recurse = [&](const std::shared_ptr<ASTNodeBase>& child) { ForEachEvaluated(child, once, visit); };

		switch (node->GetType())
		{
			case ASTNodeType::Block:
			case ASTNodeType::Lambda:
			case ASTNodeType::Defer:
			case ASTNodeType::FunctionDefinition:
			case ASTNodeType::Class:
			case ASTNodeType::GenericTemplate:
				return;
			case ASTNodeType::Once:
				// computed the first time it is reached, later places reuse the value
				if (!once.insert(node.get()).second)
					return;
				break;
			default:
				break;
		}

		if (!visit(node))
			return;

		switch (node->GetType())
		{
			case ASTNodeType::BinaryExpression:
			{
				auto n = std::dynamic_pointer_cast<ASTBinaryExpression>(node);
				recurse(n->LeftSide); recurse(n->RightSide);
				break;
			}
			case ASTNodeType::VariableDecleration:
				recurse(std::dynamic_pointer_cast<ASTVariableDeclaration>(node)->Initializer);
				break;
			case ASTNodeType::AssignmentOperator:
			{
				auto n = std::dynamic_pointer_cast<ASTAssignmentOperator>(node);
				recurse(n->Value);

				// x = v only writes x once v is made, x += v reads it (on a class too: a = a + b, reading a through a slot)
				if (n->GetAssignType() != AssignmentOperatorType::Normal || n->CompoundTarget || n->Storage->GetType() != ASTNodeType::Variable)
					recurse(n->Storage);
				break;
			}
			case ASTNodeType::FunctionCall:
			{
				auto n = std::dynamic_pointer_cast<ASTFunctionCall>(node);
				recurse(n->Callee);
				for (auto& argument : n->Arguments) recurse(argument);
				for (auto& [name, argument] : n->KeywordArguments) recurse(argument);
				break;
			}
			case ASTNodeType::Subscript:
			{
				auto n = std::dynamic_pointer_cast<ASTSubscript>(node);
				recurse(n->Target);
				for (auto& argument : n->SubscriptArgs) recurse(argument);
				break;
			}
			case ASTNodeType::SliceExpr:
			{
				auto n = std::dynamic_pointer_cast<ASTSliceExpr>(node);
				recurse(n->Target); recurse(n->Start); recurse(n->End);
				break;
			}
			case ASTNodeType::ListExpr:
				for (auto& value : std::dynamic_pointer_cast<ASTListExpr>(node)->Values) recurse(value);
				break;
			case ASTNodeType::StructExpr:
				for (auto& value : std::dynamic_pointer_cast<ASTStructExpr>(node)->Values) recurse(value);
				break;
			case ASTNodeType::ReturnStatement:
				recurse(std::dynamic_pointer_cast<ASTReturn>(node)->ReturnValue);
				break;
			case ASTNodeType::UnaryExpression:
				recurse(std::dynamic_pointer_cast<ASTUnaryExpression>(node)->Operand);
				break;
			case ASTNodeType::Load:
				recurse(std::dynamic_pointer_cast<ASTLoad>(node)->Operand);
				break;
			case ASTNodeType::IfExpression:
				for (auto& block : std::dynamic_pointer_cast<ASTIfExpression>(node)->ConditionalBlocks) recurse(block.Condition);
				break;
			case ASTNodeType::WhileLoop:
				recurse(std::dynamic_pointer_cast<ASTWhileExpression>(node)->WhileBlock.Condition);
				break;
			case ASTNodeType::ForLoop:
			{
				auto n = std::dynamic_pointer_cast<ASTForExpression>(node);
				recurse(n->Start); recurse(n->End); recurse(n->Iterable);
				break;
			}
			case ASTNodeType::TernaryExpression:
			{
				auto n = std::dynamic_pointer_cast<ASTTernaryExpression>(node);
				recurse(n->Condition); recurse(n->Truthy); recurse(n->Falsy);
				break;
			}
			case ASTNodeType::DefaultArgument:
				recurse(std::dynamic_pointer_cast<ASTDefaultArgument>(node)->Value);
				break;
			case ASTNodeType::Switch:
			{
				auto n = std::dynamic_pointer_cast<ASTSwitch>(node);
				recurse(n->Value);
				for (auto& switchCase : n->Cases) for (auto& value : switchCase.Values) recurse(value);
				break;
			}
			case ASTNodeType::Temporary:
				recurse(std::dynamic_pointer_cast<ASTTemporary>(node)->Operand);
				break;
			case ASTNodeType::Construct:
			{
				auto n = std::dynamic_pointer_cast<ASTConstruct>(node);
				recurse(n->Initial); recurse(n->InitCall);
				break;
			}
			case ASTNodeType::Assert:
			{
				auto n = std::dynamic_pointer_cast<ASTAssert>(node);
				recurse(n->Condition); recurse(n->Message);
				break;
			}
			case ASTNodeType::Contains:
			{
				auto n = std::dynamic_pointer_cast<ASTContains>(node);
				recurse(n->Needle); recurse(n->Haystack);
				break;
			}
			case ASTNodeType::Intrinsic:
				for (auto& argument : std::dynamic_pointer_cast<ASTIntrinsic>(node)->Arguments) recurse(argument);
				break;
			case ASTNodeType::TupleExpr:
				for (auto& value : std::dynamic_pointer_cast<ASTTupleExpr>(node)->Values) recurse(value);
				break;
			case ASTNodeType::TupleGet:
				recurse(std::dynamic_pointer_cast<ASTTupleGet>(node)->Tuple);
				break;
			case ASTNodeType::Sequence:
				for (auto& child : std::dynamic_pointer_cast<ASTSequence>(node)->Children) recurse(child);
				break;
			case ASTNodeType::Destructure:
			{
				auto n = std::dynamic_pointer_cast<ASTDestructure>(node);
				recurse(n->Value);
				for (auto& target : n->Targets) recurse(target);
				break;
			}
			case ASTNodeType::Once:
				recurse(std::dynamic_pointer_cast<ASTOnce>(node)->Operand);
				break;
			case ASTNodeType::Move:
				recurse(std::dynamic_pointer_cast<ASTMove>(node)->Value);
				break;
			case ASTNodeType::Copy:
				recurse(std::dynamic_pointer_cast<ASTCopy>(node)->Value);
				break;
			case ASTNodeType::Destroy:
				recurse(std::dynamic_pointer_cast<ASTDestroy>(node)->Pointer);
				break;
			case ASTNodeType::Yield:
				recurse(std::dynamic_pointer_cast<ASTYield>(node)->Value);
				break;
			case ASTNodeType::Await:
				recurse(std::dynamic_pointer_cast<ASTAwait>(node)->Operand);
				break;
			case ASTNodeType::VariantConstruct:
				for (auto& value : std::dynamic_pointer_cast<ASTVariantConstruct>(node)->Values) recurse(value);
				break;
			case ASTNodeType::VariantField:
				recurse(std::dynamic_pointer_cast<ASTVariantField>(node)->Subject);
				break;
			case ASTNodeType::VariantTag:
				recurse(std::dynamic_pointer_cast<ASTVariantTag>(node)->Subject);
				break;
			case ASTNodeType::OptionalUnwrap:
				recurse(std::dynamic_pointer_cast<ASTOptionalUnwrap>(node)->Subject);
				break;
			case ASTNodeType::OptionalValueOr:
			{
				auto n = std::dynamic_pointer_cast<ASTOptionalValueOr>(node);
				recurse(n->Subject); recurse(n->Default);
				break;
			}
			case ASTNodeType::UnionConstruct:
				recurse(std::dynamic_pointer_cast<ASTUnionConstruct>(node)->Value);
				break;
			case ASTNodeType::CastExpr:
				recurse(std::dynamic_pointer_cast<ASTCastExpr>(node)->Object);
				break;
			case ASTNodeType::IsExpr:
				recurse(std::dynamic_pointer_cast<ASTIsExpr>(node)->Object);
				break;
			default:
				break;
		}
	}

	void Sema::CheckStatementUses(const std::shared_ptr<ASTNodeBase>& statement, size_t firstCandidate)
	{
		// print(k, size(k)), m[k] += 1, a * grow(a): the last use of k may not take it while another use in the
		// same statement still reads it (before or after, the order arguments are evaluated in does not matter)
		if (firstCandidate >= m_Copies.Candidates.size() || !statement)
			return;

		// each place a local is reached, in the order the code runs: through a copy (finished before anything
		// later runs) or anything else (a read or a pointer that may still be in use when a later part runs)
		struct Use { Symbol* Variable; ASTCopy* Copy; };
		std::vector<Use> order;
		std::unordered_map<ASTCopy*, size_t> reached;
		std::unordered_set<ASTNodeBase*> once;

		auto localOf = [&](std::shared_ptr<ASTNodeBase> node) -> Symbol*
		{
			if (auto unwrap = std::dynamic_pointer_cast<ASTOptionalUnwrap>(node))
				node = unwrap->Subject;

			auto load = std::dynamic_pointer_cast<ASTLoad>(node);
			auto variable = load ? std::dynamic_pointer_cast<ASTVariable>(load->Operand) : nullptr;
			Symbol* symbol = variable && variable->Variable ? MoveRoot(variable->Variable.get()) : nullptr;
			return symbol && m_LocalVariables.contains(symbol) ? symbol : nullptr;
		};

		ForEachEvaluated(statement, once, [&](const std::shared_ptr<ASTNodeBase>& node)
		{
			if (node->GetType() == ASTNodeType::Variable)
			{
				auto variable = std::static_pointer_cast<ASTVariable>(node);
				Symbol* symbol = variable->Variable ? MoveRoot(variable->Variable.get()) : nullptr;

				if (symbol && m_LocalVariables.contains(symbol))
					order.push_back(Use { symbol, nullptr });
			}
			else if (node->GetType() == ASTNodeType::Copy)
			{
				auto copy = static_cast<ASTCopy*>(node.get());
				reached[copy]++;

				if (Symbol* local = localOf(copy->Value))
				{
					order.push_back(Use { local, copy });
					return false;
				}
			}

			return true;
		});

		for (size_t i = firstCandidate; i < m_Copies.Candidates.size(); i++)
		{
			auto& candidate = m_Copies.Candidates[i];
			auto copy = reached.find(candidate.Copy.get());

			if (copy == reached.end())
				continue;

			// the same copy reached twice (m[k] += 1 passes k to get and to set): the first would empty k for the second
			if (copy->second > 1)
			{
				candidate.Valid = false;
				continue;
			}

			// (e, e): copies of e made before this one are finished, any other use of e is not
			bool before = true;

			for (auto& use : order)
			{
				if (use.Copy == candidate.Copy.get())
				{
					before = false;
					continue;
				}

				if (use.Variable == candidate.Variable && (!use.Copy || !before))
				{
					candidate.Valid = false;
					break;
				}
			}
		}
	}

	void Sema::MarkMovedFrom(const std::shared_ptr<ASTNodeBase>& storage)
	{
		// the variable gets a drop flag: once its value is moved out, it is not cleaned up (nor its destruct run)
		if (auto variable = std::dynamic_pointer_cast<ASTVariable>(storage); variable && variable->Variable)
		{
			if (auto decl = m_LocalDeclarations.find(variable->Variable.get()); decl != m_LocalDeclarations.end())
				decl->second->MovedFrom = true;
		}
	}

	void Sema::NeverMove(const std::shared_ptr<ASTNodeBase>& node)
	{
		if (auto root = RootVariable(node, nullptr); root && root->Variable)
			m_Copies.NeverMove.insert(MoveRoot(root->Variable.get()));
	}

	void Sema::FinishCopies()
	{
		// views first: a variable that only looks at the item is never moved from, and is not a copy
		for (auto& view : m_Copies.Views)
		{
			Symbol* variable = view.Variable;

			if (m_Copies.NeverMove.contains(variable) || m_Copies.NeverMove.contains(view.Source) || m_Copies.NotViewable.contains(variable) ||
				!m_Copies.Writes[variable].empty())
				continue;

			// the place it came from must not change while it is in use
			size_t last = m_Copies.LastUse.contains(variable) ? m_Copies.LastUse[variable] : view.Since;
			auto& writes = m_Copies.Writes[view.Source];

			if (std::any_of(writes.begin(), writes.end(), [&](size_t clock) { return clock > view.Since && clock <= last; }))
				continue;

			auto address = AddressOfRead(view.Copy->Value);

			if (!address)
				continue;

			view.Declaration->Initializer = address;
			view.Declaration->IsAlias = true;
			view.Copy->Elided = true;
			m_Copies.NeverMove.insert(variable);
		}

		// a copy whose variable is not used after it (and is not looked into by a pointer, a defer or a lambda)
		// can take the value instead: nobody would see the difference, and nothing is allocated
		for (auto& candidate : m_Copies.Candidates)
		{
			if (!candidate.Valid || candidate.Copy->Elided || m_Copies.NeverMove.contains(candidate.Variable) || 
				(m_Copies.Uses[candidate.Variable] != candidate.Use && !candidate.Final))
				continue;

			candidate.Copy->MoveFrom = candidate.Storage;
			MarkMovedFrom(candidate.Storage);
		}

		// clearc --copies: say where the rest are (in the program, not the standard library)
		if (!m_Module->ReportCopies)
			return;

		for (auto& copy : m_Copies.Copies)
		{
			if (copy->MoveFrom || copy->Elided)
				continue;

			Token location = GetNodeLocation(copy->Value);

			if (location.GetSourceFile().empty() || location.GetSourceFile().string().starts_with(CLEAR_STANDARD_DIR))
				continue;

			size_t width = location.GetData().size();
			location.SetData(GetDisplayName(copy->ValueType));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::None, location, DiagnosticCode_CopyMade, std::max<size_t>(width, 1));
		}
	}

	// if/else and switch: each branch starts from the same state, afterwards a variable is moved if any branch that carries on moved it
	Sema::BranchMoves Sema::BeginBranches()
	{
		BranchMoves branches { m_Moved, {}, false };
		branches.StaleStart = m_StalePointers;
		return branches;
	}

	void Sema::BeginBranch(BranchMoves& branches)
	{
		m_Moved = branches.Start;
		m_StalePointers = branches.StaleStart;
		m_Unreachable = false;
	}

	void Sema::EndBranch(BranchMoves& branches)
	{
		// a branch that ends in return/break/continue does not reach what follows: what it changed does not count there
		if (m_Unreachable)
			return;

		branches.Out.insert(m_Moved.begin(), m_Moved.end());
		branches.StaleOut.insert(m_StalePointers.begin(), m_StalePointers.end());
		branches.AnyLive = true;
	}

	void Sema::EndBranches(BranchMoves& branches, bool fallsThrough)
	{
		if (fallsThrough)
		{
			branches.Out.insert(branches.Start.begin(), branches.Start.end());
			branches.StaleOut.insert(branches.StaleStart.begin(), branches.StaleStart.end());
			branches.AnyLive = true;
		}

		m_Moved = std::move(branches.Out);
		m_StalePointers = std::move(branches.StaleOut);
		m_Unreachable = !branches.AnyLive;
	}

	void Sema::BeginLoop()
	{
		m_LoopMoves.push_back(LoopMoves { m_LocalCounter, {}, {}, m_Copies.Candidates.size() });
	}

	void Sema::EndLoop(const MovedSet& beforeLoop)
	{
		LoopMoves loop = std::move(m_LoopMoves.back());
		m_LoopMoves.pop_back();

		// a copy of a variable from outside the loop is not its last use: the next time round uses it again
		for (size_t i = loop.FirstCandidate; i < m_Copies.Candidates.size(); i++)
		{
			auto order = m_LocalOrder.find(m_Copies.Candidates[i].Variable);

			if ((order == m_LocalOrder.end() || order->second < loop.FirstLocal) && !m_Copies.Candidates[i].Final)
				m_Copies.Candidates[i].Valid = false;
		}

		// whatever is still moved when the body starts again was moved by the previous time round
		MovedSet atEnd = m_Unreachable ? MovedSet {} : m_Moved;
		atEnd.insert(loop.AtContinue.begin(), loop.AtContinue.end());

		for (auto& [variable, where] : atEnd)
		{
			auto order = m_LocalOrder.find(variable);

			if (beforeLoop.contains(variable) || order == m_LocalOrder.end() || order->second >= loop.FirstLocal)
				continue;

			Token location = where;
			size_t width = location.GetData().size();
			location.SetData(std::format("‘{}’", location.GetData()));
			Report(DiagnosticCode_MovedInLoop, location, width);
		}

		// after the loop: it may not have run at all, or ended at the condition or at a break
		MovedSet after = beforeLoop;
		after.insert(atEnd.begin(), atEnd.end());
		after.insert(loop.AtBreak.begin(), loop.AtBreak.end());
		m_Moved = std::move(after);
		m_Unreachable = false;
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitBinaryExprArithmetic(std::shared_ptr<ASTBinaryExpression> binaryExpression, SemaContext context)
	{
		context.ValueReq = ValueRequired::RValue;

		binaryExpression->LeftSide = Visit(binaryExpression->LeftSide, context);
		binaryExpression->RightSide = Visit(binaryExpression->RightSide, context);

		if (!binaryExpression->LeftSide || !binaryExpression->RightSide)
			return nullptr;

		if (binaryExpression->GetExpression() == OperatorType::Add)
		{
			if (auto text = TextConcat(binaryExpression->LeftSide, binaryExpression->RightSide, binaryExpression->Location))
				return text;
		}

		AdaptLiterals(binaryExpression, context);

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

	static std::string OperatorWord(const char* dunder)
	{
		// __add__ -> add: how operators are written in Clear
		static const std::unordered_map<std::string, std::string> words = {
			{ "__add__", "add" }, { "__sub__", "subtract" }, { "__mul__", "multiply" }, { "__div__", "divide" }, { "__mod__", "modulo" },
			{ "__pow__", "power" }, { "__eq__", "equals" }, { "__ne__", "not_equals" }, { "__lt__", "less" }, { "__le__", "less_equal" },
			{ "__gt__", "greater" }, { "__ge__", "greater_equal" },
		};

		if (!dunder)
			return "?";

		auto found = words.find(dunder);
		return found != words.end() ? found->second : std::string(dunder);
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

		// *p: the object p points at (its own operators run on it, not on a copy of its Base part)
		if (auto deref = std::dynamic_pointer_cast<ASTUnaryExpression>(node); deref && deref->GetOperatorType() == OperatorType::Dereference)
		{
			if (auto pointer = m_TypeInferEngine.InferTypeFromNode(deref->Operand); pointer && pointer->IsPointer())
				return deref->Operand;
		}

		auto temporary = std::make_shared<ASTTemporary>();
		temporary->Operand = node;
		temporary->ValueType = m_TypeInferEngine.InferTypeFromNode(node);
		temporary->Location = GetNodeLocation(node);
		temporary->DestroyAtScopeEnd = IsFreshValue(node) && IsOwning(temporary->ValueType);
		return temporary;
	}

	std::optional<std::shared_ptr<ASTNodeBase>> Sema::TryOperatorOverload(std::shared_ptr<ASTBinaryExpression> expr)
	{
		auto lhsType = m_TypeInferEngine.InferTypeFromNode(expr->LeftSide);
		const char* name = GetDunderName(expr->GetExpression());

		// self + self in a method (self is a *V): the V it points at, when V defines the operator and a pointer
		// has no such operator itself (p + 1 and p == q stay pointer arithmetic and comparison)
		if (lhsType && lhsType->IsPointer() && name)
		{
			auto pointee = lhsType->As<PointerType>()->GetBaseType();
			auto rhsType = m_TypeInferEngine.InferTypeFromNode(expr->RightSide);
			auto op = expr->GetExpression();
			bool pointerOperation = op == OperatorType::IsEqual || op == OperatorType::NotEqual || op == OperatorType::LessThan ||
									op == OperatorType::LessThanEqual || op == OperatorType::GreaterThan || op == OperatorType::GreaterThanEqual ||
									((op == OperatorType::Add || op == OperatorType::Sub) && rhsType && rhsType->IsIntegral());

			if (!pointerOperation && pointee && pointee->IsClass() && pointee->As<ClassType>()->MemberFunctions.contains(name))
			{
				auto dereference = [&](std::shared_ptr<ASTNodeBase> pointer)
				{
					auto value = std::make_shared<ASTUnaryExpression>(OperatorType::Dereference);
					value->Location = GetNodeLocation(pointer);
					value->Operand = pointer;
					return value;
				};

				expr->LeftSide = dereference(expr->LeftSide);

				if (rhsType && rhsType->IsPointer() && rhsType->As<PointerType>()->GetBaseType() == pointee)
					expr->RightSide = dereference(expr->RightSide);

				lhsType = pointee;
			}
		}

		if (!lhsType || !lhsType->IsClass())
			return std::nullopt;

		auto classType = lhsType->As<ClassType>();
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
			// as written in Clear (operator add), and for an optional: take the value out first
			std::string word = OperatorWord(name);

			if (classType->IsOptional)
				location.SetData(std::format("‘{}’ is optional, so it may hold no value: use its value with `x ?? fallback`, `if x:` or `.value` first", 
											 GetDisplayName(classType)));
			else
				location.SetData(std::format("‘{}’ has no ‘operator {}’ for ‘{}’. Define it in the class: operator {}(self, other: ...)", 
											 GetDisplayName(classType), word, GetOperatorSpelling(expr->GetExpression()), word));

			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_MissingOperatorOverload, 1);
			return std::shared_ptr<ASTNodeBase>(nullptr);
		}

		auto function = method->GetFunctionSymbol().FunctionNode;
		EnsureDefined(function);

		if (function->Arguments.size() != 2)
		{
			location.SetData(std::format("operator {}’ of ‘{}", OperatorWord(negate ? "__eq__" : name), GetDisplayName(classType)));
			m_DiagBuilder.Report(Stage::CodeGeneration, Severity::High, location, DiagnosticCode_BadOperatorSignature, 1);
			return std::shared_ptr<ASTNodeBase>(nullptr);
		}

		auto callee = std::make_shared<ASTVariable>(Token(TokenType::Identifier, negate ? "__eq__" : name, location.GetSourceFile(), location.LineNumber, location.ColumnNumber));
		callee->Variable = method;

		auto call = std::make_shared<ASTFunctionCall>();
		call->Location = location;
		call->Callee = callee;
		call->Arguments.push_back(AddressOf(expr->LeftSide));
		size_t firstCandidate = m_Copies.Candidates.size();
		struct Lent { Sema* S; std::shared_ptr<ASTFunctionCall> Call; size_t First; ~Lent() { S->KeepLentArguments(Call->Arguments, First); } } lent { this, call, firstCandidate };

		// the other operand is passed the way the method declares it: by value or by pointer
		auto otherType = function->Arguments[1]->ResolvedType;
		auto rhsType = m_TypeInferEngine.InferTypeFromNode(expr->RightSide);

		if (otherType && otherType->IsPointer() && rhsType && rhsType->IsClass())
			call->Arguments.push_back(AddressOf(expr->RightSide));
		else if (IsOwning(otherType) && !IsCopyable(otherType) && !IsFreshValue(expr->RightSide))
		{
			// a == b must not empty b: owning operands are taken by pointer
			Token where = GetNodeLocation(expr->RightSide);
			where.SetData(GetDisplayName(otherType));
			Report(DiagnosticCode_OwningOperatorArgument, where);
			return std::shared_ptr<ASTNodeBase>(nullptr);
		}
		else
			call->Arguments.push_back(Coerce(expr->RightSide, otherType));

		DispatchOnObject(call, classType);

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
		auto isText    = [](std::shared_ptr<Type> t) { return t->GetHash() == "str"; };

		// name == "ada" with name a String: compared as text (a String becomes a str without copying)
		bool comparison = expr->GetExpression() == OperatorType::IsEqual || expr->GetExpression() == OperatorType::NotEqual ||
						  expr->GetExpression() == OperatorType::LessThan || expr->GetExpression() == OperatorType::LessThanEqual ||
						  expr->GetExpression() == OperatorType::GreaterThan || expr->GetExpression() == OperatorType::GreaterThanEqual;

		// text == null: whether it points at any bytes (a str that was never set does not)
		if (comparison && isText(lhs) != isText(rhs) && (lhs->GetHash() == "opaque_ptr" || rhs->GetHash() == "opaque_ptr" || 
			lhs->GetHash() == "null" || rhs->GetHash() == "null" || isPointer(isText(lhs) ? rhs : lhs)))
		{
			auto& text = isText(lhs) ? expr->LeftSide : expr->RightSide;
			auto bytes = m_Module->GetTypeRegistry()->GetPointerTo(m_Module->Lookup("int8").value()->GetType());
			text = SliceIntrinsic("slice_data", bytes, { text }, GetNodeLocation(text));
			lhs = m_TypeInferEngine.InferTypeFromNode(expr->LeftSide);
			rhs = m_TypeInferEngine.InferTypeFromNode(expr->RightSide);
		}

		if (comparison && isText(lhs) != isText(rhs))
		{
			auto& other = isText(lhs) ? expr->RightSide : expr->LeftSide;
			auto text = isText(lhs) ? lhs : rhs;

			if (auto converted = Coerce(other, text); converted && m_TypeInferEngine.InferTypeFromNode(converted) == text)
			{
				other = converted;
				lhs = rhs = text;
			}
		}

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
				valid = (isNumber(lhs) && isNumber(rhs)) || (isPointer(lhs) && isPointer(rhs)) || (isText(lhs) && isText(rhs)) ||
						(isBool(lhs) && isBool(rhs)) || (lhs->IsEnum() && lhs->GetHash() == rhs->GetHash());
				break;
			case OperatorType::LessThan:
			case OperatorType::LessThanEqual:
			case OperatorType::GreaterThan:
			case OperatorType::GreaterThanEqual:
				valid = (isNumber(lhs) && isNumber(rhs)) || (isPointer(lhs) && isPointer(rhs)) || (isText(lhs) && isText(rhs)) ||
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

		// u > i with u: uint32 and i: int32: i is compared as unsigned, so -1 is bigger than everything (like -Wsign-compare).
		// Only when the unsigned type wins (the signed side is not wider), and not for a constant that is >= 0 (u > 0)
		bool signSensitive = comparison || expr->GetExpression() == OperatorType::Div || expr->GetExpression() == OperatorType::Mod;

		if (valid && signSensitive && isInteger(lhs) && isInteger(rhs) && lhs->IsSigned() != rhs->IsSigned() &&
			!IsStandardLibraryFile(expr->Location.GetSourceFile()))
		{
			auto& signedSide = lhs->IsSigned() ? expr->LeftSide : expr->RightSide;
			auto signedType = lhs->IsSigned() ? lhs : rhs, unsignedType = lhs->IsSigned() ? rhs : lhs;
			auto constant = EvaluateInteger(signedSide);

			if (signedType->Get()->getIntegerBitWidth() <= unsignedType->Get()->getIntegerBitWidth() && !(constant && *constant >= 0))
			{
				Token location = expr->Location.GetSourceFile().empty() ? GetNodeLocation(expr->LeftSide) : expr->Location;
				location.SetData(std::format("{} {} {}", GetDisplayName(lhs), GetOperatorSpelling(expr->GetExpression()), GetDisplayName(rhs)));
				Warn(DiagnosticCode_SignedUnsignedMix, location, 1);
			}
		}

		return valid;
	}

	std::shared_ptr<ASTNodeBase> Sema::ModuleMember(std::shared_ptr<ASTBinaryExpression> access)
	{
		// m.name where m is a module imported `as m`: the name as that module exposes it (null if m is not a module)
		auto left = std::dynamic_pointer_cast<ASTVariable>(access->LeftSide);

		if (!left)
			return nullptr;

		std::shared_ptr<Symbol> module = left->Variable;

		if (!module)
		{
			auto [entry, scope] = LookupSymbol(left->GetName().GetData());
			module = entry ? entry->Symbol : nullptr;
		}

		if (!module || module->Kind != SymbolKind::Module)
			return nullptr;

		auto& exposed = module->GetModule()->GetExposedSymbols();

		// m.twice!(x)
		if (auto macro = std::dynamic_pointer_cast<ASTMacroCall>(access->RightSide))
		{
			auto found = exposed.find(macro->Name.GetData());

			if (found == exposed.end() || found->second->Kind != SymbolKind::Macro)
			{
				Report(DiagnosticCode_UndeclaredIdentifier, macro->Name);
				return nullptr;
			}

			macro->ResolvedMacro = found->second;
			return macro;
		}

		auto name = std::dynamic_pointer_cast<ASTVariable>(access->RightSide);

		if (!name)
			return nullptr;

		auto found = exposed.find(name->GetName().GetData());

		if (found == exposed.end())
		{
			// g.NOPE: "‘g.NOPE’ is not defined"
			Token where = name->GetName();
			where.SetData(std::format("{}.{}", left->GetName().GetData(), name->GetName().GetData()));
			Report(DiagnosticCode_UndeclaredIdentifier, where, name->GetName().GetData().size());
			return nullptr;
		}

		auto member = std::make_shared<ASTVariable>(name->GetName());
		member->Variable = found->second;
		return member;
	}

	void Sema::ReportMissingMember(const Token& name, const std::shared_ptr<Type>& type)
	{
		if (auto classType = ClassOf(type); classType && m_BrokenClasses.contains(classType.get()))
			return; // the class itself was reported already

		// x.items with x: ?Box: the member is there, but only when x holds a value
		auto optional = ClassOf(type);
		auto valueType = optional && optional->As<ClassType>()->IsOptional ? OptionalValueType(optional) : nullptr;
		auto valueClass = valueType ? ClassOf(valueType) : nullptr;

		if (valueClass && (valueClass->As<ClassType>()->GetMember(name.GetData()) || valueClass->As<ClassType>()->MemberFunctions.contains(name.GetData())))
		{
			Token where = name;
			where.SetData(std::format("{}’ belongs to ‘{}’, but this is a ‘{}", name.GetData(), GetDisplayName(valueType), GetDisplayName(optional)));
			Report(DiagnosticCode_OptionalMember, where, name.GetData().size());
			return;
		}

		Report(DiagnosticCode_UnknownMember, name);
	}

	std::shared_ptr<ASTNodeBase> Sema::VisitBinaryExprMemberAccess(std::shared_ptr<ASTBinaryExpression> binaryExpr, SemaContext context)
	{
		bool insertLoad = context.ValueReq == ValueRequired::RValue;

		// x.value where x is narrowed (`if x is not none:`, after `if x is none: return`): still the optional's
		// value, exactly as without the narrowing (it may move the value out)
		if (auto left = std::dynamic_pointer_cast<ASTVariable>(binaryExpr->LeftSide), right = std::dynamic_pointer_cast<ASTVariable>(binaryExpr->RightSide);
			left && right && !left->Variable && right->GetName().GetData() == "value")
		{
			auto [entry, scope] = LookupSymbol(left->GetName().GetData());
			auto narrowed = entry ? m_NarrowedNames.find(entry->Symbol.get()) : m_NarrowedNames.end();
			auto valueClass = narrowed != m_NarrowedNames.end() ? std::dynamic_pointer_cast<ClassType>(OptionalValueType(narrowed->second.Optional)) : nullptr;
			bool ownValue = valueClass && (valueClass->GetMemberValueIndex("value") || valueClass->MemberFunctions.contains("value"));

			if (narrowed != m_NarrowedNames.end() && !ownValue && scope < m_ScopeStack.size())
			{
				SymbolEntry alias = *entry;
				m_ScopeStack[scope].Set(left->GetName().GetData(), SymbolEntry { SymbolEntryType::Variable, narrowed->second.Variable });
				auto result = VisitBinaryExprMemberAccess(binaryExpr, context);
				m_ScopeStack[scope].Set(left->GetName().GetData(), alias);
				return result;
			}
		}

		// m.NAME, m.N, m.twice!(x) through an import alias
		if (auto member = ModuleMember(binaryExpr))
		{
			if (member->GetType() == ASTNodeType::MacroCall)
				return Visit(member, context);

			auto variable = std::dynamic_pointer_cast<ASTVariable>(member);

			if (insertLoad && variable->Variable->Kind == SymbolKind::Value)
			{
				auto load = std::make_shared<ASTLoad>();
				load->Operand = variable;
				return load;
			}

			return variable;
		}

		// obj.field read as a value only reads obj (a method call or a write does not come through here as a value)
		context.ValueReq = ValueRequired::LValue;
		context.AssignmentTarget = false; // m[k].n = v changes the element m[k] gives, it does not call operator set
		bool wasReading = std::exchange(m_ReadingUse, insertLoad || m_ReadingUse);
		binaryExpr->LeftSide = Visit(binaryExpr->LeftSide, context);
		m_ReadingUse = wasReading;

		if (std::shared_ptr<ASTVariable> var = std::dynamic_pointer_cast<ASTVariable>(binaryExpr->LeftSide); var && var->Variable->Kind == SymbolKind::Module)
		{
			std::shared_ptr<ASTVariable> member = std::dynamic_pointer_cast<ASTVariable>(binaryExpr->RightSide);
			auto& exposed = var->Variable->GetModule()->GetExposedSymbols();
			auto found = member ? exposed.find(member->GetName().GetData()) : exposed.end();

			// g.NOPE: not something the module has (reported once, with its location)
			if (found == exposed.end())
			{
				Token where = member ? member->GetName() : GetNodeLocation(binaryExpr->RightSide);
				where.SetData(std::format("{}.{}", var->GetName().GetData(), where.GetData()));
				Report(DiagnosticCode_UndeclaredIdentifier, where, member ? member->GetName().GetData().size() : 1);
				return nullptr;
			}

			std::shared_ptr<Symbol> symbol = found->second;
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

		// work().run(): the new task is kept in a temporary that is cleaned up at the end of the block
		if (std::dynamic_pointer_cast<CoroutineType>(lhsType) && IsFreshValue(binaryExpr->LeftSide))
		{
			auto load = std::make_shared<ASTLoad>();
			load->Operand = AddressOf(binaryExpr->LeftSide);
			binaryExpr->LeftSide = load;
		}

		// make_list().length: the new list is cleaned up at the end of the block
		if (IsOwning(lhsType) && IsFreshValue(binaryExpr->LeftSide) && !std::dynamic_pointer_cast<CoroutineType>(lhsType))
		{
			binaryExpr->LeftSide = AddressOf(binaryExpr->LeftSide);
			lhsType = m_TypeInferEngine.InferTypeFromNode(binaryExpr->LeftSide);
		}

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
				ReportMissingMember(member ? member->GetName() : GetNodeLocation(binaryExpr->RightSide), lhsType);
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
			ReportMissingMember(member->GetName(), lhsType);
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

		// `a and a > 2`, `not a or a < 0`: the right side only runs when the left says a holds a value
		std::vector<Narrowing> narrowings;

		if (binaryExpr->GetExpression() == OperatorType::And || binaryExpr->GetExpression() == OperatorType::Or)
			CollectNarrowings(binaryExpr->LeftSide, binaryExpr->GetExpression() == OperatorType::And, narrowings);

		binaryExpr->LeftSide = Visit(binaryExpr->LeftSide, context);

		if (narrowings.empty())
		{
			binaryExpr->RightSide = Visit(binaryExpr->RightSide, context);
		}
		else if (binaryExpr->LeftSide)
		{
			auto sequence = std::make_shared<ASTSequence>();
			sequence->Location = binaryExpr->RightSide->Location;
			m_ScopeStack.emplace_back();

			for (auto& narrowing : narrowings)
			{
				if (auto declaration = Visit(NarrowedDeclaration(narrowing), context))
					sequence->Children.push_back(declaration);
			}

			auto right = TestCondition(Visit(binaryExpr->RightSide, context), true);
			m_ScopeStack.pop_back();

			sequence->Value = right;
			binaryExpr->RightSide = right ? sequence : nullptr;
		}

		if (!binaryExpr->LeftSide || !binaryExpr->RightSide)
			return nullptr;

		// a and flag, not x or not y: an optional operand means "holds a value", like in a condition
		if (binaryExpr->GetExpression() == OperatorType::And || binaryExpr->GetExpression() == OperatorType::Or)
		{
			binaryExpr->LeftSide = TestCondition(binaryExpr->LeftSide, true);
			binaryExpr->RightSide = TestCondition(binaryExpr->RightSide, true);

			if (!binaryExpr->LeftSide || !binaryExpr->RightSide)
				return nullptr;
		}

		if (binaryExpr->GetExpression() != OperatorType::And && binaryExpr->GetExpression() != OperatorType::Or)
		{
			SemaContext comparison = context;
			comparison.ExpectedType = nullptr; // a comparison's operands are not the bool it produces
			AdaptLiterals(binaryExpr, comparison);

			// name == "ada", "ada" < name: compared as text (a String becomes a str view, nothing is copied)
			auto left = m_TypeInferEngine.InferTypeFromNode(binaryExpr->LeftSide);
			auto right = m_TypeInferEngine.InferTypeFromNode(binaryExpr->RightSide);
			bool leftText = left && left->GetHash() == "str", rightText = right && right->GetHash() == "str";

			if (leftText != rightText && left && right && !left->IsPointer() && !right->IsPointer())
			{
				auto& other = leftText ? binaryExpr->RightSide : binaryExpr->LeftSide;
				auto text = leftText ? left : right;

				if (auto converted = Coerce(other, text); converted && m_TypeInferEngine.InferTypeFromNode(converted) == text)
					other = converted;
			}
		}

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
		if (!node)
			return nullptr; // its error was reported where it failed

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

				return nullptr; // -x, not x: a value, not a type
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

		// found as the instance itself (a class another file made and exposed) or as its generic record
		if (instanceSymbol)
			return instanceSymbol->Kind == SymbolKind::Generic ? instanceSymbol->GetGeneric().GeneratedSymbol : instanceSymbol;

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
		
		for (size_t i = 0; i < substitutedArgs.size() && i < node->GenericTypeNames.size(); i++)
		{
			cloner.SubstitutionMap[node->GenericTypeNames[i]] = substitutedArgs[i]; 
		}

		// apply_to[int, int](...): a `function(T) -> U` parameter's own type follows from the types written
		if (auto callables = m_CallablePatterns.find(node.get()); callables != m_CallablePatterns.end())
		{
			for (size_t i = substitutedArgs.size(); i < node->GenericTypeNames.size(); i++)
			{
				auto pattern = callables->second.find(node->GenericTypeNames[i]);

				if (pattern == callables->second.end())
					continue;

				Cloner patternCloner;
				patternCloner.DestinationModule = m_Module;
				patternCloner.SubstitutionMap = cloner.SubstitutionMap;

				auto resolved = Visit(patternCloner.Clone(pattern->second), SemaContext { .ValueReq = ValueRequired::Any });
				auto type = resolved ? GetTypeFromNode(resolved) : nullptr;

				if (!type)
					return nullptr;

				cloner.SubstitutionMap[node->GenericTypeNames[i]] = Symbol::CreateType(type);
			}
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
