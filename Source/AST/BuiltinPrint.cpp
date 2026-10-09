#include "ASTNode.h"

#include "Symbols/Module.h"

#include <llvm/IR/Intrinsics.h>
#include <optional>

namespace clear
{
	// Builds a single printf call whose format string is decided at compile time from the
	// argument types. Floats are the exception: Python prints 5.0 as "5.0" but 0.1 as "0.1",
	// which needs a runtime choice of format, so each float gets its own small printf call.
	class PrintBuilder
	{
	public:
		PrintBuilder(CodegenContext& ctx)
			: m_Ctx(ctx)
		{
			llvm::FunctionType* printfType = llvm::FunctionType::get(ctx.Builder.getInt32Ty(), { ctx.Builder.getPtrTy() }, true);
			m_Printf = ctx.Module.getOrInsertFunction("printf", printfType);
		}

		void Text(llvm::StringRef text)
		{
			for (char c : text)
			{
				if (c == '%')
					m_Format += "%%";
				else
					m_Format += c;
			}
		}

		void Value(llvm::Value* value, std::shared_ptr<Type> type)
		{
			// lists, maps, classes and enums are printed by a function per type, which can call itself for a type
			// that contains itself (class Node: kids: List[Node])
			if (IsComposite(value, type))
			{
				llvm::Function* printer = PrinterFor(type);
				Flush();
				Symbol slot = CreateStackSlot(type, value);
				m_Ctx.Builder.CreateCall(printer, { slot.GetLLVMValue() });
				return;
			}

			Inline(value, type);
		}

		static bool IsComposite(llvm::Value* value, const std::shared_ptr<Type>& type)
		{
			return value->getType()->isStructTy() && type && type->IsClass();
		}

		// void clear.print.<type>(ptr): prints the value at the address
		llvm::Function* PrinterFor(const std::shared_ptr<Type>& type)
		{
			std::string name = std::format("clear.print.{}", type->GetHash());
			llvm::Module& module = m_Ctx.Module;

			if (llvm::Function* existing = module.getFunction(name))
				return existing;

			auto& builder = m_Ctx.Builder;
			auto function = llvm::Function::Create(llvm::FunctionType::get(builder.getVoidTy(), { builder.getPtrTy() }, false),
												   llvm::Function::InternalLinkage, name, module);

			llvm::IRBuilderBase::InsertPointGuard guard(builder);
			builder.SetInsertPoint(llvm::BasicBlock::Create(m_Ctx.Context, "entry", function));

			PrintBuilder body(m_Ctx);
			body.Inline(builder.CreateLoad(type->Get(), function->getArg(0)), type);
			body.Flush();
			builder.CreateRetVoid();
			return function;
		}

		void Inline(llvm::Value* value, std::shared_ptr<Type> type)
		{
			auto& builder = m_Ctx.Builder;
			llvm::Type* llvmType = value->getType();

			// str: its length says where it ends (part of a text has no zero after it)
			if (type && type->GetHash() == "str")
			{
				m_Format += "%.*s";
				m_Args.push_back(builder.CreateTrunc(builder.CreateExtractValue(value, 1), builder.getInt32Ty()));
				m_Args.push_back(builder.CreateExtractValue(value, 0));
			}
			else if (llvmType->isStructTy() && type && type->IsClass() && type->As<ClassType>()->GenericOrigin == "List" && Field(type, "data") && Field(type, "length"))
			{
				// [1, 2, 3]
				auto classType = type->As<ClassType>();
				Items(builder.CreateExtractValue(value, { *Field(type, "data") }), builder.CreateExtractValue(value, { *Field(type, "length") }), classType->GenericArguments[0]);
			}
			else if (llvmType->isStructTy() && type && type->IsClass() && type->As<ClassType>()->GenericOrigin == "Map" && Field(type, "keys") && Field(type, "states"))
			{
				// {ada: 36, alan: 41}
				auto classType = type->As<ClassType>();
				Entries(builder.CreateExtractValue(value, { *Field(type, "keys") }), builder.CreateExtractValue(value, { *Field(type, "values") }),
						builder.CreateExtractValue(value, { *Field(type, "states") }), builder.CreateExtractValue(value, { *Field(type, "capacity") }),
						classType->GenericArguments[0], classType->GenericArguments[1]);
			}
			else if (type && type->IsEnum())
			{
				// Color.Red rather than 0
				auto enumType = std::dynamic_pointer_cast<EnumType>(type);
				llvm::Value* name = String(std::format("{}(?)", enumType->GetHash()));

				for (const auto& [member, constant] : enumType->GetValues())
				{
					llvm::Value* matches = builder.CreateICmpEQ(value, llvm::ConstantInt::get(llvmType, constant, true));
					name = builder.CreateSelect(matches, String(std::format("{}.{}", enumType->GetHash(), member)), name);
				}

				m_Format += "%s";
				m_Args.push_back(name);
			}
			else if (llvmType->isIntegerTy(1))
			{
				m_Format += "%s";
				m_Args.push_back(builder.CreateSelect(value, String("true"), String("false")));
			}
			else if (llvmType->isIntegerTy())
			{
				bool isSigned = type && type->IsSigned();
				m_Format += isSigned ? "%lld" : "%llu";
				m_Args.push_back(isSigned ? builder.CreateSExt(value, builder.getInt64Ty()) : builder.CreateZExt(value, builder.getInt64Ty()));
			}
			else if (llvmType->isFloatingPointTy())
			{
				Float(value);
			}
			else if (llvmType->isPointerTy())
			{
				bool isString = type && type->IsPointer() && type->As<PointerType>()->GetBaseType() &&
								type->As<PointerType>()->GetBaseType()->GetHash() == "int8";

				m_Format += isString ? "%s" : "%p";
				m_Args.push_back(value);
			}
			else if (llvmType->isArrayTy())
			{
				auto elementType = type ? type->As<ArrayType>()->GetBaseType() : nullptr;

				Text("[");

				for (uint64_t i = 0; i < llvmType->getArrayNumElements(); i++)
				{
					if (i > 0) Text(", ");
					Value(builder.CreateExtractValue(value, { (unsigned)i }), elementType);
				}

				Text("]");
			}
			else if (llvmType->isStructTy() && type && type->IsTuple())
			{
				auto& elements = type->As<TupleType>()->GetElements();
				Text("(");

				for (unsigned i = 0; i < elements.size(); i++)
				{
					if (i > 0) Text(", ");
					Value(builder.CreateExtractValue(value, { i }), elements[i]);
				}

				Text(")");
			}
			else if (llvmType->isStructTy() && type && type->IsClass() && (type->As<ClassType>()->IsVariant || type->As<ClassType>()->IsUnion))
			{
				Variant(value, type->As<ClassType>());
			}
			else if (llvmType->isStructTy() && type && type->IsClass() && PrintsItself(type->As<ClassType>()))
			{
				// operator str (a String inside a list, an array or a field prints its text)
				auto classType = type->As<ClassType>();
				auto node = classType->MemberFunctions.at("__str__")->GetFunctionSymbol().FunctionNode;
				llvm::Function* method = GetFunctionHere(classType->MemberFunctions.at("__str__"), m_Ctx);
				auto result = node->ReturnTypeVal;
				bool byPointer = node->Arguments[0]->ResolvedType && node->Arguments[0]->ResolvedType->IsPointer();
				llvm::Value* self = byPointer ? CreateStackSlot(classType, value).GetLLVMValue() : value;
				llvm::Value* text = builder.CreateCall(method, { self });

				// one that makes a String: printed, then cleaned up
				if (IsOwning(result))
				{
					Symbol made = CreateStackSlot(result, text);
					Value(text, result);
					Flush();
					EmitDestroy(m_Ctx, result, made.GetLLVMValue());
				}
				else
					Value(text, result);
			}
			else if (llvmType->isStructTy() && type && type->IsClass())
			{
				// dataclass style: Point(x=1, y=2)
				auto classType = type->As<ClassType>();
				Text(classType->GetHash());
				Text("(");

				unsigned index = 0;
				for (const auto& [name, memberType] : classType->GetMemberValues())
				{
					// the hidden vtable pointer is not part of the value
					if (index == 0 && classType->HasVTable)
					{
						index++;
						continue;
					}

					if (index > (classType->HasVTable ? 1u : 0u)) Text(", ");
					Text(name);
					Text("=");
					Value(builder.CreateExtractValue(value, { index }), memberType);
					index++;
				}

				Text(")");
			}
			else if (auto slice = std::dynamic_pointer_cast<SliceType>(type))
			{
				Items(builder.CreateExtractValue(value, 0), builder.CreateExtractValue(value, 1), slice->GetBaseType());
			}
			else
			{
				Text("<value>");
			}
		}

		static std::optional<unsigned> Field(const std::shared_ptr<Type>& type, const char* name)
		{
			auto index = type->As<ClassType>()->GetMemberValueIndex(name);
			return index ? std::optional<unsigned>((unsigned)*index) : std::nullopt;
		}

		// [a, b, c]: the length is only known at run time, so a loop printing one item at a time
		void Items(llvm::Value* data, llvm::Value* length, std::shared_ptr<Type> element)
		{
			auto& builder = m_Ctx.Builder;
			Text("[");
			Flush();

			llvm::Function* function = builder.GetInsertBlock()->getParent();
			llvm::BasicBlock* before = builder.GetInsertBlock();
			llvm::BasicBlock* check = llvm::BasicBlock::Create(m_Ctx.Context, "print.items", function);
			llvm::BasicBlock* body = llvm::BasicBlock::Create(m_Ctx.Context, "print.item", function);
			llvm::BasicBlock* done = llvm::BasicBlock::Create(m_Ctx.Context, "print.items_done", function);
			builder.CreateBr(check);

			builder.SetInsertPoint(check);
			llvm::PHINode* index = builder.CreatePHI(builder.getInt64Ty(), 2);
			index->addIncoming(builder.getInt64(0), before);
			builder.CreateCondBr(builder.CreateICmpSLT(index, length), body, done);

			builder.SetInsertPoint(body);
			m_Format += "%s";
			m_Args.push_back(builder.CreateSelect(builder.CreateICmpEQ(index, builder.getInt64(0)), String(""), String(", ")));
			Value(builder.CreateLoad(element->Get(), builder.CreateInBoundsGEP(element->Get(), data, { index })), element);
			Flush();
			index->addIncoming(builder.CreateAdd(index, builder.getInt64(1)), builder.GetInsertBlock());
			builder.CreateBr(check);

			builder.SetInsertPoint(done);
			Text("]");
		}

		// {key: value, ...}: the slots in use (state 1) of a Map's table
		void Entries(llvm::Value* keys, llvm::Value* values, llvm::Value* states, llvm::Value* capacity, std::shared_ptr<Type> keyType, std::shared_ptr<Type> valueType)
		{
			auto& builder = m_Ctx.Builder;
			Text("{");
			Flush();

			llvm::Function* function = builder.GetInsertBlock()->getParent();
			llvm::BasicBlock* before = builder.GetInsertBlock();
			llvm::BasicBlock* check = llvm::BasicBlock::Create(m_Ctx.Context, "print.slots", function);
			llvm::BasicBlock* slot = llvm::BasicBlock::Create(m_Ctx.Context, "print.slot", function);
			llvm::BasicBlock* entry = llvm::BasicBlock::Create(m_Ctx.Context, "print.entry", function);
			llvm::BasicBlock* next = llvm::BasicBlock::Create(m_Ctx.Context, "print.next", function);
			llvm::BasicBlock* done = llvm::BasicBlock::Create(m_Ctx.Context, "print.slots_done", function);
			builder.CreateBr(check);

			builder.SetInsertPoint(check);
			llvm::PHINode* index = builder.CreatePHI(builder.getInt64Ty(), 2);
			llvm::PHINode* printed = builder.CreatePHI(builder.getInt1Ty(), 2);
			index->addIncoming(builder.getInt64(0), before);
			printed->addIncoming(builder.getFalse(), before);
			builder.CreateCondBr(builder.CreateICmpSLT(index, capacity), slot, done);

			builder.SetInsertPoint(slot);
			llvm::Value* state = builder.CreateLoad(builder.getInt8Ty(), builder.CreateInBoundsGEP(builder.getInt8Ty(), states, { index }));
			builder.CreateCondBr(builder.CreateICmpEQ(state, builder.getInt8(1)), entry, next);

			builder.SetInsertPoint(entry);
			m_Format += "%s";
			m_Args.push_back(builder.CreateSelect(printed, String(", "), String("")));
			Value(builder.CreateLoad(keyType->Get(), builder.CreateInBoundsGEP(keyType->Get(), keys, { index })), keyType);
			Text(": ");
			Value(builder.CreateLoad(valueType->Get(), builder.CreateInBoundsGEP(valueType->Get(), values, { index })), valueType);
			Flush();
			llvm::BasicBlock* entryEnd = builder.GetInsertBlock();
			builder.CreateBr(next);

			builder.SetInsertPoint(next);
			llvm::PHINode* nowPrinted = builder.CreatePHI(builder.getInt1Ty(), 2);
			nowPrinted->addIncoming(printed, slot);
			nowPrinted->addIncoming(builder.getTrue(), entryEnd);
			index->addIncoming(builder.CreateAdd(index, builder.getInt64(1)), next);
			printed->addIncoming(nowPrinted, next);
			builder.CreateBr(check);

			builder.SetInsertPoint(done);
			Text("}");
		}

		// a class with an operator str that has been analysed (and takes just self)
		static bool PrintsItself(const std::shared_ptr<ClassType>& classType)
		{
			auto method = classType->MemberFunctions.find("__str__");

			if (method == classType->MemberFunctions.end())
				return false;

			auto node = method->second->GetFunctionSymbol().FunctionNode;
			// (a str, or a value that prints, such as a String, which is cleaned up after printing)
			return node && node->BodyResolved && node->Arguments.size() == 1 && node->ReturnTypeVal && node->ReturnTypeVal->GetHash() != classType->GetHash();
		}

		void Flush()
		{
			if (m_Format.empty())
				return;

			llvm::SmallVector<llvm::Value*> args = { String(m_Format) };
			args.append(m_Args.begin(), m_Args.end());

			m_Ctx.Builder.CreateCall(m_Printf, args);

			m_Format.clear();
			m_Args.clear();
		}

	private:
		// rich enums and optionals print the case they hold (Shape.Circle(radius=2.0), some value or none),
		// which is only known at run time: one branch per case, each printing its own part
		void Variant(llvm::Value* value, std::shared_ptr<ClassType> classType)
		{
			auto& builder = m_Ctx.Builder;
			Flush();

			llvm::Function* function = builder.GetInsertBlock()->getParent();
			Symbol slot = CreateStackSlot(classType, value);

			if (classType->IsUnion)
			{
				// every field reads the same bytes
				Text(classType->GetHash());
				Text("(");
				unsigned index = 0;
				for (const auto& [name, memberType] : classType->GetMemberValues())
				{
					if (index++ > 0) Text(", ");
					Text(name);
					Text("=");
					Value(builder.CreateLoad(memberType->Get(), slot.GetLLVMValue()), memberType);
				}
				Text(")");
				Flush();
				return;
			}

			llvm::BasicBlock* done = llvm::BasicBlock::Create(m_Ctx.Context, "print.variant.done", function);
			llvm::Value* tag = builder.CreateExtractValue(value, { 0u });
			llvm::SwitchInst* switchInst = builder.CreateSwitch(tag, done, (unsigned)classType->Cases.size());

			for (size_t i = 0; i < classType->Cases.size(); i++)
			{
				auto& variantCase = classType->Cases[i];
				llvm::BasicBlock* block = llvm::BasicBlock::Create(m_Ctx.Context, "print.case", function);
				switchInst->addCase(builder.getInt32((uint32_t)i), block);
				builder.SetInsertPoint(block);

				llvm::Value* payload = LoadVariantPayload(m_Ctx, classType, i, slot.GetLLVMValue());

				// optionals and type variants print the value they hold
				if (classType->IsOptional || classType->IsTypeVariant)
				{
					if (variantCase.Fields.empty())
						Text("none");
					else
						Value(builder.CreateExtractValue(payload, { 0u }), variantCase.Fields[0].second);
				}
				else
				{
					Text(classType->GetHash() + "." + variantCase.Name);

					if (!variantCase.Fields.empty())
					{
						Text("(");
						for (unsigned f = 0; f < variantCase.Fields.size(); f++)
						{
							if (f > 0) Text(", ");
							Text(variantCase.Fields[f].first);
							Text("=");
							Value(builder.CreateExtractValue(payload, { f }), variantCase.Fields[f].second);
						}
						Text(")");
					}
				}

				Flush();
				builder.CreateBr(done);
			}

			builder.SetInsertPoint(done);
		}

		Symbol CreateStackSlot(std::shared_ptr<Type> type, llvm::Value* value)
		{
			llvm::BasicBlock& entry = m_Ctx.Builder.GetInsertBlock()->getParent()->getEntryBlock();
			llvm::IRBuilder<> entryBuilder(&entry, entry.getFirstInsertionPt());
			llvm::Value* slot = entryBuilder.CreateAlloca(type->Get());
			m_Ctx.Builder.CreateStore(value, slot);
			return Symbol::CreateValue(slot, type);
		}

		void Float(llvm::Value* value)
		{
			auto& builder = m_Ctx.Builder;
			bool single = value->getType()->isFloatTy();

			if (!value->getType()->isDoubleTy())
				value = builder.CreateFPExt(value, builder.getDoubleTy());

			Flush();
			builder.CreateCall(FloatPrinter(), { value, builder.getInt1(single) });
		}

		// like Python's repr: the shortest text that reads back as the same number (0.1 + 0.2 is
		// 0.30000000000000004, a float32 0.1 is 0.1), and whole numbers keep a ".0"
		llvm::Function* FloatPrinter()
		{
			llvm::Module& module = m_Ctx.Module;

			if (llvm::Function* existing = module.getFunction("clear.print_float"))
				return existing;

			llvm::LLVMContext& context = m_Ctx.Context;
			llvm::IRBuilder<> b(context);
			auto doubleTy = b.getDoubleTy();
			auto function = llvm::Function::Create(llvm::FunctionType::get(b.getVoidTy(), { doubleTy, b.getInt1Ty() }, false),
												   llvm::Function::LinkOnceODRLinkage, "clear.print_float", module);
			llvm::Value* value = function->getArg(0);
			llvm::Value* single = function->getArg(1);

			auto snprintf = module.getOrInsertFunction("snprintf", llvm::FunctionType::get(b.getInt32Ty(), { b.getPtrTy(), b.getInt64Ty(), b.getPtrTy() }, true));
			auto strtod = module.getOrInsertFunction("strtod", llvm::FunctionType::get(doubleTy, { b.getPtrTy(), b.getPtrTy() }, false));

			auto entry = llvm::BasicBlock::Create(context, "entry", function);
			auto whole = llvm::BasicBlock::Create(context, "whole", function);
			auto tryDigits = llvm::BasicBlock::Create(context, "try", function);
			auto check = llvm::BasicBlock::Create(context, "check", function);
			auto done = llvm::BasicBlock::Create(context, "done", function);

			b.SetInsertPoint(entry);
			llvm::Value* buffer = b.CreateAlloca(llvm::ArrayType::get(b.getInt8Ty(), 48), nullptr, "text");
			llvm::Value* truncated = b.CreateUnaryIntrinsic(llvm::Intrinsic::trunc, value);
			llvm::Value* magnitude = b.CreateUnaryIntrinsic(llvm::Intrinsic::fabs, value);
			llvm::Value* isWhole = b.CreateAnd(b.CreateFCmpOEQ(value, truncated), b.CreateFCmpOLT(magnitude, llvm::ConstantFP::get(doubleTy, 1e16)));
			llvm::Value* first = b.CreateSelect(single, b.getInt32(6), b.getInt32(15));
			b.CreateCondBr(isWhole, whole, tryDigits);

			b.SetInsertPoint(whole);
			b.CreateCall(m_Printf, { b.CreateGlobalStringPtr("%.1f", "print.whole"), value });
			b.CreateRetVoid();

			// digits = 15, 16, 17 (6..9 for a float32) until the text reads back as the same number
			b.SetInsertPoint(tryDigits);
			llvm::PHINode* digits = b.CreatePHI(b.getInt32Ty(), 2, "digits");
			digits->addIncoming(first, entry);
			b.CreateCall(snprintf, { buffer, b.getInt64(48), b.CreateGlobalStringPtr("%.*g", "print.digits"), digits, value });
			llvm::Value* back = b.CreateCall(strtod, { buffer, llvm::ConstantPointerNull::get(b.getPtrTy()) });
			llvm::Value* backSingle = b.CreateFPExt(b.CreateFPTrunc(back, b.getFloatTy()), doubleTy);
			llvm::Value* same = b.CreateFCmpOEQ(b.CreateSelect(single, backSingle, back), value);
			llvm::Value* last = b.CreateSelect(single, b.getInt32(9), b.getInt32(17));
			b.CreateCondBr(b.CreateOr(same, b.CreateICmpSGE(digits, last)), done, check);

			b.SetInsertPoint(check);
			digits->addIncoming(b.CreateAdd(digits, b.getInt32(1)), check);
			b.CreateBr(tryDigits);

			b.SetInsertPoint(done);
			b.CreateCall(m_Printf, { b.CreateGlobalStringPtr("%s", "print.text"), buffer });
			b.CreateRetVoid();

			return function;
		}

		llvm::Value* String(llvm::StringRef text)
		{
			auto it = m_Strings.find(text);

			if (it != m_Strings.end())
				return it->second;

			llvm::Value* global = m_Ctx.Builder.CreateGlobalStringPtr(text, "print.fmt");
			m_Strings[text] = global;
			return global;
		}

	private:
		CodegenContext& m_Ctx;
		llvm::FunctionCallee m_Printf;
		std::string m_Format;
		llvm::SmallVector<llvm::Value*> m_Args;
		llvm::StringMap<llvm::Value*> m_Strings;
	};

	void EmitBuiltinPrint(CodegenContext& ctx, llvm::ArrayRef<Symbol> values)
	{
		PrintBuilder printer(ctx);

		for (size_t i = 0; i < values.size(); i++)
		{
			if (i > 0)
				printer.Text(" ");

			const Symbol& symbol = values[i];

			if (symbol.Kind != SymbolKind::Value)
			{
				printer.Text("<none>");
				continue;
			}

			printer.Value(symbol.GetLLVMValue(), symbol.GetType());
		}

		printer.Text("\n");
		printer.Flush();
	}
}
