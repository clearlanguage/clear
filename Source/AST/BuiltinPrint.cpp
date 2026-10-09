#include "ASTNode.h"

#include "Symbols/Module.h"

#include <llvm/IR/Intrinsics.h>

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
			auto& builder = m_Ctx.Builder;
			llvm::Type* llvmType = value->getType();

			// str: its length says where it ends (part of a text has no zero after it)
			if (type && type->GetHash() == "str")
			{
				m_Format += "%.*s";
				m_Args.push_back(builder.CreateTrunc(builder.CreateExtractValue(value, 1), builder.getInt32Ty()));
				m_Args.push_back(builder.CreateExtractValue(value, 0));
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
			else
			{
				Text("<value>");
			}
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

			if (!value->getType()->isDoubleTy())
				value = builder.CreateFPExt(value, builder.getDoubleTy());

			Flush();

			// whole numbers keep a ".0" like Python, everything else uses the shortest sensible form
			llvm::Value* truncated = builder.CreateUnaryIntrinsic(llvm::Intrinsic::trunc, value);
			llvm::Value* magnitude = builder.CreateUnaryIntrinsic(llvm::Intrinsic::fabs, value);
			llvm::Value* isWhole = builder.CreateAnd(builder.CreateFCmpOEQ(value, truncated),
													 builder.CreateFCmpOLT(magnitude, llvm::ConstantFP::get(builder.getDoubleTy(), 1e16)));

			llvm::Value* format = builder.CreateSelect(isWhole, String("%.1f"), String("%.15g"));
			builder.CreateCall(m_Printf, { format, value });
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
