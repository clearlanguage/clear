//
// Created by Kareem Fares on 5/24/25.
//

#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>
#include <utility>
#include <new>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

template<typename T>
using Ref = std::shared_ptr<T>;

class Type {
public:
    virtual ~Type() = default;

    virtual llvm::Type* GetLLVMType() const = 0;
    virtual std::string GetName() const = 0;

    virtual bool IsSigned() const { return false; }
    virtual bool IsFloatingPoint() const { return false; }
    virtual bool IsPointer() const { return false; }
};

class BuiltinType : public Type {
public:
    BuiltinType(std::string name, llvm::Type* llvmType, bool isSigned, bool isFloatingPoint)
        : name(std::move(name)), llvmType(llvmType), signedFlag(isSigned), floatFlag(isFloatingPoint) {}

    llvm::Type* GetLLVMType() const override { return llvmType; }
    std::string GetName() const override { return name; }

    bool IsSigned() const override { return signedFlag; }
    bool IsFloatingPoint() const override { return floatFlag; }

private:
    std::string name;
    llvm::Type* llvmType;
    bool signedFlag;
    bool floatFlag;
};

class PointerType : public Type {
public:
    PointerType(Ref<Type> baseType, llvm::Type* llvmPtrType)
        : base(baseType), llvmType(llvmPtrType) {}

    llvm::Type* GetLLVMType() const override { return llvmType; }
    std::string GetName() const override { return base->GetName() + "*"; }

    bool IsPointer() const override { return true; }
    Ref<Type> GetBaseType() const { return base; }

private:
    Ref<Type> base;
    llvm::Type* llvmType;
};

class StructType : public Type {
public:
    StructType(std::string name, llvm::StructType* llvmType,
               std::vector<std::pair<std::string, Ref<Type>>> members)
        : name(std::move(name)), llvmType(llvmType), members(std::move(members)) {}

    llvm::Type* GetLLVMType() const override { return llvmType; }
    std::string GetName() const override { return name; }

    const auto& GetMembers() const { return members; }

private:
    std::string name;
    llvm::StructType* llvmType;
    std::vector<std::pair<std::string, Ref<Type>>> members;
};

class TypeRegistry {
public:
    explicit TypeRegistry(llvm::LLVMContext& ctx) : context(ctx) {
        RegisterBuiltinTypes();
    }

    Ref<Type> GetType(const std::string& name) const {
        auto it = types.find(name);
        if (it != types.end()) return it->second;
        return nullptr;
    }

    Ref<StructType> CreateStruct(const std::string& name,
                                 const std::vector<std::pair<std::string, Ref<Type>>>& members) {
        std::vector<llvm::Type*> llvmMembers;
        for (const auto& [_, type] : members) {
            llvmMembers.push_back(type->GetLLVMType());
        }
        auto llvmStruct = llvm::StructType::create(context, llvmMembers, name);
        auto structType = std::make_shared<StructType>(name, llvmStruct, members);
        types[name] = structType;
        return structType;
    }

    Ref<PointerType> CreatePointerType(Ref<Type> base) {
        auto llvmPtr = llvm::PointerType::getUnqual(base->GetLLVMType());
        auto ptrType = std::make_shared<PointerType>(base, llvmPtr);
        return ptrType;
    }

private:
    void RegisterBuiltinTypes() {
        types["int"]    = std::make_shared<BuiltinType>("int", llvm::Type::getInt32Ty(context), true, false);
        types["float"]  = std::make_shared<BuiltinType>("float", llvm::Type::getFloatTy(context), true, true);
        types["bool"]   = std::make_shared<BuiltinType>("bool", llvm::Type::getInt1Ty(context), false, false);
    }

    llvm::LLVMContext& context;
    std::unordered_map<std::string, Ref<Type>> types;
};
