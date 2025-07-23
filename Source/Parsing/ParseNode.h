#pragma once 

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"

#include <memory>
#include <vector>

namespace clear 
{

    enum class ParseNodeType 
    {
        Base = 0, Literal, BinaryExpression, VariableDecleration,
		FunctionDefinition, FunctionDecleration,
		ReturnStatement, Expression, Struct,
		FunctionCall, IfExpression, WhileLoop,
		UnaryExpression, Break, Continue, 
		InitializerList, MemberAccess, AssignmentOperator, Import, Member, 
		Variable, ForLoop, InferredDecleration, Class, LoopControlFlow, 
		DefaultArgument, Trait, Raise, TryCatch, DefaultInitializer, 
		Enum, Defer, TypeResolver, TypeSpecifier, TernaryExpression, 
		Switch, ListExpr, StructExpr
    };

    struct ParseNode 
    {
        ParseNodeType Type;
        llvm::SmallString<32> Data;
        llvm::SmallVector<std::shared_ptr<ParseNode>> Nodes;
    };

}