#include "Lexing/Lexer.h"
#include "AST/ASTNodeN.h"

#include "API/LLVM/LLVMBackend.h"

#include <iostream>
#include <filesystem>

using namespace clear;

int main()
{
    std::filesystem::path current = __FILE__;
    std::filesystem::current_path(current.parent_path());

    LLVM::Backend::Init();

    std::cout << "------PARSER TESTS--------" << std::endl;
    Lexer parser;
    ProgramInfo info = parser.CreateTokensFromFile("Tests/test.cl");

    for (size_t i = 0; i < info.Tokens.size(); i++)
    {
        std::cout << "Token Type: " << TokenToString(info.Tokens[i].TokenType);
        std::cout << ", Data: " << info.Tokens[i].Data;
        std::cout << std::endl;
    }

    std::cout << "------AST TESTS--------" << std::endl;
    /* {
        AST ast(info);
        ast.BuildIR("Tests/test.ir");
    } */

    {      
        auto& builder = *LLVM::Backend::GetBuilder();
        auto& context = *LLVM::Backend::GetContext();
        auto& module  = *LLVM::Backend::GetModule();

        llvm::FunctionType* funcType = llvm::FunctionType::get(builder.getInt32Ty(), false);
        llvm::Function* mainFunc = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "main", module);

        llvm::BasicBlock* entry = llvm::BasicBlock::Create(context, "entry", mainFunc);
        builder.SetInsertPoint(entry);

        std::shared_ptr<ASTNodeBase> root = std::make_shared<ASTNodeBase>();
        root->CreateSymbolTable();

        root->Push(std::make_shared<ASTVariableDeclaration>("my_var", std::make_shared<Type>(TypeID::Float32)));

        std::shared_ptr<ASTNodeLiteral> left  = std::make_shared<ASTNodeLiteral>(Token{TokenType::RValueNumber, "50"});
        std::shared_ptr<ASTNodeLiteral> right = std::make_shared<ASTNodeLiteral>(Token{TokenType::RValueNumber, "60.5"});

        std::shared_ptr<ASTBinaryExpression> add = std::make_shared<ASTBinaryExpression>(BinaryExpressionType::Add);
        
        std::shared_ptr<ASTAssignmentOperator> assignment = std::make_shared<ASTAssignmentOperator>(AssignmentOperatorType::Normal);
        root->Push(assignment);

        assignment->Push(std::make_shared<ASTVariableReference>("my_var"));
        assignment->Push(add);

        add->Push(right);
        add->Push(left);

        std::shared_ptr<ASTAssignmentOperator> addOp = std::make_shared<ASTAssignmentOperator>(AssignmentOperatorType::Add);
        addOp->Push(std::make_shared<ASTVariableReference>("my_var"));
        addOp->Push(std::make_shared<ASTNodeLiteral>(Token{TokenType::RValueNumber, "10"}));

        root->Push(addOp);

        std::filesystem::path path = "Tests/test.ir";

		std::error_code EC;
		llvm::raw_fd_stream stream(path.string(), EC);

        root->PropagateSymbolTableToChildren();
		root->Codegen();

		module.print(stream, nullptr);
    }   

    //LLVM::Backend::BuildModule();

    LLVM::Backend::Shutdown();

    return 0;
}
