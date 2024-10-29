#pragma once

#include "ASTNode.h"
#include "Parsing/Parser.h"
#include "API/LLVM/LLVMInclude.h"

#include <filesystem>
#include <stack>

namespace clear {

    class AST
    {
    public:
        AST(ProgramInfo& info);
        ~AST() = default;

        void BuildIR(const std::filesystem::path& out);

    private:
        Ref<ASTExpression>   _CreateExpression(std::vector<Token>& tokens, const std::string& root, size_t& start, const AbstractType& expected);
        Ref<ASTFunctionCall> _CreateFunctionCall(std::vector<Token>& tokens, const std::string& root, size_t& i);

        std::list<std::string> _RetrieveChain(const std::vector<Token>& tokens, size_t current);
        std::list<std::string> _RetrieveForwardChain(const std::vector<Token>& tokens, size_t& current);
        AbstractType _RetrieveAssignmentType(const std::vector<Token>& tokens, const std::string& currentFunctionName, size_t current);
        AbstractType _GetTypeFromToken(const Token& token, bool isPointer);

    private:

        struct StackNode
        {
            Ref<ASTNodeBase> Node;
            AbstractType ExpectedReturnType;
        };

    private:
        Ref<ASTFunctionDecleration> m_Root;
        std::stack<StackNode> m_Stack;
    };

}