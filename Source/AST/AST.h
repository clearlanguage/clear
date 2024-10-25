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
        AST(const ProgramInfo& info);
        ~AST() = default;

        void BuildIR(const std::filesystem::path& out);

    private:
        Ref<ASTExpression> _CreateExpression(const std::vector<Token>& tokens, const std::string& root, size_t& start);

        std::list<std::string> _RetrieveChain(const std::vector<Token>& tokens, size_t current);
        std::list<std::string> _RetrieveForwardChain(const std::vector<Token>& tokens, size_t& current);
        AbstractType _RetrieveAssignmentType(const std::vector<Token>& tokens, const std::string& currentFunctionName, size_t current);

    private:
        Ref<ASTFunctionDecleration> m_Root;
        std::stack<Ref<ASTFunctionDecleration>> m_Stack;
    };

}