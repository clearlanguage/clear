#pragma once

#include "ASTNode.h"
#include "Parser.h"
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
        std::shared_ptr<ASTExpression> _CreateExpression(const std::vector<Token>& tokens, size_t& start);

    private:
        std::shared_ptr<ASTNodeBase> m_Root;
        std::stack<std::shared_ptr<ASTNodeBase>> m_Stack;
    };

}