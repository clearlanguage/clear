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
        Ref<ASTExpression> _CreateExpression(const std::vector<Token>& tokens, const std::string& root,
                                             size_t& start, AbstractType expectedType);

    private:
        Ref<ASTFunctionDecleration> m_Root;
        std::stack<Ref<ASTFunctionDecleration>> m_Stack;
    };

}