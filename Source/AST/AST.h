/* #pragma once

#include "ASTNode.h"
#include "Lexing/Lexer.h"
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
        void _CreateArrayInitializer(Ref<ASTArrayInitializer>& initializer, std::vector<Token>& tokens, const std::string& root, size_t& i, const Ref<Type>& expected);
        
        Ref<Type> _GetAssignmentType(const std::vector<Token>& tokens, const std::string& currentFunctionName, size_t current);
        Ref<Type> _GetTypeFromToken(const Token& token, bool isPointer);
        Ref<Type> _GetTypeFromList(std::list<std::string>& list);

        bool _IsUnary(const Token& token);

    private:

        struct StackNode
        {
            Ref<ASTNodeBase> Node;
            Ref<Type> ExpectedReturnType;
        };

    private:
        Ref<ASTFunctionDefinition> m_Root;
        std::stack<StackNode> m_Stack;
    };

} */