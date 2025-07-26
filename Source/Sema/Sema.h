#pragma once 
#include "AST/ASTNode.h"


namespace clear 
{
    class Sema
    {
    Sema(std::shared_ptr<Module> module);

    private:
       std::shared_ptr<Module> m_Module;
    public:
    void SemaPass(std::shared_ptr<ASTBlock> ast);
    void SemaPass(std::shared_ptr<ASTVariable> variable);
    void SemaPass(std::shared_ptr<ASTNodeBase> ast);

    Sema();
    };
}