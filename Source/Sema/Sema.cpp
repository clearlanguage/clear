#include "Sema.h"

#include <Symbols/Module.h>

#include "AST/ASTNode.h"

namespace clear
{
    Sema::Sema(std::shared_ptr<Module> module){
        m_Module = module;
    }


    void Sema::SemaPass(std::shared_ptr<ASTBlock> ast) {
        // magic

        for (auto i: ast->Children){
            SemaPass(i);
        }
    }


    void Sema::SemaPass(std::shared_ptr<ASTVariable> variable) {
        const std::string& name = variable->GetName().GetData();
        auto tbl = variable->GetSymbolTable();

        if (tbl->HasAlloca(name)) {
            Allocation alloca = tbl->GetAlloca(name);
            variable->ResolvedSymbol = Symbol::CreateValue(nullptr, alloca.Type);
            variable->ResolvedAllocation = alloca;

            return;
        }

        if (auto ty = m_Module->Lookup(name).GetType()) {
            variable->ResolvedSymbol = Symbol::CreateType(ty);
            return;
        }

        if (auto mod = m_Module->Return(name)) {
            variable->ResolvedSymbol = Symbol::CreateModule(mod);
            return;
        }


    }



    void Sema::SemaPass(std::shared_ptr<ASTNodeBase> ast) {
        //magic
        for (auto i : ast->Children) {
            SemaPass(i);
        }
    }

}
