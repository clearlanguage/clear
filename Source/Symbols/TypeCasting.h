#include "API/LLVM/LLVMInclude.h"

#include "Symbols/Type.h"

namespace clear 
{
    class TypeCasting
    {
    public:
        static llvm::Value* Cast(llvm::Value* value, 
                                 std::shared_ptr<Type> src,
                                 std::shared_ptr<Type> dst, 
                                 llvm::IRBuilder<>& builder);

        static llvm::Value* Promote(llvm::Value* value1, 
                                    llvm::Value* value2, 
                                    std::shared_ptr<Type> type1, 
                                    std::shared_ptr<Type> type2) = delete; //TODO

        static bool CanBeCasted(std::shared_ptr<Type> src,
                                std::shared_ptr<Type> dst);

        static bool CanBePromoted(std::shared_ptr<Type> src,
                                  std::shared_ptr<Type> dst);
    };
}