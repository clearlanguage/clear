#pragma once 

#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/SmallString.h>


namespace clear 
{
    class SemaVariable 
    {
    public:
        SemaVariable(llvm::StringRef name);
        ~SemaVariable() = default;

    private:
        llvm::SmallString<32> m_Name;
    };
}