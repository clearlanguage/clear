#pragma once 

#include "Diagnostic.h"
#include "Lexing/Token.h"

#include <unordered_map>
#include <vector>

namespace clear 
{
    struct FileReference
    {
        std::string Contents;
        llvm::StringRef ContentsRef;
        std::vector<llvm::StringRef> Lines;
    };

    class DiagnosticsBuilder 
    {
    public:
        DiagnosticsBuilder() = default;
        ~DiagnosticsBuilder() = default;

        void Report(Stage stage, 
                    Severity severity, 
                    const Token& token,
                    DiagnosticCode code);            

    private:
        std::unordered_map<std::filesystem::path, FileReference> m_LoadedFiles;
        std::vector<Diagnostic> m_ReportedErrors;
    };
}