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

        void Report(Stage stage,
               Severity severity,
               const Token& token,
               DiagnosticCode code,
               size_t expectedLength);


        void Dump(std::FILE* output = stdout);
        bool IsFatal();
                    
    private:
        std::string LoadFile(const std::filesystem::path& path);

        llvm::SmallVector<llvm::StringRef, g_SnippetHeight> CreateCodeSnippet(llvm::StringRef file, size_t line);

    private:
        std::unordered_map<std::filesystem::path, FileReference> m_LoadedFiles;
        std::vector<Diagnostic> m_ReportedErrors;
        bool m_IsFatal = false;
    };
}