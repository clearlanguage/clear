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


        void Dump(std::FILE* output = stderr);
        bool IsFatal();
        size_t ErrorCount() const { return m_ErrorCount; } // errors reported so far (they stop compilation)

        // the line of the program being analysed: an error inside the standard library gets a note pointing here
        void SetUserLine(const Token* token) { m_UserLine = token; }
                    
    private:
        std::string LoadFile(const std::filesystem::path& path);

        std::string GetSourceLine(const std::filesystem::path& path, size_t line);

    private:
        size_t m_ErrorCount = 0;
        const Token* m_UserLine = nullptr;
        std::unordered_map<std::filesystem::path, FileReference> m_LoadedFiles;
        std::vector<Diagnostic> m_ReportedErrors;
        bool m_IsFatal = false;
    };
}