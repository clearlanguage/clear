#include "DiagnosticsBuilder.h"

#include "Core/Log.h"

#include <fstream>

namespace clear 
{
    void DiagnosticsBuilder::Report(Stage stage, Severity severity, const Token& token, DiagnosticCode code)
    {
        auto [it, success] = m_LoadedFiles.try_emplace(token.GetSourceFile(), FileReference());
        auto& fileRef = it->second;

        if(success)
        {
            fileRef.Contents = LoadFile(token.GetSourceFile());
            fileRef.ContentsRef = fileRef.Contents;
        }

        Diagnostic diag;
        diag.Code         = code;
        diag.DiagSeverity = severity;
        diag.DiagStage    = stage;
        diag.File         = token.GetSourceFile();
        diag.CodeSnippet  = CreateCodeSnippet(fileRef.ContentsRef, token.GetLineNumber());
        diag.Message      = g_DiagnosticMessages[code];
        diag.Advice       = std::vformat(g_DiagnosticAdvices[code],  std::make_format_args(token.GetData()));
        diag.Line         = token.GetLineNumber();
        diag.Column       = token.GetColumnNumber();

        m_ReportedErrors.push_back(diag);
    }

    void DiagnosticsBuilder::PrintErrors()
    {
        for(const auto& error : m_ReportedErrors)
        {
            std::println("{}", error);
        }
    }

    std::string DiagnosticsBuilder::LoadFile(const std::filesystem::path& path)
    {
        std::fstream file(path);
        CLEAR_VERIFY(file.is_open(), "failed to open file ", path);

        std::stringstream stream;
        stream << file.rdbuf();

        return stream.str();
    }

    llvm::SmallVector<llvm::StringRef, g_SnippetHeight> DiagnosticsBuilder::CreateCodeSnippet(llvm::StringRef file, size_t line)
    {        
        llvm::SmallVector<llvm::StringRef, g_SnippetHeight> codeSnippet;
        size_t index = 0;

        line += g_SnippetHeight / 2;

        while(index < file.size() && line)
        {
            size_t endln = file.find("\n", index);

            if (endln == llvm::StringRef::npos)
                endln = file.size();
            
            if(codeSnippet.size() == g_SnippetHeight)
            {
                codeSnippet.erase(codeSnippet.begin());
            }
            
            codeSnippet.push_back(file.substr(index, endln - index));

            index = endln + 1;
            line--;
        }

        return codeSnippet;
    }
}