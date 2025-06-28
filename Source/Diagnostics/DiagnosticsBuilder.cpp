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

        Diagnostic diag = {
            .Code = code,
            .DiagSeverity = severity,
            .DiagStage = stage,

            .File = token.GetSourceFile(),

            .CodeSnippet = CreateCodeSnippet(fileRef.ContentsRef, token.GetLineNumber()),

            .Message = g_DiagnosticMessages[code],
            .Advice  = std::format(g_DiagnosticAdvices[code], token.GetData()),

            .Line = token.GetLineNumber(),
            .Column = token.GetColumnNumber()
        };

        m_ReportedErrors.push_back(diag);
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

        while(index < file.size() && line)
        {
            size_t endln = file.find(file, index);

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