#include "DiagnosticsBuilder.h"

#include "Core/Log.h"
#include <sstream>
#include <fstream>

namespace clear 
{
    void DiagnosticsBuilder::Report(Stage stage, Severity severity, const Token& token, DiagnosticCode code)
    {
        Report(stage, severity, token, code, std::max<size_t>(token.GetData().length(), 1));
    }

    void DiagnosticsBuilder::Report(Stage stage, Severity severity, const Token& token, DiagnosticCode code, size_t expectedLength)
    {
        Diagnostic diag;
        diag.Code         = code;
        diag.DiagSeverity = severity;
        diag.DiagStage    = stage;
        diag.File         = token.GetSourceFile();
        diag.Message      = g_DiagnosticMessages[code];
        diag.Line         = token.LineNumber;
        diag.Column       = token.ColumnNumber;
        diag.ArrowsWidth  = expectedLength;

        const std::string& data = token.GetData();
        std::string shown = data.empty() ? std::string("here") : data;
        diag.Advice = std::vformat(g_DiagnosticAdvices[code], std::make_format_args(shown));

        if (!diag.File.empty())
            diag.SourceLine = GetSourceLine(diag.File, diag.Line);

        if (severity == Severity::High)
        {
            m_IsFatal = true;
        }

        // the parser can report the same problem several times while recovering, keep the first one
        for (const auto& existing : m_ReportedErrors)
        {
            if (existing.Code == diag.Code && existing.File == diag.File && existing.Line == diag.Line && existing.Column == diag.Column)
                return;
        }

        m_ReportedErrors.push_back(diag);
    }

    void DiagnosticsBuilder::Dump(std::FILE* output)
    {
        for(const auto& error : m_ReportedErrors)
        {
            std::println(output, "{}", error);
        }

        // each diagnostic is only ever printed once, even if several stages dump
        m_ReportedErrors.clear();
    }

    std::string DiagnosticsBuilder::LoadFile(const std::filesystem::path& path)
    {
        std::fstream file(path);

        if (!file.is_open())
            return "";

        std::stringstream stream;
        stream << file.rdbuf();

        return stream.str();
    }

    bool DiagnosticsBuilder::IsFatal() 
    {
        return m_IsFatal;
    }

    std::string DiagnosticsBuilder::GetSourceLine(const std::filesystem::path& path, size_t line)
    {
        auto [it, inserted] = m_LoadedFiles.try_emplace(path, FileReference());

        if (inserted)
        {
            it->second.Contents = LoadFile(path);
            it->second.ContentsRef = it->second.Contents;
        }

        llvm::StringRef contents = it->second.ContentsRef;
        size_t index = 0;

        for (size_t i = 0; i < line && index != llvm::StringRef::npos; i++)
        {
            index = contents.find('\n', index);

            if (index != llvm::StringRef::npos)
                index++;
        }

        if (index == llvm::StringRef::npos || index > contents.size())
            return "";

        size_t end = contents.find('\n', index);
        return contents.substr(index, end == llvm::StringRef::npos ? llvm::StringRef::npos : end - index).str();
    }
}
