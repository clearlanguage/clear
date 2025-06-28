#include "DiagnosticsBuilder.h"

namespace clear 
{
    void DiagnosticsBuilder::Report(Stage stage, Severity severity, const Token& token, DiagnosticCode code)
    {
        auto [it, success] = m_LoadedFiles.try_emplace(token.GetSourceFile(), FileReference());
    }
}