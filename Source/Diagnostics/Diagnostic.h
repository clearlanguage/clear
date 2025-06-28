#pragma once 

#include "DiagnosticCode.h"

#include <print>
#include <string_view>
#include <filesystem>

#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/SmallVector.h>

namespace clear 
{
    enum class Severity 
    {
        None = 0, Low, Medium, High, Count
    };

    enum class Stage 
    {
        None = 0, Lexing, Parsing, CodeGeneration, Count
    };

    inline constexpr size_t g_SnippetHeight = 6;

    struct Diagnostic
    {
        DiagnosticCode Code = DiagnosticCode_None;
        Severity DiagSeverity = Severity::None;
        Stage DiagStage = Stage::None;

        std::filesystem::path File;

        llvm::SmallVector<llvm::StringRef, g_SnippetHeight> CodeSnippet;
        std::string Message;
        std::string Advice;

        size_t Line = 0;
        size_t Column = 0;
    };

    inline std::string_view g_SeverityStrings[(size_t)Severity::Count] = {
        "None", "Low", "Medium", "High"
    };

    inline std::string_view g_StageStrings[(size_t)Stage::Count] = {
        "None", "Lexing", "Parsing", "CodeGeneration"
    };



}

namespace std 
{
    template<>
    struct formatter<clear::Diagnostic> : formatter<std::string>
    {
        auto format(const clear::Diagnostic& error, format_context& ctx) const 
        { 
            std::string formatString = std::format(
                "File: {}; Line: {}; Column: {}; Stage: {}; Error Code: {};\n"
                "{}\n"
                "{}\n"
                "{}\n",
                error.File, error.Line, error.Column, clear::g_SeverityStrings[(size_t)error.DiagSeverity], clear::g_StageStrings[(size_t)error.DiagStage],
                error.Code, error.CodeSnippet, error.Message, error.Advice
            );
        
            return formatter<std::string>::format(formatString, ctx);
        }
    };
}