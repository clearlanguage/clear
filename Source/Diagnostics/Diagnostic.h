#pragma once 

#include "DiagnosticCode.h"

#include <print>
#include <string_view>
#include <filesystem>
#include <format>

#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/SmallString.h>


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
            llvm::SmallString<256> codeSnippet;

            for (auto line : error.CodeSnippet)
            {
                codeSnippet.append(line);
                codeSnippet.push_back('\n');
            }

            std::string_view codeSnippetView(codeSnippet.data(), codeSnippet.size());
            const auto& severityStr = clear::g_SeverityStrings[(size_t)error.DiagSeverity];
            const auto& stageStr    = clear::g_StageStrings[(size_t)error.DiagStage];
            int codeInt             = (int)error.Code; 
            std::string filePath    = error.File.string();

            std::string formatString = std::vformat(
                "File: {}; Line: {}; Column: {}; Stage: {}; Error Code: {};\n"
                "{}\n"
                "{}\n"
                "{}\n",
                std::make_format_args(filePath, error.Line, error.Column,
                    stageStr,
                    codeInt,
                    codeSnippetView, 
                    error.Message,
                    error.Advice
                )
            );
        
            return formatter<std::string>::format(formatString, ctx);
        }
    };
}