#pragma once 

#include "DiagnosticCode.h"

#include <print>
#include <string_view>
#include <filesystem>
#include <format>
#include <algorithm>

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

        std::filesystem::path File; // empty when the problem has no source location
        std::string SourceLine;     // the offending line, without its newline
        std::string Message;
        std::string Advice;

        size_t Line = 0;   // zero based
        size_t Column = 0; // zero based

        size_t ArrowsWidth = 1;
    };

    inline std::string_view g_SeverityStrings[(size_t)Severity::Count] = {
        "note", "warning", "warning", "error"
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
            std::string out;
            std::string_view severity = clear::g_SeverityStrings[(size_t)error.DiagSeverity];

            out += std::format("{}[E{:03}]: {}\n", severity, (int)error.Code, error.Message);

            if (!error.File.empty())
            {
                std::string lineNumber = std::to_string(error.Line + 1);
                std::string gutter(lineNumber.size(), ' ');

                // tabs would misalign the carets, render them as single spaces
                std::string line = error.SourceLine;
                std::replace(line.begin(), line.end(), '\t', ' ');

                out += std::format("{} --> {}:{}:{}\n", gutter, error.File.string(), error.Line + 1, error.Column + 1);
                out += std::format("{} |\n", gutter);
                out += std::format("{} | {}\n", lineNumber, line);
                out += std::format("{} | {}{}\n", gutter, std::string(error.Column, ' '), std::string(std::max<size_t>(error.ArrowsWidth, 1), '^'));
            }

            if (!error.Advice.empty())
                out += std::format("  = help: {}\n", error.Advice);

            return formatter<std::string>::format(out, ctx);
        }
    };
}
