#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

namespace clear
{
    namespace CommandLine
    {
        enum class ProgramMode
        {
            None = 0,
            ShowHelp,             // --help
            BuildTemplateConfig,  // --build_template directory
            Compile,              // --compile directory (with build.toml)
            Build,                // build file.cl | directory
            Run                   // run file.cl [-- program args]
        };

        struct ParsingResult
        {
            ProgramMode Options = ProgramMode::None;
            std::filesystem::path Directory; // target file or directory
            std::optional<std::filesystem::path> Output;  // -o
            std::optional<std::string> OptimizationLevel; // -O0 .. -O3
            std::vector<std::string> ProgramArguments;    // forwarded to the program by `run`
            bool EmitIR = false;   // --emit-ir
            std::optional<std::string> TargetCPU; // --native, --cpu=<name>
            std::optional<bool> RuntimeChecks;     // --checks, --no-checks
            bool Verbose = false;  // -v
            bool Successful = false;
            std::string Error;
        };

        ParsingResult Parse(int argc, char* argv[]);
        void PrintHelp();
    }
}
