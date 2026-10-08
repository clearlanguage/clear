#include "CommandLineParsing.h"

#include <filesystem>
#include <print>
#include <vector>
#include <string_view>

namespace clear
{
    namespace CommandLine
    {
        static ParsingResult Fail(std::string message)
        {
            ParsingResult result;
            result.Error = std::move(message);
            return result;
        }

        ParsingResult Parse(int argc, char* argv[])
        {
            const std::vector<std::string_view> arguments(argv + 1, argv + argc);

            ParsingResult result;

            if (arguments.empty())
            {
                result.Options = ProgramMode::ShowHelp;
                result.Successful = true;
                return result;
            }

            for (size_t i = 0; i < arguments.size(); i++)
            {
                std::string_view arg = arguments[i];

                // everything after `--` belongs to the program being run
                if (arg == "--")
                {
                    for (size_t j = i + 1; j < arguments.size(); j++)
                        result.ProgramArguments.emplace_back(arguments[j]);

                    break;
                }

                if (arg == "--help" || arg == "-h" || arg == "help")
                {
                    result.Options = ProgramMode::ShowHelp;
                    result.Successful = true;
                    return result;
                }

                if (arg == "--build_template") { result.Options = ProgramMode::BuildTemplateConfig; continue; }
                if (arg == "--compile")        { result.Options = ProgramMode::Compile;             continue; }
                if (arg == "--emit-ir")        { result.EmitIR = true;                              continue; }
                if (arg == "-v" || arg == "--verbose") { result.Verbose = true;                     continue; }
                if (arg == "--native" || arg == "-march=native") { result.TargetCPU = "native";      continue; }
                if (arg.starts_with("--cpu=")) { result.TargetCPU = std::string(arg.substr(6));      continue; }

                if (arg == "build" && result.Options == ProgramMode::None) { result.Options = ProgramMode::Build; continue; }
                if (arg == "run"   && result.Options == ProgramMode::None) { result.Options = ProgramMode::Run;   continue; }

                if (arg == "-o")
                {
                    if (i + 1 >= arguments.size())
                        return Fail("-o expects an output path");

                    result.Output = std::filesystem::path(arguments[++i]);
                    continue;
                }

                if (arg.size() == 3 && arg.starts_with("-O") && arg[2] >= '0' && arg[2] <= '3')
                {
                    result.OptimizationLevel = std::string(arg.substr(2));
                    continue;
                }

                if (arg.starts_with("-"))
                    return Fail(std::format("unknown option '{}'", arg));

                if (!result.Directory.empty())
                {
                    // extra positional arguments after the file are forwarded when running
                    if (result.Options == ProgramMode::Run)
                    {
                        result.ProgramArguments.emplace_back(arg);
                        continue;
                    }

                    return Fail(std::format("unexpected argument '{}'", arg));
                }

                result.Directory = std::filesystem::path(arg);

                if (!std::filesystem::exists(result.Directory))
                    return Fail(std::format("'{}' does not exist", result.Directory.string()));
            }

            if (result.Options == ProgramMode::None)
            {
                // `clearc file.cl` is shorthand for `clearc run file.cl`
                if (result.Directory.extension() == ".cl")
                    result.Options = ProgramMode::Run;
                else
                    return Fail("no command given");
            }

            if (result.Directory.empty())
            {
                if (result.Options == ProgramMode::Run)
                    return Fail("run expects a .cl file");

                result.Directory = std::filesystem::current_path();
            }

            result.Successful = true;
            return result;
        }

        void PrintHelp()
        {
            std::println("usage: clearc <command> [options]\n");
            std::println("commands:");
            std::println("  run <file.cl> [-- args]     compile a single file and run it");
            std::println("  build <file.cl | dir>       compile a file, or a project directory with a build.toml");
            std::println("  --compile <dir>             compile a project directory with a build.toml");
            std::println("  --build_template <dir>      write a template build.toml into <dir>\n");
            std::println("options:");
            std::println("  -o <path>                   output path (single file builds)");
            std::println("  -O0 | -O1 | -O2 | -O3       optimization level (default -O1, -O3 is fastest)");
            std::println("  --emit-ir                   also write the LLVM IR next to the output (.ll)");
            std::println("  --native                    optimize for this machine's CPU (default for run)");
            std::println("  --cpu=<name>                optimize for a specific CPU, e.g. --cpu=x86-64-v3");
            std::println("  -v, --verbose               print progress while compiling");
        }
    }
}
