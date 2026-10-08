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
                if (arg == "--checks")         { result.RuntimeChecks = true;                       continue; }
                if (arg == "--no-checks")      { result.RuntimeChecks = false;                      continue; }
                if (arg.starts_with("--cpu=")) { result.TargetCPU = std::string(arg.substr(6));      continue; }

                if (result.Options == ProgramMode::None && result.Directory.empty())
                {
                    if (arg == "fetch")  { result.Options = ProgramMode::Fetch;  continue; }
                    if (arg == "update") { result.Options = ProgramMode::Update; continue; }

                    // new <directory>: the directory does not exist yet
                    if (arg == "new")
                    {
                        if (i + 1 >= arguments.size())
                            return Fail("new expects a project directory");

                        result.Options = ProgramMode::New;
                        result.Directory = std::filesystem::path(arguments[++i]);
                        result.Successful = true;
                        return result;
                    }

                    if (arg == "add")
                    {
                        if (i + 1 >= arguments.size() || arguments[i + 1].starts_with("-"))
                            return Fail("add expects a package name, e.g. clearc add colors --git <url>");

                        result.Options = ProgramMode::Add;
                        result.PackageName = std::string(arguments[++i]);

                        for (i++; i < arguments.size(); i++)
                        {
                            std::string_view option = arguments[i];

                            if (i + 1 >= arguments.size())
                                return Fail(std::format("{} expects a value", option));

                            std::string value(arguments[++i]);

                            if (option == "--git")         result.Git = value;
                            else if (option == "--tag")    result.Tag = value;
                            else if (option == "--branch") result.Branch = value;
                            else if (option == "--rev")    result.Rev = value;
                            else if (option == "--path")   result.PackagePath = value;
                            else return Fail(std::format("unknown option '{}' for add", option));
                        }

                        if (result.Git.empty() == result.PackagePath.empty())
                            return Fail("add needs either --git <url> or --path <directory>");

                        result.Directory = std::filesystem::current_path();
                        result.Successful = true;
                        return result;
                    }
                }

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
                // inside a project, `clearc run` runs it
                if (result.Options == ProgramMode::Run && !std::filesystem::exists(std::filesystem::current_path() / "clear.toml"))
                    return Fail("run expects a .cl file or a project directory");

                result.Directory = std::filesystem::current_path();
            }

            result.Successful = true;
            return result;
        }

        void PrintHelp()
        {
            std::println("usage: clearc <command> [options]\n");
            std::println("commands:");
            std::println("  run <file.cl | project> [-- args]   compile and run a file or a project (clear.toml)");
            std::println("  build <file.cl | project>   compile a file or a project (clear.toml, or a legacy build.toml)");
            std::println("  new <directory>             start a project: clear.toml and main.cl");
            std::println("  add <name> --git <url> [--tag t | --branch b | --rev r]");
            std::println("  add <name> --path <dir>     add a dependency to clear.toml");
            std::println("  fetch                       download dependencies (exact versions from clear.lock)");
            std::println("  update                      move dependencies to their newest matching versions");
            std::println("  --compile <dir>             compile a project directory with a build.toml");
            std::println("  --build_template <dir>      write a template build.toml into <dir>\n");
            std::println("options:");
            std::println("  -o <path>                   output path (single file builds)");
            std::println("  -O0 | -O1 | -O2 | -O3       optimization level (default -O1, -O3 is fastest)");
            std::println("  --emit-ir                   also write the LLVM IR next to the output (.ll)");
            std::println("  --native                    optimize for this machine's CPU (default for run)");
            std::println("  --cpu=<name>                optimize for a specific CPU, e.g. --cpu=x86-64-v3");
            std::println("  --checks, --no-checks       run-time safety checks (default: on, off with -O2/-O3)");
            std::println("  -v, --verbose               print progress while compiling");
        }
    }
}
