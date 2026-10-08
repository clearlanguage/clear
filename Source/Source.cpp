#include "Compilation/BuildConfig.h"
#include "Compilation/CompilationManager.h"
#include "CommandLine/CommandLineParsing.h"

#include <llvm/Config/llvm-config.h>
#include <llvm/Support/Program.h>
#include <toml++/toml.h>
#include <filesystem>
#include <cstdlib>
#include <print>
#include <unistd.h>

using namespace clear;

static void ApplyOptions(BuildConfig& config, const CommandLine::ParsingResult& options)
{
    if (options.OptimizationLevel)
    {
        const std::string& level = *options.OptimizationLevel;

        if (level == "0")      config.OptimizationLevel = BuildConfig::OptimizationLevelType::None;
        else if (level == "1") config.OptimizationLevel = BuildConfig::OptimizationLevelType::Development;
        else                   config.OptimizationLevel = BuildConfig::OptimizationLevelType::Distribution;
    }

    config.EmitIntermiediateIR = config.EmitIntermiediateIR || options.EmitIR;

    if (options.TargetCPU)
        config.TargetCPU = *options.TargetCPU;

    if (options.RuntimeChecks)
        config.RuntimeChecks = *options.RuntimeChecks ? 1 : 0;
    config.Verbose = config.Verbose || options.Verbose;
}

static int CompileProject(const std::filesystem::path& directory, const CommandLine::ParsingResult& options, bool verbose)
{
    BuildConfig config = BuildConfig::BuildConfigFromToml(directory / "build.toml");
    config.Verbose = verbose;
    ApplyOptions(config, options);

    if (config.Verbose)
    {
        std::println("Using llvm version {}", LLVM_VERSION_STRING);
        std::println("Compiling application {}",  config.ApplicationName);
        std::println("Using standard library {}", config.StandardLibrary.string());
    }

    CompilationManager manager(config);
    bool success = manager.RunPipeline();

    if (config.Verbose)
        std::println("{}", success ? "Finished compilation" : "Compilation failed");

    return success ? 0 : 1;
}

static int CompileFile(const std::filesystem::path& file, const std::filesystem::path& output, const CommandLine::ParsingResult& options)
{
    BuildConfig config = BuildConfig::ForSingleFile(file, output);
    ApplyOptions(config, options);

    CompilationManager manager(config);
    return manager.RunPipeline() ? 0 : 1;
}

static int RunFile(const CommandLine::ParsingResult& options)
{
    std::filesystem::path tempDir = std::filesystem::temp_directory_path() / std::format("clear-run-{}", getpid());
    std::filesystem::create_directories(tempDir);

    std::filesystem::path executable = tempDir / options.Directory.stem();

    // the program runs right here, so it can use every feature of this CPU
    CommandLine::ParsingResult runOptions = options;
    if (!runOptions.TargetCPU)
        runOptions.TargetCPU = "native";

    int status = CompileFile(options.Directory, executable, runOptions);

    if (status == 0)
    {
        std::vector<std::string> args = { executable.string() };
        args.insert(args.end(), options.ProgramArguments.begin(), options.ProgramArguments.end());

        std::vector<llvm::StringRef> refs(args.begin(), args.end());

        std::fflush(stdout);
        status = llvm::sys::ExecuteAndWait(executable.string(), refs);
    }

    std::error_code ec;
    std::filesystem::remove_all(tempDir, ec);

    return status;
}

int main(int argc, char* argv[])
{
    CommandLine::ParsingResult result = CommandLine::Parse(argc, argv);

    if (!result.Successful)
    {
        std::println(stderr, "clearc: {}", result.Error);
        std::println(stderr, "run 'clearc --help' for usage");
        return 2;
    }

    switch (result.Options)
    {
        case CommandLine::ProgramMode::ShowHelp:
        {
            CommandLine::PrintHelp();
            return 0;
        }
        case CommandLine::ProgramMode::BuildTemplateConfig:
        {
            BuildConfig config;
            config.Serialize(result.Directory / "build.toml");

            std::println("Created build.toml at {}", result.Directory.string());

            return 0;
        }
        case CommandLine::ProgramMode::Compile:
        {
            return CompileProject(result.Directory, result, /* verbose = */ true);
        }
        case CommandLine::ProgramMode::Build:
        {
            if (std::filesystem::is_directory(result.Directory))
                return CompileProject(result.Directory, result, result.Verbose);

            std::filesystem::path output = result.Output.value_or(result.Directory.stem());
            return CompileFile(result.Directory, output, result);
        }
        case CommandLine::ProgramMode::Run:
        {
            return RunFile(result);
        }
        default:
        {
            std::println(stderr, "Not a valid option");
            return 2;
        }
    }
}
