#include "Compilation/BuildConfig.h"
#include "Compilation/CompilationManager.h"
#include "CommandLine/CommandLineParsing.h"

#include <llvm/Config/llvm-config.h>
#include <llvm/MC/MCSection.h>
#include <toml++/toml.h>
#include <filesystem>
#include <cstdlib>
#include <print>

using namespace clear;

int main(int argc, char* argv[])
{
    CommandLine::ParsingResult result = CommandLine::Parse(argc, argv);

    switch (result.Options) 
    {
        case CommandLine::ProgramMode::ShowHelp:
        {
            std::println("help is still being developed");
            return 0;
        }   
        case CommandLine::ProgramMode::BuildTemplateConfig:
        {
            BuildConfig config;
            config.Serialize(result.Directory / "build.toml");

            std::println("Created build.config at {}", result.Directory.string());
            
            return 0;
        }
        case CommandLine::ProgramMode::Compile:
        {
            std::println("Using llvm version {}", LLVM_VERSION_STRING);

            BuildConfig config = BuildConfig::BuildConfigFromToml(result.Directory / "build.toml");

            std::println("Compiling application {}",  config.ApplicationName);
            std::println("Using standard library {}", config.StandardLibrary.string());

            CompilationManager manager(config);
            manager.LoadSources();
            manager.PropagateSymbolTables();
            manager.GenerateIRAndObjectFiles();
            manager.Emit();

            std::println("Finished compilation");

            return 0;

        }
        default:
        {
            std::println("Not a valid option");
            return -1;
        }
    }
    
}
