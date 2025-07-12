#pragma once 

#include <filesystem>

namespace clear 
{
    namespace CommandLine
    {
         // May include adding source directories and file directly in command line in future 
         // for now build.toml is required for compilation

        enum class ProgramMode
        {
            None = 0, 
            ShowHelp, // --help
            BuildTemplateConfig,  // --build_template source_directory
            Compile  // --compile directory with build.toml
        };

        struct ParsingResult
        {
            ProgramMode Options = ProgramMode::None;
            std::filesystem::path Directory;
        };  

        ParsingResult Parse(int argc, char* argv[]);
    }
}