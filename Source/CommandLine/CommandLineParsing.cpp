#include "CommandLineParsing.h"
#include "Core/Log.h"

#include <filesystem>
#include <vector>
#include <string_view>

namespace clear 
{
    namespace CommandLine 
    {
        ParsingResult Parse(int argc, char* argv[])
        {
            const std::vector<std::string_view> arguments(argv + 1, argv + argc);

            CLEAR_VERIFY(arguments.size() <= 2, "too many arguments");

            ParsingResult result;
            
            for(std::string_view arg : arguments)
            {
                if(arg == "--help")
                {
                    result.Options = ProgramMode::ShowHelp;
                    return result;
                }

                if(arg == "--build_template")
                {
                    result.Options = ProgramMode::BuildTemplateConfig;
                    continue;
                }

                if(arg == "--compile")
                {
                    result.Options = ProgramMode::Compile;
                    continue;
                }

                result.Directory = std::filesystem::path(arg);
                CLEAR_VERIFY(std::filesystem::exists(result.Directory), "directory ", result.Directory, " doesn't exist");
            }
            
            return result;
        }
    }
}
