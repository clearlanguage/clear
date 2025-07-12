#include "BuildConfig.h"
#include "Core/Log.h"

#include <filesystem>
#include <toml++/toml.h>
#include <map>

namespace clear
{
    BuildConfig BuildConfig::BuildConfigFromToml(const std::filesystem::path& path)
    {
        CLEAR_VERIFY(std::filesystem::exists(path), "TOML config not found: ", path);

        toml::table tbl = toml::parse_file(path.string());

        BuildConfig config;

        auto getStrEnum = [](const std::string& str, auto map, auto defaultVal) 
        {
            auto it = map.find(str);
            return it != map.end() ? it->second : defaultVal;
        };  

        const std::map<std::string, BuildConfig::OptimizationLevelType> optLevels = {
            {"None", BuildConfig::OptimizationLevelType::None},
            {"Debugging", BuildConfig::OptimizationLevelType::Debugging},
            {"Development", BuildConfig::OptimizationLevelType::Development},
            {"Distribution", BuildConfig::OptimizationLevelType::Distribution}
        };

        const std::map<std::string, BuildConfig::WarningLevelType> warningLevels = {
            {"None", BuildConfig::WarningLevelType::None},
            {"Low", BuildConfig::WarningLevelType::Low},
            {"Medium", BuildConfig::WarningLevelType::Medium},
            {"High", BuildConfig::WarningLevelType::High}
        };

        const std::map<std::string, BuildConfig::OutputFormatType> outputFormats = {
            {"Executable", BuildConfig::OutputFormatType::Executable},
            {"IR", BuildConfig::OutputFormatType::IR},
            {"ObjectFile", BuildConfig::OutputFormatType::ObjectFile},
            {"StaticLibrary", BuildConfig::OutputFormatType::StaticLibrary},
            {"DynamicLibrary", BuildConfig::OutputFormatType::DynamicLibrary}
        };

        const std::map<std::string, BuildConfig::TargetArchitectureType> archMap = {
            {"Default", BuildConfig::TargetArchitectureType::Default},
            {"X86", BuildConfig::TargetArchitectureType::X86},
            {"X86_64", BuildConfig::TargetArchitectureType::X86_64},
            {"ARM", BuildConfig::TargetArchitectureType::ARM},
            {"ARM64", BuildConfig::TargetArchitectureType::ARM64},
            {"MIPS", BuildConfig::TargetArchitectureType::MIPS},
            {"PowerPC", BuildConfig::TargetArchitectureType::PowerPC},
            {"RISCV", BuildConfig::TargetArchitectureType::RISCV},
            {"WebAssembly", BuildConfig::TargetArchitectureType::WebAssembly},
            {"SPARC", BuildConfig::TargetArchitectureType::SPARC},
            {"IBMZ", BuildConfig::TargetArchitectureType::IBMZ},
            {"Hexagon", BuildConfig::TargetArchitectureType::Hexagon},
            {"NVPTX", BuildConfig::TargetArchitectureType::NVPTX}
        };

        auto loadPathArray = [](const toml::array* arr) -> std::vector<std::filesystem::path> {
            std::vector<std::filesystem::path> result;
            if (!arr) return result;
            for (const auto& val : *arr) 
            {
                if (auto s = val.value<std::string>()) 
                {
                    result.emplace_back(*s);
                }
            }
            return result;
        };

        config.ApplicationName = tbl["ApplicationName"].value_or("UnnamedApp");

        config.SourceDirectories = loadPathArray(tbl["SourceDirectories"].as_array());
        config.SourceFiles = loadPathArray(tbl["SourceFiles"].as_array());
        config.LibraryDirectories = loadPathArray(tbl["LibraryDirectories"].as_array());
        config.LibraryNames = loadPathArray(tbl["LibraryNames"].as_array());
        config.LibraryFilePaths = loadPathArray(tbl["LibraryFilePaths"].as_array());

        if (auto v = tbl["OutputPath"].value<std::string>()) config.OutputPath = *v;
        if (auto v = tbl["OutputFilename"].value<std::string>()) config.OutputFilename = *v;
        if (auto v = tbl["StandardLibrary"].value<std::string>()) config.StandardLibrary = *v;

        config.OptimizationLevel = getStrEnum(tbl["OptimizationLevel"].value_or("Development"), optLevels, BuildConfig::OptimizationLevelType::Development);
        config.WarningLevel = getStrEnum(tbl["WarningLevel"].value_or("Medium"), warningLevels, BuildConfig::WarningLevelType::Medium);
        config.OutputFormat = getStrEnum(tbl["OutputFormat"].value_or("Executable"), outputFormats, BuildConfig::OutputFormatType::Executable);
        config.TargetArchitecture = getStrEnum(tbl["TargetArchitecture"].value_or("Default"), archMap, BuildConfig::TargetArchitectureType::Default);

        config.DebugInfo = tbl["DebugInfo"].value_or(false);
        config.FavourSize = tbl["FavourSize"].value_or(false);
        config.EmitIntermiediateIR = tbl["EmitIntermiediateIR"].value_or(false);
        config.ParallelBuild = tbl["ParallelBuild"].value_or(false);
        config.IncludeCStandard = tbl["IncludeCStandard"].value_or(true);
        config.IncludeClearStandard = tbl["IncludeClearStandard"].value_or(true);

        if (const auto* cpu = tbl["EnabledCPUFeatures"].as_table()) 
        {
            auto& f = config.EnabledCPUFeatures;
            auto get = [&](const char* key) 
            {
                if(cpu->contains(key)) 
                    return cpu->at(key).value_or(false); 

                return false;
            };

            f.SSE = get("SSE");
            f.SSE2 = get("SSE2");
            f.SSE3 = get("SSE3");
            f.SSSE3 = get("SSSE3");
            f.SSE4_1 = get("SSE4_1");
            f.SSE4_2 = get("SSE4_2");
            f.AVX = get("AVX");
            f.AVX2 = get("AVX2");
            f.AVX512 = get("AVX512");
            f.NEON = get("NEON");
            f.FMA = get("FMA");
            f.BMI1 = get("BMI1");
            f.BMI2 = get("BMI2");
        }

        return config;
    }


    void BuildConfig::Serialize(const std::filesystem::path& path)
    {
        auto enumToString = [](auto value) -> std::string {
            using T = decltype(value);
            if constexpr (std::is_same_v<T, OptimizationLevelType>) 
            {
                switch (value) 
                {
                    case OptimizationLevelType::None:         return "None";
                    case OptimizationLevelType::Debugging:    return "Debugging";
                    case OptimizationLevelType::Development:  return "Development";
                    case OptimizationLevelType::Distribution: return "Distribution";
                }
            }
            else if constexpr (std::is_same_v<T, WarningLevelType>) 
            {
                switch (value) 
                {
                    case WarningLevelType::None:   return "None";
                    case WarningLevelType::Low:    return "Low";
                    case WarningLevelType::Medium: return "Medium";
                    case WarningLevelType::High:   return "High";
                }
            }
            else if constexpr (std::is_same_v<T, OutputFormatType>) 
            {
                switch (value) 
                {
                    case OutputFormatType::Executable:     return "Executable";
                    case OutputFormatType::IR:             return "IR";
                    case OutputFormatType::ObjectFile:     return "ObjectFile";
                    case OutputFormatType::StaticLibrary:  return "StaticLibrary";
                    case OutputFormatType::DynamicLibrary: return "DynamicLibrary";
                }
            }
            else if constexpr (std::is_same_v<T, TargetArchitectureType>) 
            {
                switch (value) 
                {
                    case TargetArchitectureType::Default:     return "Default";
                    case TargetArchitectureType::X86:         return "X86";
                    case TargetArchitectureType::X86_64:      return "X86_64";
                    case TargetArchitectureType::ARM:         return "ARM";
                    case TargetArchitectureType::ARM64:       return "ARM64";
                    case TargetArchitectureType::MIPS:        return "MIPS";
                    case TargetArchitectureType::PowerPC:     return "PowerPC";
                    case TargetArchitectureType::RISCV:       return "RISCV";
                    case TargetArchitectureType::WebAssembly: return "WebAssembly";
                    case TargetArchitectureType::SPARC:       return "SPARC";
                    case TargetArchitectureType::IBMZ:        return "IBMZ";
                    case TargetArchitectureType::Hexagon:     return "Hexagon";
                    case TargetArchitectureType::NVPTX:       return "NVPTX";
                }
            }
            return "Unknown";
        };

        auto toTomlArray = [](const auto& paths) 
        {
            toml::array arr;
            for (const auto& p : paths)
                arr.push_back(p.string());
            return arr;
        };

        toml::table tbl = toml::table {
            { "ApplicationName",        ApplicationName },
            { "SourceDirectories",      toTomlArray(SourceDirectories) },
            { "SourceFiles",            toTomlArray(SourceFiles) },
            { "LibraryDirectories",     toTomlArray(LibraryDirectories) },
            { "LibraryNames",           toTomlArray(LibraryNames)},
            { "TargetExtension",        TargetExtension.string() },
            { "OutputPath",             OutputPath.string() },
            { "OutputFilename",         OutputFilename.string() },
            { "StandardLibrary",        StandardLibrary.string()},
            { "OptimizationLevel",      enumToString(OptimizationLevel) },
            { "DebugInfo",              DebugInfo },
            { "FavourSize",             FavourSize },
            { "WarningLevel",           enumToString(WarningLevel) },
            { "OutputFormat",           enumToString(OutputFormat) },
            { "EmitIntermiediateIR",    EmitIntermiediateIR },
            { "TargetArchitecture",     enumToString(TargetArchitecture) },
            { "ParallelBuild",          ParallelBuild },
            { "IncludeCStandard",       IncludeCStandard },
            { "IncludeClearStandard",   IncludeClearStandard },
            { "CPUFeatures", toml::table{
                    { "SSE",    EnabledCPUFeatures.SSE },
                    { "SSE2",   EnabledCPUFeatures.SSE2 },
                    { "SSE3",   EnabledCPUFeatures.SSE3 },
                    { "SSSE3",  EnabledCPUFeatures.SSSE3 },
                    { "SSE4_1", EnabledCPUFeatures.SSE4_1 },
                    { "SSE4_2", EnabledCPUFeatures.SSE4_2 },
                    { "AVX",    EnabledCPUFeatures.AVX },
                    { "AVX2",   EnabledCPUFeatures.AVX2 },
                    { "AVX512", EnabledCPUFeatures.AVX512 },
                    { "NEON",   EnabledCPUFeatures.NEON },
                    { "FMA",    EnabledCPUFeatures.FMA },
                    { "BMI1",   EnabledCPUFeatures.BMI1 },
                    { "BMI2",   EnabledCPUFeatures.BMI2 }
                }
            },
        };

        std::ofstream ofs(path);
        ofs << tbl;
    }
}   
