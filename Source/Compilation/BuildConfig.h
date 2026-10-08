#pragma once

#include <filesystem>
#include <vector>

namespace clear 
{
    struct CPUFeatures
    {
        bool SSE = false;
        bool SSE2 = false;
        bool SSE3 = false;
        bool SSSE3 = false;
        bool SSE4_1 = false;
        bool SSE4_2 = false;
        bool AVX = false;
        bool AVX2 = false;
        bool AVX512 = false;
        bool NEON = false;       
        bool FMA = false;
        bool BMI1 = false;
        bool BMI2 = false;
    };

    struct BuildConfig
    {
        std::string ApplicationName = "Application name";

        std::vector<std::filesystem::path> SourceDirectories = { "Source directories go here" };
        std::vector<std::filesystem::path> SourceFiles = { "Source files go here" };
        std::vector<std::filesystem::path> LibraryDirectories = { "Any library directories go here" };
        std::vector<std::filesystem::path> LibraryNames = { "Library names go here" };
        std::vector<std::filesystem::path> LibraryFilePaths = {" Library file paths go here " };

        std::filesystem::path TargetExtension = ".cl";
        
        std::filesystem::path OutputPath = "Output path goes here";
        std::filesystem::path OutputFilename = "Output filename goes here";
        std::filesystem::path StandardLibrary = "Input standard library path here";
        
        enum class OptimizationLevelType
        {
            None,        
            Debugging,   
            Development, 
            Distribution 
        } OptimizationLevel = OptimizationLevelType::Development;

        bool DebugInfo  = false;

        bool FavourSize = false; // otherwise performance

        enum class WarningLevelType
        {
            None, Low, Medium, High
        } WarningLevel = WarningLevelType::Medium;

        enum class OutputFormatType
        {
            Executable, IR, ObjectFile, 
            StaticLibrary, DynamicLibrary
        } OutputFormat = OutputFormatType::Executable;

        bool EmitIntermiediateIR = true;

        enum class TargetArchitectureType
        {
            Default, X86, X86_64, ARM,
            ARM64, MIPS, PowerPC,RISCV,
            WebAssembly, SPARC, IBMZ,
            Hexagon, NVPTX
        } TargetArchitecture = TargetArchitectureType::Default;

        bool ParallelBuild = false;

        bool IncludeCStandard = true;
        bool IncludeClearStandard = false;

        CPUFeatures EnabledCPUFeatures;

        bool Verbose = false; // print progress while compiling

        // "generic" runs on any CPU of the target architecture, "native" uses everything the
        // building machine supports (fastest, but the binary may not run elsewhere)
        std::string TargetCPU = "generic";

        static BuildConfig BuildConfigFromToml(const std::filesystem::path& path);

        // configuration used when compiling a single file without a build.toml
        static BuildConfig ForSingleFile(const std::filesystem::path& file, const std::filesystem::path& outputPath);

        void Serialize(const std::filesystem::path& path);
    };
}