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
        std::string ApplicationName;

        std::vector<std::filesystem::path> SourceDirectories;
        std::vector<std::filesystem::path> SourceFiles;
        std::vector<std::filesystem::path> LibraryDirectories;
        std::vector<std::filesystem::path> LibraryNames;
        std::vector<std::filesystem::path> LibraryFilePaths;

        std::filesystem::path TargetExtension = ".cl";
        
        std::filesystem::path OutputPath;
        std::filesystem::path OutputFilename;
        
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

        CPUFeatures EnabledCPUFeatures;
    };
}