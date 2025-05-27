//
// Created by Kareem Fares on 5/27/25.
//

#pragma once
#include <vector>
#include <string>
#include <filesystem>

namespace clear {

struct Libraries {
    std::vector<std::string> ClearFiles;
    std::vector<std::string> LibFiles;

};

struct BuildConfig {
     std::filesystem::path EntryPoint;
};


class Module {
    public:
    std::filesystem::path Path;

    Module(const std::filesystem::path& path);
    ~Module() = default;
    void Build();

};
class Linker {

    void GenerateLibraries(Libraries& lib,const std::filesystem::path& file);
    public:
        int Build(BuildConfig config);
};

}


