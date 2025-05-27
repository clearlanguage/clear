//
// Created by Kareem Fares on 5/27/25.
//

#pragma once
#include <vector>
#include <string>
#include <filesystem>
#include <AST/ASTNode.h>

namespace clear {

struct Libraries {
    std::vector<std::string> ClearImports;
    std::vector<std::string> LibImports;

};

struct BuildConfig {
     std::filesystem::path EntryPoint;
};


class Module {
    void CollectImportPaths(const std::shared_ptr<ASTNodeBase>& node,std::vector<std::string>& importPaths);
    public:
    std::filesystem::path Path;
    std::vector<std::string> importPaths;

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


