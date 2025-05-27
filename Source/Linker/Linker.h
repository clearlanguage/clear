//
// Created by Kareem Fares on 5/27/25.
//

#pragma ONCE
#include <vector>
#include <string>
#include <filesystem>

namespace clear {

struct Libraries {
    std::vector<std::string> ClearFiles;
    std::vector<std::string> LibFiles;

};
class Linker {

    void GenerateLibraries(Libraries* lib,const std::filesystem::path& file);
};

}


