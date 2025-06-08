//
// Created by Kareem Fares on 6/8/25.
//


#pragma once
#include <string>
#include <vector>
#include <filesystem>

namespace clear {
    int LevenshteinDistance(const std::string& s1, const std::string& s2);
    std::string SuggestClosestModule(const std::string& missingModule, const std::string& libraryPath);
    std::string Strip(const std::string& str);
    void FindByKeyword(const std::string& keyword, const std::string& end, std::vector<std::string>& res, const std::string& buffer);
    std::vector<std::string> ProcessClearFile(const std::filesystem::path& path);

} // clear

