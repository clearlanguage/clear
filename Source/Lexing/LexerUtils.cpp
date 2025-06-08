//
// Created by Kareem Fares on 6/8/25.
//
#include "LexerUtils.h"
#include <string>
#include <sstream>
#include <vector>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <algorithm>

namespace clear {

	int LevenshteinDistance(const std::string& s1, const std::string& s2) {
		const size_t m = s1.size(), n = s2.size();
		std::vector<std::vector<int>> dp(m + 1, std::vector<int>(n + 1));

		for (size_t i = 0; i <= m; ++i) dp[i][0] = i;
		for (size_t j = 0; j <= n; ++j) dp[0][j] = j;

		for (size_t i = 1; i <= m; ++i) {
			for (size_t j = 1; j <= n; ++j) {
				int cost = (tolower(s1[i - 1]) == tolower(s2[j - 1])) ? 0 : 1;
				dp[i][j] = std::min({
					dp[i - 1][j] + 1,
					dp[i][j - 1] + 1,
					dp[i - 1][j - 1] + cost
				});
			}
		}
		return dp[m][n];
	}

	std::string SuggestClosestModule(const std::string& missingModule, const std::string& libraryPath) {
		std::string bestMatch;
		int minDistance = std::numeric_limits<int>::max();

		for (const auto& entry : std::filesystem::directory_iterator(libraryPath)) {
			if (entry.is_regular_file() && entry.path().extension() == ".cl") {
				std::string filename = entry.path().filename().string();
				int dist = LevenshteinDistance(missingModule, filename);
				if (dist < minDistance) {
					minDistance = dist;
					bestMatch = filename;
				}
			}
		}

		if (!bestMatch.empty() && minDistance <= 3) {
			return "Did you mean to import \"" + bestMatch + "\"?";
		} else {
			return "";
		}
	}

	std::string Strip(const std::string& str)
	{
		size_t start = 0;
		while (start < str.size() && std::isspace(static_cast<unsigned char>(str[start])))
		{
			start++;
		}

		size_t end = str.size();
		while (end > start && std::isspace(static_cast<unsigned char>(str[end - 1])))
		{
			end++;
		}

		return str.substr(start, end - start);
	}

	void FindByKeyword(const std::string& keyword, const std::string& end, std::vector<std::string>& res, const std::string& buffer)
	{
		size_t index = 0;

		while ((index = buffer.find(keyword + " ", index)) != std::string::npos)
		{
			index += keyword.size();
			size_t next = buffer.find(end, index);
			res.push_back(buffer.substr(index, next - index));
			index = next + 1;
		}
	}

	std::vector<std::string> ProcessClearFile(const std::filesystem::path& path)
	{
		std::fstream file;

		file.open(path);

		if (!file.is_open())
		{
			std::cout << "failed to open file " << path << std::endl;
			return {};
		}

		std::string line;
		std::string buffer;
		while(std::getline(file, line))
		{
			buffer += line + '\n';
		}

		std::vector<std::string> result;
		FindByKeyword("class", ":", result, buffer);
		FindByKeyword("restriction", ":", result, buffer);
		FindByKeyword("struct", ":", result, buffer);

		for(size_t i = 0; i < result.size(); i++)
		{
			result[i] = Strip(result[i]);
		}

		return result;
	}
}