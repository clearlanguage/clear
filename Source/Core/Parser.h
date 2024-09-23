#pragma once

#include <filesystem>

namespace clear
{

	class Parser
	{
	public:
		static std::vector<std::string> ParseFile(const std::filesystem::path& path);
	};

}
