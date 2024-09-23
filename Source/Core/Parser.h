#pragma once

#include <filesystem>

namespace alkhat
{

	class Parser
	{
	public:
		static std::vector<std::string> ParseFile(const std::filesystem::path& path);
	};

}
