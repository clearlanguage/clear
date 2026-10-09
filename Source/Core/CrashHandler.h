#pragma once

#include <cstddef>
#include <filesystem>

namespace clear
{
	// what the compiler is working on, so a crash can say where it happened
	struct CompilerProgress
	{
		const char* Stage = "starting up";
		const std::filesystem::path* File = nullptr;
		size_t Line = 0;
		size_t Column = 0;
	};

	inline CompilerProgress g_Progress;

	inline void NoteProgress(const char* stage, const std::filesystem::path& file, size_t line, size_t column)
	{
		if (file.empty())
			return;

		g_Progress = { stage, &file, line, column };
	}

	// turns a crash inside the compiler (segfault, failed internal check) into an "internal compiler error" report
	void InstallCrashHandler();
}
