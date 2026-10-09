#pragma once

#include <cstddef>
#include <filesystem>

namespace clear
{
	// what the compiler is working on, so a crash can say where it happened
	struct CompilerProgress
	{
		const char* Stage = "starting up";
		char File[1024] = {};              // a copy: the node the name came from may be gone by the time of a crash
		const void* FileSource = nullptr;  // where the copy was taken from (copied again only when this changes)
		size_t Line = 0;
		size_t Column = 0;
	};

	inline CompilerProgress g_Progress;

	inline void NoteProgress(const char* stage, const std::filesystem::path& file, size_t line, size_t column)
	{
		if (file.empty())
			return;

		if (g_Progress.FileSource != &file)
		{
			const std::string& name = file.native();
			size_t length = std::min(name.size(), sizeof(g_Progress.File) - 1);
			name.copy(g_Progress.File, length);
			g_Progress.File[length] = 0;
			g_Progress.FileSource = &file;
		}

		g_Progress.Stage = stage;
		g_Progress.Line = line;
		g_Progress.Column = column;
	}

	// turns a crash inside the compiler (segfault, failed internal check) into an "internal compiler error" report
	void InstallCrashHandler();
}
