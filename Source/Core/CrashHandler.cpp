#include "CrashHandler.h"

#include <llvm/Support/Signals.h>
#include <llvm/Support/raw_ostream.h>

#include <csignal>
#include <cstdio>
#include <unistd.h>

namespace clear
{
	static const char* SignalName(int signal)
	{
		switch (signal)
		{
			case SIGSEGV: return "segmentation fault";
			case SIGBUS:  return "bus error";
			case SIGILL:  return "illegal instruction";
			case SIGTRAP: return "internal check failed";
			case SIGABRT: return "aborted";
			case SIGFPE:  return "arithmetic error";
			default:      return "signal";
		}
	}

	static void OnCrash(int signal)
	{
		// only what is needed to point at the cause, then die from the same signal so callers still see a crash
		std::fprintf(stderr, "\ninternal compiler error: %s while %s", SignalName(signal), g_Progress.Stage);

		if (g_Progress.File[0])
			std::fprintf(stderr, " %s:%zu:%zu", g_Progress.File, g_Progress.Line + 1, g_Progress.Column + 1);

		std::fprintf(stderr, "\n  = help: this is a bug in clearc, not in your program. Please report it with the code around that line.\n\n");
		llvm::sys::PrintStackTrace(llvm::errs());
		llvm::errs().flush();

		std::signal(signal, SIG_DFL);
		std::raise(signal);
	}

	void InstallCrashHandler()
	{
		for (int signal : { SIGSEGV, SIGBUS, SIGILL, SIGTRAP, SIGABRT, SIGFPE })
			std::signal(signal, OnCrash);
	}
}
