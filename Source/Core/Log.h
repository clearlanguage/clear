#pragma once

#include <cstdlib>

namespace clear {

	struct TextColor
	{
		static const char* Reset = "\033[0m";
		static const char* Black = "\033[0;30m";
		static const char* Red = "\033[0;31m";
		static const char* Green = "\033[0;32m";
		static const char* Yellow = "\033[0;33m";
		static const char* Blue = "\033[0;34m";
		static const char* Magenta = "\033[0;35m";
		static const char* Cyan = "\033[0;36m";
		static const char* White = "\033[0;37m";
	};

	template<typename ...Args>
	void Log(const char* Color, Args&&... args)
	{
		std::cout << Color;
		(std::cout << ... << std::forward<Args>(args));
		std::cout << TextColor::Reset << std::endl;
	}

	#define CLEAR_LOG_INFO(...)    clear::Log(TextColor::Green,  "[Info] ",    __VA_ARGS__)
	#define CLEAR_LOG_WARNING(...) clear::Log(TextColor::Yellow, "[Warning] ", __VA_ARGS__)
	#define CLEAR_LOG_ERROR(...)   clear::Log(TextColor::Red,    "[Error] ",   __VA_ARGS__)
	
	#ifdef WIN32
		#define CLEAR_HALT() __debugbreak()
	#elif defined(__APPLE__)
		#define CLEAR_HALT() __builtin_trap()
	#else 
		#define CLEAR_HALT() std::abort()
	#endif

	#ifdef _DEBUG
		#define CLEAR_ASSERT(condition, ...) if(!(condition)) {TGE_LOG_ERROR(__VA_ARGS__); TGE_HALT();}
	#else 
		#define CLEAR_ASSERT(condition, ...) 
	#endif

	#define CLEAR_VERIFY(condition, ...) if(!(condition)) {TGE_LOG_ERROR(__VA_ARGS__); TGE_HALT();}
}