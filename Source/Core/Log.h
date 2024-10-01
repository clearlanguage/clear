#pragma once

#include <cstdlib>
#include <iostream>

namespace clear {

	struct TextColor
	{
		inline static const char* Reset = "\033[0m";
		inline static const char* Black = "\033[0;30m";
		inline static const char* Red = "\033[0;31m";
		inline static const char* Green = "\033[0;32m";
		inline static const char* Yellow = "\033[0;33m";
		inline static const char* Blue = "\033[0;34m";
		inline static const char* Magenta = "\033[0;35m";
		inline static const char* Cyan = "\033[0;36m";
		inline static const char* White = "\033[0;37m";
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
		#define CLEAR_ASSERT(condition, ...) if(!(condition)) {CLEAR_LOG_ERROR(__VA_ARGS__); CLEAR_HALT();}
	#else 
		#define CLEAR_ASSERT(condition, ...) 
	#endif

	#define CLEAR_VERIFY(condition, ...) if(!(condition)) {CLEAR_LOG_ERROR(__VA_ARGS__); CLEAR_HALT();}
}