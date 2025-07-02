#pragma once

#include <cstdlib>
#include <iostream>

namespace clear {

	struct TextColor
	{
		inline static std::string_view Reset = "\033[0m";
		inline static std::string_view Black = "\033[0;30m";
		inline static std::string_view Red = "\033[0;31m";
		inline static std::string_view Green = "\033[0;32m";
		inline static std::string_view Yellow = "\033[0;33m";
		inline static std::string_view Blue = "\033[0;34m";
		inline static std::string_view Magenta = "\033[0;35m";
		inline static std::string_view Cyan = "\033[0;36m";
		inline static std::string_view White = "\033[0;37m";
	};

	template<typename ...Args>
	void Log(std::string_view color, Args&&... args)
	{
		std::cout << color;
		(std::cout << ... << std::forward<Args>(args));
		std::cout << TextColor::Reset << std::endl;
	}

	#define CLEAR_LOG_INFO(...)    clear::Log(TextColor::Green,  "[Info] ",    __VA_ARGS__)
	#define CLEAR_LOG_WARNING(...) clear::Log(TextColor::Yellow, "[Warning] ", __VA_ARGS__)
	#define CLEAR_LOG_ERROR(...)   clear::Log(TextColor::Red,    "[Error] ",   __VA_ARGS__)
	
	#ifdef WIN32
		#define CLEAR_HALT() __debugbreak()
	#else
		#define CLEAR_HALT() __builtin_trap()
	#endif

	#ifdef _DEBUG
		#define CLEAR_ASSERT(condition, ...) if(!(condition)) {CLEAR_LOG_ERROR(__VA_ARGS__); CLEAR_HALT();}
	#else 
		#define CLEAR_ASSERT(condition, ...) 
	#endif

	#define CLEAR_VERIFY(condition, ...) if(!(condition)) {CLEAR_LOG_ERROR(__VA_ARGS__); CLEAR_HALT();}
	#define CLEAR_PARSER_VERIFY(condition, ...) if(!(condition)) {CLEAR_LOG_ERROR(INTERNAL_PARSER_ERROR_MESSAGE,__VA_ARGS__); CLEAR_HALT();}

	#define CLEAR_UNREACHABLE(...) { CLEAR_LOG_WARNING(__VA_ARGS__); CLEAR_HALT(); }
}