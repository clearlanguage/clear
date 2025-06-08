#pragma once
namespace clear
{
	enum ErrorCode
	{
		ErrorCode_InvalidSyntax,
		ErrorCode_DivisionByZero,
		ErrorCode_Count
	};
	static const char* g_ErrorMessages[] = {
		"Syntax is invalid",
		"Cannot divide by 0",
	};
	static const char* g_ErrorAdvices[] = {
		"",
		"Check denominator",
	};
}
