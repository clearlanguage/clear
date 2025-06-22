#pragma once
namespace clear
{
	enum ErrorCode
	{
		ErrorCode_InvalidSyntax,
		ErrorCode_DivisionByZero,
		ErrorCode_InvalidOperator,
		ErrorCode_Count
	};
	static const char* g_ErrorMessages[] = {
		"Syntax is invalid",
		"Cannot divide by 0",
		"whatever",
	};
	static const char* g_ErrorAdvices[] = {
		"",
		"Check denominator",
		"get Good",
	};
}
