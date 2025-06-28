#pragma once
namespace clear
{
	enum DiagnosticCode
	{
		DiagnosticCode_None,
		DiagnosticCode_InvalidSyntax,
		DiagnosticCode_DivisionByZero,
		DiagnosticCode_InvalidOperator,
		Diagnostic_Count
	};
	static const char* g_DiagnosticMessages[] = {
		"An unknown error has occurred.",
		"Syntax error: unexpected or invalid token.",
		"Division by zero is undefined.",
		"Invalid or unsupported operator used.",
	};
	static const char* g_DiagnosticAdvices[] = {
		"The issue occurred at {}. Please report this if unexpected.",
		"‘{}’ is not recognized as valid syntax. Check for typos or missing characters.",
		"Ensure the denominator is not zero before performing division.",
		"‘{}’ is not a valid operator in this context. Check the operator or operand types.",
	};
}
