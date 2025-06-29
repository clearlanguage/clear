#pragma once
namespace clear
{
	enum DiagnosticCode
	{
		DiagnosticCode_None,
		DiagnosticCode_InvalidSyntax,
		DiagnosticCode_DivisionByZero,
		DiagnosticCode_InvalidOperator,
		DiagnosticCode_UnterminatedComment,
		DiagnosticCode_UnterminatedString,
		DiagnosticCode_UnexpectedEOF,
		DiagnosticCode_InvalidNumberLiteral,
		DiagnosticCode_RedefinedIdentifier,
		DiagnosticCode_UndeclaredIdentifier,
		DiagnosticCode_TypeMismatch,
		DiagnosticCode_InvalidEscapeSequence,
		DiagnosticCode_UnmatchedBracket,
		Diagnostic_Count
	};
	inline const char* g_DiagnosticMessages[] = {
		"An unknown error has occurred.",
		"Syntax error: unexpected or invalid token.",
		"Division by zero is undefined.",
		"Invalid or unsupported operator used.",
		"Unterminated block comment detected.",
		"Unterminated string literal.",
		"Unexpected end of file.",
		"Invalid numeric literal.",
		"Redefinition of identifier.",
		"Use of undeclared identifier.",
		"Type mismatch in assignment or operation.",
		"Invalid escape sequence in string literal.",
		"Unmatched closing or opening bracket.",
	};
	inline const char* g_DiagnosticAdvices[] = {
		"The issue occurred at {}. Please report this if unexpected.",
		"‘{}’ is not recognized as valid syntax. Check for typos or missing characters.",
		"Ensure the denominator is not zero before performing division.",
		"‘{}’ is not a valid operator in this context. Check the operator or operand types.",
		"Make sure every ‘/*’ has a matching ‘*/’. Comment started near {}.",
		"A string literal starting at ‘{}’ was not closed. Ensure all strings end with a quote.",
		"The file ended prematurely. Check for unclosed brackets, comments, or incomplete statements.",
		"‘{}’ is not a valid number. Ensure correct syntax for integers and decimals.",
		"‘{}’ was already defined in this scope. Rename or remove the duplicate declaration.",
		"‘{}’ is not defined in the current scope. Check for typos or missing declarations.",
		"Cannot assign or operate on incompatible types like ‘{}’. Check for implicit casts or incorrect types.",
		"‘{}’ is not a recognized escape sequence. Use sequences like \n, \t, \".",
		"There is an unmatched ‘{}’. Check your use of (), {}, and [].",
	};
}
