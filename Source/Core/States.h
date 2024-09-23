#pragma once

#include "Tokenizer.h"

#include <string>

namespace clear
{
	class States
	{
	public:
		static void NoneState(std::string& current, ProgramInfo& info, Tokenizer::CurrentState& state);
		static void VariableNameState(std::string& current, ProgramInfo& info, Tokenizer::CurrentState& state);
		static void RValueState(std::string& current, ProgramInfo& info, Tokenizer::CurrentState& state);
		static void ParsingArguments(std::string& current, ProgramInfo& info, Tokenizer::CurrentState& state);
		static void AssignmentState(std::string& current, ProgramInfo& info, Tokenizer::CurrentState& state);

	private:
		static void _SwitchState(Tokenizer::CurrentState& state, TokenType token);
	};

}