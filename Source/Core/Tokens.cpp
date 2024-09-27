//
// Created by Kareem Fares on 9/27/24.
//

#include "Tokens.h"
#include <string_view>

namespace clear{
    const OperatorMapType s_OperatorMap = {

        {"=", {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::Assignment}},
        {"*", {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::MulOp}},
        {"/", {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::DivOp}},
        {"-", {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::SubOp}},
        {"%", {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::ModOp}},
        {"+", {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::AddOp}},
        {"==", {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::IsEqual}},
        {"<", {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::LessThan}},
        {">", {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::GreaterThan}},
        {"!=", {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::NotEqual}},
        {"<=", {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::LessThanEqual}},
        {">=", {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::GreaterThanEqual}},
        {"...", {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::Ellipsis}},
        {".", {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::DotOp}},
        {"!", {.NextState = CurrentParserState::Operator, .TokenToPush = TokenType::Not}},
    };

    const KeyWordMapType s_KeyWordMap  = {
    {"int8",{.NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::Int8Type }},
    {"int16",{ .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::Int16Type  }},
	{"int32",{ .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::Int32Type  }},
	{"int64",{ .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::Int64Type  }},
	{"uint8",{ .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::UInt8Type  }},
	{"uint16",{ .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::UInt16Type  }},
	{"uint32",{ .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::UInt32Type  }},
	{"uint64",{ .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::UInt64Type  }},
	{"string",{ .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::StringType  }},
	{"bool",{ .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::Bool  }},
	{"float32" , { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::Float32Type  }},
	{"float64" , { .NextState = CurrentParserState::VariableName, .TokenToPush = TokenType::Float64Type  }},
	{"false" , {.NextState = CurrentParserState::Default, .TokenToPush = TokenType::BooleanData }},
	{"true" ,  {.NextState = CurrentParserState::Default, .TokenToPush = TokenType::BooleanData }},
	{"null" , {.NextState = CurrentParserState::Default, .TokenToPush = TokenType::Null }},
	{"if" , {.NextState = CurrentParserState::RValue, .TokenToPush = TokenType::ConditionalIf }}

};


    std::string_view TokenToString(TokenType token)
    {
        switch (token)
        {
            case TokenType::None:			return "None";
            case TokenType::Int8Type:		return "Int8Type";
            case TokenType::Int16Type:		return "Int16Type";
            case TokenType::Int32Type:		return "Int32Type";
            case TokenType::Int64Type:		return "Int64Type";
            case TokenType::UInt8Type:		return "UInt8Type";
            case TokenType::UInt16Type:		return "UInt16Type";
            case TokenType::UInt32Type:		return "UInt32Type";
            case TokenType::UInt64Type:		return "UInt64Type";
            case TokenType::Float32Type:	return "Float32Type";
            case TokenType::Float64Type:	return "Float64Type";
            case TokenType::StringType:		return "StringType";
            case TokenType::VariableName:	return "VariableName";
            case TokenType::Assignment:		return "Assignment";
            case TokenType::RValueNumber:	return "RValueNumber";
            case TokenType::RValueString:	return "RValueString";
            case TokenType::MulOp:			return "MulOp";
            case TokenType::AddOp:			return "AddOp";
            case TokenType::DivOp:			return "DivOp";
            case TokenType::SubOp:			return "SubOp";
            case TokenType::Bool:			return "Bool";
            case TokenType::CloseBracket:	return "CloseBracket";
            case TokenType::OpenBracket:	return "OpenBracket";
            case TokenType::BooleanData:	return "BooleanData";
            case TokenType::ConditionalIf:  return "ConditionalIf";
            case TokenType::IsEqual:		return "IsEqual";
            case TokenType::Null:           return "Null";
            case TokenType::NotEqual:       return "NotEqual";
            case TokenType::GreaterThan:	return "GreaterThan";
            case TokenType::GreaterThanEqual:return "GreaterThanEqual";
            case TokenType::LessThanEqual:	return "LessThanEqual";
            case TokenType::LessThan:		return "LessThan";
            case TokenType::Not:			return "Not";
            case TokenType::Ellipsis:		return "Ellipsis";
            case TokenType::DotOp: 			return "DotOp";


            default: break;
        }

        return "";
    }
    
    

}