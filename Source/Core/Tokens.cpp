//
// Created by Kareem Fares on 9/27/24.
//

#include "Tokens.h"
#include <string_view>
#include <set>

namespace clear {
    const OperatorMapType g_OperatorMap = {
        {"=", {.NextState = ParserState::RValue, .TokenToPush = TokenType::Assignment}},
        {"*", {.NextState = ParserState::RValue, .TokenToPush = TokenType::MulOp}},
        {"/", {.NextState = ParserState::RValue, .TokenToPush = TokenType::DivOp}},
        {"-", {.NextState = ParserState::RValue, .TokenToPush = TokenType::SubOp}},
        {"%", {.NextState = ParserState::RValue, .TokenToPush = TokenType::ModOp}},
        {"+", {.NextState = ParserState::RValue, .TokenToPush = TokenType::AddOp}},
        {"==", {.NextState = ParserState::RValue, .TokenToPush = TokenType::IsEqual}},
        {"<", {.NextState = ParserState::RValue, .TokenToPush = TokenType::LessThan}},
        {">", {.NextState = ParserState::RValue, .TokenToPush = TokenType::GreaterThan}},
        {"!=", {.NextState = ParserState::RValue, .TokenToPush = TokenType::NotEqual}},
        {"<=", {.NextState = ParserState::RValue, .TokenToPush = TokenType::LessThanEqual}},
        {">=", {.NextState = ParserState::RValue, .TokenToPush = TokenType::GreaterThanEqual}},
        {"...", {.NextState = ParserState::RValue, .TokenToPush = TokenType::Ellipsis}},
         {"!", {.NextState = ParserState::RValue, .TokenToPush = TokenType::Not}},
        {"->", {.NextState = ParserState::ArrowState, .TokenToPush = TokenType::Arrow}},
        {".",{.NextState = ParserState::RValue, .TokenToPush = TokenType::DotOp}},
        {"//",{.NextState = ParserState::Comment, .TokenToPush = TokenType::None}},
        {"/*",{.NextState = ParserState::MultilineComment, .TokenToPush = TokenType::None}},
        {",",{.NextState = ParserState::Default, .TokenToPush = TokenType::Comma}},
    };

        const KeyWordMapType g_KeyWordMap = {
        {"int8", {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Int8Type}},
        {"int16", {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Int16Type}},
        {"int32", {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Int32Type}},
        {"int64", {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Int64Type}},
        {"uint8", {.NextState = ParserState::VariableName, .TokenToPush = TokenType::UInt8Type}},
        {"uint16", {.NextState = ParserState::VariableName, .TokenToPush = TokenType::UInt16Type}},
        {"uint32", {.NextState = ParserState::VariableName, .TokenToPush = TokenType::UInt32Type}},
        {"uint64", {.NextState = ParserState::VariableName, .TokenToPush = TokenType::UInt64Type}},
        {"string", {.NextState = ParserState::VariableName, .TokenToPush = TokenType::StringType}},
        {"bool", {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Bool}},
        {"float32", {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Float32Type}},
        {"float64", {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Float64Type}},
        {"false", {.NextState = ParserState::Default, .TokenToPush = TokenType::BooleanData}},
        {"true", {.NextState = ParserState::Default, .TokenToPush = TokenType::BooleanData}},
        {"null", {.NextState = ParserState::Default, .TokenToPush = TokenType::Null}},
        {"if", {.NextState = ParserState::RValue, .TokenToPush = TokenType::ConditionalIf}},
        {"function", {.NextState = ParserState::FunctionName, .TokenToPush = TokenType::Function}},
        {"int",{.NextState = ParserState::VariableName, .TokenToPush = TokenType::Int32Type}},
        {"uint", {.NextState = ParserState::VariableName, .TokenToPush = TokenType::UInt32Type}},
        {"struct",{.NextState = ParserState::StructName, .TokenToPush = TokenType::Struct}},

    };



    const std::set<std::string> g_DataTypes = {
        "float64", "float32", "bool", "string", "uint64", "uint32", "uint16", "uint8", "int64", "int32", "int16", "int8","int","uint"
    };

    std::string_view TokenToString(TokenType token) {
        switch (token) {
            case TokenType::None: return "None";
            case TokenType::Int8Type: return "Int8Type";
            case TokenType::Int16Type: return "Int16Type";
            case TokenType::Int32Type: return "Int32Type";
            case TokenType::Int64Type: return "Int64Type";
            case TokenType::UInt8Type: return "UInt8Type";
            case TokenType::UInt16Type: return "UInt16Type";
            case TokenType::UInt32Type: return "UInt32Type";
            case TokenType::UInt64Type: return "UInt64Type";
            case TokenType::Float32Type: return "Float32Type";
            case TokenType::Float64Type: return "Float64Type";
            case TokenType::StringType: return "StringType";
            case TokenType::VariableName: return "VariableName";
            case TokenType::Assignment: return "Assignment";
            case TokenType::RValueNumber: return "RValueNumber";
            case TokenType::RValueString: return "RValueString";
            case TokenType::MulOp: return "MulOp";
            case TokenType::AddOp: return "AddOp";
            case TokenType::DivOp: return "DivOp";
            case TokenType::SubOp: return "SubOp";
            case TokenType::Bool: return "Bool";
            case TokenType::CloseBracket: return "CloseBracket";
            case TokenType::OpenBracket: return "OpenBracket";
            case TokenType::BooleanData: return "BooleanData";
            case TokenType::ConditionalIf: return "ConditionalIf";
            case TokenType::IsEqual: return "IsEqual";
            case TokenType::Null: return "Null";
            case TokenType::NotEqual: return "NotEqual";
            case TokenType::GreaterThan: return "GreaterThan";
            case TokenType::GreaterThanEqual: return "GreaterThanEqual";
            case TokenType::LessThanEqual: return "LessThanEqual";
            case TokenType::LessThan: return "LessThan";
            case TokenType::Not: return "Not";
            case TokenType::Ellipsis: return "Ellipsis";
            case TokenType::DotOp: return "DotOp";
            case TokenType::StartIndentation: return "StartIndentation";
            case TokenType::EndIndentation: return "EndIndentation";
            case TokenType::EndLine: return "EndLine";
            case TokenType::FunctionName: return "FunctionName";
            case TokenType::Function: return "Function";
            case TokenType::EndFunctionParameters: return "EndFunctionParameters";
            case TokenType::StartFunctionParameters: return "StartFunctionParameters";
            case TokenType::FunctionType: return "FunctionType";
            case TokenType::Arrow: return "Arrow";
            case TokenType::Lambda: return "Lambda";
            case TokenType::VariableReference: return "VariableReference";
            case TokenType::Struct: return "Struct";
            case TokenType::StructName: return "StructName";
            case TokenType::FunctionCall: return "FunctionCall";
            case TokenType::Comma: return "Comma";
            case TokenType::RValueChar : return "RValueChar";
            case TokenType::IndexOperator : return "IndexOperator";

            default:
                break;
        }

        return "";
    }
}
