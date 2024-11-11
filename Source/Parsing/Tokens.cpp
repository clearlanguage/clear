#include "Tokens.h"

#include <string_view>
#include <set>

namespace clear {
    const std::set<char> g_Operators = {'=','*','/','-','%','+','<','>','!','.',',','&','^','|','~',';'};
    const OperatorMapType g_OperatorMap = {
        {"=",   {.NextState = ParserState::RValue, .TokenToPush = TokenType::Assignment}},
        {"*",   {.NextState = ParserState::AsterisksOperator, .TokenToPush = TokenType::None}},
        {"/",   {.NextState = ParserState::RValue, .TokenToPush = TokenType::DivOp}},
        {"-",   {.NextState = ParserState::MinusOperator, .TokenToPush = TokenType::None}},
        {"%",   {.NextState = ParserState::RValue, .TokenToPush = TokenType::ModOp}},
        {"+",   {.NextState = ParserState::RValue, .TokenToPush = TokenType::AddOp}},
        {"==",  {.NextState = ParserState::RValue, .TokenToPush = TokenType::IsEqual}},
        {"<",   {.NextState = ParserState::RValue, .TokenToPush = TokenType::LessThan}},
        {">",   {.NextState = ParserState::RValue, .TokenToPush = TokenType::GreaterThan}},
        {"!=",  {.NextState = ParserState::RValue, .TokenToPush = TokenType::NotEqual}},
        {"<=",  {.NextState = ParserState::RValue, .TokenToPush = TokenType::LessThanEqual}},
        {">=",  {.NextState = ParserState::RValue, .TokenToPush = TokenType::GreaterThanEqual}},
        {"...", {.NextState = ParserState::Default, .TokenToPush = TokenType::Ellipsis}},
         {"!",  {.NextState = ParserState::RValue, .TokenToPush = TokenType::Not}},
        {"->",  {.NextState = ParserState::ArrowState, .TokenToPush = TokenType::RightArrow}},
        {".",   {.NextState = ParserState::RValue, .TokenToPush = TokenType::DotOp}},
        {"//",  {.NextState = ParserState::Comment, .TokenToPush = TokenType::None}},
        {"/*",  {.NextState = ParserState::MultilineComment, .TokenToPush = TokenType::None}},
        {",",   {.NextState = ParserState::Default, .TokenToPush = TokenType::Comma}},
        {"&",   {.NextState = ParserState::AmpersandOperator, .TokenToPush = TokenType::None}},
        {"^",   {.NextState = ParserState::RValue, .TokenToPush = TokenType::BitwiseXor}},
        {"|",   {.NextState = ParserState::RValue, .TokenToPush = TokenType::BitwiseOr}},
        {"~",   {.NextState = ParserState::RValue, .TokenToPush = TokenType::BitwiseNot}},
        {"<<",  {.NextState = ParserState::RValue, .TokenToPush = TokenType::LeftShift}},
        {">>",  {.NextState = ParserState::RValue, .TokenToPush = TokenType::RightShift}},
        {"*=",  {.NextState = ParserState::RValue, .TokenToPush = TokenType::MultiplyAssign}},
        {"/=",  {.NextState = ParserState::RValue, .TokenToPush = TokenType::DivideAssign}},
        {"%=",  {.NextState = ParserState::RValue, .TokenToPush = TokenType::ModuloAssign}},
        {"+=",  {.NextState = ParserState::RValue, .TokenToPush = TokenType::PlusAssign}},
        {"-=",  {.NextState = ParserState::RValue, .TokenToPush = TokenType::MinusAssign}},
        {";",   {.NextState = ParserState::Default,.TokenToPush  =TokenType::EndLine}},
        {"<-",  {.NextState = ParserState::Default, .TokenToPush =TokenType::LeftArrow}},
        { "++", {.NextState = ParserState::Increment, .TokenToPush = TokenType::Increment} },
        { "--", {.NextState = ParserState::Increment, .TokenToPush = TokenType::Decrement}},
        {"^^",{.NextState = ParserState::RValue, .TokenToPush = TokenType::Power}},

    };

    const KeyWordMapType g_KeyWordMap = {
        {"int8",      {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Int8Type}},
        {"int16",     {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Int16Type}},
        {"int32",     {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Int32Type}},
        {"int64",     {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Int64Type}},
        {"uint8",     {.NextState = ParserState::VariableName, .TokenToPush = TokenType::UInt8Type}},
        {"uint16",    {.NextState = ParserState::VariableName, .TokenToPush = TokenType::UInt16Type}},
        {"uint32",    {.NextState = ParserState::VariableName, .TokenToPush = TokenType::UInt32Type}},
        {"uint64",    {.NextState = ParserState::VariableName, .TokenToPush = TokenType::UInt64Type}},
        {"string",    {.NextState = ParserState::VariableName, .TokenToPush = TokenType::StringType}},
        {"bool",      {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Bool}},
        {"float32",   {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Float32Type}},
        {"float64",   {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Float64Type}},
        {"false",     {.NextState = ParserState::Default, .TokenToPush = TokenType::BooleanData}},
        {"true",      {.NextState = ParserState::Default, .TokenToPush = TokenType::BooleanData}},
        {"null",      {.NextState = ParserState::Default, .TokenToPush = TokenType::Null}},
        {"if",        {.NextState = ParserState::RValue, .TokenToPush = TokenType::ConditionalIf}},
        {"function",  {.NextState = ParserState::FunctionName, .TokenToPush = TokenType::Function}},
        {"int",       {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Int32Type}},
        {"uint",      {.NextState = ParserState::VariableName, .TokenToPush = TokenType::UInt32Type}},
        {"struct",    {.NextState = ParserState::StructName, .TokenToPush = TokenType::Struct}},
        {"return",    {.NextState = ParserState::Default, .TokenToPush = TokenType::Return}},
        {"char",      {.NextState = ParserState::VariableName, .TokenToPush = TokenType::CharType}},
        {"declare",   {.NextState = ParserState::Declaration, .TokenToPush = TokenType::Declaration}},
        {"defer",     {.NextState = ParserState::Default, .TokenToPush = TokenType::Defer}},
        {"else",      {.NextState = ParserState::Default, .TokenToPush = TokenType::Else}},
        {"elseif",    {.NextState = ParserState::Default, .TokenToPush = TokenType::ElseIf}},
        {"while",     {.NextState = ParserState::Default, .TokenToPush = TokenType::While }}, 
        {"break",     {.NextState = ParserState::Default, .TokenToPush = TokenType::Break }},
        {"continue",  {.NextState = ParserState::Default, .TokenToPush = TokenType::Continue }},
        {"float",   {.NextState = ParserState::VariableName, .TokenToPush = TokenType::Float32Type}},

    };

    const std::map<char,char> g_CloserToOpeners = {{')','('},{']','['},{'}','{'}};
    extern const std::map<std::string,std::set<TokenType>> g_TokenTypes = {
        {"allow_op",{TokenType::RValueNumber,TokenType::RValueString,TokenType::CloseBracket,TokenType::BooleanData,TokenType::EndArray,TokenType::VariableReference,TokenType::EndFunctionArguments,TokenType::Increment,TokenType::Decrement}}


    };
    const std::set<char> g_Openers = {'(','{','['};

    const std::set<std::string> g_DataTypes = {
        "float64", "float32", "bool", "string", "uint64", "uint32", "uint16", "uint8", "int64", "int32", "int16", "int8","int","uint","char"
    };

    bool IsTokenOfType(Token tok,std::string type) {
        return g_TokenTypes.at(type).contains(tok.TokenType);
    }


    std::string_view TokenToString(TokenType token) 
    {

        switch (token)
        {
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
            case TokenType::RightArrow: return "RightArrow";
            case TokenType::Lambda: return "Lambda";
            case TokenType::VariableReference: return "VariableReference";
            case TokenType::Struct: return "Struct";
            case TokenType::StructName: return "StructName";
            case TokenType::FunctionCall: return "FunctionCall";
            case TokenType::Comma: return "Comma";
            case TokenType::RValueChar : return "RValueChar";
            case TokenType::IndexOperator : return "IndexOperator";
            case TokenType::Return: return "Return";
            case TokenType::DeclarationOperator: return "DeclarationOperator";
            case TokenType::ModOp: return "ModOp";
            case TokenType::BinaryShiftLeft: return "BinaryShiftLeft";
            case TokenType::AddressOp: return "AddressOp";
            case TokenType::DynamicArrayDef : return "DynamicArrayDef";
            case TokenType::StaticArrayDef: return "StaticArrayDef";
            case TokenType::PointerDef: return "PointerDef";
            case TokenType::DereferenceOp : return "DereferenceOp";
            case TokenType::CharType: return "CharType";
            case TokenType::Declaration: return "Declaration";
            case TokenType::Defer: return "Defer";
            case TokenType::BitwiseNot: return "BitwiseNot";
            case TokenType::BitwiseOr: return "BitwiseOr";
            case TokenType::BitwiseXor: return "BitwiseXor";
            case TokenType::DivideAssign: return "DivideAssign";
            case TokenType::LeftShift: return "LeftShift";
            case TokenType::RightShift: return "RightShift";
            case TokenType::MinusAssign: return "MinusAssign";
            case TokenType::PlusAssign: return "PlusAssign";
            case TokenType::ModuloAssign: return "ModuloAssign";
            case TokenType::MultiplyAssign: return "MultiplyAssign";
            case TokenType::LeftArrow: return "LeftArrow";
            case TokenType::Else: return "Else";
            case TokenType::ElseIf: return "ElseIf";
            case TokenType::BitwiseAnd: return "BitwiseAnd";
            case TokenType::EndArray: return "EndArray";
            case TokenType::StartArray: return "StartArray";
            case TokenType::While:  return "While";
            case TokenType::Increment:  return "Increment";
            case TokenType::Decrement:  return "Decrement";
            case TokenType::Negation: return "Negation";
            case TokenType::Power: return "Power";
            case TokenType::Continue:   return "Continue";
            case TokenType::Break:   return "Break";
            case TokenType::EndFunctionArguments: return "EndFunctionArguments";
            case TokenType::TypeIdentifier: return "TypeIdentifier";

            default:
                break;
        }

        return "";
    }
}
