#include "Tokens.h"

#include <string_view>
#include <set>
#include <Core/Log.h>

namespace clear 
{
    const std::set<char> g_Operators = {'=','*','/','-','%','+','<','>','!','.',',','&','^','|','~',';'};
    const OperatorMapType g_OperatorMap = {
        {"=",   {.NextState = LexerState::RValue, .TokenToPush = TokenType::Assignment}},
        {"*",   {.NextState = LexerState::AsterisksOperator, .TokenToPush = TokenType::None}},
        {"/",   {.NextState = LexerState::RValue, .TokenToPush = TokenType::DivOp}},
        {"-",   {.NextState = LexerState::MinusOperator, .TokenToPush = TokenType::None}},
        {"%",   {.NextState = LexerState::RValue, .TokenToPush = TokenType::ModOp}},
        {"+",   {.NextState = LexerState::RValue, .TokenToPush = TokenType::AddOp}},
        {"==",  {.NextState = LexerState::RValue, .TokenToPush = TokenType::IsEqual}},
        {"<",   {.NextState = LexerState::RValue, .TokenToPush = TokenType::LessThan}},
        {">",   {.NextState = LexerState::RValue, .TokenToPush = TokenType::GreaterThan}},
        {"!=",  {.NextState = LexerState::RValue, .TokenToPush = TokenType::NotEqual}},
        {"<=",  {.NextState = LexerState::RValue, .TokenToPush = TokenType::LessThanEqual}},
        {">=",  {.NextState = LexerState::RValue, .TokenToPush = TokenType::GreaterThanEqual}},
        {"...", {.NextState = LexerState::Default, .TokenToPush = TokenType::Ellipsis}},
         {"!",  {.NextState = LexerState::RValue, .TokenToPush = TokenType::Not}},
        {"->",  {.NextState = LexerState::ArrowState, .TokenToPush = TokenType::RightArrow}},
        {".",   {.NextState = LexerState::DotOp, .TokenToPush = TokenType::DotOp}},
        {"//",  {.NextState = LexerState::Comment, .TokenToPush = TokenType::None}},
        {"/*",  {.NextState = LexerState::MultilineComment, .TokenToPush = TokenType::None}},
        {",",   {.NextState = LexerState::Default, .TokenToPush = TokenType::Comma}},
        {"&",   {.NextState = LexerState::AmpersandOperator, .TokenToPush = TokenType::None}},
        {"^",   {.NextState = LexerState::RValue, .TokenToPush = TokenType::BitwiseXor}},
        {"|",   {.NextState = LexerState::RValue, .TokenToPush = TokenType::BitwiseOr}},
        {"~",   {.NextState = LexerState::RValue, .TokenToPush = TokenType::BitwiseNot}},
        {"<<",  {.NextState = LexerState::RValue, .TokenToPush = TokenType::LeftShift}},
        {">>",  {.NextState = LexerState::RValue, .TokenToPush = TokenType::RightShift}},
        {"*=",  {.NextState = LexerState::RValue, .TokenToPush = TokenType::MultiplyAssign}},
        {"/=",  {.NextState = LexerState::RValue, .TokenToPush = TokenType::DivideAssign}},
        {"%=",  {.NextState = LexerState::RValue, .TokenToPush = TokenType::ModuloAssign}},
        {"+=",  {.NextState = LexerState::RValue, .TokenToPush = TokenType::PlusAssign}},
        {"-=",  {.NextState = LexerState::RValue, .TokenToPush = TokenType::MinusAssign}},
        {";",   {.NextState = LexerState::Default,.TokenToPush  =TokenType::EndLine}},
        {"<-",  {.NextState = LexerState::Default, .TokenToPush =TokenType::LeftArrow}},
        { "++", {.NextState = LexerState::Increment, .TokenToPush = TokenType::Increment} },
        { "--", {.NextState = LexerState::Increment, .TokenToPush = TokenType::Decrement}},
        {"^^",{.NextState = LexerState::RValue, .TokenToPush = TokenType::Power}},

    };

    const KeyWordMapType g_KeyWordMap = {
        {"int8",      {.NextState = LexerState::VariableName, .TokenToPush = TokenType::Int8Type}},
        {"int16",     {.NextState = LexerState::VariableName, .TokenToPush = TokenType::Int16Type}},
        {"int32",     {.NextState = LexerState::VariableName, .TokenToPush = TokenType::Int32Type}},
        {"int64",     {.NextState = LexerState::VariableName, .TokenToPush = TokenType::Int64Type}},
        {"uint8",     {.NextState = LexerState::VariableName, .TokenToPush = TokenType::UInt8Type}},
        {"uint16",    {.NextState = LexerState::VariableName, .TokenToPush = TokenType::UInt16Type}},
        {"uint32",    {.NextState = LexerState::VariableName, .TokenToPush = TokenType::UInt32Type}},
        {"uint64",    {.NextState = LexerState::VariableName, .TokenToPush = TokenType::UInt64Type}},
        {"string",    {.NextState = LexerState::VariableName, .TokenToPush = TokenType::StringType}},
        {"bool",      {.NextState = LexerState::VariableName, .TokenToPush = TokenType::Bool}},
        {"float32",   {.NextState = LexerState::VariableName, .TokenToPush = TokenType::Float32Type}},
        {"float64",   {.NextState = LexerState::VariableName, .TokenToPush = TokenType::Float64Type}},
        {"false",     {.NextState = LexerState::Default, .TokenToPush = TokenType::BooleanData}},
        {"true",      {.NextState = LexerState::Default, .TokenToPush = TokenType::BooleanData}},
        {"null",      {.NextState = LexerState::Default, .TokenToPush = TokenType::Null}},
        {"if",        {.NextState = LexerState::RValue, .TokenToPush = TokenType::ConditionalIf}},
        {"function",  {.NextState = LexerState::FunctionName, .TokenToPush = TokenType::Function}},
        {"int",       {.NextState = LexerState::VariableName, .TokenToPush = TokenType::Int32Type}},
        {"uint",      {.NextState = LexerState::VariableName, .TokenToPush = TokenType::UInt32Type}},
        {"struct",    {.NextState = LexerState::StructName, .TokenToPush = TokenType::Struct}},
        {"return",    {.NextState = LexerState::Default, .TokenToPush = TokenType::Return}},
        {"char",      {.NextState = LexerState::VariableName, .TokenToPush = TokenType::CharType}},
        {"declare",   {.NextState = LexerState::Declaration, .TokenToPush = TokenType::Declaration}},
        {"defer",     {.NextState = LexerState::Default, .TokenToPush = TokenType::Defer}},
        {"else",      {.NextState = LexerState::Default, .TokenToPush = TokenType::Else}},
        {"elseif",    {.NextState = LexerState::Default, .TokenToPush = TokenType::ElseIf}},
        {"while",     {.NextState = LexerState::Default, .TokenToPush = TokenType::While }}, 
        {"break",     {.NextState = LexerState::Default, .TokenToPush = TokenType::Break }},
        {"continue",  {.NextState = LexerState::Default, .TokenToPush = TokenType::Continue }},
        {"float",   {.NextState = LexerState::VariableName, .TokenToPush = TokenType::Float32Type}},
        {"switch",  {.NextState = LexerState::Default, .TokenToPush = TokenType::Switch }},
        {"when",  {.NextState = LexerState::Default, .TokenToPush = TokenType::When }},
        {"case",  {.NextState = LexerState::Default, .TokenToPush = TokenType::Case }},
        {"switch",  {.NextState = LexerState::Default, .TokenToPush = TokenType::Switch }},
        {"default",  {.NextState = LexerState::Default, .TokenToPush = TokenType::Default }},
        {"restriction",  {.NextState = LexerState::Restriction, .TokenToPush = TokenType::Restriction }},







    };

    const std::map<char,char> g_CloserToOpeners = {{')','('},{']','['},{'}','{'}};
    extern const std::map<std::string,std::set<TokenType>> g_TokenTypes = {
        {"allow_op",{TokenType::RValueNumber,TokenType::RValueString,TokenType::CloseBracket,TokenType::BooleanData,TokenType::EndArray,TokenType::VariableReference,TokenType::EndFunctionArguments,TokenType::Increment,TokenType::Decrement}},
            {"is_declaration",{TokenType::EndLine,TokenType::StartIndentation,TokenType::EndIndentation}},
        {"callable",{TokenType::CloseBracket,TokenType::MemberName,TokenType::TypeIdentifier,TokenType::VariableReference}},
            {"named_callable",{TokenType::TypeIdentifier,TokenType::VariableReference}},
        {"has_members",{TokenType::TypeIdentifier,TokenType::VariableReference,TokenType::MemberName}}



    };
    const std::set<char> g_Openers = {'(','{','['};

    const std::set<std::string> g_DataTypes = {
        "float64", "float32", "bool", "string", "uint64", "uint32", "uint16", "uint8", "int64", "int32", "int16", "int8","int","uint","char","float"
    };

    bool IsTokenOfType(Token tok,std::string type) 
    {
        CLEAR_PARSER_VERIFY(g_TokenTypes.count(type),"96");
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
            case TokenType::GenericDeclarationStart: return "GenericDeclarationStart";
            case TokenType::GenericDeclarationEnd: return "GenericDeclarationEnd";
            case TokenType::MemberName: return "MemberName";
            case TokenType::Case: return "Case";
            case TokenType::Default: return "Default";
            case TokenType::Switch: return "Switch";
            case TokenType::When: return "When";
            case TokenType::Restriction: return "Restriction";
            case TokenType::RestrictionName: return "RestrictionName";
            case TokenType::RestrictionTypeName: return "RestrictionTypeName";

            default:
                break;
        }

        return "";
    }
}
