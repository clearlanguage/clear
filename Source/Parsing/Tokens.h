#pragma once

#include <string_view>
#include <string>
#include <map>
#include <set>


namespace clear {

    enum class TokenType
    {
        None = 0, Int8Type, Int16Type, Int32Type, Int64Type, UInt8Type, UInt16Type, UInt32Type, UInt64Type,
        Bool, Float32Type, Float64Type, RValueNumber, RValueString, VariableName, StringType,
        Assignment, MulOp, AddOp, DivOp, SubOp, ModOp, OpenBracket, CloseBracket, BooleanData, ConditionalIf,
        IsEqual, Null, NotEqual, GreaterThan, LessThan, LessThanEqual, GreaterThanEqual, Not, Ellipsis, DotOp, BinaryShiftLeft,
        StartIndentation, EndIndentation, EndLine,VariableReference ,Function,FunctionName,StartFunctionParameters,EndFunctionParameters,Arrow,FunctionType,
        Lambda,Struct,StructName,FunctionCall,Comma,RValueChar,IndexOperator,DeclarationOperator,Return,AddressOp,DynamicArrayDef,StaticArrayDef,PointerDef,DereferenceOp,CharType,Declaration
    };

    enum class ParserState
    {
        Default = 0,
        RValue,
        VariableName,
        FunctionName,
        Operator,
        Indentation,
        FunctionParameters,
        ArrowState,
        FunctionTypeState,
        StructName,
        FunctionParamaters,
        Comment,
        MultilineComment,
        IndexOperator,
        AsterisksOperator
    };


    struct ParserMapValue
    {
        ParserState NextState = ParserState::Default;
        TokenType TokenToPush = TokenType::None;
    };


    struct Token
    {
        TokenType TokenType = TokenType::None;
        std::string Data = "";
    };

    std::string_view TokenToString(TokenType token);

    using OperatorMapType = std::map<std::string, ParserMapValue>;
    using KeyWordMapType  = std::map<std::string, ParserMapValue>;

    extern const OperatorMapType g_OperatorMap;
    extern const KeyWordMapType  g_KeyWordMap;
    extern const std::set<std::string> g_DataTypes;
}