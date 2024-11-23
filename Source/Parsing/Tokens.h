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
        StartIndentation, EndIndentation, EndLine,VariableReference ,Function,FunctionName,StartFunctionParameters,EndFunctionParameters,RightArrow,FunctionType,
        Lambda,Struct,StructName,FunctionCall,Comma,RValueChar,IndexOperator,DeclarationOperator,Return,AddressOp,DynamicArrayDef,StaticArrayDef,PointerDef,DereferenceOp,CharType,Declaration,Defer,
        BitwiseXor,BitwiseOr,BitwiseNot,LeftShift,RightShift,MultiplyAssign,DivideAssign,ModuloAssign,PlusAssign,MinusAssign,LeftArrow,Else,BitwiseAnd,
        ElseIf,StartArray, EndArray, While, Increment, Decrement,Negation,Power, Break, Continue,EndFunctionArguments,TypeIdentifier,GenericDeclarationStart,GenericDeclarationEnd,MemberName,When,Switch,Case,Default
    };

    enum class ParserSecondaryState {
        None = 0,
        Declaration
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
        AsterisksOperator,
        AmpersandOperator,
        MinusOperator,
        Declaration,
        Increment
    };


    struct ParserMapValue
    {
        ParserState NextState = ParserState::Default;
        TokenType TokenToPush = TokenType::None;
    };

    struct TokenLocation {
        int to=-1;
        int from=-1;
        int line= -1;
    };

    struct Token
    {
        TokenType TokenType = TokenType::None;
        std::string Data = "";
        TokenLocation Location;
    };

    std::string_view TokenToString(TokenType token);
    bool IsTokenOfType(Token tok,std::string type);

    using OperatorMapType = std::map<std::string, ParserMapValue>;
    using KeyWordMapType  = std::map<std::string, ParserMapValue>;

    extern const OperatorMapType g_OperatorMap;
    extern const KeyWordMapType  g_KeyWordMap;
    extern const std::set<std::string> g_DataTypes;
    extern const std::map<char,char> g_CloserToOpeners;
    extern const std::set<char> g_Openers;
    extern const std::set<char> g_Operators;
    extern const std::map<std::string,std::set<TokenType>> g_TokenTypes;
}