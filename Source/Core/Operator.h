#pragma once 

#include <bitset>
#include <map>

namespace clear 
{
     enum class OperatorType 
    {
        None = 0, Or, And, 

        Ellipsis, NotEqual, IsEqual, 
        GreaterThanEqual, LessThanEqual, 
        GreaterThan, LessThan, 

        BitwiseAnd, BitwiseXor, BitwiseOr, 

        LeftShift, RightShift,

        Add, Sub,

        Mul, Div, Mod,

        Power,

        Not, Dereference, Address, 
        BitwiseNot, Decrement, Increment,  
        PostDecrement, PostIncrement,      

        Negation, Dot, Index, 
        Pow,

        Count
    };

    using OperatorSet = std::bitset<(size_t)OperatorType::Count>;

    static constexpr OperatorSet CreateOperatorSet(std::initializer_list<OperatorType> operatorTypes)
    {
        OperatorSet operatorSet;

        for(auto type : operatorTypes)
            operatorSet.set((size_t)type);

        return operatorSet;
    }

    inline OperatorSet g_PreUnaryOperators = CreateOperatorSet({
            OperatorType::Increment,    
            OperatorType::Decrement,    
            OperatorType::BitwiseNot,   
            OperatorType::Address,	  
            OperatorType::Dereference,
            OperatorType::Negation, 
            OperatorType::Not,
            OperatorType::Ellipsis, 
    });

    inline OperatorSet g_PostUnaryOperators = CreateOperatorSet({
            OperatorType::PostIncrement,    
            OperatorType::PostDecrement
    });

    inline std::map<OperatorType, int> g_Precedence = {
        {OperatorType::Index,            7}, // x[y]
        {OperatorType::Dot,              7}, // x.y

        {OperatorType::Negation,           6}, // -x
        {OperatorType::Increment,          6}, // ++x
        {OperatorType::Decrement,          6}, // --x
        {OperatorType::PostIncrement,      6}, // x++
        {OperatorType::PostDecrement,      6}, // x--

        {OperatorType::BitwiseNot,         6}, // ~x
        {OperatorType::Address,            6}, // &x
        {OperatorType::Dereference,        6}, // *x
        {OperatorType::Not,                6}, // not x

        {OperatorType::Power,              5}, // x ** y

        {OperatorType::Mul,              4}, // x * y
        {OperatorType::Div,              4}, // x / y
        {OperatorType::Mod,              4}, // x % y

        {OperatorType::Add,              3}, // x + y
        {OperatorType::Sub,              3}, // x - y

        {OperatorType::LeftShift,          2}, // x << y
        {OperatorType::RightShift,         2}, // x >> y

        {OperatorType::BitwiseAnd,         1}, // x & y
        {OperatorType::BitwiseXor,         1}, // x ^ y
        {OperatorType::BitwiseOr,          1}, // x | y

        {OperatorType::LessThan,           0},
        {OperatorType::GreaterThan,        0},
        {OperatorType::LessThanEqual,      0},
        {OperatorType::GreaterThanEqual,   0},
        {OperatorType::IsEqual,            0},
        {OperatorType::NotEqual,           0},
        {OperatorType::Ellipsis,           0},

        {OperatorType::And,               -1}, // x and y
        {OperatorType::Or,                -2}, // x or y
    }; 

    struct Operator
    {
        OperatorType OperatorExpr;

        bool IsUnary        = false;
        bool IsBinary       = false;
        bool IsOpenBracket  = false;

        int Precedence = 0;
    };
}