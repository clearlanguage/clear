#pragma once 

#include <bitset>
#include <map>
#include <memory>

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
        Pow, Ternary, FunctionCall,

        Count
    };

    using OperatorSet = std::bitset<(size_t)OperatorType::Count>;

    inline constexpr OperatorSet CreateOperatorSet(std::initializer_list<OperatorType> operatorTypes)
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

	struct ASTFunctionCall; 
	class Parser;  

	inline std::map<OperatorType, int> g_Precedence = {
        {OperatorType::Index,            7}, // x[y]
        {OperatorType::Dot,              7}, // x.y
		{OperatorType::FunctionCall,     7}, // x.y

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

        {OperatorType::LessThan,           0}, // x < y
        {OperatorType::GreaterThan,        0}, // x > y
        {OperatorType::LessThanEqual,      0}, // x <= y 
        {OperatorType::GreaterThanEqual,   0}, // x >= y
        {OperatorType::IsEqual,            0}, // x == y
        {OperatorType::NotEqual,           0}, // x != y 
        {OperatorType::Ellipsis,           0}, // ...x

        {OperatorType::And,               -1}, // x and y
        {OperatorType::Or,                -2}, // x or y
        {OperatorType::Ternary,           -3}, // x ? y : z
    }; 

	class ASTNodeBase;

    struct Operator
    {
        OperatorType OperatorExpr;

        bool IsUnary        = false;
        bool IsBinary       = false;
        bool IsOpenBracket  = false;
        bool IsBeginTernary = false;
		
		std::shared_ptr<ASTNodeBase> OperatorNode;

        int Precedence = 0;

        bool IsRightAssociative() const
        {
            return OperatorExpr == OperatorType::Power || 
                   OperatorExpr == OperatorType::Ternary;
        }
    };
}
