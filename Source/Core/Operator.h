#pragma once 

namespace clear 
{
    enum class OperatorType 
    {
        None = 0, Assignment, Or, And, 

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
        PostDecrement, PostIncrement, Cast,
		Sizeof, Is,

        Negation, Dot, Index, 
        Pow, Ternary, FunctionCall,
		Subscript, StructInitializer,
		ListInitializer, ArrayType, 

        Count
    };
}
