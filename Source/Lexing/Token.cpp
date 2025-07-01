//
// Created by Kareem Fares on 7/1/25.
//


#include "Token.h"

#include <Core/Log.h>


namespace clear
{

    const std::map<TokenType, size_t> g_TokenTypeToExpectedLength = {
        {TokenType::Identifier,5},

    };

    size_t getExpectedLength(TokenType type) {
        CLEAR_VERIFY(g_TokenTypeToExpectedLength.contains(type),"TokenType not in map");
        return g_TokenTypeToExpectedLength.at(type);
    }
}
