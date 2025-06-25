#include "Lexer.h"

#include <iostream>

int main()
{
    std::filesystem::current_path(std::filesystem::path(__FILE__).parent_path());
    clear::Lexer lexer("clear.test");

    for(const auto& token : lexer.GetTokens())
    {
        std::cout << token.GetTypeAsString() << " " << token.GetData() << std::endl;
    }
}