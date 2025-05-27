#include "Parser.h"
#include "AST/ASTNode.h"
#include "Core/Log.h"
#include "Core/TypeRegistry.h"


#include <stack>

namespace clear 
{
    static std::map<TokenType, int32_t> s_Precedence = {
	    	{TokenType::DotOp,			    5},
	        {TokenType::IndexOperator,      5}, 
	        {TokenType::Power,              4}, 
	        {TokenType::Negation,           4},
	        {TokenType::Increment,          4},
	        {TokenType::Decrement,          4},
	        {TokenType::BitwiseNot,         4},
	        {TokenType::DereferenceOp,      4},
	        {TokenType::AddressOp,          4},
	        {TokenType::DivOp,              3},
	        {TokenType::MulOp,              3},
	        {TokenType::LeftShift,          3},
	        {TokenType::RightShift,         3},
	        {TokenType::AddOp,              2},
	        {TokenType::SubOp,              2},
	        {TokenType::BitwiseAnd,         1}, 
	        {TokenType::BitwiseXor,         1},
	        {TokenType::BitwiseOr,          1},
	        {TokenType::IsEqual,            0}, 
	        {TokenType::NotEqual,           0},
	        {TokenType::LessThan,           0},
	        {TokenType::GreaterThan,        0},
	        {TokenType::LessThanEqual,      0},
	        {TokenType::GreaterThanEqual,   0},
	        {TokenType::OpenBracket,       -1}  
	    };

    Parser::Parser(const ProgramInfo& info)
        : m_Tokens(info.Tokens)
    {
        std::shared_ptr<ASTNodeBase> root = std::make_shared<ASTNodeBase>();
        root->CreateSymbolTable();
        m_RootStack.push_back(root);

        m_Tokens.push_back({TokenType::Eof});

        m_VariableType = CreateTokenSet({
            TokenType::Int8Type, 
            TokenType::Int16Type, 
            TokenType::Int32Type, 
            TokenType::Int64Type, 
            TokenType::UInt8Type,
            TokenType::UInt16Type,
            TokenType::UInt32Type,
            TokenType::UInt64Type,
            TokenType::Float32Type,
            TokenType::Float64Type,
            TokenType::Bool,
            TokenType::StructName,
            TokenType::StringType, 
            TokenType::CharType, 
            TokenType::TypeIdentifier
        });

        m_AssignmentOperators = CreateTokenSet({
            TokenType::Assignment, 
            TokenType::MultiplyAssign,
            TokenType::DivideAssign, 
            TokenType::PlusAssign,
            TokenType::MinusAssign,
            TokenType::ModuloAssign
        });

        m_Terminators = CreateTokenSet({
            TokenType::EndLine, 
		    TokenType::EndIndentation, 
		    TokenType::Comma,  
		    TokenType::EndFunctionArguments, 
		    TokenType::EndArray,
		    TokenType::Assignment, 
		    TokenType::StartIndentation, 
            TokenType::Eof
        });

        m_UnaryExpression = CreateTokenSet({
            TokenType::Increment,    
            TokenType::Decrement,    
            TokenType::BitwiseNot,   
            TokenType::AddressOp,	  
            TokenType::DereferenceOp,
            TokenType::Negation     
        });

        m_Literals = CreateTokenSet({
            TokenType::RValueNumber,
		    TokenType::RValueString,
		    TokenType::BooleanData,
		    TokenType::Null
        });

        m_IgnoredTokens = CreateTokenSet({
            TokenType::StartIndentation,
            TokenType::EndLine
        });

        m_TypeIndirection = CreateTokenSet({
            TokenType::PointerDef,
            TokenType::StaticArrayDef
        });

        m_ValueReferences = CreateTokenSet({
            TokenType::VariableReference, 
            TokenType::DereferenceOp,
        });

        while(!Match(TokenType::Eof))
        {
            ParseStatement();
        }
    }

    std::shared_ptr<ASTNodeBase> Parser::GetResult()
    {
        return m_RootStack[0];
    }

    std::shared_ptr<ASTNodeBase> Parser::Root()
    {
        return m_RootStack.back();
    }

    Token Parser::Consume()
    {
        return m_Tokens[m_Position++];
    }

    Token Parser::Peak()
    {
        return m_Tokens[m_Position];
    }

    bool Parser::Match(TokenType token)
    {
        return Peak().TokenType == token;
    }

    bool Parser::MatchAny(TokenSet tokenSet)
    {
        return tokenSet.test((size_t)Peak().TokenType);
    }

    void Parser::Expect(TokenType tokenType)
    {
        if(Match(tokenType)) return;
        
        CLEAR_LOG_ERROR("missing expected token ", TokenToString(tokenType));
        CLEAR_UNREACHABLE("TODO");
    }

    void Parser::ExpectAny(TokenSet tokenSet)
    {
        if(MatchAny(tokenSet)) return;
        
        CLEAR_LOG_ERROR("missing expected token from token set");    
        CLEAR_UNREACHABLE("TODO");
    }

    void Parser::ParseStatement()
    {
        if(MatchAny(m_IgnoredTokens))
        {
            CLEAR_LOG_WARNING("ignoring token ", TokenToString(Consume().TokenType));
            return;
        }

        if(MatchAny(m_VariableType))
        {
            ParseVariableDecleration();
        }
        else if (Match(TokenType::Import))
        {
            ParseImport();
        }
        else if (Match(TokenType::Struct))
        {
            ParseStruct();
        }
        else if (Match(TokenType::Declaration))
        {
            ParseFunctionDecleration();
        }
        else if (Match(TokenType::Function))
        {
            ParseFunctionDefinition();
        }
        else if (Match(TokenType::EndIndentation))
        {
            ParseIndentation();
        }
        else 
        {
            ParseGeneric();
        }
    }

    void Parser::ParseGeneric()
    {
        std::shared_ptr<ASTNodeBase> expression = ParseExpression(true);

        if(MatchAny(m_AssignmentOperators))
        {
            Root()->Push(ParseAssignment(expression));
            return;
        }

        Root()->Push(expression);
    }

    void Parser::ParseVariableDecleration()
    {
        std::shared_ptr<Type> variableType = ParseVariableType();

        Expect(TokenType::VariableName);

        std::string variableName = Consume().Data;

        Root()->Push(std::make_shared<ASTVariableDeclaration>(variableName, variableType));

        if(Match(TokenType::EndLine))
        {
           Consume();
           return; 
        }

        ExpectAny(m_AssignmentOperators);

        Root()->Push(ParseAssignment(variableName));
    }

    void Parser::ParseStruct()
    {
        Expect(TokenType::Struct);

        Consume();

        Expect(TokenType::StructName);

        std::string structName = Consume().Data;

        Expect(TokenType::EndLine);
        Consume();

        Expect(TokenType::StartIndentation);
        Consume();

        std::vector<std::pair<std::string, std::shared_ptr<Type>>> types;

        while(!Match(TokenType::EndIndentation))
        {
            std::shared_ptr<Type> type = ParseVariableType();
            
            Expect(TokenType::VariableName);

            std::string name = Consume().Data;

            Expect(TokenType::EndLine);
            Consume();

            types.push_back({name, type});
        }

        auto typeReg = TypeRegistry::GetGlobal();
        typeReg->CreateStruct(structName, types);

        Consume();
    }

    void Parser::ParseImport()
    {
        Expect(TokenType::Import);

        Consume();

        Expect(TokenType::RValueString);

        std::shared_ptr<ASTImport> import = std::make_shared<ASTImport>(Consume().Data);

        Expect(TokenType::EndLine);

        Root()->Push(import);
    }

    void Parser::ParseFunctionDefinition()
    {
        Expect(TokenType::Function);

        Consume();

        Expect(TokenType::FunctionName);

        std::string name = Consume().Data;
        std::shared_ptr<Type> returnType;
        std::vector<Parameter> params;

        Expect(TokenType::StartFunctionParameters);

        Consume();

        while(!Match(TokenType::EndFunctionParameters)) 
        {
            Parameter param;

            if(Match(TokenType::Ellipsis))
            {
                param.IsVariadic = true;
                params.push_back(param);

                Consume();

                Expect(TokenType::EndFunctionParameters);
                break;
            }

            param.Type = ParseVariableType();
            
            Expect(TokenType::VariableReference);

            param.Name = Consume().Data;
            params.push_back(param);

            if(!Match(TokenType::EndFunctionParameters)) 
            {
                Expect(TokenType::Comma);
                Consume();
            }
        }

        Consume();

        if(Match(TokenType::EndLine))
        {
            std::shared_ptr<ASTFunctionDefinition> func = std::make_shared<ASTFunctionDefinition>(name, returnType, params);
            Root()->Push(func);
            m_RootStack.push_back(func);

            Consume();

            return;
        }

        Expect(TokenType::RightArrow);

        Consume();

        Expect(TokenType::FunctionType);

        Consume();

        ExpectAny(m_VariableType);

        returnType = ParseVariableType();

        std::shared_ptr<ASTFunctionDefinition> func = std::make_shared<ASTFunctionDefinition>(name, returnType, params);
        Root()->Push(func);
        m_RootStack.push_back(func);

        Expect(TokenType::EndLine);

        Consume();
    }

    void Parser::ParseFunctionDecleration()
    {
        Expect(TokenType::Declaration);

        Consume();

        Expect(TokenType::VariableReference);

        Consume();

        Expect(TokenType::FunctionCall);

        std::string functionName = Consume().Data;

        Expect(TokenType::OpenBracket);

        Consume();

        std::vector<Parameter> params;

        while(!MatchAny(m_Terminators))
        {
            Parameter param;
            
            if(Match(TokenType::Ellipsis))
            {
                param.IsVariadic = true;
                Consume();
                Expect(TokenType::EndFunctionArguments);
                params.push_back(param);

                break;
            }
            
            param.Type = ParseVariableType();

            if(Match(TokenType::VariableReference)) 
                Consume();
            
            if(!Match(TokenType::EndFunctionArguments))
            {
                Expect(TokenType::Comma);
                Consume();
            }

            params.push_back(param);
        }

        Expect(TokenType::EndFunctionArguments);

        Consume();

        std::shared_ptr<Type> returnType;

        if(Match(TokenType::RightArrow))
        {
            Consume();

            Expect(TokenType::FunctionType);

            Consume();

            returnType = ParseVariableType();
        }

        Root()->Push(std::make_shared<ASTFunctionDecleration>(functionName, returnType, params));
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseFunctionCall()
    {
        Expect(TokenType::FunctionCall);

        std::string functionName = Consume().Data;

        Expect(TokenType::OpenBracket);

        Consume();

        std::shared_ptr<ASTFunctionCall> call = std::make_shared<ASTFunctionCall>(functionName);

        while(!MatchAny(m_Terminators))
        {
            call->Push(ParseExpression());

            if(!Match(TokenType::EndFunctionArguments)) 
            {
                Expect(TokenType::Comma);
                Consume();
            }
        }

        Expect(TokenType::EndFunctionArguments);
        Consume();

        return call;
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseVariableReference(bool isValueReference)
    {
        if(Match(TokenType::AddressOp))
        {
            Consume();
            Expect(TokenType::VariableReference);
            return std::make_shared<ASTVariableReference>(Consume().Data);
        }

        Expect(TokenType::VariableReference);

        std::string name = Consume().Data;

        if(Match(TokenType::FunctionCall)) 
            return ParseFunctionCall();

        if(isValueReference)
        {
            return std::make_shared<ASTVariableReference>(name);
        }

        return std::make_shared<ASTVariableExpression>(name);
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseOperand(bool isValueReference)
    {
        if(MatchAny(m_Literals)) 
            return std::make_shared<ASTNodeLiteral>(Consume());

        std::shared_ptr<ASTNodeBase> variableReference;

        if (Match(TokenType::VariableReference) || 
            Match(TokenType::AddressOp) || 
            isValueReference) 
        {
            variableReference = ParseVariableReference(isValueReference);
        }

        if(!Match(TokenType::DotOp)) 
            return variableReference;

        std::shared_ptr<ASTMemberAccess> memberAccess = std::make_shared<ASTMemberAccess>(isValueReference);
        memberAccess->Push(variableReference);

        Consume();
        
        while(Match(TokenType::MemberName))
        {
            memberAccess->Push(std::make_shared<ASTMember>(Consume().Data));
        }

        return nullptr;
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseArrayInitializer(std::shared_ptr<ASTNodeBase> storage)
    {
        Expect(TokenType::StartArray);

        std::vector<std::vector<size_t>> indices;
        std::vector<size_t> currentIndex = { 0 };

        std::shared_ptr<ASTArrayInitializer> initializer = std::make_shared<ASTArrayInitializer>();
        initializer->Push(storage);

        while(!Match(TokenType::EndLine))
        {
            if(Match(TokenType::StartArray))
            {
                currentIndex.push_back(0);
                Consume();
            }
            else if (Match(TokenType::EndArray))
            {
                currentIndex.pop_back();
                currentIndex.back()++;

                Consume();
            }
            else if (Match(TokenType::Comma))
            {
                Consume();
            }
            else 
            {
                initializer->Push(ParseExpression());
                indices.push_back(currentIndex);

                currentIndex.back()++;
            }
        }

        initializer->SetIndices(indices);
        Consume();

        return initializer;
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseAssignment(const std::string& variableName)
    {
        return ParseAssignment(std::make_shared<ASTVariableReference>(variableName));
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseAssignment(std::shared_ptr<ASTNodeBase> storage)
    {
        ExpectAny(m_AssignmentOperators);

        Token assignmentToken = Consume();

        if(Match(TokenType::StartArray))
        {
            CLEAR_VERIFY(assignmentToken.TokenType == TokenType::Assignment, "invalid assignment");

            auto initializer = ParseArrayInitializer(storage);
            return initializer;
        }

        std::shared_ptr<ASTAssignmentOperator> assign = std::make_shared<ASTAssignmentOperator>(AssignmentOperatorType::Normal);
        assign->Push(storage);            
        assign->Push(ParseExpression()); 

        return assign;
    }

    std::shared_ptr<ASTNodeBase> Parser::ParseExpression(bool isValueReference)
    {
        struct Operator
        {
            BinaryExpressionType BinaryExpression;
            UnaryExpressionType UnaryExpression;
            bool IsOpenBracket = false;
            int32_t Precedence = 0;
        };

        std::shared_ptr<ASTExpression> expression = std::make_shared<ASTExpression>();
        std::stack<Operator> operators;

        auto PopOperatorsUntil = [&](auto condition) 
        {
            while (!operators.empty() && !condition(operators.top())) 
            {
                const auto& currentOperator = operators.top();

                if (currentOperator.BinaryExpression != BinaryExpressionType::None)
                    expression->Push(std::make_shared<ASTBinaryExpression>(currentOperator.BinaryExpression));

                //TODO: unary expressions go here
                
                operators.pop();
            }
        };

        auto IsOperand = [&]()
        {
            return Match(TokenType::VariableReference) || 
                   Match(TokenType::AddressOp) || 
                   MatchAny(m_Literals);
        };

        auto HandleOpenBracket = [&]() 
        {
            operators.push({ BinaryExpressionType::None, UnaryExpressionType::None, true, 0 });
            Consume();
        };

        auto HandleCloseBracket = [&]() 
        {
            PopOperatorsUntil([](const Operator& op) { return op.IsOpenBracket; });
            
            if (!operators.empty()) 
                operators.pop(); // remove the open bracket

            Consume();
        };

        auto HandleOperator = [&]() 
        {
            TokenType tokenType = Peak().TokenType;
            int precedence = s_Precedence.at(tokenType);

            PopOperatorsUntil([&](const Operator& op) 
            {
                return op.IsOpenBracket || precedence > op.Precedence;
            });

            operators.push({
                GetBinaryExpressionFromTokenType(tokenType),
                UnaryExpressionType::None,
                false,
                precedence
            });

            Consume();
        };

        while (!MatchAny(m_Terminators)) 
        {
            if (IsOperand()) 
            {
                expression->Push(ParseOperand(isValueReference));
            }
            else if (Match(TokenType::OpenBracket)) 
            {
                HandleOpenBracket();
            }
            else if (Match(TokenType::CloseBracket)) 
            {
                HandleCloseBracket();
            }
            else if (s_Precedence.contains(Peak().TokenType)) 
            {
                HandleOperator();
            }
            else 
            {
                CLEAR_UNREACHABLE("unimplemented token");
                break;
            }
        }

        PopOperatorsUntil([](const Operator&) { return false; });

        return expression;
    }

    std::shared_ptr<Type> Parser::ParseVariableType()
    {
        auto globalReg = TypeRegistry::GetGlobal();

        std::shared_ptr<Type> type = globalReg->GetTypeFromToken(Consume());

        while(MatchAny(m_TypeIndirection))
        {
            if(Match(TokenType::PointerDef))
            {
                type = globalReg->GetPointerTo(type); 
                Consume();
            }
            else 
            {
                type = globalReg->GetArrayFrom(type, std::stoull(Consume().Data));            
            }
        }

        return type;
    }

    void Parser::ParseIndentation()
    {
        while(Match(TokenType::EndIndentation))
        {
            m_RootStack.pop_back();
            Consume();
        }
    }

    BinaryExpressionType Parser::GetBinaryExpressionFromTokenType(TokenType type)
    {
        switch (type)
	    {
			case TokenType::Assignment:			return BinaryExpressionType::Assignment;
			case TokenType::MultiplyAssign:
			case TokenType::MulOp:				return BinaryExpressionType::Mul;
			case TokenType::PlusAssign:
			case TokenType::AddOp:				return BinaryExpressionType::Add;
			case TokenType::DivideAssign:
			case TokenType::DivOp:				return BinaryExpressionType::Div;
			case TokenType::MinusAssign:
			case TokenType::SubOp:				return BinaryExpressionType::Sub;
			case TokenType::ModuloAssign:
			case TokenType::ModOp:				return BinaryExpressionType::Mod;
			case TokenType::IsEqual:			return BinaryExpressionType::Eq;
			case TokenType::NotEqual:			return BinaryExpressionType::NotEq;
			case TokenType::GreaterThan:		return BinaryExpressionType::Greater;
			case TokenType::LessThan:			return BinaryExpressionType::Less;
			case TokenType::LessThanEqual:		return BinaryExpressionType::LessEq;
			case TokenType::GreaterThanEqual:	return BinaryExpressionType::GreaterEq;
			case TokenType::BitwiseNot:			return BinaryExpressionType::BitwiseNot;
			case TokenType::LeftShift:			return BinaryExpressionType::BitwiseLeftShift;
			case TokenType::RightShift:			return BinaryExpressionType::BitwiseRightShift;
			case TokenType::BitwiseOr:			return BinaryExpressionType::BitwiseOr;
			case TokenType::BitwiseXor:			return BinaryExpressionType::BitwiseXor;
			case TokenType::BitwiseAnd:			return BinaryExpressionType::BitwiseAnd;
			case TokenType::IndexOperator:		return BinaryExpressionType::Index;	
			case TokenType::DotOp:				return BinaryExpressionType::AccessOp;
			default:
				break;
		}

		return BinaryExpressionType::None;
    }
}
