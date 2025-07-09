#pragma once 

#include "TokenDefinitions.h"

#include <inttypes.h>
#include <string>
#include <string_view>
#include <filesystem>

namespace clear 
{
    class Token 
    {
    public:
        size_t LineNumber = 0;
        size_t ColumnNumber = 0;

    public:
        Token() = default;
        Token(TokenType type, const std::string& data, const std::filesystem::path& path = "", size_t line = 0, size_t col = 0) 
            : m_Type(type), m_Data(data), m_SourceFile(path), LineNumber(line), ColumnNumber(col) {}
        
        ~Token() = default;

        int64_t   AsInt()    const { return std::stoll(m_Data); }
        uint64_t  AsUInt()   const { return std::stoull(m_Data); }
        bool      AsBool()   const { return m_Data == "true"; }
        double    AsFloat()  const { return std::stod(m_Data); }
        char      AsChar()   const { return m_Data.empty() ? '\0' : m_Data[0]; }

        bool IsType(TokenType type)  const { return m_Type == type; }
        const std::string& GetData() const { return m_Data; }
        TokenType GetType()          const { return m_Type; }

        const std::filesystem::path& GetSourceFile() const { return m_SourceFile; }

        std::string_view GetTypeAsString() const;

    private:
        TokenType m_Type = TokenType::None;
        
        std::string m_Data;
        std::filesystem::path m_SourceFile;
    };

    size_t GetExpectedLength(TokenType type);
}