#ifndef PJPPROJECT_LEXER_HPP
#define PJPPROJECT_LEXER_HPP

#include <iostream>
#include <fstream>


/*
 * Lexer returns tokens [0-255] if it is an unknown character, otherwise one of these for known things.
 * Here are all valid tokens:
 */
enum Token {
    tok_eof,   //0

    // numbers and identifiers
    tok_identifier,    //1
    tok_number_int,
    tok_number_double,

    // keywords
    tok_begin,
    tok_end,
    tok_const,
    tok_procedure,
    tok_forward,
    tok_function,
    tok_if,
    tok_then,
    tok_else,
    tok_program,
    tok_while,
    tok_exit,
    tok_var,
    tok_integer, //17
    tok_double,
    tok_for,
    tok_do,

    // 2-character operators
    tok_notequal,
    tok_lessequal,
    tok_greaterequal,
    tok_assign,
    tok_or,   //25

    //less or greater
    tok_less,
    tok_greater,

    //equal
    tok_equal,

    // 3-character operators (keywords)
    tok_mod,
    tok_div,
    tok_not,
    tok_and, //32
    tok_xor,

    // keywords in for loop
    tok_to,
    tok_downto,

    // keywords for array
    tok_array,

    //spes symbols

    tok_colon,
    tok_semicolon,

    //math operators
    tok_plus,
    tok_minus,
    tok_mul, // 41

    //wtf
    tok_dot,
    tok_comma,
    tok_opbrak,
    tok_clbrak,
    tok_opfigbrak,
    tok_clfigbrak,
    tok_opsqbrak,
    tok_clsqbrak,


    //read write
    tok_readln,
    tok_writeln,
    tok_write,
    tok_int,
    tok_float,
    tok_of,
    tok_dec,
    tok_string

};

typedef enum {
    LETTER, NUMBER, WHITE_SPACE, END, NO_TYPE, SPE_SYMB
} InputCharType;

class Lexer {
public:
    Lexer() = default;

    ~Lexer() = default;

    Token gettok();

    const std::string &identifierStr() const { return this->m_IdentifierStr; }

    int numVal() { return this->m_NumVal; }

    double douVal() { return this->m_DouVal; }

    void InitInput(char *name);

    int readSymbol();

    InputCharType type_of_char();

    Token readString();

    Token readNumber();

    Token readNumber_oct();

    Token readNumber_hex();

    Token readSpe();

private:
    std::ifstream file;
    bool open = false;

    int cur_symb = -1;

    std::string m_IdentifierStr;
    int m_NumVal;
    double m_DouVal;
};

#endif //PJPPROJECT_LEXER_HPP
