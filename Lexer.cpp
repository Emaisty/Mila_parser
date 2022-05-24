#include "Lexer.hpp"

/**
 * @brief Function to return the next token from standard input
 *
 * the variable 'm_IdentifierStr' is set there in case of an identifier,
 * the variable 'm_NumVal' is set there in case of a number.
 */

InputCharType Lexer::type_of_char() {
    if ((cur_symb >= 'A' && cur_symb <= 'Z') || (cur_symb >= 'a' && cur_symb <= 'z'))
        return LETTER;
    else if (cur_symb >= '0' && cur_symb <= '9')
        return NUMBER;
    else if (cur_symb == EOF || cur_symb == 0)
        return END;
    else if ((cur_symb >= ':' && cur_symb <= '?') || cur_symb == '+' || cur_symb == '-' || cur_symb == '.')
        return SPE_SYMB;
    else if (cur_symb <= ' ')
        return WHITE_SPACE;
    else
        return NO_TYPE;

}

const struct {
    char *slovo;
    Token symb;
} keyWordTable[] = {
        {"begin",   tok_begin},
        {"program", tok_program},
        {"end",     tok_end},
        {"var",     tok_var},
        {"integer", tok_integer},
        {"div",     tok_div}
};


Token Lexer::readString() {
    std::string str;
    while (type_of_char() == LETTER || type_of_char() == NUMBER) {
        str += (char) cur_symb;
        cur_symb = readSymbol();
    }

    for (auto &i: keyWordTable) {
        if (i.slovo == str)
            return i.symb;
    }

    m_IdentifierStr = str;
    return tok_identifier;

}

Token Lexer::readNumber() {
    int sum = 0;
    while (type_of_char() == NUMBER) {
        sum *= 10;
        sum += cur_symb - 48;
        cur_symb = readSymbol();
    }
    m_NumVal = sum;
    return tok_number;
}

Token Lexer::readSpe() {
    switch (cur_symb) {
        case ':':
            cur_symb = readSymbol();
            if (type_of_char() == SPE_SYMB && cur_symb == '=') {
                cur_symb = readSymbol();
                return tok_assign;
            }
            return tok_colon;

        case ';':
            cur_symb = readSymbol();
            return tok_semicolon;
        case '+':
            cur_symb = readSymbol();
            return tok_plus;
        case '-':
            cur_symb = readSymbol();
            return tok_minus;
        case '*':
            cur_symb = readSymbol();
            return tok_mul;
        case '.':
            cur_symb = readSymbol();
            return tok_dot;
        case '<':
            cur_symb = readSymbol();
            if (type_of_char() == SPE_SYMB && cur_symb == '>') {
                cur_symb = readSymbol();
                return tok_notequal;
            }
            if (type_of_char() == SPE_SYMB && cur_symb == '=') {
                cur_symb = readSymbol();
                return tok_lessequal;
            }
            return tok_lessequal;
        case '=':
            cur_symb = readSymbol();
            return tok_equal;
        case '>':
            cur_symb = readSymbol();
            if (type_of_char() == SPE_SYMB && cur_symb == '=') {
                cur_symb = readSymbol();
                return tok_greaterequal;
            }
            return tok_greater;

    }
}

Token Lexer::gettok() {
    std::cout << cur_symb << std::endl;
    if (type_of_char() == LETTER) {
        return readString();
    }
    if (type_of_char() == NUMBER) {
        return readNumber();
    }
    if (type_of_char() == END) {
        return tok_eof;
    }
    if (type_of_char() == SPE_SYMB) {
        return readSpe();
    }
    if (type_of_char() == WHITE_SPACE) {
        cur_symb = readSymbol();
        return gettok();
    }
}

void Lexer::InitInput(char *name) {
    if (!name)
        int a;
    else {
        file.open(name);
    }
    cur_symb = readSymbol();
}

int Lexer::readSymbol() {
    char c;
    file >> std::noskipws >> c;
    return c;
}
