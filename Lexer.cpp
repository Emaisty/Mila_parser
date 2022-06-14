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
    else if ((cur_symb >= ':' && cur_symb <= '?') || (cur_symb >= '(' && cur_symb <= '-'))
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
        {"const",   tok_const},
        {"integer", tok_integer},
        {"div",     tok_div},
        {"mod",     tok_mod},
        {"readln",  tok_readln},
        {"write",   tok_write},
        {"writeln", tok_writeln},
        {"if",      tok_if},
        {"then",    tok_then},
        {"else",    tok_else}
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
    return tok_integer;
}

Token Lexer::readSpe() {
    char symb = cur_symb;
    cur_symb = readSymbol();
    switch (symb) {
        case ':':
            if (type_of_char() == SPE_SYMB && cur_symb == '=') {
                cur_symb = readSymbol();
                return tok_assign;
            }
            return tok_colon;

        case ';':
            return tok_semicolon;
        case '+':
            return tok_plus;
        case '-':
            return tok_minus;
        case '*':
            return tok_mul;
        case '.':
            return tok_dot;
        case '<':
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
            return tok_equal;
        case '>':
            if (type_of_char() == SPE_SYMB && cur_symb == '=') {
                cur_symb = readSymbol();
                return tok_greaterequal;
            }
            return tok_greater;
        case '|':
            if (type_of_char() == SPE_SYMB && cur_symb == '|') {
                cur_symb = readSymbol();
                return tok_or;
            }
            throw "ERROR. Unknown operator.";
        case '&':
            if (type_of_char() == SPE_SYMB && cur_symb == '&') {
                cur_symb = readSymbol();
                return tok_and;
            }
            throw "ERROR. Unknown operator.";
        case '!':
            return tok_not;
        case ',':
            return tok_comma;
        case '(':
            return tok_opbrak;
        case ')':
            return tok_clbrak;
    }
}

Token Lexer::gettok() {
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
    if (name) {
        file.open(name);
        open = true;
    }
    cur_symb = readSymbol();
}

int Lexer::readSymbol() {
    char c;
    if (!file.eof()) {
        if (open) {
            file >> std::noskipws >> c;
        } else {
            std::cin >> std::noskipws >> c;
        }
        return c;
    } else
        return 0;

}
