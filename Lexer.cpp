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
    else if ((cur_symb >= ':' && cur_symb <= '?') || (cur_symb >= '(' && cur_symb <= '.') || cur_symb == '$' ||
             cur_symb == '&' || (cur_symb >= '[' && cur_symb <= ']'))
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
        {"float",   tok_double},
        {"div",     tok_div},
        {"mod",     tok_mod},
        {"readln",  tok_readln},
        {"write",   tok_write},
        {"writeln", tok_writeln},
        {"if",      tok_if},
        {"then",    tok_then},
        {"else",    tok_else},
        {"while",   tok_while},
        {"do",      tok_do},
        {"int",     tok_int},
        {"of",      tok_of},
        {"array",   tok_array},
        {"to",      tok_to},
        {"downto",  tok_downto},
        {"for",     tok_for}
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
    if (type_of_char() == SPE_SYMB && cur_symb == '.') {
        double sum_d = sum, base = 0.1;
        cur_symb = readSymbol();
        while (type_of_char() == NUMBER) {
            sum_d += base * (cur_symb - 48);
            base /= 10;
            cur_symb = readSymbol();
        }
        m_DouVal = sum_d;
        return tok_number_double;
    }
    m_NumVal = sum;
    return tok_number_int;
}

Token Lexer::readNumber_oct() {
    std::string num;
    int sum = 0, base = 1;
    while (type_of_char() == NUMBER) {
        if (cur_symb < '0' || cur_symb > '7')
            throw "Error. Number not in oct form";
        num += cur_symb;
        cur_symb = readSymbol();
    }
    for (int i = num.size() - 1; i >= 0; --i) {
        sum += (num[i] - 48) * base;
        base *= 8;
    }
    m_NumVal = sum;
    return tok_number_int;
}

Token Lexer::readNumber_hex() {
    std::string num;
    int sum = 0, base = 1;
    while (type_of_char() == NUMBER || type_of_char() == LETTER) {
        if (type_of_char() == LETTER && (cur_symb < 'a' || cur_symb > 'f'))
            throw "Error. Number not in hex form";
        num += cur_symb;
        cur_symb = readSymbol();
    }
    for (int i = num.size() - 1; i >= 0; --i) {
        if (num[i] >= '0' && num[i] <= '9')
            sum += (num[i] - 48) * base;
        else
            sum += (num[i] - 87) * base;
        base *= 16;

    }
    m_NumVal = sum;
    return tok_number_int;
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
        case ',':
            return tok_comma;
        case '[':
            return tok_opsqbrak;
        case ']':
            return tok_clsqbrak;
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
            /*case '|':
                if (type_of_char() == SPE_SYMB && cur_symb == '|') {
                    cur_symb = readSymbol();
                    return tok_or;
                }
                throw "ERROR. Unknown operator.";*/
        case '&':
            return readNumber_oct();
            /*if (type_of_char() == SPE_SYMB && cur_symb == '&') {
                cur_symb = readSymbol();
                return tok_and;
            }*/
        case '$':
            return readNumber_hex();
        case '!':
            if (type_of_char() == SPE_SYMB && cur_symb == '=') {
                cur_symb = readSymbol();
                return tok_notequal;
            }
            return tok_not;
        case '(':
            return tok_opbrak;
        case ')':
            return tok_clbrak;
        default:
            throw "Error. Unknown symbol";

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
