#ifndef PJPPROJECT_PARSER_HPP
#define PJPPROJECT_PARSER_HPP

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include "Lexer.hpp"

class Parser {
public:
    Parser();

    ~Parser() = default;

    bool Parse();                    // parse
    const llvm::Module &Generate();  // generate

    void InitLexan(char *name_of_file);

private:
    Token getNextToken();

    //matches
    void match(Token tok);

    void start_of_prog();

    void vars();

    void vars_rest();

    void consts();

    void consts_rest();

    void vars_and_const();

    void body();

    void command();

    void rest_command();

    void expression();

    Lexer m_Lexer;                   // lexer is used to read tokens
    Token cur_tok;                      // to keep the current token



    struct Variable {
        enum Type {
            integer, float_number
        };
        Type type;
        int int_val;
        float float_val;
        bool if_const;
    };

    //name of prog
    std::string name_of_prog;

    //variables
    std::map<std::string, Variable> var;

    //var read from line
    std::vector<std::string> line_vars;


    llvm::LLVMContext MilaContext;   // llvm context
    llvm::IRBuilder<> MilaBuilder;   // llvm builder
    llvm::Module MilaModule;         // llvm module
};

#endif //PJPPROJECT_PARSER_HPP
