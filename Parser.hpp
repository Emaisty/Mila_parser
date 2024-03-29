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
#include "AST.h"

class Parser {
public:
    Parser();

    ~Parser() = default;

    Module_prog *Parse();                    // parse
    const llvm::Module &Generate();  // generate

    void InitLexan(char *name_of_file);

private:
    Token getNextToken();

    //matches
    void match(Token tok);

    void start_of_prog();

    ExpAST *writeln();

    ExpAST *write();

    ExpAST *readln();

    int read_int();

    ExpAST *read_var();

    void vars();

    void vars_rest();

    void consts();

    void consts_rest();

    void vars_and_const();

    Prog *body();

    ComandAST *command();

    void rest_command();

    Variable getType();

    FuncCallAST *call_func();

    ExpAST *full_expression();

    ExpAST *full_expression_prime(ExpAST *a);

    ExpAST *full_term();

    ExpAST *full_term_prime(ExpAST *a);

    ExpAST *full_faktor();

    ExpAST *expression();

    ExpAST *term();

    ExpAST *expression_prime(ExpAST *a);

    ExpAST *term_prime(ExpAST *a);

    ExpAST *faktor();

    Funct *func();

    void arg();

    Lexer m_Lexer;                   // lexer is used to read tokens
    Token cur_tok;                      // to keep the current token

    Module_prog *program;


    //name of prog
    std::string name_of_prog;

    //variables
    std::map<std::string, Variable> var;

    //vector names of func
    std::vector<std::string> functions, procedures;

    //var read from line
    std::vector<std::string> line_vars;


    llvm::LLVMContext MilaContext;   // llvm context
    llvm::IRBuilder<> MilaBuilder;   // llvm builder
    llvm::Module MilaModule;         // llvm module
};

#endif //PJPPROJECT_PARSER_HPP
