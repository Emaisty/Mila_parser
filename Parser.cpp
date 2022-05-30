#include "Parser.hpp"

Parser::Parser()
        : MilaContext(), MilaBuilder(MilaContext), MilaModule("mila", MilaContext) {
}

void Parser::match(Token tok) {
    if (cur_tok != tok) {
        std::cout << "ERROR. Expected: " << tok << " but get " << cur_tok << std::endl;
    }
}

void Parser::start_of_prog() {
    match(tok_program);
    cur_tok = getNextToken();
    match(tok_identifier);
    name_of_prog = m_Lexer.identifierStr();
    cur_tok = getNextToken();
    match(tok_semicolon);
    cur_tok = getNextToken();
}

void Parser::vars_rest() {
    switch (cur_tok) {
        case tok_comma:
            cur_tok = getNextToken();
            match(tok_identifier);
            line_vars.push_back(m_Lexer.identifierStr());
            cur_tok = getNextToken();
            vars_rest();
            return;
        default:
            return;
    }
}

void Parser::vars() {
    match(tok_identifier);
    line_vars.push_back(m_Lexer.identifierStr());
    cur_tok = getNextToken();
    vars_rest();
    //TODO if int or float
    for (; 0 < line_vars.size();) {
        if (var.find(line_vars[0]) != var.end()) {
            throw line_vars[0] + " already exist";
        }
        Variable new_var;
        new_var.int_val = 0;
        new_var.if_const = false;
        new_var.type = Variable::integer;
        var.insert(std::make_pair(line_vars[0], new_var));
        line_vars.erase(line_vars.begin(), line_vars.begin() + 1);
    }
    match(tok_semicolon);
    cur_tok = getNextToken();
}

void Parser::consts_rest() {
    switch (cur_tok) {
        case tok_comma: {
            cur_tok = getNextToken();
            match(tok_identifier);
            std::string name;
            name = m_Lexer.identifierStr();
            if (var.find(name) != var.end())
                throw name + " already exist";
            cur_tok = getNextToken();
            match(tok_equal);
            cur_tok = getNextToken();
            //TODO if float
            match(tok_integer);
            Variable new_const;
            new_const.int_val = m_Lexer.numVal();
            new_const.if_const = true;
            new_const.type = Variable::integer;
            var.insert(std::make_pair(name, new_const));
            cur_tok = getNextToken();
            consts_rest();
            return;
        }
        default:
            return;
    }
}

void Parser::consts() {
    match(tok_identifier);
    std::string name;
    name = m_Lexer.identifierStr();
    if (var.find(name) != var.end())
        throw name + " already exist";
    cur_tok = getNextToken();
    match(tok_equal);
    cur_tok = getNextToken();
    //TODO if float
    match(tok_integer);
    Variable new_const;
    new_const.int_val = m_Lexer.numVal();
    new_const.if_const = true;
    new_const.type = Variable::integer;
    var.insert(std::make_pair(name, new_const));
    cur_tok = getNextToken();
    consts_rest();
    match(tok_semicolon);
    cur_tok = getNextToken();
}

void Parser::vars_and_const() {
    switch (cur_tok) {
        case tok_var:
            cur_tok = getNextToken();
            vars();
            cur_tok = getNextToken();
            vars_and_const();
            return;
        case tok_const:
            cur_tok = getNextToken();
            consts();
            cur_tok = getNextToken();
            vars_and_const();
            return;
        default:
            return;
    }
}

void Parser::expression() {

}

void Parser::command() {
    switch (cur_tok) {
        case tok_identifier: {
            std::string name = m_Lexer.identifierStr();
            cur_tok = getNextToken();
            match(tok_assign);
            cur_tok = getNextToken();
            expression();
            return;
        }
        case tok_if:
            //TODO if
            return;
    }
}

void Parser::rest_command() {

}

void Parser::body() {
    match(tok_begin);
    cur_tok = getNextToken();
    command();
    rest_command();
    match(tok_end);
}


bool Parser::Parse() {
    cur_tok = getNextToken();
    start_of_prog();
    vars_and_const();
    //TODO func
    body();
    return true;
}

void Parser::InitLexan(char *name_of_file) {
    m_Lexer.InitInput(name_of_file);
}

const llvm::Module &Parser::Generate() {

    // create writeln function
    {
        std::vector<llvm::Type *> Ints(1, llvm::Type::getInt32Ty(MilaContext));
        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), Ints, false);
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "writeln", MilaModule);
        for (auto &Arg: F->args())
            Arg.setName("x");
    }

    // create main function
    {
        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), false);
        llvm::Function *MainFunction = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "main", MilaModule);

        // block
        llvm::BasicBlock *BB = llvm::BasicBlock::Create(MilaContext, "entry", MainFunction);
        MilaBuilder.SetInsertPoint(BB);

        // call writeln with value from lexel
        MilaBuilder.CreateCall(MilaModule.getFunction("writeln"), {
                llvm::ConstantInt::get(MilaContext, llvm::APInt(32, 0))
        });

        // return 0
        MilaBuilder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(MilaContext), 0));
    }

    return this->MilaModule;
}

/**
 * @brief Simple token buffer.
 *
 * CurTok is the current token the parser is looking at
 * getNextToken reads another token from the lexer and updates curTok with ts result
 * Every function in the parser will assume that CurTok is the cureent token that needs to be parsed
 */
Token Parser::getNextToken() {
    return m_Lexer.gettok();
}
