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
        //llvm::AllocaInst()
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
            vars_and_const();
            return;
        case tok_const:
            cur_tok = getNextToken();
            consts();
            vars_and_const();
            return;
        default:
            return;
    }
}

int Parser::faktor() {
    switch (cur_tok) {
        case tok_identifier:
            cur_tok = getNextToken();
            return var[m_Lexer.identifierStr()].int_val;
        case tok_integer:
            cur_tok = getNextToken();
            return m_Lexer.numVal();
        case tok_opbrak: {
            cur_tok = getNextToken();
            int a;
            a = expression();
            match(tok_clbrak);
            cur_tok = getNextToken();
            return a;
        }
        case tok_minus: {
            cur_tok = getNextToken();
            int a;
            a = faktor();
            return -a;
        }
        default:
            throw "not match " + cur_tok;
    }
}

int Parser::term_prime(int a) {
    switch (cur_tok) {
        case tok_mul: {
            cur_tok = getNextToken();
            int b, c;
            b = faktor();
            c = term_prime(a * b);
            return c;
        }
        case tok_div: {
            cur_tok = getNextToken();
            int b, c;
            b = faktor();
            c = term_prime(a / b);
            return c;
        }
        default:
            return a;
    }
}

int Parser::term() {
    int a, b;
    a = faktor();
    b = term_prime(a);
    return b;
}

int Parser::expression_prime(int a) {
    switch (cur_tok) {
        case tok_plus: {
            cur_tok = getNextToken();
            int b, c;
            b = term();
            c = expression_prime(a + b);
            return c;
        }
        case tok_minus: {
            cur_tok = getNextToken();
            int b, c;
            b = term();
            c = expression_prime(a - b);
            return c;
        }
        default:
            return a;
    }
}

int Parser::expression() {
    int a, b;
    a = term();
    b = expression_prime(a);
    return b;
}

void Parser::writeln() {
    match(tok_opbrak);
    cur_tok = getNextToken();
    expression();
    match(tok_clbrak);
    cur_tok = getNextToken();
}

void Parser::readln() {
    match(tok_opbrak);
    cur_tok = getNextToken();
    match(tok_identifier);
    //TODO input
    cur_tok = getNextToken();
    match(tok_clbrak);
    cur_tok = getNextToken();
}

ComandAST *Parser::command() {
    switch (cur_tok) {
        case tok_identifier: {
            std::string name = m_Lexer.identifierStr();
            VarAST var(name);
            cur_tok = getNextToken();
            match(tok_assign);
            cur_tok = getNextToken();
            expression();
            AssignAST *assign;
            assign->var = var.clone();
            return assign->clone();
        }
        case tok_writeln:
            cur_tok = getNextToken();
            writeln();
            WritelnAST *write;
            return write;
        case tok_readln:
            cur_tok = getNextToken();
            readln();
            ReadAST *read;
            return read;
    }
}

void Parser::rest_command() {
    switch (cur_tok) {
        case tok_semicolon:
            cur_tok = getNextToken();
            command();
            rest_command();
            return;
        default:
            return;
    }
}

Prog *Parser::body() {
    Prog *main;
    match(tok_begin);
    cur_tok = getNextToken();
    main->commands.push_back(command());
    while (cur_tok == tok_semicolon) {
        cur_tok = getNextToken();
        main->commands.push_back(command());
    }
    match(tok_end);
    return main->clone();

}


Module *Parser::Parse() {
    cur_tok = getNextToken();
    start_of_prog();
    vars_and_const();
    Vars vars_and_const(var);
    //TODO func
    Prog *main;
    main = body();

    Module *module;
    module->vars = vars_and_const.clone();
    module->main = main;
    return module;
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
