#include "Parser.hpp"

Parser::Parser()
        : MilaContext(), MilaBuilder(MilaContext), MilaModule("mila", MilaContext) {
}

void Parser::match(Token tok) {
    if (cur_tok != tok) {
        std::cout << "ERROR. Expected: " << tok << " but get " << cur_tok << std::endl;
    }
}

int Parser::read_int() {
    switch (cur_tok) {
        case tok_minus:
            cur_tok = getNextToken();
            return -read_int();
        case tok_number_int:
            int a = m_Lexer.numVal();
            cur_tok = getNextToken();
            return a;
    }
}

ExpAST *Parser::read_var() {
    VarAST var;
    var.name = m_Lexer.identifierStr();
    cur_tok = getNextToken();
    switch (cur_tok) {
        case tok_opsqbrak: {
            ArrayElAST ar;
            ar.name = var.name;
            cur_tok = getNextToken();
            ar.num.push_back(full_expression());
            match(tok_clsqbrak);
            cur_tok = getNextToken();
            while (cur_tok == tok_opsqbrak) {
                cur_tok = getNextToken();
                ar.num.push_back(full_expression());
                match(tok_clsqbrak);
                cur_tok = getNextToken();
            }
            return ar.clone();
        }
        case tok_opbrak: {
            FuncCallAST func;
            func.prot.Name = var.name;
            cur_tok = getNextToken();
            func.exp.push_back(full_expression());
            while (cur_tok == tok_comma) {
                cur_tok = getNextToken();
                func.exp.push_back(full_expression());
            }
            match(tok_clbrak);
            cur_tok = getNextToken();
            return func.clone();
        }
        default:
            return var.clone();
    }
}

Variable Parser::getType() {
    Variable a;
    switch (cur_tok) {
        case tok_integer: {
            a.type = Variable::integer;
            cur_tok = getNextToken();
            break;
        }
        case tok_double: {
            a.type = Variable::float_number;
            cur_tok = getNextToken();
            break;
        }
        case tok_array: {
            cur_tok = getNextToken();
            match(tok_opsqbrak);
            cur_tok = getNextToken();
            a.st = read_int();
            match(tok_dot);
            cur_tok = getNextToken();
            match(tok_dot);
            cur_tok = getNextToken();
            a.en = read_int();
            match(tok_clsqbrak);
            cur_tok = getNextToken();
            match(tok_of);
            cur_tok = getNextToken();
            a.next = getType().clone();
            a.type = Variable::array;
            break;
        }
        default: {
            std::cout << "wrong type of var" << std::endl;
            throw "";
        }
    }
    return a;
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

void Parser::vars() {
    std::vector<std::string> names;
    match(tok_identifier);
    names.push_back(m_Lexer.identifierStr());
    cur_tok = getNextToken();
    while (cur_tok == tok_comma) {
        cur_tok = getNextToken();
        match(tok_identifier);
        names.push_back(m_Lexer.identifierStr());
        cur_tok = getNextToken();
    }
    match(tok_colon);
    cur_tok = getNextToken();
    Variable a = getType();
    for (int i = 0; i < names.size(); ++i) {
        for (auto j = var.begin(); j != var.end(); ++j) {
            if (names[i] == j->first) {
                std::cout << "var already exist " << j->first;
                throw "";
            }
        }
        var.insert(std::make_pair(names[i], a));
    }
    names.clear();

    match(tok_semicolon);
    cur_tok = getNextToken();
    while (cur_tok == tok_identifier) {
        match(tok_identifier);
        names.push_back(m_Lexer.identifierStr());
        cur_tok = getNextToken();
        while (cur_tok == tok_comma) {
            cur_tok = getNextToken();
            match(tok_identifier);
            names.push_back(m_Lexer.identifierStr());
            cur_tok = getNextToken();
        }
        match(tok_colon);
        cur_tok = getNextToken();
        Variable a = getType();
        for (int i = 0; i < names.size(); ++i) {
            for (auto j = var.begin(); j != var.end(); ++j) {
                if (names[i] == j->first) {
                    std::cout << "var already exist " << j->first;
                    throw "";
                }
            }
            var.insert(std::make_pair(names[i], a));
        }
        names.clear();
        match(tok_semicolon);
        cur_tok = getNextToken();
    }
}

void Parser::consts() {
    std::string name;
    match(tok_identifier);
    name = m_Lexer.identifierStr();
    for (auto j = var.begin(); j != var.end(); ++j) {
        if (name == j->first) {
            std::cout << "var already exist";
            throw "";
        }
    }
    cur_tok = getNextToken();
    match(tok_equal);
    cur_tok = getNextToken();
    Variable a;
    if (cur_tok == tok_number_int) {
        a.type = Variable::integer;
        a.int_val = m_Lexer.numVal();
    } else {
        a.type = Variable::float_number;
        a.float_val = m_Lexer.douVal();
    }
    a.if_const = true;
    a.exp = full_expression();
    var.insert(std::make_pair(name, a));
    while (cur_tok == tok_comma) {
        cur_tok = getNextToken();
        match(tok_identifier);
        name = m_Lexer.identifierStr();
        for (auto j = var.begin(); j != var.end(); ++j) {
            if (name == j->first) {
                std::cout << "var already exist";
                throw "";
            }
        }
        cur_tok = getNextToken();
        match(tok_equal);
        cur_tok = getNextToken();
        Variable a;
        if (cur_tok == tok_number_int) {
            a.type = Variable::integer;
            a.int_val = m_Lexer.numVal();
        } else {
            a.type = Variable::float_number;
            a.float_val = m_Lexer.douVal();
        }
        a.if_const = true;
        a.exp = full_expression();
        var.insert(std::make_pair(name, a));
    }
    match(tok_semicolon);
    cur_tok = getNextToken();
    while (cur_tok == tok_identifier) {
        name = m_Lexer.identifierStr();
        for (auto j = var.begin(); j != var.end(); ++j) {
            if (name == j->first) {
                std::cout << "var already exist";
                throw "";
            }
        }
        cur_tok = getNextToken();
        match(tok_equal);
        cur_tok = getNextToken();
        Variable a;
        if (cur_tok == tok_number_int) {
            a.type = Variable::integer;
            a.int_val = m_Lexer.numVal();
        } else {
            a.type = Variable::float_number;
            a.float_val = m_Lexer.douVal();
        }
        a.if_const = true;
        a.exp = full_expression();
        var.insert(std::make_pair(name, a));
        while (cur_tok == tok_comma) {
            cur_tok = getNextToken();
            match(tok_identifier);
            name = m_Lexer.identifierStr();
            for (auto j = var.begin(); j != var.end(); ++j) {
                if (name == j->first) {
                    std::cout << "var already exist";
                    throw "";
                }
            }
            cur_tok = getNextToken();
            match(tok_equal);
            cur_tok = getNextToken();
            Variable a;
            if (cur_tok == tok_number_int) {
                a.type = Variable::integer;
                a.int_val = m_Lexer.numVal();
            } else {
                a.type = Variable::float_number;
                a.float_val = m_Lexer.douVal();
            }
            a.if_const = true;
            a.exp = full_expression();
            var.insert(std::make_pair(name, a));
        }
        match(tok_semicolon);
        cur_tok = getNextToken();
    }
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

std::vector<std::string> arg_ord;
std::map<std::string, Variable> arg_;

void Parser::arg() {
    std::string name;
    if (cur_tok == tok_identifier) {
        name = m_Lexer.identifierStr();
        cur_tok = getNextToken();
        match(tok_colon);
        cur_tok = getNextToken();
        Variable a;
        arg_ord.push_back(name);
        a = getType();
        arg_.insert(std::make_pair(name, a));
        while (cur_tok == tok_semicolon) {
            cur_tok = getNextToken();
            match(tok_identifier);
            name = m_Lexer.identifierStr();
            cur_tok = getNextToken();
            match(tok_colon);
            cur_tok = getNextToken();
            Variable a;
            arg_ord.push_back(name);
            a = getType();
            arg_.insert(std::make_pair(name, a));
        }
    }
}


Funct *Parser::func() {
    Funct res;
    while (cur_tok == tok_function || cur_tok == tok_procedure) {
        PrototypeAST p;
        FunctionAST f;
        if (cur_tok == tok_function) {
            cur_tok = getNextToken();
            match(tok_identifier);
            p.Name = m_Lexer.identifierStr();
            functions.push_back(p.Name);
            cur_tok = getNextToken();
            match(tok_opbrak);
            cur_tok = getNextToken();
            arg();
            match(tok_clbrak);
            cur_tok = getNextToken();
            match(tok_colon);
            cur_tok = getNextToken();
            Variable a = getType();
            arg_.insert(std::make_pair(p.Name, a));
            p.Args_order = arg_ord;
            p.Args = arg_;
            arg_ord.clear();
            arg_.clear();
            match(tok_semicolon);
            cur_tok = getNextToken();
            vars_and_const();
            p.Vars = var;
            f.Body = command();
            var.clear();
            match(tok_semicolon);
            cur_tok = getNextToken();
        } else {
            cur_tok = getNextToken();
            match(tok_identifier);
            p.Name = m_Lexer.identifierStr();
            procedures.push_back(p.Name);
            cur_tok = getNextToken();
            match(tok_opbrak);
            cur_tok = getNextToken();
            arg();
            match(tok_clbrak);
            cur_tok = getNextToken();
            p.Args_order = arg_ord;
            p.Args = arg_;
            arg_ord.clear();
            arg_.clear();
            match(tok_semicolon);
            cur_tok = getNextToken();
            vars_and_const();
            p.Vars = var;
            f.Body = command();
            var.clear();
            match(tok_semicolon);
            cur_tok = getNextToken();
        }
        f.Proto = p.clone();
        res.func.push_back(f.clone());
    }

    return res.clone();
}


ExpAST *Parser::faktor() {
    switch (cur_tok) {
        case tok_identifier: {
            return read_var();
        }
        case tok_number_int: {
            IntAST integer;
            integer.value = m_Lexer.numVal();
            cur_tok = getNextToken();
            return integer.clone();
        }
        case tok_number_double: {
            FloatAST fl;
            fl.value = m_Lexer.douVal();
            cur_tok = getNextToken();
            return fl.clone();
        }
        case tok_opbrak: {
            cur_tok = getNextToken();
            BranchAST branch;
            branch.exp = full_expression();
            match(tok_clbrak);
            cur_tok = getNextToken();
            return branch.clone();
        }
        case tok_minus: {
            cur_tok = getNextToken();
            UnaroperAST unar;
            unar.exp = faktor();
            unar.op = '-';
            return unar.clone();
        }
        case tok_int: {
            cur_tok = getNextToken();
            IntCastCallAST i;
            i.exp = full_expression();
            return i.clone();
        }
        case tok_double: {
            cur_tok = getNextToken();
            DoubleCastCallAST i;
            i.exp = full_expression();
            return i.clone();
        }
        case tok_string: {
            StringAST st;
            st.str = m_Lexer.identifierStr();
            cur_tok = getNextToken();
            return st.clone();
        }
        default:
            std::cout << "unknown command\n";
            throw "not match " + cur_tok;
    }
}

ExpAST *Parser::term_prime(ExpAST *a) {
    switch (cur_tok) {
        case tok_mul: {
            cur_tok = getNextToken();
            BinoperAST mul_exp;
            ExpAST *mul_res;
            mul_exp.op = '*';
            mul_exp.left = a;
            mul_exp.right = faktor();
            mul_res = term_prime(mul_exp.clone());
            return mul_res;
        }
        case tok_div: {
            cur_tok = getNextToken();
            BinoperAST div_exp;
            ExpAST *div_res;
            div_exp.op = '/';
            div_exp.left = a;
            div_exp.right = faktor();
            div_res = term_prime(div_exp.clone());
            return div_res;
        }
        case tok_mod: {
            cur_tok = getNextToken();
            BinoperAST mod_exp;
            ExpAST *mod_res;
            mod_exp.op = 'm';
            mod_exp.left = a;
            mod_exp.right = faktor();
            mod_res = term_prime(mod_exp.clone());
            return mod_res;
        }
        default:
            return a;
    }
}

ExpAST *Parser::term() {
    ExpAST *a, *b;
    a = faktor();
    b = term_prime(a);
    return b;
}

ExpAST *Parser::expression_prime(ExpAST *a) {
    switch (cur_tok) {
        case tok_plus: {
            cur_tok = getNextToken();
            BinoperAST plus_exp;
            ExpAST *plus_res;
            plus_exp.op = '+';
            plus_exp.left = a;
            plus_exp.right = term();
            plus_res = expression_prime(plus_exp.clone());
            return plus_res;
        }
        case tok_minus: {
            cur_tok = getNextToken();
            BinoperAST minus_exp;
            ExpAST *minus_res;
            minus_exp.op = '-';
            minus_exp.left = a;
            minus_exp.right = term();
            minus_res = expression_prime(minus_exp.clone());
            return minus_res;
        }
        default:
            return a;
    }
}

ExpAST *Parser::expression() {
    ExpAST *a, *b;
    a = term();
    b = expression_prime(a);
    return b;
}

ExpAST *Parser::full_faktor() {
    switch (cur_tok) {
        case tok_not: {
            cur_tok = getNextToken();
            UnaroperAST op;
            op.op = '!';
            op.exp = full_faktor();
            return op.clone();
        }
        default: {
            ExpAST *res;
            res = expression();
            return res;
        }
    }
}

ExpAST *Parser::full_term_prime(ExpAST *a) {
    switch (cur_tok) {
        case tok_and: {
            cur_tok = getNextToken();
            BinoperAST op;
            ExpAST *res;
            op.op = '&';
            op.left = a;
            op.right = full_faktor();
            res = full_term_prime(op.clone());
            return res;
        }
        case tok_or: {
            cur_tok = getNextToken();
            BinoperAST op;
            ExpAST *res;
            op.op = '|';
            op.left = a;
            op.right = full_faktor();
            res = full_term_prime(op.clone());
            return res;
        }
        default:
            return a;
    }
}

ExpAST *Parser::full_term() {
    ExpAST *a, *b;
    a = full_faktor();
    b = full_term_prime(a);
    return b;
}

ExpAST *Parser::full_expression_prime(ExpAST *a) {
    switch (cur_tok) {
        case tok_greater: {
            cur_tok = getNextToken();
            BinoperAST gtop;
            ExpAST *res;
            gtop.op = '>';
            gtop.adop = '!';
            gtop.left = a;
            gtop.right = full_term();
            res = full_expression_prime(gtop.clone());
            return res;
        }
        case tok_greaterequal: {
            cur_tok = getNextToken();
            BinoperAST gtop;
            ExpAST *res;
            gtop.op = '>';
            gtop.adop = '=';
            gtop.left = a;
            gtop.right = full_term();
            res = full_expression_prime(gtop.clone());
            return res;
        }
        case tok_less: {
            cur_tok = getNextToken();
            BinoperAST gtop;
            ExpAST *res;
            gtop.op = '<';
            gtop.adop = '!';
            gtop.left = a;
            gtop.right = full_term();
            res = full_expression_prime(gtop.clone());
            return res;
        }
        case tok_lessequal: {
            cur_tok = getNextToken();
            BinoperAST gtop;
            ExpAST *res;
            gtop.op = '<';
            gtop.adop = '=';
            gtop.left = a;
            gtop.right = full_term();
            res = full_expression_prime(gtop.clone());
            return res;
        }
        case tok_equal: {
            cur_tok = getNextToken();
            BinoperAST gtop;
            ExpAST *res;
            gtop.op = '=';
            gtop.left = a;
            gtop.right = full_term();
            res = full_expression_prime(gtop.clone());
            return res;
        }
        case tok_notequal: {
            cur_tok = getNextToken();
            BinoperAST gtop;
            ExpAST *res;
            gtop.op = '!';
            gtop.left = a;
            gtop.right = full_term();
            res = full_expression_prime(gtop.clone());
            return res;
        }
        default:
            return a;
    }
}

ExpAST *Parser::full_expression() {
    ExpAST *a, *b;
    a = full_term();
    b = full_expression_prime(a);
    return b;
}

ExpAST *Parser::write() {
    match(tok_opbrak);
    cur_tok = getNextToken();
    ExpAST *a;
    a = full_expression();
    match(tok_clbrak);
    cur_tok = getNextToken();
    return a;
}

ExpAST *Parser::writeln() {
    match(tok_opbrak);
    cur_tok = getNextToken();
    ExpAST *a;
    a = full_expression();
    match(tok_clbrak);
    cur_tok = getNextToken();
    return a;
}

ExpAST *Parser::readln() {
    match(tok_opbrak);
    cur_tok = getNextToken();
    match(tok_identifier);
    ExpAST *a = read_var();
    match(tok_clbrak);
    cur_tok = getNextToken();
    return a;
}

ComandAST *Parser::command() {
    switch (cur_tok) {
        case tok_identifier: {
            for (int i = 0; i < procedures.size(); ++i)
                if (m_Lexer.identifierStr() == procedures[i]) {
                    ProcCallAST proc;
                    proc.prot.Name = m_Lexer.identifierStr();
                    cur_tok = getNextToken();
                    match(tok_opbrak);
                    cur_tok = getNextToken();
                    proc.exp.push_back(full_expression());
                    while (cur_tok == tok_comma) {
                        cur_tok = getNextToken();
                        proc.exp.push_back(full_expression());
                    }
                    match(tok_clbrak);
                    cur_tok = getNextToken();
                    return proc.clone();
                }

            ExpAST *id = read_var();
            match(tok_assign);
            cur_tok = getNextToken();
            AssignAST assign;
            assign.var = id;
            assign.exp = full_expression();
            return assign.clone();
        }
        case tok_write: {
            cur_tok = getNextToken();
            WriteAST write;
            write.exp = writeln();
            return write.clone();
        }
        case tok_writeln: {
            cur_tok = getNextToken();
            WritelnAST write;
            write.exp = writeln();
            return write.clone();
        }
        case tok_readln: {
            cur_tok = getNextToken();
            ReadAST read;
            read.var = readln();
            return read.clone();
        }
        case tok_dec: {
            cur_tok = getNextToken();
            DecAST dec;
            match(tok_opbrak);
            cur_tok = getNextToken();
            dec.var = read_var();
            match(tok_clbrak);
            cur_tok = getNextToken();
            return dec.clone();
        }
        case tok_if: {
            cur_tok = getNextToken();
            IfAST stat;
            stat.exp = full_expression();
            match(tok_then);
            cur_tok = getNextToken();
            stat.if_st = command();
            if (cur_tok == tok_else) {
                cur_tok = getNextToken();
                stat.else_st = command();
            } else
                stat.else_st = nullptr;
            return stat.clone();
        }
        case tok_while: {
            cur_tok = getNextToken();
            WhileAST st;
            st.exp = full_expression();
            match(tok_do);
            cur_tok = getNextToken();
            st.body = command();
            return st.clone();
        }
        case tok_for: {
            cur_tok = getNextToken();
            ForAST f;
            match(tok_identifier);
            ExpAST *id = read_var();
            match(tok_assign);
            cur_tok = getNextToken();
            AssignAST assign;
            assign.var = id;
            assign.exp = full_expression();
            f.assign = assign.clone();
            switch (cur_tok) {
                case tok_to:
                    f.dir = 1;
                    break;
                case tok_downto:
                    f.dir = -1;
                    break;
                default:
                    throw "456543456";
            }
            cur_tok = getNextToken();
            f.exp = full_expression();
            match(tok_do);
            cur_tok = getNextToken();
            f.body = command();
            return f.clone();
        }
        case tok_begin: {
            cur_tok = getNextToken();
            BlockAST block;
            ComandAST *cmd;
            cmd = command();
            block.commands.push_back(cmd);
            if (cur_tok == tok_semicolon) {
                cur_tok = getNextToken();
                while (cmd = command()) {
                    block.commands.push_back(cmd);
                    if (cur_tok == tok_semicolon)
                        cur_tok = getNextToken();
                    else
                        break;
                }
            }
            match(tok_end);
            cur_tok = getNextToken();
            return block.clone();
        }
        case tok_exit: {
            ExitAST ex;
            cur_tok = getNextToken();
            return ex.clone();
        }
        default:
            return nullptr;
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
    Prog main;
    match(tok_begin);
    cur_tok = getNextToken();
    ComandAST *cmd = command();
    main.commands.push_back(cmd);
    if (cur_tok == tok_semicolon) {
        cur_tok = getNextToken();
        while (cmd = command()) {
            main.commands.push_back(cmd);
            if (cur_tok == tok_semicolon)
                cur_tok = getNextToken();
            else
                break;
        }
    }
    match(tok_end);
    cur_tok = getNextToken();
    match(tok_dot);
    return main.clone();

}


Module_prog *Parser::Parse() {
    Module_prog module;
    Prog *main;
    cur_tok = getNextToken();
    start_of_prog();
    module.func = func();
    vars_and_const();
    Vars vars_and_const(var);

    main = body();


    module.vars = vars_and_const.clone();
    module.main = main;
    program = module.clone();
    return program;
}

void Parser::InitLexan(char *name_of_file) {
    m_Lexer.InitInput(name_of_file);
}

const llvm::Module &Parser::Generate() {

    // create writecln function
    {
        std::vector<llvm::Type *> Ints(1, llvm::Type::getInt32Ty(MilaContext));
        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), Ints, false);
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "writecln", MilaModule);
        for (auto &Arg: F->args())
            Arg.setName("x");
    }

    // create writec function
    {
        std::vector<llvm::Type *> Ints(1, llvm::Type::getInt32Ty(MilaContext));
        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), Ints, false);
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "writec", MilaModule);
        for (auto &Arg: F->args())
            Arg.setName("x");
    }

    // create writeln function
    {
        std::vector<llvm::Type *> Ints(1, llvm::Type::getInt32Ty(MilaContext));
        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), Ints, false);
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "writeln", MilaModule);
        for (auto &Arg: F->args())
            Arg.setName("x");
    }

    // create write function
    {
        std::vector<llvm::Type *> Ints(1, llvm::Type::getInt32Ty(MilaContext));
        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), Ints, false);
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "write", MilaModule);
        for (auto &Arg: F->args())
            Arg.setName("x");
    }
    //create readln function
    {
        std::vector<llvm::Type *> Ints(1, llvm::Type::getInt32PtrTy(MilaContext));
        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), Ints, false);
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "readln", MilaModule);
        for (auto &Arg: F->args())
            Arg.setName("x");
    }

    // create writefln function
    {
        std::vector<llvm::Type *> Floats(1, llvm::Type::getDoubleTy(MilaContext));
        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), Floats, false);
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "writefln", MilaModule);
        for (auto &Arg: F->args())
            Arg.setName("x");
    }

    // create writef function
    {
        std::vector<llvm::Type *> Floats(1, llvm::Type::getDoubleTy(MilaContext));
        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), Floats, false);
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "writef", MilaModule);
        for (auto &Arg: F->args())
            Arg.setName("x");
    }
    //create readfln function
    {
        std::vector<llvm::Type *> Floats(1, llvm::Type::getDoublePtrTy(MilaContext));
        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), Floats, false);
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "readfln", MilaModule);
        for (auto &Arg: F->args())
            Arg.setName("x");
    }

    //create cast to double func
    {
        std::vector<llvm::Type *> Ints(1, llvm::Type::getInt32Ty(MilaContext));
        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getDoubleTy(MilaContext), Ints, false);
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "int_to_double", MilaModule);
        for (auto &Arg: F->args())
            Arg.setName("x");
    }

    //create to cast to int func
    {
        std::vector<llvm::Type *> Floats(1, llvm::Type::getDoubleTy(MilaContext));
        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), Floats, false);
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "double_to_int", MilaModule);
        for (auto &Arg: F->args())
            Arg.setName("x");
    }

    program->func->codegen(MilaContext, MilaBuilder, MilaModule);

    program->vars->codegen(MilaContext, MilaBuilder, MilaModule);

    // create main function
    {

        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), false);
        llvm::Function *MainFunction = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "main", MilaModule);
        cur_func = MainFunction;
        // block
        llvm::BasicBlock *BB = llvm::BasicBlock::Create(MilaContext, "entry", MainFunction);
        MilaBuilder.SetInsertPoint(BB);


        program->codegen(MilaContext, MilaBuilder, MilaModule);
        // return 0
        MilaBuilder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(MilaContext), 0));
        cur_func = nullptr;

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
