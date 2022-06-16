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
    switch (cur_tok) {
        case tok_integer:
            for (int i = 0; i < names.size(); ++i) {
                for (auto j = var.begin(); j != var.end(); ++j) {
                    if (names[i] == j->first) {
                        std::cout << "var already exist";
                        throw "";
                    }
                }
                Variable a;
                a.type = Variable::integer;
                a.int_val = 0;
                a.if_const = false;
                a.exp = nullptr;
                var.insert(std::make_pair(names[i], a));
            }
            names.clear();
            break;
        case tok_double:
            for (int i = 0; i < names.size(); ++i) {
                for (auto j = var.begin(); j != var.end(); ++j) {
                    if (names[i] == j->first) {
                        std::cout << "var already exist";
                        throw "";
                    }
                }
                Variable a;
                a.type = Variable::float_number;
                a.float_val = 0;
                a.if_const = false;
                a.exp = nullptr;
                var.insert(std::make_pair(names[i], a));
            }
            names.clear();
            break;
        default:
            std::cout << "Error. Unknown type of var";
            throw "";
    }
    cur_tok = getNextToken();
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
        switch (cur_tok) {
            case tok_integer:
                for (int i = 0; i < names.size(); ++i) {
                    for (auto j = var.begin(); j != var.end(); ++j) {
                        if (names[i] == j->first) {
                            std::cout << "var already exist";
                            throw "";
                        }
                    }
                    Variable a;
                    a.type = Variable::integer;
                    a.int_val = 0;
                    a.if_const = false;
                    a.exp = nullptr;
                    var.insert(std::make_pair(names[i], a));
                }
                names.clear();
                break;
            case tok_double:
                for (int i = 0; i < names.size(); ++i) {
                    for (auto j = var.begin(); j != var.end(); ++j) {
                        if (names[i] == j->first) {
                            std::cout << "var already exist";
                            throw "";
                        }
                    }
                    Variable a;
                    a.type = Variable::float_number;
                    a.float_val = 0;
                    a.if_const = false;
                    a.exp = nullptr;
                    var.insert(std::make_pair(names[i], a));
                }
                names.clear();
                break;
            default:
                std::cout << "Error. Unknown type of var";
                throw "";
        }
        cur_tok = getNextToken();
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

ExpAST *Parser::faktor() {
    switch (cur_tok) {
        case tok_identifier: {
            VarAST var;
            var.name = m_Lexer.identifierStr();
            cur_tok = getNextToken();
            return var.clone();
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

VarAST *Parser::readln() {
    match(tok_opbrak);
    cur_tok = getNextToken();
    match(tok_identifier);
    VarAST var;
    var.name = m_Lexer.identifierStr();
    cur_tok = getNextToken();
    match(tok_clbrak);
    cur_tok = getNextToken();
    return var.clone();
}

ComandAST *Parser::command() {
    switch (cur_tok) {
        case tok_identifier: {
            std::string name = m_Lexer.identifierStr();
            VarAST var(name);
            cur_tok = getNextToken();
            match(tok_assign);
            cur_tok = getNextToken();
            AssignAST assign;
            assign.var = var.clone();
            assign.exp = full_expression();
            match(tok_semicolon);
            cur_tok = getNextToken();
            return assign.clone();
        }
        case tok_write: {
            cur_tok = getNextToken();
            WriteAST write;
            write.exp = writeln();
            match(tok_semicolon);
            cur_tok = getNextToken();
            return write.clone();
        }
        case tok_writeln: {
            cur_tok = getNextToken();
            WritelnAST write;
            write.exp = writeln();
            match(tok_semicolon);
            cur_tok = getNextToken();
            return write.clone();
        }
        case tok_readln: {
            cur_tok = getNextToken();
            ReadAST read;
            read.var = readln();
            match(tok_semicolon);
            cur_tok = getNextToken();
            return read.clone();
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
        case tok_begin: {
            cur_tok = getNextToken();
            BlockAST block;
            ComandAST *cmd;
            cmd = command();
            block.commands.push_back(cmd);
            while (cmd = command())
                block.commands.push_back(cmd);
            match(tok_end);
            cur_tok = getNextToken();
            match(tok_semicolon);
            cur_tok = getNextToken();
            return block.clone();
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
    while (cmd = command())
        main.commands.push_back(cmd);
    match(tok_end);
    return main.clone();

}


Module_prog *Parser::Parse() {
    cur_tok = getNextToken();
    start_of_prog();
    vars_and_const();
    Vars vars_and_const(var);
    //TODO func
    Prog *main;
    main = body();

    Module_prog module;
    module.vars = vars_and_const.clone();
    module.main = main;
    program = module.clone();
    return program;
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

    // create main function
    {
        llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), false);
        llvm::Function *MainFunction = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "main", MilaModule);

        // block
        llvm::BasicBlock *BB = llvm::BasicBlock::Create(MilaContext, "entry", MainFunction);
        MilaBuilder.SetInsertPoint(BB);

        // declare variables
        for (auto i = program->vars->vars_and_const.begin(); i != program->vars->vars_and_const.end(); ++i) {
            GlobalVariable *glb;
            if (i->second.type == Variable::integer) {
                if (i->second.if_const) {
                    glb = new GlobalVariable(MilaModule, Type::getInt32Ty(MilaContext),
                                             true,
                                             GlobalValue::ExternalLinkage,
                                             0,
                                             i->first);
                    if (i->second.exp)
                        glb->setInitializer(
                                dyn_cast<ConstantInt>(i->second.exp->codegen(MilaContext, MilaBuilder, MilaModule)));
                    else
                        glb->setInitializer(ConstantInt::get(MilaContext, APInt(32, i->second.int_val)));

                } else {
                    glb = new GlobalVariable(MilaModule, Type::getInt32Ty(MilaContext),
                                             false,
                                             GlobalValue::ExternalLinkage,
                                             0,
                                             i->first);
                    if (i->second.exp)
                        glb->setInitializer(
                                dyn_cast<ConstantInt>(i->second.exp->codegen(MilaContext, MilaBuilder, MilaModule)));
                    else
                        glb->setInitializer(ConstantInt::get(MilaContext, APInt(32, i->second.int_val)));

                }
            }
            if (i->second.type == Variable::float_number) {
                if (i->second.if_const) {
                    glb = new GlobalVariable(MilaModule, Type::getDoubleTy(MilaContext),
                                             true,
                                             GlobalValue::ExternalLinkage,
                                             0,
                                             i->first);
                    if (i->second.exp)
                        glb->setInitializer(
                                dyn_cast<ConstantFP>(
                                        i->second.exp->codegen(MilaContext, MilaBuilder, MilaModule)));
                    else
                        glb->setInitializer(ConstantFP::get(MilaContext, APFloat(i->second.float_val)));

                } else {
                    glb = new GlobalVariable(MilaModule, Type::getDoubleTy(MilaContext),
                                             false,
                                             GlobalValue::ExternalLinkage,
                                             0,
                                             i->first);
                    if (i->second.exp)
                        glb->setInitializer(
                                dyn_cast<ConstantFP>(
                                        i->second.exp->codegen(MilaContext, MilaBuilder, MilaModule)));
                    else
                        glb->setInitializer(ConstantFP::get(MilaContext, APFloat(i->second.float_val)));

                }
            }
            GlobNamedValues[i->first] = glb;
            /*AllocaInst *Alloca = CreateEntryBlockAllocaInt(MainFunction, i->first, MilaContext);
            Value *StartVal;
            if (i->second.exp) {
                StartVal = i->second.exp->codegen(MilaContext, MilaBuilder, MilaModule);
            } else {
                StartVal = ConstantInt::get(MilaContext, APInt(32, i->second.int_val));
            }
            MilaBuilder.CreateStore(StartVal, Alloca);
            NamedValues[i->first] = Alloca;
        } else {
            AllocaInst *Alloca = CreateEntryBlockAllocaDouble(MainFunction, i->first, MilaContext);
            Value *StartVal;
            if (i->second.exp) {
                StartVal = i->second.exp->codegen(MilaContext, MilaBuilder, MilaModule);
            } else {
                StartVal = ConstantFP::get(MilaContext, APFloat(i->second.float_val));
            }
            MilaBuilder.CreateStore(StartVal, Alloca);
            NamedValues[i->first] = Alloca;
        }*/

        }

        program->codegen(MilaContext, MilaBuilder, MilaModule);

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
