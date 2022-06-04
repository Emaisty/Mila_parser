#ifndef SEMESTRALWORK_AST_H
#define SEMESTRALWORK_AST_H

#include <iostream>
#include <vector>

struct Variable {
    enum Type {
        integer, float_number
    };
    Type type;
    int int_val;
    float float_val;
    bool if_const;
};

class ExpAST {
public:
    virtual ~ExpAST();
};

class NumberAST : public ExpAST {
public:
    double value;
};

class VarAST : public ExpAST {
public:

    VarAST(std::string name = "") : name(name) {}

    std::string getName();

    VarAST *clone() {
        return new VarAST(*this);
    }

    std::string name;
};

class BinoperAST : public ExpAST {
public:
    char op;
    ExpAST *left;
    ExpAST *right;
};

class BranchAST : public ExpAST {
public:
    ExpAST *exp;
};

//command
class ComandAST {
public:
    virtual ~ComandAST();
};

//assign operator
class AssignAST : public ComandAST {
public:

    AssignAST *clone() {
        return new AssignAST(*this);
    }

    VarAST *var;
    ExpAST exp;
};

class WritelnAST : public ComandAST {

};

class ReadAST : public ComandAST {

};

class Prog {
public:

    Prog *clone() {
        return new Prog(*this);
    }

    std::vector<ComandAST *> commands;
};

class Vars {
public:
    Vars(std::map<std::string, Variable> new_vars) : vars_and_const(new_vars) {}

    Vars *clone() {
        return new Vars(*this);
    }

private:
    std::map<std::string, Variable> vars_and_const;
};


class Module {
public:
    Module(Vars *new_vars = nullptr, Prog *new_main = nullptr) : vars(new_vars), main(new_main) {}

    Vars *vars;
    //TODO func
    Prog *main;

};

#endif //SEMESTRALWORK_AST_H
