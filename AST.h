#ifndef SEMESTRALWORK_AST_H
#define SEMESTRALWORK_AST_H

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include <llvm/IR/Value.h>

#include <iostream>
#include <vector>
#include <map>

using namespace llvm;

static std::map<std::string, AllocaInst *> NamedValues;

static std::map<std::string, GlobalValue *> GlobNamedValues;

static AllocaInst *CreateEntryBlockAllocaInt(Function *TheFunction, StringRef VarName, llvm::LLVMContext &MilaContext) {
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(Type::getInt32Ty(MilaContext), nullptr, VarName);
}

static AllocaInst *
CreateEntryBlockAllocaDouble(Function *TheFunction, StringRef VarName, llvm::LLVMContext &MilaContext) {
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(Type::getDoubleTy(MilaContext), nullptr, VarName);
}

class ExpAST {
public:
    virtual ~ExpAST() {};

    virtual ExpAST *clone() const = 0;

    virtual Value *
    codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) = 0;
};

class FloatAST : public ExpAST {
public:
    double value;

    FloatAST *clone() const override {
        return new FloatAST(*this);
    }

    Value *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        return ConstantFP::get(MilaContext, APFloat(value));
    }
};

class IntAST : public ExpAST {
public:
    int value;

    IntAST *clone() const override {
        return new IntAST(*this);
    }

    Value *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        return ConstantInt::get(MilaContext, APInt(32, value));
    }
};

class VarAST : public ExpAST {
public:

    VarAST(std::string name = "") : name(name) {}

    std::string getName();

    VarAST *clone() const override {
        return new VarAST(*this);
    }

    Value *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        AllocaInst *A = NamedValues[name];
        GlobalValue *B = GlobNamedValues[name];
        if (!A && !B) {
            std::cout << "not known value\n";
            throw "Error";
        }
        if (A)
            return MilaBuilder.CreateLoad(A->getAllocatedType(), A, name.c_str());
        else
            return MilaBuilder.CreateLoad(B->getType()->getElementType(), B, name.c_str());
    }

    std::string name;
};

class BinoperAST : public ExpAST {
public:
    char op, adop;
    ExpAST *left;
    ExpAST *right;

    BinoperAST *clone() const override {
        return new BinoperAST(*this);
    }

    Value *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        Value *L = left->codegen(MilaContext, MilaBuilder, MilaModule);
        Value *R = right->codegen(MilaContext, MilaBuilder, MilaModule);
        if (!L || !R)
            return nullptr;
        if (L->getType()->isIntegerTy() && R->getType()->isIntegerTy()) {
            switch (op) {
                case '+':
                    return MilaBuilder.CreateAdd(L, R, "addtmp");
                case '-':
                    return MilaBuilder.CreateSub(L, R, "subtmp");
                case '*':
                    return MilaBuilder.CreateMul(L, R, "multmp");
                case '/':
                    return MilaBuilder.CreateUDiv(L, R, "divtmp");
                case 'm':
                    return MilaBuilder.CreateURem(L, R, "modtmp");
                    /*case '&':
                        L = MilaBuilder.CreateLogicalAnd(L, R, "andtmp");
                        return MilaBuilder.CreateLogicalAnd(L, R, "andtmp");
                        return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                    case '|':
                        L = MilaBuilder.CreateLogicalOr(L, R, "ortmp");
                        return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);*/
                case '>':
                    switch (adop) {
                        case '=':
                            L = MilaBuilder.CreateICmpSGE(L, R, "getmp");
                            return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                        default:
                            L = MilaBuilder.CreateICmpSGT(L, R, "gttmp");
                            return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                    }
                case '<':
                    switch (adop) {
                        case '=':
                            L = MilaBuilder.CreateICmpSLE(L, R, "letmp");
                            return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                        default:
                            L = MilaBuilder.CreateICmpSLT(L, R, "lttmp");
                            return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                    }
                case '=':
                    L = MilaBuilder.CreateICmpEQ(L, R, "eqtmp");
                    return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                case '!':
                    L = MilaBuilder.CreateICmpNE(L, R, "netmp");
                    return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);

            }
        } else {
            L = MilaBuilder.CreateFPCast(L, Type::getDoubleTy(MilaContext));
            R = MilaBuilder.CreateFPCast(R, Type::getDoubleTy(MilaContext));
            switch (op) {
                case '+':
                    return MilaBuilder.CreateFAdd(L, R, "addtmp");
                case '-':
                    return MilaBuilder.CreateFSub(L, R, "subtmp");
                case '*':
                    return MilaBuilder.CreateFMul(L, R, "multmp");
                case '/':
                    return MilaBuilder.CreateFDiv(L, R, "divtmp");
                case 'm':
                    return MilaBuilder.CreateFRem(L, R, "modtmp");
                    /*case '&':
                        L = MilaBuilder.CreateLogicalAnd(L, R, "andtmp");
                        return MilaBuilder.CreateLogicalAnd(L, R, "andtmp");
                        return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                    case '|':
                        L = MilaBuilder.CreateLogicalOr(L, R, "ortmp");
                        return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);*/
                case '>':
                    switch (adop) {
                        case '=':
                            L = MilaBuilder.CreateICmpSGT(L, R, "getmp");
                            return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                        default:
                            L = MilaBuilder.CreateICmpSGT(L, R, "gttmp");
                            return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                    }
                case '<':
                    switch (adop) {
                        case '=':
                            L = MilaBuilder.CreateICmpSLE(L, R, "letmp");
                            return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                        default:
                            L = MilaBuilder.CreateICmpSLT(L, R, "lttmp");
                            return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                    }
                case '=':
                    L = MilaBuilder.CreateICmpEQ(L, R, "eqtmp");
                    return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                case '!':
                    L = MilaBuilder.CreateICmpNE(L, R, "netmp");
                    return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);

            }
        }
    }
};

class UnaroperAST : public ExpAST {
public:
    char op;
    ExpAST *exp;

    UnaroperAST *clone() const override {
        return new UnaroperAST(*this);
    }

    Value *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        Value *L = exp->codegen(MilaContext, MilaBuilder, MilaModule);
        if (L->getType()->isIntegerTy()) {
            switch (op) {
                case '-':
                    return MilaBuilder.CreateSub(ConstantInt::get(MilaContext, APInt(32, 0)), L, "subtmp");
                case '!':
                    L = MilaBuilder.CreateICmpEQ(L, ConstantInt::get(MilaContext, APInt(32, 0)), "eqtmp");
                    return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
            }
        }
        if (L->getType()->isDoubleTy()) {
            switch (op) {
                case '-':
                    return MilaBuilder.CreateFSub(ConstantFP::get(MilaContext, APFloat(0.0)), L, "subtmp");
                case '!':
                    L = MilaBuilder.CreateICmpEQ(L, ConstantFP::get(MilaContext, APFloat(0.0)), "eqtmp");
                    return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
            }
        }
    }
};

class BranchAST : public ExpAST {
public:
    ExpAST *exp;

    BranchAST *clone() const override {
        return new BranchAST(*this);
    }

    Value *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        return exp->codegen(MilaContext, MilaBuilder, MilaModule);
    }

};

struct Variable {
    enum Type {
        integer, float_number
    };
    Type type;
    int int_val;
    double float_val;
    bool if_const;
    ExpAST *exp;
};

//command
class ComandAST {
public:
    virtual ~ComandAST() {};

    virtual ComandAST *clone() const = 0;

    virtual void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) = 0;
};

class WhileAST : public ComandAST {
public:
    WhileAST *clone() const override {
        return new WhileAST(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        Function *TheFunction = MilaBuilder.GetInsertBlock()->getParent();

        BasicBlock *CondBB = BasicBlock::Create(MilaContext, "condition", TheFunction);
        BasicBlock *BodyBB = BasicBlock::Create(MilaContext, "while_body");
        BasicBlock *ExitBB = BasicBlock::Create(MilaContext, "exit");

        MilaBuilder.CreateBr(CondBB);

        MilaBuilder.SetInsertPoint(CondBB);
        Value *Val = exp->codegen(MilaContext, MilaBuilder, MilaModule);
        Val = MilaBuilder.CreateICmpNE(Val, ConstantInt::get(MilaContext, APInt(32, 0)), "whilecond");

        MilaBuilder.CreateCondBr(Val, BodyBB, ExitBB);

        TheFunction->getBasicBlockList().push_back(BodyBB);
        MilaBuilder.SetInsertPoint(BodyBB);
        body->codegen(MilaContext, MilaBuilder, MilaModule);
        MilaBuilder.CreateBr(CondBB);

        TheFunction->getBasicBlockList().push_back(ExitBB);
        MilaBuilder.SetInsertPoint(ExitBB);
    }

    ExpAST *exp;
    ComandAST *body;
};

class BlockAST : public ComandAST {
public:
    BlockAST *clone() const override {
        return new BlockAST(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        for (auto &i: commands) {
            i->codegen(MilaContext, MilaBuilder, MilaModule);
        }
    }

    std::vector<ComandAST *> commands;
};

//assign operator
class AssignAST : public ComandAST {
public:

    AssignAST *clone() const override {
        return new AssignAST(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        Value *Variable = NamedValues[var->name];
        Value *Val = exp->codegen(MilaContext, MilaBuilder, MilaModule);
        //MilaBuilder.CreateStore(Val, Variable);
        MilaBuilder.CreateStore(Val, Variable);
    }

    VarAST *var;
    ExpAST *exp;
};

class IfAST : public ComandAST {
public:
    IfAST *clone() const override {
        return new IfAST(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        Value *Val = exp->codegen(MilaContext, MilaBuilder, MilaModule);
        Val = MilaBuilder.CreateICmpNE(Val, ConstantInt::get(MilaContext, APInt(32, 0)), "ifcond");

        Function *TheFunction = MilaBuilder.GetInsertBlock()->getParent();

        BasicBlock *ThenBB = BasicBlock::Create(MilaContext, "then", TheFunction);
        BasicBlock *ElseBB = BasicBlock::Create(MilaContext, "else");
        BasicBlock *MergeBB = BasicBlock::Create(MilaContext, "ifcont");
        if (else_st)
            MilaBuilder.CreateCondBr(Val, ThenBB, ElseBB);
        else
            MilaBuilder.CreateCondBr(Val, ThenBB, MergeBB);
        MilaBuilder.SetInsertPoint(ThenBB);

        if_st->codegen(MilaContext, MilaBuilder, MilaModule);

        MilaBuilder.CreateBr(MergeBB);
        // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
        // Emit else block.
        if (else_st) {
            TheFunction->getBasicBlockList().push_back(ElseBB);
            MilaBuilder.SetInsertPoint(ElseBB);
            else_st->codegen(MilaContext, MilaBuilder, MilaModule);


            MilaBuilder.CreateBr(MergeBB);
        }
        // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
        ElseBB = MilaBuilder.GetInsertBlock();

        // Emit merge block.
        TheFunction->getBasicBlockList().push_back(MergeBB);
        MilaBuilder.SetInsertPoint(MergeBB);


    }

    ExpAST *exp;
    ComandAST *if_st, *else_st;
};

class WritelnAST : public ComandAST {
public:

    WritelnAST *clone() const override {
        return new WritelnAST(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        Value *val = exp->codegen(MilaContext, MilaBuilder, MilaModule);
        if (val->getType()->isIntegerTy()) {
            MilaBuilder.CreateCall(MilaModule.getFunction("writeln"), {
                    val
            });
        }
        if (val->getType()->isDoubleTy()) {
            MilaBuilder.CreateCall(MilaModule.getFunction("writefln"), {
                    val
            });
        }
    }

    ExpAST *exp;
};

class WriteAST : public ComandAST {
public:

    WriteAST *clone() const override {
        return new WriteAST(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        Value *val = exp->codegen(MilaContext, MilaBuilder, MilaModule);
        if (val->getType()->isIntegerTy()) {
            MilaBuilder.CreateCall(MilaModule.getFunction("write"), {
                    val
            });
        }
        if (val->getType()->isDoubleTy()) {
            MilaBuilder.CreateCall(MilaModule.getFunction("writef"), {
                    val
            });
        }
    }

    ExpAST *exp;
};

class ReadAST : public ComandAST {
public:
    ReadAST *clone() const override {
        return new ReadAST(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        Value *A = NamedValues[var->name], *B = GlobNamedValues[var->name], *C;
        if (!A && !B) {
            std::cout << "not known value\n";
            throw "Error";
        }
        if (A)
            C = A;
        else
            C = B;
        if (C->getType()->getPointerElementType()->isIntegerTy()) {
            MilaBuilder.CreateCall(MilaModule.getFunction("readln"), {
                    C
            });
        }
        if (C->getType()->getPointerElementType()->isDoubleTy()) {
            MilaBuilder.CreateCall(MilaModule.getFunction("readfln"), {
                    C
            });
        }
    }

    VarAST *var;
};

class Prog {
public:

    Prog *clone() {
        return new Prog(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) {
        for (auto &i: commands) {
            i->codegen(MilaContext, MilaBuilder, MilaModule);
        }
    }

    std::vector<ComandAST *> commands;
};

class Vars {
public:
    Vars(std::map<std::string, Variable> new_vars) : vars_and_const(new_vars) {}

    Vars *clone() {
        return new Vars(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) {

    }

    std::map<std::string, Variable> vars_and_const;
};


class Module_prog {
public:
    Module_prog(Vars *new_vars = nullptr, Prog *new_main = nullptr) : vars(new_vars), main(new_main) {}

    Module_prog *clone() const {
        return new Module_prog(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) {
        //vars->codegen();
        main->codegen(MilaContext, MilaBuilder, MilaModule);
    }

    Vars *vars;
    //TODO func
    Prog *main;

};

#endif //SEMESTRALWORK_AST_H
