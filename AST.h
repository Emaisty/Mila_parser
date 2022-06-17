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

struct Alloc {
    enum type {
        array, not_array
    };
    type type;
    AllocaInst *alloca;

    int st, end;
};

struct Glob {
    enum type {
        array, not_array
    };
    type type;
    GlobalValue *glob;

    int st, end;
};

static std::map<std::string, Alloc> NamedValues;

static std::map<std::string, Glob> GlobNamedValues;

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

class PrototypeAST {
public:
    std::string Name;
    std::vector<std::string> Args;

    Function *
    codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) {

    }

};

class FunctionAST {
public:
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExpAST> Body;

    Function *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) {

    }
};

class FuncCallAST : public ExpAST {
public:
    FuncCallAST *clone() const override {
        return new FuncCallAST(*this);
    }

    Value *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {

    }

    PrototypeAST prot;

};

class IntCastCallAST : public ExpAST {
public:
    IntCastCallAST *clone() const override {
        return new IntCastCallAST(*this);
    }

    Value *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        return MilaBuilder.CreateCall(MilaModule.getFunction("double_to_int"), {
                exp
        });
    }

    Value *exp;
};

class DoubleCastCallAST : public ExpAST {
public:
    DoubleCastCallAST *clone() const override {
        return new DoubleCastCallAST(*this);
    }

    Value *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        return MilaBuilder.CreateCall(MilaModule.getFunction("int_to_double"), {
                exp
        });
    }

    Value *exp;
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
        AllocaInst *A = NamedValues[name].alloca;
        GlobalValue *B = GlobNamedValues[name].glob;
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

class ArrayElAST : public ExpAST {
public:
    ArrayElAST *clone() const override {
        return new ArrayElAST(*this);
    }

    Value *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        GlobalValue *B = GlobNamedValues[name].glob;
        Value *a = MilaBuilder.CreateSub(num->codegen(MilaContext, MilaBuilder, MilaModule),
                                         ConstantInt::get(MilaContext, APInt(32, GlobNamedValues[name].st)));
        Value *i32zero = ConstantInt::get(MilaContext, APInt(32, 0));
        Value *indices[2] = {i32zero, a};
        auto i = MilaBuilder.CreateInBoundsGEP(B, ArrayRef<Value *>(indices, 2));
        return MilaBuilder.CreateLoad(i->getType()->getPointerElementType(), i, name.c_str());
    }

    std::string name;
    ExpAST *num;
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
            if (L->getType()->isIntegerTy()) {
                DoubleCastCallAST d;
                d.exp = L;
                L = d.codegen(MilaContext, MilaBuilder, MilaModule);
            }
            if (R->getType()->isIntegerTy()) {
                DoubleCastCallAST d;
                d.exp = R;
                R = d.codegen(MilaContext, MilaBuilder, MilaModule);
            }
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
        integer, float_number, array_int, array_double
    };
    Type type;
    int int_val, st, en;
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
        VarAST *var_ = dynamic_cast<VarAST *>(var);
        Value *Val = exp->codegen(MilaContext, MilaBuilder, MilaModule);
        if (var_) {
            Value *A = NamedValues[var_->name].alloca, *B = GlobNamedValues[var_->name].glob;
            //MilaBuilder.CreateStore(Val, Variable);
            if (A) {
                if (A->getType()->getPointerElementType()->isIntegerTy() && Val->getType()->isDoubleTy()) {
                    IntCastCallAST d;
                    d.exp = Val;
                    Val = d.codegen(MilaContext, MilaBuilder, MilaModule);
                }
                if (A->getType()->getPointerElementType()->isDoubleTy() && Val->getType()->isIntegerTy()) {
                    DoubleCastCallAST d;
                    d.exp = Val;
                    Val = d.codegen(MilaContext, MilaBuilder, MilaModule);
                }
                MilaBuilder.CreateStore(Val, A);
            } else {
                if (B->getType()->getPointerElementType()->isIntegerTy() && Val->getType()->isDoubleTy()) {
                    IntCastCallAST d;
                    d.exp = Val;
                    Val = d.codegen(MilaContext, MilaBuilder, MilaModule);
                }
                if (B->getType()->getPointerElementType()->isDoubleTy() && Val->getType()->isIntegerTy()) {
                    DoubleCastCallAST d;
                    d.exp = Val;
                    Val = d.codegen(MilaContext, MilaBuilder, MilaModule);
                }
                MilaBuilder.CreateStore(Val, B);
            }
        } else {
            ArrayElAST *al = dynamic_cast<ArrayElAST *>(var);
            GlobalValue *B = GlobNamedValues[al->name].glob;

            Value *a = MilaBuilder.CreateSub(al->num->codegen(MilaContext, MilaBuilder, MilaModule),
                                             ConstantInt::get(MilaContext, APInt(32, GlobNamedValues[al->name].st)));
            Value *i32zero = ConstantInt::get(MilaContext, APInt(32, 0));
            Value *indices[2] = {i32zero, a};

            auto i = MilaBuilder.CreateInBoundsGEP(B, ArrayRef<Value *>(indices, 2));
            if (i->getType()->getPointerElementType()->isIntegerTy() && Val->getType()->isDoubleTy()) {
                IntCastCallAST d;
                d.exp = Val;
                Val = d.codegen(MilaContext, MilaBuilder, MilaModule);
            }
            if (i->getType()->getPointerElementType()->isDoubleTy() && Val->getType()->isIntegerTy()) {
                DoubleCastCallAST d;
                d.exp = Val;
                Val = d.codegen(MilaContext, MilaBuilder, MilaModule);
            }
            MilaBuilder.CreateStore(Val, i);
        }
    }

    ExpAST *var;
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

class ForAST : public ComandAST {
public:
    ForAST *clone() const override {
        return new ForAST(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        assign->codegen(MilaContext, MilaBuilder, MilaModule);
        Value *res = exp->codegen(MilaContext, MilaBuilder, MilaModule);
        if (dir == 1) {
            res = MilaBuilder.CreateAdd(res, ConstantInt::get(MilaContext, APInt(32, 1)));
        } else {
            res = MilaBuilder.CreateSub(res, ConstantInt::get(MilaContext, APInt(32, 1)));
        }
        VarAST *var = dynamic_cast<VarAST *>(assign->var);
        ArrayElAST *el;
        if (!var) {
            el = dynamic_cast<ArrayElAST *> (assign->var);
        }

        Function *TheFunction = MilaBuilder.GetInsertBlock()->getParent();

        BasicBlock *CondBB = BasicBlock::Create(MilaContext, "condition", TheFunction);
        BasicBlock *BodyBB = BasicBlock::Create(MilaContext, "for_body");
        BasicBlock *ExitBB = BasicBlock::Create(MilaContext, "exit");

        MilaBuilder.CreateBr(CondBB);
        MilaBuilder.SetInsertPoint(CondBB);
        Value *r, *L, *all;

        if (var) {
            Value *L1 = NamedValues[var->name].alloca, *L2 = GlobNamedValues[var->name].glob;
            if (L1)
                all = L1;
            else
                all = L2;
        } else {

        }
        L = var->codegen(MilaContext, MilaBuilder, MilaModule);
        r = MilaBuilder.CreateICmpEQ(L, res, "eqtmp");
        MilaBuilder.CreateCondBr(r, ExitBB, BodyBB);

        TheFunction->getBasicBlockList().push_back(BodyBB);
        MilaBuilder.SetInsertPoint(BodyBB);
        body->codegen(MilaContext, MilaBuilder, MilaModule);
        if (dir == 1)
            MilaBuilder.CreateStore(
                    MilaBuilder.CreateAdd(L, ConstantInt::get(MilaContext, APInt(32, 1)), "addtmp"), all);
        else
            MilaBuilder.CreateStore(
                    MilaBuilder.CreateSub(L, ConstantInt::get(MilaContext, APInt(32, 1)), "addtmp"), all);

        MilaBuilder.CreateBr(CondBB);
        TheFunction->getBasicBlockList().push_back(ExitBB);
        MilaBuilder.SetInsertPoint(ExitBB);
    }

    AssignAST *assign;
    ComandAST *body;
    ExpAST *exp;
    int dir;
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
        VarAST *var_ = dynamic_cast<VarAST *>(var);
        if (var_) {
            Value *A = NamedValues[var_->name].alloca, *B = GlobNamedValues[var_->name].glob, *C;
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
        } else {
            ArrayElAST *ar = dynamic_cast<ArrayElAST *>(var);
        }
    }

    ExpAST *var;
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
        for (auto i = vars_and_const.begin(); i != vars_and_const.end(); ++i) {
            GlobalVariable *glb;
            Glob a;
            if (i->second.type == Variable::integer) {
                a.type = Glob::not_array;
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
                a.type = Glob::not_array;
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
            if (i->second.type == Variable::array_int) {
                a.type = Glob::array;
                a.st = i->second.st;
                a.end = i->second.en;
                glb = new GlobalVariable(MilaModule, ArrayType::get(IntegerType::getInt32Ty(MilaContext),
                                                                    i->second.en - i->second.st + 1), false,
                                         GlobalValue::ExternalLinkage, 0, i->first);
                glb->setInitializer(ConstantAggregateZero::get(
                        ArrayType::get(IntegerType::getInt32Ty(MilaContext),
                                       i->second.en - i->second.st + 1)));

            }
            if (i->second.type == Variable::array_double) {
                a.type = Glob::array;
                a.st = i->second.st;
                a.end = i->second.en;
                glb = new GlobalVariable(MilaModule, ArrayType::get(IntegerType::getDoubleTy(MilaContext),
                                                                    i->second.en - i->second.st + 1), false,
                                         GlobalValue::ExternalLinkage, 0, i->first);
                glb->setInitializer(ConstantAggregateZero::get(
                        ArrayType::get(IntegerType::getDoubleTy(MilaContext),
                                       i->second.en - i->second.st + 1)));
            }
            a.glob = glb;
            GlobNamedValues[i->first] = a;
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
