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

static std::string GlobName = "";

static Function *cur_func;


class ExpAST {
public:
    virtual ~ExpAST() {};

    virtual ExpAST *clone() const = 0;

    virtual Value *
    codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) = 0;
};

class ComandAST {
public:
    virtual ~ComandAST() {};

    virtual ComandAST *clone() const = 0;

    virtual void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) = 0;
};

struct Variable {
    enum Type {
        integer, float_number, array
    };

    Variable *next = nullptr;
    Type type;
    int int_val = 0, st, en;
    double float_val = 0.0;
    bool if_const = false;
    ExpAST *exp = nullptr;

    Variable *clone() const {
        return new Variable(*this);
    }
};

static Type *returnType(Variable *a, llvm::LLVMContext &MilaContext) {
    if (a->type == Variable::integer) {
        return Type::getInt32Ty(MilaContext);
    }
    if (a->type == Variable::float_number) {
        return Type::getDoubleTy(MilaContext);
    }
    return ArrayType::get(returnType(a->next, MilaContext),
                          a->en - a->st + 1);

}

static AllocaInst *
CreateEntryBlockAlloca(Function *TheFunction, StringRef VarName, llvm::LLVMContext &MilaContext, Variable *a) {
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(returnType(a, MilaContext), nullptr, VarName);
}

static GlobalVariable *
CreateGlob(StringRef VarName, llvm::LLVMContext &MilaContext, llvm::Module &MilaModule,
           llvm::IRBuilder<> &MilaBuilder,
           Variable *a) {
    GlobalVariable *g;
    if (a->if_const) {
        g = new GlobalVariable(MilaModule, returnType(a, MilaContext),
                               true,
                               GlobalValue::ExternalLinkage,
                               0,
                               VarName);
    } else {
        g = new GlobalVariable(MilaModule, returnType(a, MilaContext),
                               false,
                               GlobalValue::ExternalLinkage,
                               0,
                               VarName);
    }
    if (a->exp) {
        if (a->type == Variable::integer)
            g->setInitializer(
                    dyn_cast<ConstantInt>(a->exp->codegen(MilaContext, MilaBuilder, MilaModule)));
        else
            g->setInitializer(
                    dyn_cast<ConstantFP>(
                            a->exp->codegen(MilaContext, MilaBuilder, MilaModule)));
    } else
        g->setInitializer(ConstantAggregateZero::get(returnType(a, MilaContext)));
    return g;
}

class PrototypeAST {
public:
    std::string Name;
    std::map<std::string, Variable> Args;
    std::vector<std::string> Args_order;
    std::map<std::string, Variable> Vars;

    PrototypeAST *clone() const {
        return new PrototypeAST(*this);
    }

    Function *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) {
        Type *res;
        if (Args.find(Name) != Args.end()) {
            res = returnType(&Args[Name], MilaContext);
        } else {
            res = Type::getVoidTy(MilaContext);

        }


        std::vector<Type *> arg;
        for (int i = 0; i < Args_order.size(); ++i) {
            arg.push_back(returnType(&Args[Args_order[i]], MilaContext));
        }
        llvm::FunctionType *FT = llvm::FunctionType::get(res, arg, false);
        llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, Name, MilaModule);
        int i = 0;
        for (auto &Arg: F->args())
            Arg.setName("st" + Args_order[i++]);
        return F;
    }

};

class FunctionAST {
public:
    PrototypeAST *Proto;
    ComandAST *Body;

    FunctionAST *clone() const {
        return new FunctionAST(*this);
    }

    Function *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) {
        Function *TheFunction = MilaModule.getFunction(Proto->Name);
        TheFunction = Proto->codegen(MilaContext, MilaBuilder, MilaModule);
        BasicBlock *BB = BasicBlock::Create(MilaContext, "entry" + Proto->Name, TheFunction);
        MilaBuilder.SetInsertPoint(BB);
        GlobName = Proto->Name;
        cur_func = TheFunction;
        //arg
        int i = 0;
        for (auto &Arg: TheFunction->args()) {
            Value *val = &Arg;
            AllocaInst *Alloca;
            Alloca = CreateEntryBlockAlloca(TheFunction, Proto->Args_order[i], MilaContext,
                                            &Proto->Args[Proto->Args_order[i]]);
            MilaBuilder.CreateStore(val, Alloca);
            Alloc a;
            a.alloca = Alloca;
            NamedValues[Proto->Args_order[i]] = a;
            i++;
        }
        //create return alloca;
        {
            AllocaInst *Alloca;
            if (Proto->Args.find(Proto->Name) != Proto->Args.end()) {
                Alloca = CreateEntryBlockAlloca(TheFunction, Proto->Name, MilaContext,
                                                &Proto->Args[Proto->Name]);
                Alloc a;
                a.alloca = Alloca;
                NamedValues[Proto->Name] = a;
            }
        }

        //vars
        for (auto i: Proto->Vars) {
            AllocaInst *Alloca;
            Alloca = CreateEntryBlockAlloca(TheFunction, i.first, MilaContext, &i.second);
            Alloc a;
            a.alloca = Alloca;
            NamedValues[i.first] = a;
        }


        Body->codegen(MilaContext, MilaBuilder, MilaModule);
        if (Proto->Args.find(Proto->Name) != Proto->Args.end())
            MilaBuilder.CreateRet(MilaBuilder.CreateLoad(NamedValues[Proto->Name].alloca->getAllocatedType(),
                                                         NamedValues[Proto->Name].alloca));
        else
            MilaBuilder.CreateRet(nullptr);
        NamedValues.clear();
        GlobName = "";
        cur_func = nullptr;
        return TheFunction;
    }
};

class FuncCallAST : public ExpAST {
public:
    FuncCallAST *clone() const override {
        return new FuncCallAST(*this);
    }

    Value *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        std::vector<Value *> arg;
        for (int i = 0; i < exp.size(); ++i) {
            arg.push_back(exp[i]->codegen(MilaContext, MilaBuilder, MilaModule));
        }
        Value *res = MilaBuilder.CreateCall(MilaModule.getFunction(prot.Name), {
                arg
        });
        return res;
    }

    std::vector<ExpAST *> exp;

    PrototypeAST prot;

};

class IntCastCallAST : public ExpAST {
public:
    IntCastCallAST *clone() const override {
        return new IntCastCallAST(*this);
    }

    Value *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        return MilaBuilder.CreateCall(MilaModule.getFunction("double_to_int"), {
                exp->codegen(MilaContext, MilaBuilder, MilaModule)
        });
    }

    ExpAST *exp;
};

class DoubleCastCallAST : public ExpAST {
public:
    DoubleCastCallAST *clone() const override {
        return new DoubleCastCallAST(*this);
    }

    Value *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        return MilaBuilder.CreateCall(MilaModule.getFunction("int_to_double"), {
                exp->codegen(MilaContext, MilaBuilder, MilaModule)
        });
    }

    ExpAST *exp;
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
        AllocaInst *A = NamedValues[name].alloca;
        Value *i;
        if (A)
            i = A;
        else
            i = B;
        for (int j = 0; j < num.size(); ++j) {
            Value *a = MilaBuilder.CreateSub(num[j]->codegen(MilaContext, MilaBuilder, MilaModule),
                                             ConstantInt::get(MilaContext, APInt(32, GlobNamedValues[name].st)));
            Value *i32zero = ConstantInt::get(MilaContext, APInt(32, 0));
            Value *indices[2] = {i32zero, a};
            i = MilaBuilder.CreateInBoundsGEP(i, ArrayRef<Value *>(indices, 2));
        }
        return MilaBuilder.CreateLoad(i->getType()->getPointerElementType(), i, name.c_str());
    }

    std::string name;
    std::vector<ExpAST *> num;
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
                case '&': {
                    Value *v = MilaBuilder.CreateMul(L, R, "multmp");
                    v = MilaBuilder.CreateICmpNE(v, ConstantInt::get(Type::getInt32Ty(MilaContext), APInt(32, 0)),
                                                 "noteq");
                    return MilaBuilder.CreateIntCast(v, Type::getInt32Ty(MilaContext), false);
                }
                case '|': {
                    Value *v = MilaBuilder.CreateAdd(L, R, "multmp");
                    v = MilaBuilder.CreateICmpNE(v, ConstantInt::get(Type::getInt32Ty(MilaContext), APInt(32, 0)),
                                                 "noteq");
                    return MilaBuilder.CreateIntCast(v, Type::getInt32Ty(MilaContext), false);
                }
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
                d.exp = left;
                L = d.codegen(MilaContext, MilaBuilder, MilaModule);
            }
            if (R->getType()->isIntegerTy()) {
                DoubleCastCallAST d;
                d.exp = right;
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
                case '&': {
                    Value *v = MilaBuilder.CreateFMul(L, R, "multmp");
                    v = MilaBuilder.CreateFCmpONE(v, ConstantFP::get(Type::getDoubleTy(MilaContext), APFloat(0.0)),
                                                  "noteq");
                    return MilaBuilder.CreateIntCast(v, Type::getInt32Ty(MilaContext), false);
                }
                case '|': {
                    Value *v = MilaBuilder.CreateFAdd(L, R, "multmp");
                    v = MilaBuilder.CreateFCmpONE(v, ConstantFP::get(Type::getDoubleTy(MilaContext), APFloat(0.0)),
                                                  "noteq");
                    return MilaBuilder.CreateIntCast(v, Type::getInt32Ty(MilaContext), false);
                }
                case '>':
                    switch (adop) {
                        case '=':
                            L = MilaBuilder.CreateFCmpOGE(L, R, "getmp");
                            return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                        default:
                            L = MilaBuilder.CreateFCmpOGT(L, R, "gttmp");
                            return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                    }
                case '<':
                    switch (adop) {
                        case '=':
                            L = MilaBuilder.CreateFCmpOLE(L, R, "letmp");
                            return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                        default:
                            L = MilaBuilder.CreateFCmpOLT(L, R, "lttmp");
                            return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                    }
                case '=':
                    L = MilaBuilder.CreateFCmpOEQ(L, R, "eqtmp");
                    return MilaBuilder.CreateIntCast(L, Type::getInt32Ty(MilaContext), false);
                case '!':
                    L = MilaBuilder.CreateFCmpONE(L, R, "netmp");
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

class StringAST : public ExpAST {
public:
    StringAST *clone() const override {
        return new StringAST(*this);
    }

    Value *codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        return nullptr;
    }

    std::string str;
};

//command


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
        if (Val->getType()->isIntegerTy())
            Val = MilaBuilder.CreateICmpNE(Val, ConstantInt::get(MilaContext, APInt(32, 0)), "whilecond");
        else
            Val = MilaBuilder.CreateFCmpONE(Val, ConstantFP::get(MilaContext, APFloat(0.0)), "whilecond");

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

class ExitAST : public ComandAST {
public:
    ExitAST *clone() const override {
        return new ExitAST(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        if (GlobName != "") {
            if (cur_func->getReturnType()->isVoidTy())
                MilaBuilder.CreateRet(nullptr);
            else
                MilaBuilder.CreateRet(MilaBuilder.CreateLoad(NamedValues[GlobName].alloca->getAllocatedType(),
                                                             NamedValues[GlobName].alloca));
        } else
            MilaBuilder.CreateRet(ConstantInt::get(Type::getInt32Ty(MilaContext), APInt(32, 0)));

        BasicBlock *after = BasicBlock::Create(MilaContext, "afterret");
        cur_func->getBasicBlockList().push_back(after);
        MilaBuilder.SetInsertPoint(after);


    }
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
                    d.exp = exp;
                    Val = d.codegen(MilaContext, MilaBuilder, MilaModule);
                }
                if (A->getType()->getPointerElementType()->isDoubleTy() && Val->getType()->isIntegerTy()) {
                    DoubleCastCallAST d;
                    d.exp = exp;
                    Val = d.codegen(MilaContext, MilaBuilder, MilaModule);
                }
                MilaBuilder.CreateStore(Val, A);
            } else {
                if (B->getType()->getPointerElementType()->isIntegerTy() && Val->getType()->isDoubleTy()) {
                    IntCastCallAST d;
                    d.exp = exp;
                    Val = d.codegen(MilaContext, MilaBuilder, MilaModule);
                }
                if (B->getType()->getPointerElementType()->isDoubleTy() && Val->getType()->isIntegerTy()) {
                    DoubleCastCallAST d;
                    d.exp = exp;
                    Val = d.codegen(MilaContext, MilaBuilder, MilaModule);
                }
                MilaBuilder.CreateStore(Val, B);
            }
        } else {
            ArrayElAST *al = dynamic_cast<ArrayElAST *>(var);
            Value *A = GlobNamedValues[al->name].glob, *B = NamedValues[al->name].alloca, *i;
            if (A)
                i = A;
            else
                i = B;
            for (int j = 0; j < al->num.size(); ++j) {
                Value *a = MilaBuilder.CreateSub(al->num[j]->codegen(MilaContext, MilaBuilder, MilaModule),
                                                 ConstantInt::get(MilaContext,
                                                                  APInt(32, GlobNamedValues[al->name].st)));
                Value *i32zero = ConstantInt::get(MilaContext, APInt(32, 0));
                Value *indices[2] = {i32zero, a};

                i = MilaBuilder.CreateInBoundsGEP(i, ArrayRef<Value *>(indices, 2));
            }
            if (i->getType()->getPointerElementType()->isIntegerTy() && Val->getType()->isDoubleTy()) {
                IntCastCallAST d;
                d.exp = exp;
                Val = d.codegen(MilaContext, MilaBuilder, MilaModule);
            }
            if (i->getType()->getPointerElementType()->isDoubleTy() && Val->getType()->isIntegerTy()) {
                DoubleCastCallAST d;
                d.exp = exp;
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
        if (Val->getType()->isIntegerTy()) {
            Val = MilaBuilder.CreateICmpNE(Val, ConstantInt::get(MilaContext, APInt(32, 0)), "ifcond");
        } else {
            Val = MilaBuilder.CreateFCmpONE(Val, ConstantFP::get(MilaContext, APFloat(0.0)), "ifcond");
        }

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
        L = assign->var->codegen(MilaContext, MilaBuilder, MilaModule);
        if (L->getType()->isIntegerTy() && res->getType()->isDoubleTy()) {
            IntCastCallAST i;
            i.exp = exp;
            res = i.exp->codegen(MilaContext, MilaBuilder, MilaModule);
        }
        if (L->getType()->isDoubleTy() && res->getType()->isIntegerTy()) {
            DoubleCastCallAST i;
            i.exp = exp;
            res = i.exp->codegen(MilaContext, MilaBuilder, MilaModule);
        }
        if (var) {
            Value *L1 = NamedValues[var->name].alloca, *L2 = GlobNamedValues[var->name].glob;
            if (L1)
                all = L1;
            else
                all = L2;
        } else {
            Value *A = GlobNamedValues[el->name].glob, *B = NamedValues[el->name].alloca, *i;
            if (A)
                i = A;
            else
                i = B;
            for (int j = 0; j < el->num.size(); ++j) {
                Value *a = MilaBuilder.CreateSub(el->num[j]->codegen(MilaContext, MilaBuilder, MilaModule),
                                                 ConstantInt::get(MilaContext,
                                                                  APInt(32, GlobNamedValues[el->name].st)));
                Value *i32zero = ConstantInt::get(MilaContext, APInt(32, 0));
                Value *indices[2] = {i32zero, a};
                i = MilaBuilder.CreateInBoundsGEP(i, ArrayRef<Value *>(indices, 2));
            }

            all = i;
        }
        if (L->getType()->isIntegerTy())
            r = MilaBuilder.CreateICmpEQ(L, res, "eqtmp");
        else
            r = MilaBuilder.CreateFCmpOEQ(L, res, "eqtmp");
        MilaBuilder.CreateCondBr(r, ExitBB, BodyBB);

        TheFunction->getBasicBlockList().push_back(BodyBB);
        MilaBuilder.SetInsertPoint(BodyBB);
        body->codegen(MilaContext, MilaBuilder, MilaModule);
        if (dir == 1)
            MilaBuilder.CreateStore(
                    MilaBuilder.CreateAdd(L, ConstantInt::get(MilaContext, APInt(32, 1)), "addtmp"), all);
        else
            MilaBuilder.CreateStore(
                    MilaBuilder.CreateSub(L, ConstantInt::get(MilaContext, APInt(32, 1)), "subtmp"), all);

        MilaBuilder.CreateBr(CondBB);
        TheFunction->getBasicBlockList().push_back(ExitBB);
        MilaBuilder.SetInsertPoint(ExitBB);
    }

    AssignAST *assign;
    ComandAST *body;
    ExpAST *exp;
    int dir;
};

class ProcCallAST : public ComandAST {
public:
    ProcCallAST *clone() const override {
        return new ProcCallAST(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        std::vector<Value *> arg;
        for (int i = 0; i < exp.size(); ++i) {
            arg.push_back(exp[i]->codegen(MilaContext, MilaBuilder, MilaModule));
        }
        MilaBuilder.CreateCall(MilaModule.getFunction(prot.Name), {
                arg
        });
    }

    std::vector<ExpAST *> exp;

    PrototypeAST prot;

};

class WritelnAST : public ComandAST {
public:

    WritelnAST *clone() const override {
        return new WritelnAST(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        StringAST *str = dynamic_cast<StringAST * >(exp);
        if (str) {
            for (int i = 0; i < str->str.size(); ++i) {
                if (i != str->str.size() - 1)
                    MilaBuilder.CreateCall(MilaModule.getFunction("writec"), {
                            ConstantInt::get(Type::getInt32Ty(MilaContext), APInt(32, int(str->str[i])))
                    });
                else
                    MilaBuilder.CreateCall(MilaModule.getFunction("writecln"), {
                            ConstantInt::get(Type::getInt32Ty(MilaContext), APInt(32, int(str->str[i])))
                    });
            }
        } else {
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
    }

    ExpAST *exp;
};

class WriteAST : public ComandAST {
public:

    WriteAST *clone() const override {
        return new WriteAST(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) override {
        StringAST *str = dynamic_cast<StringAST * >(exp);
        if (str) {
            for (int i = 0; i < str->str.size(); ++i) {
                MilaBuilder.CreateCall(MilaModule.getFunction("writec"), {
                        ConstantInt::get(Type::getInt32Ty(MilaContext), APInt(32, int(str->str[i])))
                });
            }
        } else {
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
            ArrayElAST *al = dynamic_cast<ArrayElAST *>(var);
            GlobalValue *B = GlobNamedValues[al->name].glob;
            AllocaInst *A = NamedValues[al->name].alloca;
            Value *i;
            if (A)
                i = A;
            else
                i = B;
            for (int j = 0; j < al->num.size(); ++j) {
                Value *a = MilaBuilder.CreateSub(al->num[j]->codegen(MilaContext, MilaBuilder, MilaModule),
                                                 ConstantInt::get(MilaContext,
                                                                  APInt(32, GlobNamedValues[al->name].st)));
                Value *i32zero = ConstantInt::get(MilaContext, APInt(32, 0));
                Value *indices[2] = {i32zero, a};

                i = MilaBuilder.CreateInBoundsGEP(i, ArrayRef<Value *>(indices, 2));
            }
            if (i->getType()->getPointerElementType()->isIntegerTy()) {
                MilaBuilder.CreateCall(MilaModule.getFunction("readln"), {
                        i
                });
            } else {
                MilaBuilder.CreateCall(MilaModule.getFunction("readfln"), {
                        i
                });
            }
        }
    }

    ExpAST *var;
};

class DecAST : public ComandAST {
public:
    DecAST *clone() const override {
        return new DecAST(*this);
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
                MilaBuilder.CreateStore(MilaBuilder.CreateSub(
                        MilaBuilder.CreateLoad(C->getType()->getPointerElementType(), C, var_->name),
                        ConstantInt::get(MilaContext, APInt(32, 1))
                ), C);
            }
            if (C->getType()->getPointerElementType()->isDoubleTy()) {
                MilaBuilder.CreateStore(MilaBuilder.CreateFSub(
                        MilaBuilder.CreateLoad(C->getType()->getPointerElementType(), C, var_->name),
                        ConstantFP::get(MilaContext, APFloat(1.1))
                ), C);
            }
        } else {
            ArrayElAST *al = dynamic_cast<ArrayElAST *>(var);
            GlobalValue *B = GlobNamedValues[al->name].glob;
            AllocaInst *A = NamedValues[al->name].alloca;
            Value *i;
            if (A)
                i = A;
            else
                i = B;
            for (int j = 0; j < al->num.size(); ++j) {

                Value *a = MilaBuilder.CreateSub(al->num[j]->codegen(MilaContext, MilaBuilder, MilaModule),
                                                 ConstantInt::get(MilaContext,
                                                                  APInt(32, GlobNamedValues[al->name].st)));
                Value *i32zero = ConstantInt::get(MilaContext, APInt(32, 0));
                Value *indices[2] = {i32zero, a};
                i = MilaBuilder.CreateInBoundsGEP(i, ArrayRef<Value *>(indices, 2));
            }
            if (i->getType()->getPointerElementType()->isIntegerTy()) {
                MilaBuilder.CreateStore(MilaBuilder.CreateSub(
                        MilaBuilder.CreateLoad(i->getType()->getPointerElementType(), i, var_->name),
                        ConstantInt::get(MilaContext, APInt(32, 1))
                ), i);
            } else {
                MilaBuilder.CreateStore(MilaBuilder.CreateFSub(
                        MilaBuilder.CreateLoad(i->getType()->getPointerElementType(), i, var_->name),
                        ConstantFP::get(MilaContext, APFloat(1.1))
                ), i);
            }
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
            glb = CreateGlob(i->first, MilaContext, MilaModule, MilaBuilder, &i->second);
            if (i->second.type == Variable::integer)
                a.type = Glob::not_array;
            if (i->second.type == Variable::float_number)
                a.type = Glob::not_array;
            if (i->second.type == Variable::array) {
                a.type = Glob::array;
                a.st = i->second.st;
                a.end = i->second.en;
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

class Funct {
public:

    Funct *clone() const {
        return new Funct(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) {
        for (auto &i: func)
            i->codegen(MilaContext, MilaBuilder, MilaModule);

    }

    std::vector<FunctionAST *> func;
};


class Module_prog {
public:
    Module_prog(Vars *new_vars = nullptr, Prog *new_main = nullptr, Funct *func = nullptr) : vars(
            new_vars),
                                                                                             main(new_main),
                                                                                             func(func) {}

    Module_prog *clone() const {
        return new Module_prog(*this);
    }

    void codegen(llvm::LLVMContext &MilaContext, llvm::IRBuilder<> &MilaBuilder, llvm::Module &MilaModule) {
        main->codegen(MilaContext, MilaBuilder, MilaModule);
    }

    Vars *vars;
    Funct *func;
    Prog *main;

};

#endif //SEMESTRALWORK_AST_H
