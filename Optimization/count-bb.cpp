//===- count-bb.cpp - Counts number of executed basic-blocks -------------===//
//
// This file implements an LLVM pass that counts the number of BasicBlocks 
// that are executed by a program outputs the total count of executed 
// BasicBlocks when the program terminates by using a separately defined 
// function that is called atexit.
//
//===---------------------------------------------------------------------===//
#define DEBUG_TYPE "count-bb"

#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Function.h"
#include "llvm/BasicBlock.h"
#include "llvm/Instructions.h"
#include "llvm/GlobalVariable.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {

    struct CountBasicBlocks : public ModulePass {

        static char ID; // Pass identification, replacement for typeid

        CountBasicBlocks() : ModulePass(ID) {} 

        bool runOnModule(Module &M);
        bool runOnFunction(Function &F, Module &M);
        bool runOnBasicBlock(BasicBlock &BB, Module &M);

        bool initialize(Module &M);
    };
}

// ----- Static Variables -----
char CountBasicBlocks::ID = 0;

// ----- Pass Registration -----
static RegisterPass<CountBasicBlocks> 
    X("count-bb", "Counts number of executed BasicBlocks", false, false);


//===---------------------------------------------------------------------===//
//===- runOnModule(Module &M)
//===---------------------------------------------------------------------===//
bool CountBasicBlocks::runOnModule(Module &M) {
    bool modified = initialize(M);

    for(Module::iterator F = M.begin(), E = M.end(); F != E; ++F) { 
        modified |= runOnFunction(*F, M);
    }

    return modified;
}

//===---------------------------------------------------------------------===//
//===- runOnFunction(Function &F, Module &M)
//===---------------------------------------------------------------------===//
bool CountBasicBlocks::runOnFunction(Function &F, Module &M) {
    bool modified = false;

    for(Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
        modified |= runOnBasicBlock(*BB, M);
    }

    return modified;
}

//===---------------------------------------------------------------------===//
//===- runOnBasicBlock(BasicBlock &BB, Module &M)
//===---------------------------------------------------------------------===//
bool CountBasicBlocks::runOnBasicBlock(BasicBlock &BB, Module &M) {
    // Get the bbCounter GlobalVariable
    GlobalVariable *bbCounter = M.getGlobalVariable("bbCounter");
    assert(bbCounter && "Error: unable to get BasicBlock counter");

    // Insert instructions at the end of this BasicBlock
    TerminatorInst *InsertPos = BB.getTerminator(); 

    // Load, increment and store the value back
    Value *OldVal = new LoadInst(bbCounter, "old.bb.count", InsertPos);
    Value *NewVal = BinaryOperator::Create(
              Instruction::Add
            , OldVal
            , ConstantInt::get(Type::getInt64Ty(BB.getContext()), 1)
            , "new.bb.count"
            , InsertPos);
    new StoreInst(NewVal, bbCounter, InsertPos);

    return true; // IR modified
}

//===---------------------------------------------------------------------===//
//===- initialize() 
//===---------------------------------------------------------------------===//
bool CountBasicBlocks::initialize(Module &M) { 
    Function *Main = M.getFunction("main");
    assert(Main && "Error: count-bb requires a main function");

    Type *I64Ty = Type::getInt64Ty(M.getContext());

    // Create and insert a new GlobalVariable to track the number 
    // of basic blocks that are executed during the program's run
    //   int64 bbCounter = 0;
    new GlobalVariable(
              M                           // Module
            , I64Ty                       // Type
            , false                       // isConstant
            , GlobalValue::CommonLinkage  // Linkage
            , ConstantInt::get(I64Ty, 0)  // Initializer 
            , "bbCounter");               // Name

    // Create a declaration for the setupAtExit function
    Type *VoidTy = Type::getVoidTy(M.getContext());
    FunctionType *FTy = FunctionType::get(VoidTy, VoidTy);
    Constant *setupAtExitFunc = M.getOrInsertFunction("setupAtExit", FTy);

    // Insert a call to setupAtExit() at the start of main
    BasicBlock *BB = &Main->front();
    Instruction *I = &BB->front();
    CallInst::Create(setupAtExitFunc, "", I);

    return true; // modified IR
}
