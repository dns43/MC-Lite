#define DEBUG_TYPE "simple-stats"

#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Function.h"
#include "llvm/BasicBlock.h"
#include "llvm/ADT/Statistic.h"

STATISTIC(NumFunctions, "Number of functions in module");
STATISTIC(NumBasicBlocks, "Number of basic blocks in module");
STATISTIC(NumInstructions, "Number of instructions in module");

using namespace llvm;


namespace {

    struct SimpleStatistics : public ModulePass {

        static char ID; // Pass identification, replacement for typeid

        SimpleStatistics() : ModulePass(ID) {}

        bool runOnModule(Module &M);
        bool runOnFunction(Function &F, Module &M);

    };

}

char SimpleStatistics::ID = 0;

static RegisterPass<SimpleStatistics>
    X("simple-stats", "Gathers some simple statistics", false, false);

//===---------------------------------------------------------------------===//
//===- runOnModule(Module &M)
//===---------------------------------------------------------------------===//
bool SimpleStatistics::runOnModule(Module &M) {
    for(Module::iterator F = M.begin(), E = M.end(); F != E; ++F) {
        ++NumFunctions;
        runOnFunction(*F, M);
    }
    return false; // ir not modified
}

//===---------------------------------------------------------------------===//
//===- runOnFunction(Function &F, Module &M)
//===---------------------------------------------------------------------===//
bool SimpleStatistics::runOnFunction(Function &F, Module &M) {
    for(Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
        ++NumBasicBlocks;
        NumInstructions += BB->getInstList().size();
    }
    return false; // ir not modified
}
