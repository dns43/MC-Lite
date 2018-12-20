#!/usr/bin/env python

# find llvmpy API here: https://github.com/llvmpy/llvmpy/blob/master/llvm/core.py

#import IPython

from llvm import *
from llvm.core import *
from collections import defaultdict
import enum
import sys
import logging
import pdb

#mod = Module.from_assembly(file("/home/dns43/coursework/Compilers/sprint/mc_program.ll"))

f_counter = 0;
bb_counter = 0;
insn_counter = 0;

constants = {}

def replace_constants(mod, bb):
    for insn in bb.instructions:
# if the variable is used, it gets replaced with its constant value
        if insn.opcode == OPCODE_ADD:
            for o in insn.operands:
                for c in constants:
                    if o == c:
                        builder = Builder.new(bb)
                        builder.position_before(insn)
                        builder.add(insn.opcode[0], c.value(), insn.name)
                        break

def collect_constants(mod, bb):
    global bb_counter
    global constants 
    
    bb_counter += 1;
    linsn = bb.instructions[1]
    for insn in bb.instructions:
        global insn_counter
        insn_counter += 1;
        print("instr : " + str(insn))

# A variable gots defined, and assigned a constant value
        if insn.opcode == OPCODE_ALLOCA:
            linsn = insn

        if insn.opcode == OPCODE_STORE and linsn.opcode == OPCODE_ALLOCA:
            linsn = insn
            if isinstance(insn.operands[0], ConstantInt):
                constants[insn] = insn.operands[0]

# The re-definitions of a variable kills its liveness
        if insn.opcode == OPCODE_STORE:
            linsn = insn
            for c in constants:
                if insn.operands[1] == c:
                    constants.pop(insn)

mod = Module.from_bitcode(file(sys.argv[1]))
functions = mod.functions
for f in functions:
    f_counter += 1;
    bb = f.entry_basic_block
    bb_counter += 1;
    while True:
        bb = collect_constants(mod, bb)
        if not bb: break
    if constants:
        for c in constants:
            print(c)
        while True:
            bb = replace_constants(mod, bb)
            if not bb: break
print(f_counter)
print(bb_counter)
print(insn_counter)


