#!/bin/bash


make clean;
#make 2>&1 | tee build.log && ./Main.native test_call.mc 2>&1 | tee -a build.log
#make 2>&1 | tee build.log && ./Main.native test_semant.mc 2>&1 | tee -a build.log
#make 2>&1 | tee build.log && ./Main.native test_codegen.mc 2>&1 | tee -a build.log
make 2>&1 | tee build.log && ./Main.native test_mat.mc 2>&1 | tee -a build.log
