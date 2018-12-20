# MC-Lite
A lightweight linear algebra language compiler
MC-Lite is a simple, safe language focused on matrix manipulation and linear algebra. MC-Lite provides a simple C-like syntax and includes a primitive matrix type that makes working with linear algebra seamless and enables a high level of optimization for matrix operations.

Environment SetUp

MC-Lite was developed in OCaml. In order to used the compiler, the user should download OCaml. The OCaml Package Manager (OPAM) should be installed to access packages related OCaml. The user should also ensure that the LLVM library that is being installed matches the version of LLVM that is installed on the system of their device.

After everything is insalled, go to the directory where you want MC-Lite installed and clone the git repository:

>git clone https://github.com/dns43/MC-Lite.git

Using the Compiler

Inside the directory, MC-Lite, type ./make.sh
This creates the MC-Lite compiler that takes in the “.mc” files and compiles them to corresponding “.ll” files corresponding to LLVM IR. To run any “mc” executable, we enter the filename in our makefile and then run lli mc_program.ll

The following test program - test_branch.mc  demonstrates how to run MC-Lite code- int x = 1;

if (x == 2) {
  printi(2);
} else {
  printi(x);
}

while (x < 5) {
  printi(x);
  x = x+1;
}


Makefile -
#!/bin/bash
make clean;
make 2>&1 | tee build.log && ./Main.native test_branch.mc 2>&1 | tee -a build.log && lli mc_program.ll 2>&1 | tee -a build.log

To run -
./make.sh
lli mc_program.ll

Output -
1
1
2
3
4

**Uses yojson for printing: install with opam to run

**utils.ml contains some printing utilities, these are not fully implemented and will generate some warnings when running.
