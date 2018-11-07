#!/bin/bash

make 2>&1 | tee build.log && ./Main.native test_func.mc 2>&1 | tee -a build.log
