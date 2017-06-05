#!/bin/bash
# llc --version
llvm-as < /dev/null |llc -march=arm -mcpu=help
