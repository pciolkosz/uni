#!/bin/sh

../TestLatte $1 2>prog.asm
nasm -f elf32 prog.asm
gcc -m32 -o prog prog.o ../lib/runtime.o
./prog
