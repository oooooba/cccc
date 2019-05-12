#!/bin/bash
set -eu

CC=../build/cccc
GCC=gcc
SRC_TARGET=test.c
SRC_MAIN=main.c
ASM=/tmp/output.S

$CC <$SRC_TARGET >$ASM
$GCC -std=c11 -Wall $SRC_MAIN $ASM
./a.out
