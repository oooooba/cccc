#!/bin/bash
set -eu

CC=../build/cccc
GCC=gcc
SRC_TARGET=test.c
SRC_MAIN=main.c
TMP_C=/tmp/test.c
ASM=/tmp/output.S
INCLUDE=../include

$GCC -std=c11 -Wall -I$INCLUDE -E $SRC_TARGET >$TMP_C
$CC <$TMP_C >$ASM
$GCC -std=c11 -Wall $SRC_MAIN $ASM
./a.out
