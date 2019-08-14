#!/bin/bash
set -eu

CC=../build/cccc
CC2=cccc
GCC=gcc
SRC=..
INCLUDE=../test_include
TMP=/tmp

function compile_cccc () {
    $GCC -std=c11 -Wall -I$INCLUDE -E $SRC/$1.c >$TMP/$1.c
    $CC <$TMP/$1.c >$TMP/$1.S
}

function compile_gcc () {
    $GCC -std=c11 -Wall $SRC/$1.c -S -o $TMP/$1.S
}

compile_cccc main
compile_gcc codegen
compile_cccc context
compile_cccc dump
compile_cccc fixup
compile_cccc ir
compile_gcc lexer
compile_cccc list
compile_cccc map
compile_cccc nameresolve
compile_gcc parser
compile_cccc regalloc
compile_cccc strtable
compile_cccc simplify
compile_cccc type
compile_cccc typing
compile_cccc vector
compile_cccc visitor

$GCC -std=c11 -Wall \
    $TMP/main.S \
    $TMP/codegen.S \
    $TMP/context.S \
    $TMP/dump.S \
    $TMP/fixup.S \
    $TMP/ir.S \
    $TMP/lexer.S \
    $TMP/list.S \
    $TMP/map.S \
    $TMP/nameresolve.S \
    $TMP/parser.S \
    $TMP/regalloc.S \
    $TMP/strtable.S \
    $TMP/simplify.S \
    $TMP/type.S \
    $TMP/typing.S \
    $TMP/vector.S \
    $TMP/visitor.S \
    -o $CC2

$GCC -std=c11 -Wall -I$INCLUDE -E test.c >$TMP/test.c
$CC <$TMP/test.c >/tmp/output.S
$GCC -std=c11 -Wall main.c /tmp/output.S
./a.out
