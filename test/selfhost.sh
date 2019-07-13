#!/bin/bash
set -eu

CC=../build/cccc
CC2=cccc
GCC=gcc
SRC=..
TMP=/tmp

function compile_cccc () {
    $GCC -std=c11 -Wall -E $SRC/$1.c >$TMP/$1.c
    $CC <$TMP/$1.c >$TMP/$1.S
}

function compile_gcc () {
    $GCC -std=c11 -Wall $SRC/$1.c -S -o $TMP/$1.S
}

compile_gcc main
compile_gcc codegen
compile_gcc context
compile_gcc dump
compile_gcc fixup
compile_gcc ir
compile_gcc lexer
compile_gcc list
compile_gcc map
compile_gcc nameresolve
compile_gcc parser
compile_gcc regalloc
compile_gcc strtable
compile_gcc simplify
compile_gcc type
compile_gcc typing
compile_gcc vector
compile_gcc visitor

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

$CC <test.c >/tmp/output.S
$GCC -std=c11 -Wall main.c /tmp/output.S
./a.out
