#!/bin/bash
set -eu

CCCC1=../build/cccc
CCCC2=./cccc2
CCCC3=cccc3
CCCC=false
GCC=gcc
SRC=..
INCLUDE=../test_include
TMP=/tmp

function compile_cccc () {
    src=$SRC/$2.c
    tmp=$TMP/$2_$1.c
    asm=$TMP/$2_$1.S

    $GCC -std=c11 -Wall -I$INCLUDE -E $src >$tmp
    $CCCC <$tmp >$asm
}

function compile_gcc () {
    src=$SRC/$2.c
    asm=$TMP/$2_$1.S
    $GCC -std=c11 -Wall $src -S -o $asm
}

function link () {
    gen=$1
    compiler=cccc$((gen+1))
    $GCC -std=c11 -Wall \
        $TMP/main_$gen.S \
        $TMP/codegen_$gen.S \
        $TMP/context_$gen.S \
        $TMP/dump_$gen.S \
        $TMP/fixup_$gen.S \
        $TMP/ir_$gen.S \
        $TMP/lexer_$gen.S \
        $TMP/list_$gen.S \
        $TMP/map_$gen.S \
        $TMP/nameresolve_$gen.S \
        $TMP/parser_$gen.S \
        $TMP/regalloc_$gen.S \
        $TMP/strtable_$gen.S \
        $TMP/simplify_$gen.S \
        $TMP/type_$gen.S \
        $TMP/typing_$gen.S \
        $TMP/vector_$gen.S \
        $TMP/visitor_$gen.S \
        -o $compiler
}

# compile with 1st-generation compiler

CCCC=$CCCC1

compile_cccc 1 main
compile_cccc 1 codegen
compile_cccc 1 context
compile_cccc 1 dump
compile_cccc 1 fixup
compile_cccc 1 ir
compile_cccc 1 lexer
compile_cccc 1 list
compile_cccc 1 map
compile_cccc 1 nameresolve
compile_gcc 1 parser
compile_cccc 1 regalloc
compile_cccc 1 strtable
compile_cccc 1 simplify
compile_cccc 1 type
compile_cccc 1 typing
compile_cccc 1 vector
compile_cccc 1 visitor

link 1

# test 2nd-generation compiler

$GCC -std=c11 -Wall -I$INCLUDE -E test.c >$TMP/test.c
$CCCC2 <$TMP/test.c >/tmp/output.S
$GCC -std=c11 -Wall main.c /tmp/output.S
./a.out
