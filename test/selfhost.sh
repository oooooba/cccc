#!/bin/bash
set -eu

CCCC1=../build/cccc
CCCC2=./cccc2
CCCC3=./cccc3
TEST2=./test2
TEST3=./test3
CCCC=false
GCC=gcc
SRC=..
INCLUDE=../test_include
TMP=./tmp_link

function setup_tmp() {
    tmp=tmp$1
    if [ -e $tmp ]; then
        rm -rf $tmp
    fi
    mkdir $tmp

    if [ -e $TMP ]; then
        rm -f $TMP
    fi
    ln -s $tmp $TMP
}

function compile_cccc () {
    src=$SRC/$1.c
    tmp=$TMP/$1_tmp.c
    asm=$TMP/$1.S
    dump=$TMP/$1.dump
    echo compile $src using $CCCC

    $GCC -std=c11 -Wall -I$INCLUDE -E $src >$tmp
    $CCCC <$tmp >$asm 2>$dump
}

function compile_gcc () {
    src=$SRC/$1.c
    asm=$TMP/$1.S
    $GCC -std=c11 -Wall $src -S -o $asm
}

function link () {
    cd $TMP
    gen=$1
    compiler=cccc$((gen+1))
    $GCC -std=c11 -Wall \
        main.S \
        codegen.S \
        context.S \
        dump.S \
        fixup.S \
        ir.S \
        lexer.S \
        list.S \
        map.S \
        nameresolve.S \
        parser.S \
        regalloc.S \
        strtable.S \
        simplify.S \
        type.S \
        typing.S \
        vector.S \
        visitor.S \
        -save-temps \
        -o ../$compiler
    cd -
}

# compile with 1st-generation compiler

setup_tmp 1
CCCC=$CCCC1
ls -l $TMP

compile_cccc main
compile_cccc codegen
compile_cccc context
compile_cccc dump
compile_cccc fixup
compile_cccc ir
compile_cccc lexer
compile_cccc list
compile_cccc map
compile_cccc nameresolve
compile_cccc parser
compile_cccc regalloc
compile_cccc strtable
compile_cccc simplify
compile_cccc type
compile_cccc typing
compile_cccc vector
compile_cccc visitor

link 1

# test 2nd-generation compiler

$GCC -std=c11 -Wall -I$INCLUDE -E test.c >$TMP/test.c
$CCCC2 <$TMP/test.c >/tmp/output.S 2>/dev/null
$GCC -std=c11 -Wall main.c /tmp/output.S -o $TEST2
$TEST2

# compile with 2nd-generation compiler

setup_tmp 2
CCCC=$CCCC2
ls -l $TMP

compile_cccc main
compile_cccc codegen
compile_cccc context
compile_cccc dump
compile_cccc fixup
compile_cccc ir
compile_cccc lexer
compile_cccc list
compile_cccc map
compile_cccc nameresolve
compile_cccc parser
compile_cccc regalloc
compile_cccc strtable
compile_cccc simplify
compile_cccc type
compile_cccc typing
compile_cccc vector
compile_cccc visitor

link 2

# test 3rd-generation compiler

$GCC -std=c11 -Wall -I$INCLUDE -E test.c >$TMP/test.c
$CCCC3 <$TMP/test.c >/tmp/output.S 2>/dev/null
$GCC -std=c11 -Wall main.c /tmp/output.S -o $TEST3
$TEST3

# verification

sha1sum $CCCC1 $CCCC2 $CCCC3
