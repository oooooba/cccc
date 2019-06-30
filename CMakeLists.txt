cmake_minimum_required(VERSION 2.8)
project(cccc C)

set(CMAKE_C_FLAGS "-g -std=c11 -Wall -Wextra -Werror")

add_executable(cccc
    main.c
    codegen.c
    context.c
    dump.c
    fixup.c
    ir.c
    lexer.c
    list.c
    map.c
    nameresolve.c
    parser.c
    regalloc.c
    strtable.c
    simplify.c
    type.c
    typing.c
    vector.c
    visitor.c)
