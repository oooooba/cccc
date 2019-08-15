#ifndef LEXER_H
#define LEXER_H

#include "context.h"
#include "list.h"
#include "vector.h"

#include <stdio.h>

struct Lexer {
    struct Context* context;
    char* buf;
    size_t line;
    size_t pos;
    struct List* tokens;
    FILE* input_stream;
    struct Vector reserved_keywords;
};

void lexer_initialize(struct Lexer* lexer, struct Context* context,
                      struct List* tokens, FILE* input_stream);

void lexer_read_and_tokenize(struct Lexer* lexer);

#endif
