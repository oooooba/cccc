#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "context.h"
#include "list.h"

struct Parser {
    struct Context* context;
    struct List* tokens;
    struct ListHeader* current_token;
};

struct List* parser_parse(struct Parser* parser);

void parser_initialize(struct Parser* parser, struct Context* context,
                       struct List* tokens);

#endif
