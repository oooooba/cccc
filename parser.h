#ifndef PARSER_H
#define PARSER_H

#include "context.h"
#include "ir.h"
#include "list.h"

struct Parser {
    struct Context* context;
    struct List* tokens;
    struct ListHeader* current_token;
    struct BlockIr* current_block;
};

struct BlockIr* parser_run(struct Parser* parser);

void parser_initialize(struct Parser* parser, struct Context* context,
                       struct List* tokens);

#endif
