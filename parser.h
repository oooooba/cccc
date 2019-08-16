#ifndef PARSER_H
#define PARSER_H

#include "context.h"
#include "ir.h"
#include "list.h"

#include <stdio.h>

struct Parser;

struct Parser* parser_new(struct Context* context, struct List* tokens,
                          FILE* error_stream);
void parser_run(struct Parser* parser);

#endif
