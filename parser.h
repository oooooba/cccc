#ifndef PARSER_H
#define PARSER_H

#include "context.h"
#include "ir.h"
#include "list.h"

struct Parser* parser_new(struct Context* context, struct List* tokens);
struct BlockIr* parser_run(struct Parser* parser);

#endif
