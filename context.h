#ifndef CONTEXT_H
#define CONTEXT_H

#include "map.h"
#include "strtable.h"
#include "vector.h"

struct Context {
    struct Strtable strtable;
    struct Vector register_ids;
};

void context_initialize(struct Context* context);
void context_register_registers(struct Context* context);

#endif
