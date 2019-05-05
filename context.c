#include "context.h"
#include "map.h"
#include "strtable.h"
#include "vector.h"

#include <stddef.h>
#include <stdlib.h>

void context_initialize(struct Context* context) {
    strtable_initialize(&context->strtable);
    vector_initialize(&context->register_ids, sizeof(strtable_id));
}

void context_register_registers(struct Context* context) {
    strtable_id id;

#define reg(name)                                                           \
    do {                                                                    \
        id = strtable_register(&context->strtable, "%" #name);              \
        *((strtable_id*)vector_allocate_back(&context->register_ids)) = id; \
    } while (0)

    reg(rax);
    reg(rbx);
    reg(rcx);
    reg(rdx);

#undef reg
}
