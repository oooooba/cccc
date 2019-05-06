#ifndef CONTEXT_H
#define CONTEXT_H

#include "map.h"
#include "strtable.h"
#include "vector.h"

#include <stddef.h>

struct Context {
    struct Strtable strtable;
    struct Vector register_ids;  // strtable_id vector
    size_t func_call_arg_reg_offset;
    size_t special_purpose_reg_offset;
};

void context_initialize(struct Context* context);
void context_register_registers(struct Context* context);
strtable_id context_nth_reg(struct Context* context, size_t n);
strtable_id context_nth_func_call_arg_reg(struct Context* context, size_t n);
strtable_id context_func_call_result_reg(struct Context* context);
strtable_id context_stack_pointer_reg(struct Context* context);
strtable_id context_base_pointer_reg(struct Context* context);

#endif
