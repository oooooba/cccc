#include "context.h"
#include "ir.h"
#include "map.h"
#include "strtable.h"
#include "vector.h"

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>

void context_initialize(struct Context* context) {
    strtable_initialize(&context->strtable);
    map_initialize(&context->var_for_func_map);
    vector_initialize(&context->register_ids, sizeof(strtable_id));
}

void context_register_var_for_func(struct Context* context, strtable_id index,
                                   struct VarIr* var_for_func) {
    map_insert(&context->var_for_func_map, (void*)index, var_for_func);
}

struct VarIr* context_find_var_for_func(struct Context* context,
                                        strtable_id index) {
    return map_find(&context->var_for_func_map, (void*)index);
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
    reg(r10);

    reg(rdi);
    reg(rsi);
    reg(rdx);
    reg(rcx);
    reg(r8);
    reg(r9);

    reg(rsp);
    reg(rbp);

#undef reg

    context->func_call_arg_reg_offset = 3;
    context->special_purpose_reg_offset = 9;
}

strtable_id context_nth_reg(struct Context* context, size_t n) {
    assert(n < context->special_purpose_reg_offset);
    return *((strtable_id*)vector_at(&context->register_ids, n));
}

strtable_id context_nth_func_call_arg_reg(struct Context* context, size_t n) {
    return context_nth_reg(context, context->func_call_arg_reg_offset + n);
}

strtable_id context_func_call_result_reg(struct Context* context) {
    return context_nth_reg(context, 0);
}

strtable_id context_stack_pointer_reg(struct Context* context) {
    return *((strtable_id*)vector_at(&context->register_ids,
                                     context->special_purpose_reg_offset + 0));
}

strtable_id context_base_pointer_reg(struct Context* context) {
    return *((strtable_id*)vector_at(&context->register_ids,
                                     context->special_purpose_reg_offset + 1));
}
