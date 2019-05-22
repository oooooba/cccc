#ifndef CONTEXT_H
#define CONTEXT_H

#include "ir.h"
#include "map.h"
#include "strtable.h"
#include "type.h"
#include "vector.h"

#include <stddef.h>
#include <stdio.h>

struct Context {
    struct Strtable strtable;
    struct Map function_declaration_map;  // key: strtable_id, value: Location*
    struct Map user_defined_type_map;     // key: strtable_id, value: TypeIr*
    struct Vector register_ids;           // strtable_id vector
    size_t func_call_arg_reg_offset;
    size_t special_purpose_reg_offset;
};

void context_initialize(struct Context* context);

void context_insert_function_declaration(struct Context* context,
                                         strtable_id index,
                                         struct Location* func_loc);
struct Location* context_find_function_declaration(struct Context* context,
                                                   strtable_id index);

void context_insert_user_defined_type(struct Context* context,
                                      strtable_id index, struct TypeIr* type);
struct TypeIr* context_find_user_defined_type(struct Context* context,
                                              strtable_id index);
struct ListHeader* context_user_defined_type_begin(struct Context* context);
struct ListHeader* context_user_defined_type_end(struct Context* context);

void context_register_registers(struct Context* context);
strtable_id context_nth_reg(struct Context* context, size_t n);
strtable_id context_nth_func_call_arg_reg(struct Context* context, size_t n);
strtable_id context_func_call_result_reg(struct Context* context);
strtable_id context_stack_pointer_reg(struct Context* context);
strtable_id context_base_pointer_reg(struct Context* context);

void context_dump_type(struct Context* context, FILE* stream,
                       struct TypeIr* type);

#endif
