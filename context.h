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
    struct List
        global_declaration_list;  // ListItem list, elem: struct GlobaIr*
    struct Map
        function_definition_map;       // key: strtable_id, value: FunctionIr*,
    struct Map user_defined_type_map;  // key: strtable_id, value: TypeIr*
    struct Vector register_ids;        // strtable_id vector
};

enum RegisterSizeKind {
    RegisterSizeKind_8 = 0,
    RegisterSizeKind_32,
    RegisterSizeKind_64,
    RegisterSizeKind_Num,
};

void context_initialize(struct Context* context);

void context_append_global_declaration(struct Context* context,
                                       struct GlobalIr* global);
struct ListHeader* context_global_declaration_begin(struct Context* context);
struct ListHeader* context_global_declaration_end(struct Context* context);

void context_insert_function_definition(struct Context* context,
                                        strtable_id index,
                                        struct FunctionIr* func);
struct FunctionIr* context_find_function_definition(struct Context* context,
                                                    strtable_id index);
struct ListHeader* context_function_definition_begin(struct Context* context);
struct ListHeader* context_function_definition_end(struct Context* context);

void context_insert_user_defined_type(struct Context* context,
                                      strtable_id index, struct TypeIr* type);
struct TypeIr* context_find_user_defined_type(struct Context* context,
                                              strtable_id index);
struct ListHeader* context_user_defined_type_begin(struct Context* context);
struct ListHeader* context_user_defined_type_end(struct Context* context);

void context_register_registers(struct Context* context);
strtable_id context_nth_reg(struct Context* context, size_t n,
                            enum RegisterSizeKind kind);
strtable_id context_nth_func_call_arg_reg(struct Context* context, size_t n,
                                          enum RegisterSizeKind kind);
strtable_id context_func_call_result_reg(struct Context* context,
                                         enum RegisterSizeKind kind);
strtable_id context_stack_pointer_reg(struct Context* context,
                                      enum RegisterSizeKind kind);
strtable_id context_base_pointer_reg(struct Context* context,
                                     enum RegisterSizeKind kind);

void context_dump_type(struct Context* context, FILE* stream,
                       struct TypeIr* type);

enum RegisterSizeKind context_type_to_register_size_kind(struct TypeIr* type);

#endif
