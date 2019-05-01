#include "context.h"
#include "ast.h"
#include "map.h"
#include "strtable.h"
#include "vector.h"

#include <stddef.h>
#include <stdlib.h>

struct VarInfo {
    strtable_id index;
    size_t offset;
};

struct FunInfo {
    struct Map*
        local_var_info_map;  // key = strtable_id, value = struct VarInfo*
    size_t stack_slot_size;
};

struct VarInfo* context_new_var_info(strtable_id index, size_t offset) {
    struct VarInfo* info = malloc(sizeof(struct VarInfo));
    info->index = index;
    info->offset = offset;
    return info;
}

size_t context_var_info_offset(struct VarInfo* info) { return info->offset; }

struct FunInfo* context_new_fun_info(void) {
    struct FunInfo* info = malloc(sizeof(struct FunInfo));
    info->local_var_info_map = malloc(sizeof(struct Map));
    map_initialize(info->local_var_info_map);
    info->stack_slot_size = 0;
    return info;
}

size_t context_fun_info_stack_slot_size(struct FunInfo* info) {
    return info->stack_slot_size;
}

void context_fun_info_add_stack_slot_size(struct FunInfo* info, size_t n) {
    info->stack_slot_size += n;
}

void context_fun_info_roundup_stack_slot_size(struct FunInfo* info, size_t n) {
    info->stack_slot_size = (info->stack_slot_size + n - 1) / n * n;
}

struct VarInfo* context_find_local_var_info_by_strtable_id(struct FunInfo* info,
                                                           strtable_id index) {
    return map_find(info->local_var_info_map, (void*)index);
}

void context_insert_local_var_info(struct FunInfo* fun_info, strtable_id index,
                                   struct VarInfo* var_info) {
    map_insert(fun_info->local_var_info_map, (void*)index, var_info);
}

struct FunInfo* context_find_fun_info_by_fundef_node(
    struct Context* context, struct FundefNode* fundef) {
    return map_find(&context->fun_info_map, fundef);
}

void context_insert_fun_info(struct Context* context, struct FundefNode* fundef,
                             struct FunInfo* info) {
    map_insert(&context->fun_info_map, fundef, info);
}

void context_initialize(struct Context* context) {
    strtable_initialize(&context->strtable);
    vector_initialize(&context->register_ids, sizeof(strtable_id));
    map_initialize(&context->fun_info_map);
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
