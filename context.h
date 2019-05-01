#ifndef CONTEXT_H
#define CONTEXT_H

#include "ast.h"
#include "map.h"
#include "strtable.h"
#include "vector.h"

#include <stddef.h>

struct VarInfo;
struct FunInfo;

struct Context {
    struct Strtable strtable;
    struct Vector register_ids;
    struct Map
        fun_info_map;  // key = struct FundefNode*, value = struct FunInfo*
};

struct VarInfo* context_new_var_info(strtable_id index, size_t offset);
size_t context_var_info_offset(struct VarInfo* info);
struct VarInfo* context_find_local_var_info_by_strtable_id(struct FunInfo* info,
                                                           strtable_id index);
void context_insert_local_var_info(struct FunInfo* fun_info, strtable_id index,
                                   struct VarInfo* var_info);

struct FunInfo* context_new_fun_info(void);
struct FunInfo* context_find_fun_info_by_fundef_node(struct Context* context,
                                                     struct FundefNode* fundef);
void context_insert_fun_info(struct Context* context, struct FundefNode* fundef,
                             struct FunInfo* fun_info);
size_t context_fun_info_stack_slot_size(struct FunInfo* info);
void context_fun_info_add_stack_slot_size(struct FunInfo* info, size_t n);
void context_fun_info_roundup_stack_slot_size(struct FunInfo* info, size_t n);

void context_initialize(struct Context* context);
void context_register_registers(struct Context* context);

#endif
