#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct Env {
    struct Map var_map;  // key: strtable_id, value: VarExprIr*
    struct Env* outer_env;
    struct BlockStmtIr* block;
};

static struct Env* env_new(struct Env* outer_env, struct BlockStmtIr* block) {
    struct Env* env = malloc(sizeof(struct Env));
    map_initialize(&env->var_map);
    env->outer_env = outer_env;
    env->block = block;
    return env;
}

static struct VarExprIr* env_find(struct Env* env, strtable_id index) {
    if (!env) return NULL;
    struct VarExprIr* var =
        (struct VarExprIr*)map_find(&env->var_map, (void*)index);
    if (var) {
        assert(ir_var_expr_index(var) == index);
        return var;
    } else
        return env_find(env->outer_env, index);
}

static void env_insert(struct Env* env, strtable_id index,
                       struct VarExprIr* var) {
    assert(ir_var_expr_index(var) == index);
    map_insert(&env->var_map, (void*)index, var);
}

struct NameresolveVisitor {
    struct Visitor as_visitor;
    struct Env* current_env;
};

static struct Visitor* as_visitor(struct NameresolveVisitor* visitor) {
    return &visitor->as_visitor;
}

static struct Context* ctx(struct NameresolveVisitor* visitor) {
    return visitor_context(as_visitor(visitor));
}

static struct ExprIr* visit_var_expr(struct NameresolveVisitor* visitor,
                                     struct VarExprIr* ir) {
    if (!ir_var_expr_is_invalid(ir)) return ir_var_expr_cast(ir);
    strtable_id id = ir_var_expr_index(ir);
    struct VarExprIr* src = env_find(visitor->current_env, id);
    assert(src);
    ir_var_expr_copy(ir, src);
    return ir_var_expr_cast(ir);
}

static struct StmtIr* visit_block_stmt(struct NameresolveVisitor* visitor,
                                       struct BlockStmtIr* ir) {
    struct Env* outer_env = visitor->current_env;
    visitor->current_env = env_new(outer_env, ir);
    visitor_visit_block_stmt(as_visitor(visitor), ir);
    visitor->current_env = outer_env;
    return ir_block_stmt_super(ir);
}

static struct StmtIr* visit_decl_stmt(struct NameresolveVisitor* visitor,
                                      struct DeclStmtIr* ir) {
    strtable_id var_id = ir_decl_stmt_var_id(ir);
    struct TypeIr* type = ir_decl_stmt_type(ir);
    struct BlockStmtIr* block = visitor->current_env->block;
    struct VarExprIr* var =
        ir_block_stmt_allocate_variable(block, var_id, type);
    ir_decl_stmt_set_var(ir, var);
    env_insert(visitor->current_env, var_id, var);
    return ir_decl_stmt_super(ir);
}

static struct FunctionIr* visit_function(struct NameresolveVisitor* visitor,
                                         struct FunctionIr* ir) {
    visit_block_stmt(visitor, ir_function_body2(ir));
    return ir;
}

static struct Env* create_external_declaration_env(struct Context* context) {
    // ToDo: fix to handle declarations in correct order (e.g. forward
    // declaration)
    struct Env* env = env_new(NULL, NULL);
    for (struct ListHeader *it = context_function_declaration_begin(context),
                           *eit = context_function_declaration_end(context);
         it != eit; it = list_next(it)) {
        struct MapEntry* map_entry = (struct MapEntry*)it;
        struct FunctionIr* func = map_entry_value(map_entry);
        strtable_id id = ir_function_name_index(func);
        struct VarExprIr* var = ir_new_var_expr_from_function(id, func);
        env_insert(env, id, var);
    }
    return env;
}

struct NameresolveVisitor* new_nameresolve_visitor(struct Context* context) {
    struct NameresolveVisitor* visitor =
        malloc(sizeof(struct NameresolveVisitor));
    visitor_initialize(as_visitor(visitor), context);

    register_visitor(visitor->as_visitor, visit_var_expr, visit_var_expr);

    register_visitor(visitor->as_visitor, visit_block_stmt, visit_block_stmt);
    register_visitor(visitor->as_visitor, visit_decl_stmt, visit_decl_stmt);

    register_visitor(visitor->as_visitor, visit_function, visit_function);

    visitor->current_env = NULL;
    visitor->current_env = create_external_declaration_env(ctx(visitor));
    return visitor;
}
