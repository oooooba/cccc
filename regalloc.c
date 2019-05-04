#include "ast.h"
#include "context.h"
#include "ir.h"
#include "list.h"
#include "map.h"
#include "type.h"
#include "visitor.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

struct RegallocVisitor {
    struct Visitor super_type;
    struct Context* context;

    /*
     * behave like stack pointer
     *   0 <= i < free_register_index : valid values are located
     *   free_register_index <= i     : free registers
     *   result must be located at free_register_index
     */
    size_t free_register_index;

    struct BlockStmtNode* current_block;  // for expanding sub-expressions
    struct FunInfo* current_fun_info;
    bool require_location;
};

static struct Visitor* super_type(struct RegallocVisitor* visitor) {
    return &visitor->super_type;
}

static strtable_id use_register(struct RegallocVisitor* visitor) {
    strtable_id id = *((strtable_id*)vector_at(&visitor->context->register_ids,
                                               visitor->free_register_index));
    ++visitor->free_register_index;
    return id;
}

static void kill_register(struct RegallocVisitor* visitor) {
    --visitor->free_register_index;
}

static strtable_id reuse_register(struct RegallocVisitor* visitor) {
    kill_register(visitor);
    return *((strtable_id*)vector_at(&visitor->context->register_ids,
                                     visitor->free_register_index));
}

static struct VarExprNode* create_reg_var(strtable_id index) {
    struct IdNode* id = ast_new_id(index);
    return ast_new_var_expr(id);
}

static struct ExprStmtNode* create_assembly_stmt(struct ExprNode* dst,
                                                 struct ExprNode* operation) {
    struct BinopExprNode* subst_expr =
        ast_new_binop_expr(BinopExprNodeTag_Subst, dst, operation);
    struct ExprStmtNode* subst_stmt =
        ast_new_expr_stmt(ast_binop_expr_super_type(subst_expr));
    return subst_stmt;
}

static struct ExprNode* visit_literal_expr(struct RegallocVisitor* visitor,
                                           struct LiteralExprNode* node) {
    strtable_id index = use_register(visitor);
    struct VarExprNode* reg = create_reg_var(index);
    struct ExprStmtNode* asm_stmt = create_assembly_stmt(
        ast_var_expr_super_type(reg), ast_literal_expr_super_type(node));
    ast_block_stmt_insert_at_end(visitor->current_block,
                                 ast_expr_stmt_super_type(asm_stmt));
    return NULL;
}

static struct ExprNode* visit_uniop_expr(struct RegallocVisitor* visitor,
                                         struct UniopExprNode* node) {
    bool require_location = visitor->require_location;
    if (require_location)
        assert(ast_uniop_expr_op(node) == UniopExprNodeTag_Deref);
    visitor->require_location = false;

    if (ast_uniop_expr_op(node) == UniopExprNodeTag_Addrof) {
        assert(ast_expr_tag(ast_uniop_expr_expr(node)) == ExprNodeTag_Var);
    } else {
        visitor_visit_expr(super_type(visitor), ast_uniop_expr_expr(node));

        strtable_id expr_index = reuse_register(visitor);
        struct IdNode* expr_id = ast_new_id(expr_index);
        struct VarExprNode* expr_reg = ast_new_var_expr(expr_id);
        ast_uniop_expr_set_expr(node, ast_var_expr_super_type(expr_reg));
    }

    strtable_id result_index = use_register(visitor);
    struct VarExprNode* result_reg = create_reg_var(result_index);

    if (require_location) return NULL;

    struct ExprStmtNode* asm_stmt = create_assembly_stmt(
        ast_var_expr_super_type(result_reg), ast_uniop_expr_super_type(node));
    ast_block_stmt_insert_at_end(visitor->current_block,
                                 ast_expr_stmt_super_type(asm_stmt));

    return NULL;
}

static struct ExprNode* visit_binop_expr(struct RegallocVisitor* visitor,
                                         struct BinopExprNode* node) {
    if (ast_binop_expr_op(node) == BinopExprNodeTag_Subst) {
        struct ExprNode* lhs = ast_binop_expr_lhs(node);
        assert(ast_expr_tag(lhs) == ExprNodeTag_Uniop);
        struct UniopExprNode* uniop = (struct UniopExprNode*)lhs;
        assert(ast_uniop_expr_op(uniop) == UniopExprNodeTag_Deref);

        struct ExprNode* rhs = ast_binop_expr_rhs(node);

        visitor->require_location = true;
        visitor_visit_expr(super_type(visitor), lhs);
        assert(visitor->require_location == false);

        visitor_visit_expr(super_type(visitor), rhs);

        strtable_id rhs_index = reuse_register(visitor);
        struct VarExprNode* rhs_reg = create_reg_var(rhs_index);

        struct ExprStmtNode* asm_stmt =
            create_assembly_stmt(lhs, (struct ExprNode*)rhs_reg);
        ast_block_stmt_insert_at_end(visitor->current_block,
                                     ast_expr_stmt_super_type(asm_stmt));
    } else {
        visitor_visit_expr(super_type(visitor), ast_binop_expr_lhs(node));
        visitor_visit_expr(super_type(visitor), ast_binop_expr_rhs(node));

        strtable_id rhs_index = reuse_register(visitor);
        struct VarExprNode* rhs_reg = create_reg_var(rhs_index);
        ast_binop_expr_set_rhs(node, ast_var_expr_super_type(rhs_reg));

        strtable_id lhs_index = reuse_register(visitor);
        struct VarExprNode* lhs_reg = create_reg_var(lhs_index);
        ast_binop_expr_set_lhs(node, ast_var_expr_super_type(lhs_reg));

        strtable_id result_index = use_register(visitor);
        struct VarExprNode* result_reg = create_reg_var(result_index);

        struct ExprStmtNode* asm_stmt =
            create_assembly_stmt(ast_var_expr_super_type(result_reg),
                                 ast_binop_expr_super_type(node));
        ast_block_stmt_insert_at_end(visitor->current_block,
                                     ast_expr_stmt_super_type(asm_stmt));
    }

    return NULL;
}

static struct StmtNode* visit_expr_stmt(struct RegallocVisitor* visitor,
                                        struct ExprStmtNode* node) {
    visitor->current_block = ast_new_block_stmt();

    visitor_visit_expr(super_type(visitor), ast_expr_stmt_expr(node));
    kill_register(visitor);

    struct BlockStmtNode* block = visitor->current_block;
    visitor->current_block = NULL;
    return ast_block_stmt_super_type(block);
}

static struct StmtNode* visit_decl_stmt(struct RegallocVisitor* visitor,
                                        struct DeclStmtNode* node) {
    struct DeclNode* decl = ast_decl_stmt_decl(node);

    size_t var_size = type_size(ast_decl_type(decl));
    size_t offset = context_fun_info_stack_slot_size(visitor->current_fun_info);
    if (offset % var_size != 0) {
        context_fun_info_roundup_stack_slot_size(visitor->current_fun_info,
                                                 var_size);
        offset = context_fun_info_stack_slot_size(visitor->current_fun_info);
    }
    context_fun_info_add_stack_slot_size(visitor->current_fun_info, var_size);

    strtable_id index = ast_id_index(ast_decl_name(decl));
    struct VarInfo* var_info = context_new_var_info(index, offset);
    context_insert_local_var_info(visitor->current_fun_info, index, var_info);

    return NULL;

#if 0
    struct ExprNode* initial_expr = ast_decl_initial_expr(decl);
    if (initial_expr) {
        struct BlockStmtNode* block = ast_new_block_stmt();
        visitor->current_block = block;
        visitor_visit_expr(super_type(visitor), initial_expr);

        strtable_id result_index = reuse_register(visitor);
        struct VarExprNode* result_reg = create_reg_var(result_index);

        struct VarExprNode* var = ast_new_var_expr(ast_decl_name(decl));
        struct UniopExprNode* addrof_var = ast_new_uniop_expr(
            UniopExprNodeTag_Addrof, ast_var_expr_super_type(var));
        struct UniopExprNode* deref_addrof_var = ast_new_uniop_expr(
            UniopExprNodeTag_Deref, ast_uniop_expr_super_type(addrof_var));

        struct ExprStmtNode* asm_stmt =
            create_assembly_stmt(ast_uniop_expr_super_type(deref_addrof_var),
                                 ast_var_expr_super_type(result_reg));
        ast_block_stmt_insert_at_end(visitor->current_block,
                                     ast_expr_stmt_super_type(asm_stmt));

        visitor->current_block = NULL;
        return ast_block_stmt_super_type(block);
    } else
        return NULL;
#endif
}

static struct StmtNode* visit_block_stmt(struct RegallocVisitor* visitor,
                                         struct BlockStmtNode* node) {
    struct BlockStmtNodeIterator it;
    ast_block_stmt_iterator(node, &it);
    for (;;) {
        struct StmtNode* stmt = ast_block_stmt_iterator_next(&it);
        if (!stmt) break;
        struct StmtNode* new_stmt =
            visitor_visit_stmt(super_type(visitor), stmt);
        if (new_stmt) ast_block_stmt_swap_at(&it, new_stmt);
    }

    return NULL;
}

static struct StmtNode* visit_if_stmt(struct RegallocVisitor* visitor,
                                      struct IfStmtNode* node) {
    struct BlockStmtNode* block = ast_new_block_stmt();

    visitor->current_block = block;
    visitor_visit_expr(super_type(visitor), ast_if_stmt_cond_expr(node));
    visitor->current_block = NULL;

    strtable_id cond_index = reuse_register(visitor);
    struct IdNode* cond_id = ast_new_id(cond_index);
    struct VarExprNode* cond_reg = ast_new_var_expr(cond_id);
    ast_if_stmt_set_cond_expr(node, ast_var_expr_super_type(cond_reg));
    ast_block_stmt_insert_at_end(block, ast_if_stmt_super_type(node));
    assert(visitor->free_register_index == 0);

    visit_block_stmt(visitor, ast_if_stmt_then_block(node));
    visit_block_stmt(visitor, ast_if_stmt_else_block(node));

    return ast_block_stmt_super_type(block);
}

static struct Fundef* visit_fundef(struct RegallocVisitor* visitor,
                                   struct FundefNode* node) {
    visitor->free_register_index = 0;
    visitor->current_block = NULL;
    visitor->require_location = false;

    struct FunInfo* fun_info = context_new_fun_info();
    context_insert_fun_info(visitor->context, node, fun_info);
    visitor->current_fun_info = fun_info;

    visit_block_stmt(visitor, ast_fundef_body(node));

    context_fun_info_roundup_stack_slot_size(fun_info, sizeof(void*));
    return NULL;
}

struct RegallocVisitor* new_regalloc_visitor(struct Context* context) {
    struct RegallocVisitor* visitor = malloc(sizeof(struct RegallocVisitor));
    visitor_initialize(super_type(visitor));

    register_visitor(visitor->super_type, visit_literal_expr,
                     visit_literal_expr);
    register_visitor(visitor->super_type, visit_uniop_expr, visit_uniop_expr);
    register_visitor(visitor->super_type, visit_binop_expr, visit_binop_expr);

    register_visitor(visitor->super_type, visit_block_stmt, visit_block_stmt);
    register_visitor(visitor->super_type, visit_expr_stmt, visit_expr_stmt);
    register_visitor(visitor->super_type, visit_if_stmt, visit_if_stmt);
    register_visitor(visitor->super_type, visit_decl_stmt, visit_decl_stmt);

    visitor->context = context;
    return visitor;
}

void regalloc_apply(struct RegallocVisitor* visitor, struct FundefNode* node) {
    visit_fundef(visitor, node);
}

struct RegallocVisitor2 {
    struct Visitor2 as_visitor;
    struct Context* context;

    /*
     * behave like stack pointer
     *   0 <= i < free_register_index : valid values are located
     *   free_register_index <= i     : free registers
     *   result must be located at free_register_index
     */
    size_t free_register_index;
};

static struct Visitor2* as_visitor(struct RegallocVisitor2* visitor) {
    return &visitor->as_visitor;
}

static strtable_id acquire_register(struct RegallocVisitor2* visitor) {
    strtable_id id = *((strtable_id*)vector_at(&visitor->context->register_ids,
                                               visitor->free_register_index));
    ++visitor->free_register_index;
    return id;
}

static void release_register(struct RegallocVisitor2* visitor) {
    --visitor->free_register_index;
}

static struct ExprIr* visit_const_expr2(struct RegallocVisitor2* visitor,
                                        struct ConstExprIr* ir) {
    strtable_id reg_id = acquire_register(visitor);
    ir_expr_set_reg_id(ir_const_expr_cast(ir), reg_id);
    return NULL;
}

static struct ExprIr* visit_binop_expr2(struct RegallocVisitor2* visitor,
                                        struct BinopExprIr* ir) {
    visitor2_visit_expr(as_visitor(visitor), ir_binop_expr_lhs(ir));
    visitor2_visit_expr(as_visitor(visitor), ir_binop_expr_rhs(ir));
    release_register(visitor);
    release_register(visitor);
    strtable_id reg_id = acquire_register(visitor);
    ir_expr_set_reg_id(ir_binop_expr_cast(ir), reg_id);
    return NULL;
}

static struct ExprIr* visit_addrof_expr2(struct RegallocVisitor2* visitor,
                                         struct AddrofExprIr* ir) {
    assert(ir_addrof_expr_tag(ir) == AddrTag_Var);
    strtable_id reg_id = acquire_register(visitor);
    ir_expr_set_reg_id(ir_addrof_expr_cast(ir), reg_id);
    return NULL;
}

static struct ExprIr* visit_load_expr2(struct RegallocVisitor2* visitor,
                                       struct LoadExprIr* ir) {
    visitor2_visit_expr(as_visitor(visitor), ir_load_expr_addr(ir));
    release_register(visitor);
    strtable_id reg_id = acquire_register(visitor);
    ir_expr_set_reg_id(ir_load_expr_cast(ir), reg_id);
    return NULL;
}

static struct ExprIr* visit_store_expr2(struct RegallocVisitor2* visitor,
                                        struct StoreExprIr* ir) {
    visitor2_visit_expr(as_visitor(visitor), ir_store_expr_addr(ir));
    visitor2_visit_expr(as_visitor(visitor), ir_store_expr_value(ir));
    release_register(visitor);
    release_register(visitor);
    strtable_id reg_id = acquire_register(visitor);
    ir_expr_set_reg_id(ir_store_expr_cast(ir), reg_id);
    return NULL;
}

static struct BlockIr* visit_block2(struct RegallocVisitor2* visitor,
                                    struct BlockIr* ir) {
    struct BlockIterator* it = ir_block_new_iterator(ir);
    for (;;) {
        struct Ir* stmt = ir_block_iterator_next(it);
        if (!stmt) break;

        visitor2_visit_ir(as_visitor(visitor), stmt);
        if (ir_as_expr(stmt)) release_register(visitor);
    }
    return NULL;
}

static struct FunctionIr* visit_function2(struct RegallocVisitor2* visitor,
                                          struct FunctionIr* ir) {
    struct BlockIr* body = ir_function_body(ir);
    visitor2_visit_block(as_visitor(visitor), body);
    return NULL;
}

static struct CfIr* visit_branch_cf2(struct RegallocVisitor2* visitor,
                                     struct BranchCfIr* ir) {
    visitor2_visit_expr(as_visitor(visitor), ir_branch_cf_cond_expr(ir));
    release_register(visitor);

    visitor2_visit_block(as_visitor(visitor), ir_branch_cf_true_block(ir));
    visitor2_visit_block(as_visitor(visitor), ir_branch_cf_false_block(ir));

    return NULL;
}

struct RegallocVisitor2* new_regalloc_visitor2(struct Context* context) {
    struct RegallocVisitor2* visitor = malloc(sizeof(struct RegallocVisitor2));
    visitor2_initialize(as_visitor(visitor));

    register_visitor(visitor->as_visitor, visit_const_expr, visit_const_expr2);
    register_visitor(visitor->as_visitor, visit_binop_expr, visit_binop_expr2);
    register_visitor(visitor->as_visitor, visit_addrof_expr,
                     visit_addrof_expr2);
    register_visitor(visitor->as_visitor, visit_load_expr, visit_load_expr2);
    register_visitor(visitor->as_visitor, visit_store_expr, visit_store_expr2);
    register_visitor(visitor->as_visitor, visit_block, visit_block2);
    register_visitor(visitor->as_visitor, visit_function, visit_function2);
    register_visitor(visitor->as_visitor, visit_branch_cf, visit_branch_cf2);

    visitor->context = context;
    visitor->free_register_index = 0;
    return visitor;
}

void regalloc2_apply(struct RegallocVisitor2* visitor, struct BlockIr* ir) {
    visitor2_visit_block(as_visitor(visitor), ir);
}

struct PostRegallocVisitor2 {
    struct Visitor2 as_visitor;
    struct Context* context;
    size_t parent_region_end;
    size_t max_region_end;
};

static struct Visitor2* as_visitor_post_process(
    struct PostRegallocVisitor2* visitor) {
    return &visitor->as_visitor;
}

static struct BlockIr* visit_block2_post_process(
    struct PostRegallocVisitor2* visitor, struct BlockIr* ir) {
    size_t region_base = visitor->parent_region_end;
    ir_block_commit_region_status(ir, region_base);
    size_t region_size = ir_block_region_size(ir);
    size_t new_region_end = region_base + region_size;

    if (new_region_end > visitor->max_region_end)
        visitor->max_region_end = new_region_end;

    struct BlockIterator* it = ir_block_new_iterator(ir);
    for (;;) {
        struct Ir* stmt = ir_block_iterator_next(it);
        if (!stmt) break;

        struct BlockIr* block = ir_as_block(stmt);
        if (block) {
            visitor->parent_region_end = new_region_end;
            visitor2_visit_block(as_visitor_post_process(visitor), block);
        }

        // FunctionIr-s are stored in BlockIr (translation unit)
        struct FunctionIr* function = ir_as_function(stmt);
        if (function) {
            visitor2_visit_function(as_visitor_post_process(visitor), function);
        }
    }
    return NULL;
}

static struct FunctionIr* visit_function2_post_process(
    struct PostRegallocVisitor2* visitor, struct FunctionIr* ir) {
    visitor->parent_region_end = 0;
    visitor->max_region_end = 0;

    struct BlockIr* body = ir_function_body(ir);
    visitor2_visit_block(as_visitor_post_process(visitor), body);

    ir_function_set_region_size(ir, visitor->max_region_end);
    return NULL;
}

struct PostRegallocVisitor2* new_post_regalloc_visitor2(
    struct Context* context) {
    struct PostRegallocVisitor2* visitor =
        malloc(sizeof(struct PostRegallocVisitor2));
    visitor2_initialize(as_visitor_post_process(visitor));

    register_visitor(visitor->as_visitor, visit_block,
                     visit_block2_post_process);
    register_visitor(visitor->as_visitor, visit_function,
                     visit_function2_post_process);

    visitor->context = context;
    return visitor;
}

void regalloc2_apply_post_process(struct PostRegallocVisitor2* visitor,
                                  struct BlockIr* ir) {
    visitor2_visit_block(as_visitor_post_process(visitor), ir);
}
