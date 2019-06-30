#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <assert.h>
#include <stdlib.h>

struct RegallocVisitor {
    struct Visitor as_visitor;

    /*
     * behave like stack pointer
     *   0 <= i < free_register_index : valid values are located
     *   free_register_index <= i     : free registers
     *   result must be located at free_register_index
     */
    size_t free_register_index;
};

static struct Visitor* as_visitor(struct RegallocVisitor* visitor) {
    return &visitor->as_visitor;
}

static struct Context* ctx(struct RegallocVisitor* visitor) {
    return visitor_context(as_visitor(visitor));
}

static strtable_id acquire_register(struct RegallocVisitor* visitor,
                                    struct TypeIr* type) {
    enum RegisterSizeKind kind = context_type_to_register_size_kind(type);
    strtable_id id =
        context_nth_reg(ctx(visitor), visitor->free_register_index, kind);
    ++visitor->free_register_index;
    return id;
}

static void release_register(struct RegallocVisitor* visitor) {
    --visitor->free_register_index;
}

static strtable_id get_nth_func_call_arg_register(struct Context* ctx, size_t n,
                                                  struct TypeIr* type) {
    enum RegisterSizeKind kind = context_type_to_register_size_kind(type);
    return context_nth_func_call_arg_reg(ctx, n, kind);
}

static strtable_id get_func_call_result_register(struct Context* ctx,
                                                 struct TypeIr* type) {
    enum RegisterSizeKind kind = context_type_to_register_size_kind(type);
    return context_func_call_result_reg(ctx, kind);
}

static struct ExprIr* visit_const_expr(struct RegallocVisitor* visitor,
                                       struct ConstExprIr* ir) {
    strtable_id reg_id =
        acquire_register(visitor, ir_expr_type(ir_const_expr_cast(ir)));
    ir_expr_set_reg_id(ir_const_expr_cast(ir), reg_id);
    return ir_const_expr_cast(ir);
}

static struct ExprIr* visit_binop_expr(struct RegallocVisitor* visitor,
                                       struct BinopExprIr* ir) {
    visitor_visit_binop_expr(as_visitor(visitor), ir);
    release_register(visitor);
    release_register(visitor);
    strtable_id reg_id =
        acquire_register(visitor, ir_expr_type(ir_binop_expr_cast(ir)));
    ir_expr_set_reg_id(ir_binop_expr_cast(ir), reg_id);
    return ir_binop_expr_cast(ir);
}

static struct ExprIr* visit_call_expr(struct RegallocVisitor* visitor,
                                      struct CallExprIr* ir) {
    struct VarExprIr* func_name = ir_expr_as_var(ir_call_expr_function(ir));
    assert(func_name);

    for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                           *eit = list_end(ir_call_expr_args(ir));
         it != eit; it = list_next(it)) {
        struct ExprIr* arg = ((struct ListItem*)it)->item;
        visitor_visit_expr(as_visitor(visitor), arg);
    }

    for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                           *eit = list_end(ir_call_expr_args(ir));
         it != eit; it = list_next(it)) {
        release_register(visitor);
    }

    struct TypeIr* result_type = ir_expr_type(ir_call_expr_cast(ir));
    strtable_id reg_id = acquire_register(visitor, result_type);
    ir_expr_set_reg_id(ir_call_expr_cast(ir), reg_id);

    struct BlockStmtIr* pre_block = ir_call_expr_pre_expr_block(ir);
    struct BlockStmtIr* post_block = ir_call_expr_post_expr_block(ir);

    // copy argument registers to parameter ones
    size_t i = 0;
    for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                           *eit = list_end(ir_call_expr_args(ir));
         it != eit; it = list_next(it)) {
        struct ExprIr* arg = ((struct ListItem*)it)->item;

        strtable_id arg_reg_id = ir_expr_reg_id(arg);
        strtable_id param_reg_id =
            get_nth_func_call_arg_register(ctx(visitor), i, ir_expr_type(arg));

        struct ConstExprIr* copy_instr = ir_new_register_const_expr(arg_reg_id);
        ir_expr_set_reg_id(ir_const_expr_cast(copy_instr), param_reg_id);
        struct ExprStmtIr* stmt =
            ir_new_expr_stmt(ir_const_expr_cast(copy_instr));
        ir_block_stmt_insert_at_end(pre_block, ir_expr_stmt_super(stmt));
        ++i;
    }

    if (reg_id !=
            context_func_call_result_reg(ctx(visitor), RegisterSizeKind_32) &&
        reg_id !=
            context_func_call_result_reg(ctx(visitor), RegisterSizeKind_64)) {
        strtable_id result_reg_id =
            context_func_call_result_reg(ctx(visitor), RegisterSizeKind_64);

        // save result register
        struct PushStmtIr* push_instr = ir_new_push_stmt(result_reg_id);
        ir_block_stmt_insert_at_end(pre_block, ir_push_stmt_super(push_instr));

        // copy result register to expected one
        struct ConstExprIr* copy_instr = ir_new_register_const_expr(
            get_func_call_result_register(ctx(visitor), result_type));
        ir_expr_set_reg_id(ir_const_expr_cast(copy_instr), reg_id);
        struct ExprStmtIr* stmt =
            ir_new_expr_stmt(ir_const_expr_cast(copy_instr));
        ir_block_stmt_insert_at_end(post_block, ir_expr_stmt_super(stmt));

        // restore result register
        struct PopStmtIr* pop_instr = ir_new_pop_stmt(result_reg_id);
        ir_block_stmt_insert_at_end(post_block, ir_pop_stmt_super(pop_instr));
    }

    return ir_call_expr_cast(ir);
}

static struct ExprIr* visit_var_expr(struct RegallocVisitor* visitor,
                                     struct VarExprIr* ir) {
    strtable_id reg_id =
        acquire_register(visitor, ir_expr_type(ir_var_expr_cast(ir)));
    ir_expr_set_reg_id(ir_var_expr_cast(ir), reg_id);
    return ir_var_expr_cast(ir);
}

static struct ExprIr* visit_subst_expr(struct RegallocVisitor* visitor,
                                       struct SubstExprIr* ir) {
    visitor_visit_subst_expr(as_visitor(visitor), ir);
    release_register(visitor);
    release_register(visitor);
    strtable_id reg_id =
        acquire_register(visitor, ir_expr_type(ir_subst_expr_cast(ir)));
    ir_expr_set_reg_id(ir_subst_expr_cast(ir), reg_id);
    return ir_subst_expr_cast(ir);
}

static struct ExprIr* visit_member_expr(struct RegallocVisitor* visitor,
                                        struct MemberExprIr* ir) {
    visitor_visit_member_expr(as_visitor(visitor), ir);
    release_register(visitor);
    strtable_id reg_id =
        acquire_register(visitor, ir_expr_type(ir_member_expr_cast(ir)));
    ir_expr_set_reg_id(ir_member_expr_cast(ir), reg_id);
    return ir_member_expr_cast(ir);
}

static struct ExprIr* visit_deref_expr(struct RegallocVisitor* visitor,
                                       struct DerefExprIr* ir) {
    visitor_visit_deref_expr(as_visitor(visitor), ir);
    release_register(visitor);
    strtable_id reg_id =
        acquire_register(visitor, ir_expr_type(ir_deref_expr_cast(ir)));
    ir_expr_set_reg_id(ir_deref_expr_cast(ir), reg_id);
    return ir_deref_expr_cast(ir);
}

static struct ExprIr* visit_cast_expr(struct RegallocVisitor* visitor,
                                      struct CastExprIr* ir) {
    visitor_visit_cast_expr(as_visitor(visitor), ir);
    release_register(visitor);
    strtable_id reg_id =
        acquire_register(visitor, ir_expr_type(ir_cast_expr_cast(ir)));
    ir_expr_set_reg_id(ir_cast_expr_cast(ir), reg_id);
    return ir_cast_expr_cast(ir);
}

static struct StmtIr* visit_expr_stmt(struct RegallocVisitor* visitor,
                                      struct ExprStmtIr* ir) {
    visitor_visit_expr(as_visitor(visitor), ir_expr_stmt_expr(ir));
    release_register(visitor);
    return ir_expr_stmt_super(ir);
}

static struct StmtIr* visit_if_stmt(struct RegallocVisitor* visitor,
                                    struct IfStmtIr* ir) {
    visitor_visit_expr(as_visitor(visitor), ir_if_stmt_cond_expr(ir));
    release_register(visitor);

    visitor_visit_stmt(as_visitor(visitor), ir_if_stmt_true_stmt(ir));
    visitor_visit_stmt(as_visitor(visitor), ir_if_stmt_false_stmt(ir));

    return ir_if_stmt_super(ir);
}

static struct StmtIr* visit_return_stmt(struct RegallocVisitor* visitor,
                                        struct ReturnStmtIr* ir) {
    struct ExprIr* expr = ir_return_stmt_expr(ir);
    if (expr) {
        visitor_visit_expr(as_visitor(visitor), expr);
        release_register(visitor);
    }
    return ir_return_stmt_super(ir);
}

static struct FunctionIr* visit_function(struct RegallocVisitor* visitor,
                                         struct FunctionIr* ir) {
    struct BlockStmtIr* body = ir_function_body2(ir);
    visitor_visit_stmt(as_visitor(visitor), ir_block_stmt_super(body));
    return ir;
}

struct RegallocVisitor* new_regalloc_visitor(struct Context* context) {
    struct RegallocVisitor* visitor = malloc(sizeof(struct RegallocVisitor));
    visitor_initialize(as_visitor(visitor), context);

    register_visitor(visitor->as_visitor, visit_const_expr, visit_const_expr);
    register_visitor(visitor->as_visitor, visit_binop_expr, visit_binop_expr);
    register_visitor(visitor->as_visitor, visit_call_expr, visit_call_expr);
    register_visitor(visitor->as_visitor, visit_var_expr, visit_var_expr);
    register_visitor(visitor->as_visitor, visit_subst_expr, visit_subst_expr);
    register_visitor(visitor->as_visitor, visit_member_expr, visit_member_expr);
    register_visitor(visitor->as_visitor, visit_deref_expr, visit_deref_expr);
    register_visitor(visitor->as_visitor, visit_addrof_expr, NULL);
    register_visitor(visitor->as_visitor, visit_cast_expr, visit_cast_expr);

    register_visitor(visitor->as_visitor, visit_expr_stmt, visit_expr_stmt);
    register_visitor(visitor->as_visitor, visit_if_stmt, visit_if_stmt);
    register_visitor(visitor->as_visitor, visit_return_stmt, visit_return_stmt);

    register_visitor(visitor->as_visitor, visit_function, visit_function);

    visitor->free_register_index = 0;
    return visitor;
}
