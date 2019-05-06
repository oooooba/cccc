#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <assert.h>
#include <stdlib.h>

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
    strtable_id id =
        context_nth_reg(visitor->context, visitor->free_register_index);
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

struct RegallocVisitor2* new_regalloc_visitor(struct Context* context) {
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

void regalloc_apply(struct RegallocVisitor2* visitor, struct BlockIr* ir) {
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

    struct BlockIterator* insert_point = ir_block_new_iterator(body);
    // initially, insert_point points to List::end (= List itself)
    ir_block_iterator_next(insert_point);
    size_t i = 0;
    for (struct ListHeader *it = list_begin(ir_function_params(ir)),
                           *eit = list_end(ir_function_params(ir));
         it != eit; it = list_next(it)) {
        struct VarIr* dst_var = ((struct ListItem*)it)->item;
        struct AddrofExprIr* addrof_var = ir_new_addrof_expr_with_var(dst_var);
        strtable_id tmp_reg_id = context_func_call_result_reg(visitor->context);
        ir_expr_set_reg_id(ir_addrof_expr_cast(addrof_var), tmp_reg_id);

        struct ConstExprIr* src_reg = ir_new_register_const_expr();
        strtable_id param_reg_id =
            context_nth_func_call_arg_reg(visitor->context, i);
        ir_expr_set_reg_id(ir_const_expr_cast(src_reg), param_reg_id);

        struct StoreExprIr* subst = ir_new_store_expr(
            ir_addrof_expr_cast(addrof_var), ir_const_expr_cast(src_reg));
        ir_block_insert_expr_at(insert_point, ir_store_expr_cast(subst));

        ++i;
    }

    return NULL;
}

struct PostRegallocVisitor2* new_post_regalloc_visitor(
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

void regalloc_apply_post_process(struct PostRegallocVisitor2* visitor,
                                 struct BlockIr* ir) {
    visitor2_visit_block(as_visitor_post_process(visitor), ir);
}
