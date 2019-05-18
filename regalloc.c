#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <assert.h>
#include <stdlib.h>

struct RegallocVisitor {
    struct Visitor as_visitor;
    struct Context* context;

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

static strtable_id acquire_register(struct RegallocVisitor* visitor) {
    strtable_id id =
        context_nth_reg(visitor->context, visitor->free_register_index);
    ++visitor->free_register_index;
    return id;
}

static void release_register(struct RegallocVisitor* visitor) {
    --visitor->free_register_index;
}

static struct ExprIr* visit_const_expr2(struct RegallocVisitor* visitor,
                                        struct ConstExprIr* ir) {
    strtable_id reg_id = acquire_register(visitor);
    ir_expr_set_reg_id(ir_const_expr_cast(ir), reg_id);
    return NULL;
}

static struct ExprIr* visit_binop_expr2(struct RegallocVisitor* visitor,
                                        struct BinopExprIr* ir) {
    visitor2_visit_expr(as_visitor(visitor), ir_binop_expr_lhs(ir));
    visitor2_visit_expr(as_visitor(visitor), ir_binop_expr_rhs(ir));
    release_register(visitor);
    release_register(visitor);
    strtable_id reg_id = acquire_register(visitor);
    ir_expr_set_reg_id(ir_binop_expr_cast(ir), reg_id);
    return NULL;
}

static struct ExprIr* visit_call_expr2(struct RegallocVisitor* visitor,
                                       struct CallExprIr* ir) {
    struct VarExprIr* func_name = ir_expr_as_var(ir_call_expr_function(ir));
    assert(func_name);

    for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                           *eit = list_end(ir_call_expr_args(ir));
         it != eit; it = list_next(it)) {
        struct ExprIr* arg = ((struct ListItem*)it)->item;
        visitor2_visit_expr(as_visitor(visitor), arg);
    }

    for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                           *eit = list_end(ir_call_expr_args(ir));
         it != eit; it = list_next(it)) {
        release_register(visitor);
    }
    strtable_id reg_id = acquire_register(visitor);
    ir_expr_set_reg_id(ir_call_expr_cast(ir), reg_id);

    struct BlockIr* pre_block = ir_call_expr_pre_expr_block(ir);
    struct BlockIr* post_block = ir_call_expr_post_expr_block(ir);
    strtable_id result_reg_id = context_func_call_result_reg(visitor->context);

    // copy argument registers to parameter ones
    size_t i = 0;
    for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                           *eit = list_end(ir_call_expr_args(ir));
         it != eit; it = list_next(it)) {
        struct ExprIr* arg = ((struct ListItem*)it)->item;

        strtable_id arg_reg_id = ir_expr_reg_id(arg);
        strtable_id param_reg_id =
            context_nth_func_call_arg_reg(visitor->context, i);

        struct ConstExprIr* copy_instr = ir_new_register_const_expr(arg_reg_id);
        ir_expr_set_reg_id(ir_const_expr_cast(copy_instr), param_reg_id);
        ir_block_insert_expr_at_end(pre_block, ir_const_expr_cast(copy_instr));
        ++i;
    }

    if (reg_id != result_reg_id) {
        // save result register
        struct PushCfIr* push_instr = ir_new_push_cf(result_reg_id);
        ir_block_insert_at_end(pre_block,
                               ir_cf_cast(ir_push_cf_cast(push_instr)));

        // copy result register to expected one
        struct ConstExprIr* copy_instr =
            ir_new_register_const_expr(result_reg_id);
        ir_expr_set_reg_id(ir_const_expr_cast(copy_instr), reg_id);
        ir_block_insert_expr_at_end(post_block, ir_const_expr_cast(copy_instr));

        // restore result register
        struct PopCfIr* pop_instr = ir_new_pop_cf(result_reg_id);
        ir_block_insert_at_end(post_block,
                               ir_cf_cast(ir_pop_cf_cast(pop_instr)));
    }

    return NULL;
}

static struct ExprIr* visit_var_expr2(struct RegallocVisitor* visitor,
                                      struct VarExprIr* ir) {
    strtable_id reg_id = acquire_register(visitor);
    ir_expr_set_reg_id(ir_var_expr_cast(ir), reg_id);
    return NULL;
}

static struct ExprIr* visit_unop_expr2(struct RegallocVisitor* visitor,
                                       struct UnopExprIr* ir) {
    if (ir_unop_expr_op(ir) != UnopExprIrTag_Addrof) {
        visitor2_visit_expr(as_visitor(visitor), ir_unop_expr_operand(ir));
        release_register(visitor);
    }
    strtable_id reg_id = acquire_register(visitor);
    ir_expr_set_reg_id(ir_unop_expr_cast(ir), reg_id);
    return NULL;
}

static struct ExprIr* visit_subst_expr2(struct RegallocVisitor* visitor,
                                        struct SubstExprIr* ir) {
    visitor2_visit_expr(as_visitor(visitor), ir_subst_expr_value(ir));
    visitor2_visit_expr(as_visitor(visitor), ir_subst_expr_addr(ir));
    release_register(visitor);
    release_register(visitor);
    strtable_id reg_id = acquire_register(visitor);
    ir_expr_set_reg_id(ir_subst_expr_cast(ir), reg_id);
    return NULL;
}

static struct BlockIr* visit_block2(struct RegallocVisitor* visitor,
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

static struct FunctionIr* visit_function2(struct RegallocVisitor* visitor,
                                          struct FunctionIr* ir) {
    struct BlockIr* body = ir_function_body(ir);
    visitor2_visit_block(as_visitor(visitor), body);
    return NULL;
}

static struct CfIr* visit_branch_cf2(struct RegallocVisitor* visitor,
                                     struct BranchCfIr* ir) {
    visitor2_visit_expr(as_visitor(visitor), ir_branch_cf_cond_expr(ir));
    release_register(visitor);

    visitor2_visit_block(as_visitor(visitor), ir_branch_cf_true_block(ir));
    visitor2_visit_block(as_visitor(visitor), ir_branch_cf_false_block(ir));

    return NULL;
}

static struct CfIr* visit_return_cf2(struct RegallocVisitor* visitor,
                                     struct ReturnCfIr* ir) {
    struct ExprIr* expr = ir_return_cf_expr(ir);
    if (expr) {
        visitor2_visit_expr(as_visitor(visitor), expr);
        release_register(visitor);
    }
    return NULL;
}

struct RegallocVisitor* new_regalloc_visitor(struct Context* context) {
    struct RegallocVisitor* visitor = malloc(sizeof(struct RegallocVisitor));
    visitor2_initialize(as_visitor(visitor));

    register_visitor(visitor->as_visitor, visit_const_expr, visit_const_expr2);
    register_visitor(visitor->as_visitor, visit_binop_expr, visit_binop_expr2);
    register_visitor(visitor->as_visitor, visit_call_expr, visit_call_expr2);
    register_visitor(visitor->as_visitor, visit_var_expr, visit_var_expr2);
    register_visitor(visitor->as_visitor, visit_unop_expr, visit_unop_expr2);
    register_visitor(visitor->as_visitor, visit_subst_expr, visit_subst_expr2);
    register_visitor(visitor->as_visitor, visit_block, visit_block2);
    register_visitor(visitor->as_visitor, visit_function, visit_function2);
    register_visitor(visitor->as_visitor, visit_branch_cf, visit_branch_cf2);
    register_visitor(visitor->as_visitor, visit_return_cf, visit_return_cf2);

    visitor->context = context;
    visitor->free_register_index = 0;
    return visitor;
}

void regalloc_apply(struct RegallocVisitor* visitor, struct BlockIr* ir) {
    visitor2_visit_block(as_visitor(visitor), ir);
}

struct PostRegallocVisitor {
    struct Visitor as_visitor;
    struct Context* context;
    size_t parent_region_end;
    size_t max_region_end;
};

static struct Visitor* as_visitor_post_process(
    struct PostRegallocVisitor* visitor) {
    return &visitor->as_visitor;
}

static struct BlockIr* visit_block2_post_process(
    struct PostRegallocVisitor* visitor, struct BlockIr* ir) {
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
    struct PostRegallocVisitor* visitor, struct FunctionIr* ir) {
    visitor->parent_region_end = sizeof(void*);
    visitor->max_region_end = sizeof(void*);

    struct BlockIr* body = ir_function_body(ir);
    visitor2_visit_block(as_visitor_post_process(visitor), body);

    ir_function_set_region_size(ir, visitor->max_region_end);

    struct BlockIterator* insert_point = ir_block_new_iterator(body);
    // initially, insert_point points to List::end (= List itself)
    ir_block_iterator_next(insert_point);

    strtable_id sp_reg_id = context_stack_pointer_reg(visitor->context);
    strtable_id bp_reg_id = context_base_pointer_reg(visitor->context);

    // insert saving base pointer register code
    {
        struct PushCfIr* push = ir_new_push_cf(bp_reg_id);
        ir_block_insert_at(insert_point, ir_cf_cast(ir_push_cf_cast(push)));

        struct ConstExprIr* mov = ir_new_register_const_expr(sp_reg_id);
        ir_expr_set_reg_id(ir_const_expr_cast(mov), bp_reg_id);
        ir_block_insert_expr_at(insert_point, ir_const_expr_cast(mov));
    }

    // insert allocating stack frame code
    {
        struct ConstExprIr* size =
            ir_new_integer_const_expr(ir_function_region_size(ir));
        ir_expr_set_reg_id(ir_const_expr_cast(size),
                           context_func_call_result_reg(visitor->context));
        struct ConstExprIr* sp = ir_new_register_const_expr(sp_reg_id);
        ir_expr_set_reg_id(ir_const_expr_cast(sp), sp_reg_id);
        struct BinopExprIr* sub =
            ir_new_binop_expr(BinopExprIrTag_Sub, ir_const_expr_cast(sp),
                              ir_const_expr_cast(size));
        ir_expr_set_reg_id(ir_binop_expr_cast(sub),
                           context_stack_pointer_reg(visitor->context));
        ir_block_insert_expr_at(insert_point, ir_binop_expr_cast(sub));
    }

    // insert saving parameter register code
    size_t i = 0;
    for (struct ListHeader *it = list_begin(ir_function_params(ir)),
                           *eit = list_end(ir_function_params(ir));
         it != eit; it = list_next(it)) {
        struct VarExprIr* dst_var = ((struct ListItem*)it)->item;
        struct UnopExprIr* addrof_var =
            ir_new_unop_expr(UnopExprIrTag_Addrof, ir_var_expr_cast(dst_var));
        strtable_id tmp_reg_id = context_func_call_result_reg(visitor->context);
        ir_expr_set_reg_id(ir_unop_expr_cast(addrof_var), tmp_reg_id);

        strtable_id param_reg_id =
            context_nth_func_call_arg_reg(visitor->context, i);
        struct ConstExprIr* src_reg = ir_new_register_const_expr(param_reg_id);
        ir_expr_set_reg_id(ir_const_expr_cast(src_reg), param_reg_id);

        struct SubstExprIr* subst = ir_new_subst_expr(
            ir_unop_expr_cast(addrof_var), ir_const_expr_cast(src_reg));
        ir_expr_set_reg_id(ir_subst_expr_cast(subst), param_reg_id);
        ir_block_insert_expr_at(insert_point, ir_subst_expr_cast(subst));

        ++i;
    }

    // insert saving general registers code
    {
        struct PushCfIr* push =
            ir_new_push_cf(context_nth_reg(visitor->context, 1));
        ir_block_insert_at(insert_point, ir_cf_cast(ir_push_cf_cast(push)));
    }

    // insert label for early returns
    {
        // this value means epilogue of current function, ToDo: fix
        struct LabelCfIr* label = ir_new_label_cf(STRTABLE_INVALID_ID);
        ir_block_insert_at_end(body, ir_cf_cast(ir_label_cf_cast(label)));
    }

    // insert restoring general registers code
    {
        struct PopCfIr* pop =
            ir_new_pop_cf(context_nth_reg(visitor->context, 1));
        ir_block_insert_at_end(body, ir_cf_cast(ir_pop_cf_cast(pop)));
    }

    // insert restoring base pointer register code
    {
        struct ConstExprIr* mov = ir_new_register_const_expr(bp_reg_id);
        ir_expr_set_reg_id(ir_const_expr_cast(mov), sp_reg_id);
        ir_block_insert_expr_at_end(body, ir_const_expr_cast(mov));

        struct PopCfIr* pop = ir_new_pop_cf(bp_reg_id);
        ir_block_insert_at_end(body, ir_cf_cast(ir_pop_cf_cast(pop)));
    }

    return NULL;
}

struct PostRegallocVisitor* new_post_regalloc_visitor(
    struct Context* context) {
    struct PostRegallocVisitor* visitor =
        malloc(sizeof(struct PostRegallocVisitor));
    visitor2_initialize(as_visitor_post_process(visitor));

    register_visitor(visitor->as_visitor, visit_block,
                     visit_block2_post_process);
    register_visitor(visitor->as_visitor, visit_function,
                     visit_function2_post_process);

    visitor->context = context;
    return visitor;
}

void regalloc_apply_post_process(struct PostRegallocVisitor* visitor,
                                 struct BlockIr* ir) {
    visitor2_visit_block(as_visitor_post_process(visitor), ir);
}
