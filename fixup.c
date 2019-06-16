#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <stdlib.h>

struct FixupVisitor {
    struct Visitor as_visitor;
    size_t parent_region_end;
    size_t max_region_end;
};

static struct Visitor* as_visitor(struct FixupVisitor* visitor) {
    return &visitor->as_visitor;
}

static struct Context* ctx(struct FixupVisitor* visitor) {
    return visitor_context(as_visitor(visitor));
}

static void insert(struct List* list, struct ListHeader* point, void* item) {
    struct ListItem* list_item = malloc(sizeof(struct ListItem));
    list_item->item = item;
    list_insert_at(list, point, list_from(list_item));
}

static struct BlockIr* visit_block(struct FixupVisitor* visitor,
                                   struct BlockIr* ir) {
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

        // ToDo: fix to handle blocks of IfCfIr
        struct BlockIr* block = ir_as_block(stmt);
        if (block) {
            visitor->parent_region_end = new_region_end;
            visitor_visit_block(as_visitor(visitor), block);
        }
    }
    return NULL;
}

static struct FunctionIr* visit_function(struct FixupVisitor* visitor,
                                         struct FunctionIr* ir) {
    visitor->parent_region_end = sizeof(void*);
    visitor->max_region_end = sizeof(void*);

    // ToDo: for refactoring
    struct BlockIr* old_body = ir_function_body(ir);
    visitor_visit_block(as_visitor(visitor), old_body);
    ir_function_set_body(ir, NULL);
    struct BlockStmtIr* body = ir_block_stmt_convert_for_refactoring(old_body);

    ir_function_set_region_size(ir, visitor->max_region_end);

    strtable_id sp_reg_id =
        context_stack_pointer_reg(ctx(visitor), RegisterSizeKind_64);
    strtable_id bp_reg_id =
        context_base_pointer_reg(ctx(visitor), RegisterSizeKind_64);

    struct List* stmts = ir_block_stmt_statements(body);
    struct ListHeader* insert_point = list_begin(stmts);

    // insert saving base pointer register code
    {
        struct PushCfIr* push = ir_new_push_cf(bp_reg_id);
        insert(stmts, insert_point, ir_new_cf_stmt(ir_push_cf_cast(push)));

        struct ConstExprIr* mov = ir_new_register_const_expr(sp_reg_id);
        ir_expr_set_reg_id(ir_const_expr_cast(mov), bp_reg_id);
        insert(stmts, insert_point, ir_new_expr_stmt(ir_const_expr_cast(mov)));
    }

    // insert allocating stack frame code
    {
        struct ConstExprIr* size =
            ir_new_integer_const_expr(ir_function_region_size(ir));
        ir_expr_set_reg_id(
            ir_const_expr_cast(size),
            context_func_call_result_reg(ctx(visitor), RegisterSizeKind_64));
        struct ConstExprIr* sp = ir_new_register_const_expr(sp_reg_id);
        ir_expr_set_reg_id(ir_const_expr_cast(sp), sp_reg_id);
        struct BinopExprIr* sub =
            ir_new_binop_expr(BinopExprIrTag_Sub, ir_const_expr_cast(sp),
                              ir_const_expr_cast(size));
        ir_expr_set_reg_id(ir_binop_expr_cast(sub), sp_reg_id);
        insert(stmts, insert_point, ir_new_expr_stmt(ir_binop_expr_cast(sub)));
    }

    // insert saving parameter register code
    size_t i = 0;
    for (struct ListHeader *it = list_begin(ir_function_params(ir)),
                           *eit = list_end(ir_function_params(ir));
         it != eit; it = list_next(it)) {
        struct VarExprIr* dst_var = ((struct ListItem*)it)->item;
        struct TypeIr* dst_var_type = ir_var_expr_type(dst_var);
        struct TypeIr* dst_ptr_type = ir_expr_type(ir_var_expr_cast(dst_var));

        strtable_id tmp_reg_id = context_func_call_result_reg(
            ctx(visitor), context_type_to_register_size_kind(dst_ptr_type));
        ir_expr_set_reg_id(ir_var_expr_cast(dst_var), tmp_reg_id);

        strtable_id param_reg_id = context_nth_func_call_arg_reg(
            ctx(visitor), i, context_type_to_register_size_kind(dst_var_type));
        struct ConstExprIr* src_reg = ir_new_register_const_expr(param_reg_id);
        ir_expr_set_reg_id(ir_const_expr_cast(src_reg), param_reg_id);

        struct SubstExprIr* subst = ir_new_subst_expr(
            ir_var_expr_cast(dst_var), ir_const_expr_cast(src_reg));
        ir_expr_set_reg_id(ir_subst_expr_cast(subst), param_reg_id);
        ir_expr_set_type(ir_subst_expr_cast(subst),
                         dst_var_type);  // for codegen

        insert(stmts, insert_point,
               ir_new_expr_stmt(ir_subst_expr_cast(subst)));
        ++i;
    }

    // insert saving general purpose registers code
    {
        struct PushCfIr* push = ir_new_push_cf(
            context_nth_reg(ctx(visitor), 1, RegisterSizeKind_64));
        insert(stmts, insert_point, ir_new_cf_stmt(ir_push_cf_cast(push)));
    }

    // move to end of statements
    insert_point = list_end(stmts);

    // insert label for early returns
    {
        // this value means epilogue of current function, ToDo: fix
        struct LabelCfIr* label = ir_new_label_cf(STRTABLE_INVALID_ID);
        insert(stmts, insert_point, ir_new_cf_stmt(ir_label_cf_cast(label)));
    }

    // insert restoring general purpose registers code
    {
        struct PopCfIr* pop = ir_new_pop_cf(
            context_nth_reg(ctx(visitor), 1, RegisterSizeKind_64));
        insert(stmts, insert_point, ir_new_cf_stmt(ir_pop_cf_cast(pop)));
    }

    // insert restoring base pointer register code
    {
        struct ConstExprIr* mov = ir_new_register_const_expr(bp_reg_id);
        ir_expr_set_reg_id(ir_const_expr_cast(mov), sp_reg_id);
        insert(stmts, insert_point, ir_new_expr_stmt(ir_const_expr_cast(mov)));

        struct PopCfIr* pop = ir_new_pop_cf(bp_reg_id);
        insert(stmts, insert_point, ir_new_cf_stmt(ir_pop_cf_cast(pop)));
    }

    ir_function_set_body2(ir, body);
    return ir;
}

struct FixupVisitor* new_fixup_visitor(struct Context* context) {
    struct FixupVisitor* visitor = malloc(sizeof(struct FixupVisitor));
    visitor_initialize(as_visitor(visitor), context);

    register_visitor(visitor->as_visitor, visit_block, visit_block);
    register_visitor(visitor->as_visitor, visit_function, visit_function);

    return visitor;
}
