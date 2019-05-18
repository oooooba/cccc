#include "visitor.h"
#include "ir.h"

#include <assert.h>
#include <stdbool.h>

struct Ir* visitor_visit_ir(struct Visitor* visitor, struct Ir* ir) {
    switch (ir_tag(ir)) {
        case IrTag_Expr:
            return ir_expr_cast(visitor_visit_expr(visitor, ir_as_expr(ir)));
        case IrTag_Block:
            return ir_block_cast(
                visitor_visit_block(visitor, ir_as_block(ir)));
        case IrTag_Function:
            return ir_function_cast(
                visitor_visit_function(visitor, ir_as_function(ir)));
        case IrTag_Cf:
            return ir_cf_cast(visitor_visit_cf(visitor, ir_as_cf(ir)));
        default:
            assert(false);
    }
    return NULL;
}

struct ExprIr* visitor_visit_expr(struct Visitor* visitor,
                                   struct ExprIr* ir) {
    switch (ir_expr_tag(ir)) {
        case ExprIrTag_Const:
            return visitor->visit_const_expr(visitor, ir_expr_as_const(ir));
        case ExprIrTag_Binop:
            return visitor->visit_binop_expr(visitor, ir_expr_as_binop(ir));
        case ExprIrTag_Call:
            return visitor->visit_call_expr(visitor, ir_expr_as_call(ir));
        case ExprIrTag_Var:
            return visitor->visit_var_expr(visitor, ir_expr_as_var(ir));
        case ExprIrTag_Unop:
            return visitor->visit_unop_expr(visitor, ir_expr_as_unop(ir));
        case ExprIrTag_Subst:
            return visitor->visit_subst_expr(visitor, ir_expr_as_subst(ir));
        default:
            assert(false);
    }
    return NULL;
}

struct BlockIr* visitor_visit_block(struct Visitor* visitor,
                                     struct BlockIr* ir) {
    return visitor->visit_block(visitor, ir);
}

struct FunctionIr* visitor_visit_function(struct Visitor* visitor,
                                           struct FunctionIr* ir) {
    return visitor->visit_function(visitor, ir);
}

struct CfIr* visitor_visit_cf(struct Visitor* visitor, struct CfIr* ir) {
    switch (ir_cf_tag(ir)) {
        case CfIrTag_Branch:
            return visitor->visit_branch_cf(visitor, ir_cf_as_branch(ir));
        case CfIrTag_Return:
            return visitor->visit_return_cf(visitor, ir_cf_as_return(ir));
        case CfIrTag_Label:
            return visitor->visit_label_cf(visitor, ir_cf_as_label(ir));
        case CfIrTag_Push:
            return visitor->visit_push_cf(visitor, ir_cf_as_push(ir));
        case CfIrTag_Pop:
            return visitor->visit_pop_cf(visitor, ir_cf_as_pop(ir));
        default:
            assert(false);
    }
    return NULL;
}

void visitor_initialize(struct Visitor* visitor) {
    register_visitor(*visitor, visit_const_expr, NULL);
    register_visitor(*visitor, visit_binop_expr, NULL);
    register_visitor(*visitor, visit_call_expr, NULL);
    register_visitor(*visitor, visit_var_expr, NULL);
    register_visitor(*visitor, visit_unop_expr, NULL);
    register_visitor(*visitor, visit_subst_expr, NULL);
    register_visitor(*visitor, visit_block, NULL);
    register_visitor(*visitor, visit_function, NULL);
    register_visitor(*visitor, visit_branch_cf, NULL);
    register_visitor(*visitor, visit_return_cf, NULL);
    register_visitor(*visitor, visit_label_cf, NULL);
    register_visitor(*visitor, visit_push_cf, NULL);
    register_visitor(*visitor, visit_pop_cf, NULL);
}
