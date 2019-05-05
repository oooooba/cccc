#include "visitor.h"
//#include "ast.h"
#include "ir.h"

#include <assert.h>
#include <stdbool.h>

struct Ir* visitor2_visit_ir(struct Visitor2* visitor, struct Ir* ir) {
    switch (ir_tag(ir)) {
        case IrTag_Expr:
            return ir_expr_cast(visitor2_visit_expr(visitor, ir_as_expr(ir)));
        case IrTag_Block:
            return ir_block_cast(
                visitor2_visit_block(visitor, ir_as_block(ir)));
        case IrTag_Function:
            return ir_function_cast(
                visitor2_visit_function(visitor, ir_as_function(ir)));
        case IrTag_Cf:
            return ir_cf_cast(visitor2_visit_cf(visitor, ir_as_cf(ir)));
        default:
            assert(false);
    }
    return NULL;
}

struct ExprIr* visitor2_visit_expr(struct Visitor2* visitor,
                                   struct ExprIr* ir) {
    switch (ir_expr_tag(ir)) {
        case ExprIrTag_Const:
            return visitor->visit_const_expr(visitor, ir_expr_as_const(ir));
        case ExprIrTag_Binop:
            return visitor->visit_binop_expr(visitor, ir_expr_as_binop(ir));
        case ExprIrTag_Addrof:
            return visitor->visit_addrof_expr(visitor, ir_expr_as_addrof(ir));
        case ExprIrTag_Load:
            return visitor->visit_load_expr(visitor, ir_expr_as_load(ir));
        case ExprIrTag_Store:
            return visitor->visit_store_expr(visitor, ir_expr_as_store(ir));
        default:
            assert(false);
    }
    return NULL;
}

struct BlockIr* visitor2_visit_block(struct Visitor2* visitor,
                                     struct BlockIr* ir) {
    return visitor->visit_block(visitor, ir);
}

struct FunctionIr* visitor2_visit_function(struct Visitor2* visitor,
                                           struct FunctionIr* ir) {
    return visitor->visit_function(visitor, ir);
}

struct CfIr* visitor2_visit_cf(struct Visitor2* visitor, struct CfIr* ir) {
    switch (ir_cf_tag(ir)) {
        case CfIrTag_Branch:
            return visitor->visit_branch_cf(visitor, ir_cf_as_branch(ir));
        default:
            assert(false);
    }
    return NULL;
}

void visitor2_initialize(struct Visitor2* visitor) {
    register_visitor(*visitor, visit_const_expr, NULL);
    register_visitor(*visitor, visit_binop_expr, NULL);
    register_visitor(*visitor, visit_addrof_expr, NULL);
    register_visitor(*visitor, visit_load_expr, NULL);
    register_visitor(*visitor, visit_store_expr, NULL);
    register_visitor(*visitor, visit_block, NULL);
    register_visitor(*visitor, visit_function, NULL);
    register_visitor(*visitor, visit_branch_cf, NULL);
}
