#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

struct SimplifyVisitor {
    struct Visitor as_visitor;
};

static struct Visitor* as_visitor(struct SimplifyVisitor* visitor) {
    return &visitor->as_visitor;
}

static struct ExprIr* visit_binop_expr(struct SimplifyVisitor* visitor,
                                       struct BinopExprIr* ir) {
    visitor_visit_binop_expr(as_visitor(visitor), ir);

    struct ConstExprIr* lhs_expr = ir_expr_as_const(ir_binop_expr_lhs(ir));
    if (!(lhs_expr && ir_const_expr_tag(lhs_expr) == ConstExprIrTag_Integer))
        return ir_binop_expr_cast(ir);
    struct ConstExprIr* rhs_expr = ir_expr_as_const(ir_binop_expr_rhs(ir));
    if (!(rhs_expr && ir_const_expr_tag(rhs_expr) == ConstExprIrTag_Integer))
        return ir_binop_expr_cast(ir);

    intptr_t lhs_const = ir_const_expr_integer_value(lhs_expr);
    intptr_t rhs_const = ir_const_expr_integer_value(rhs_expr);
    intptr_t value;
    switch (ir_binop_expr_op(ir)) {
        case BinopExprIrTag_Add:
            value = lhs_const + rhs_const;
            break;
        case BinopExprIrTag_Sub:
            value = lhs_const - rhs_const;
            break;
        case BinopExprIrTag_Mul:
            value = lhs_const * rhs_const;
            break;
        case BinopExprIrTag_Equal:
            value = lhs_const == rhs_const;
            break;
        case BinopExprIrTag_Lt:
            value = lhs_const < rhs_const;
            break;
        case BinopExprIrTag_Le:
            value = lhs_const <= rhs_const;
            break;
        case BinopExprIrTag_Gt:
            value = lhs_const > rhs_const;
            break;
        case BinopExprIrTag_Ge:
            value = lhs_const >= rhs_const;
            break;
        case BinopExprIrTag_LogicalAnd:
            value = lhs_const && rhs_const;
            break;
        case BinopExprIrTag_LogicalOr:
            value = lhs_const || rhs_const;
            break;
        default:
            assert(false);
    }

    struct ExprIr* new_ir =
        ir_const_expr_cast(ir_new_integer_const_expr(value));
    ir_expr_set_type(new_ir, ir_expr_type(ir_binop_expr_cast(ir)));
    return new_ir;
}

static struct ExprIr* visit_addrof_expr(struct SimplifyVisitor* visitor,
                                        struct AddrofExprIr* ir) {
    struct DerefExprIr* deref = ir_expr_as_deref(ir_addrof_expr_operand(ir));
    assert(deref);
    struct ExprIr* operand = ir_deref_expr_operand(deref);
    return visitor_visit_expr(as_visitor(visitor), operand);
}

static struct FunctionIr* visit_function(struct SimplifyVisitor* visitor,
                                         struct FunctionIr* ir) {
    struct BlockStmtIr* body = ir_function_body2(ir);
    visitor_visit_stmt(as_visitor(visitor), ir_block_stmt_super(body));
    return ir;
}

struct SimplifyVisitor* new_simplify_visitor(struct Context* context) {
    struct SimplifyVisitor* visitor = malloc(sizeof(struct SimplifyVisitor));
    visitor_initialize(as_visitor(visitor), context);

    register_visitor(visitor->as_visitor, visit_binop_expr, visit_binop_expr);
    register_visitor(visitor->as_visitor, visit_addrof_expr, visit_addrof_expr);

    register_visitor(visitor->as_visitor, visit_function, visit_function);

    return visitor;
}
