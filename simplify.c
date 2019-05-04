#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <assert.h>
#include <stdlib.h>

struct SimplifyVisitor2 {
    struct Visitor2 as_visitor;
    struct Context* context;
};

static struct Visitor2* as_visitor(struct SimplifyVisitor2* visitor) {
    return &visitor->as_visitor;
}

static struct ExprIr* visit_const_expr2(struct SimplifyVisitor2* visitor,
                                        struct ConstExprIr* ir) {
    (void)visitor;
    (void)ir;
    return NULL;
}

static struct ExprIr* visit_binop_expr2(struct SimplifyVisitor2* visitor,
                                        struct BinopExprIr* ir) {
    struct ExprIr* lhs = ir_binop_expr_lhs(ir);
    struct ExprIr* new_lhs = visitor2_visit_expr(as_visitor(visitor), lhs);
    if (new_lhs) {
        ir_binop_expr_set_lhs(ir, new_lhs);
        lhs = new_lhs;
    }

    struct ExprIr* rhs = ir_binop_expr_rhs(ir);
    struct ExprIr* new_rhs = visitor2_visit_expr(as_visitor(visitor), rhs);
    if (new_rhs) {
        ir_binop_expr_set_rhs(ir, new_rhs);
        rhs = new_rhs;
    }

    struct ConstExprIr* lhs_expr = ir_expr_as_const(lhs);
    if (!lhs_expr) return NULL;
    struct ConstExprIr* rhs_expr = ir_expr_as_const(rhs);
    if (!rhs_expr) return NULL;

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
        default:
            assert(false);
    }

    return ir_const_expr_cast(ir_new_integer_const_expr(value));
}

static struct ExprIr* visit_addrof_expr2(struct SimplifyVisitor2* visitor,
                                         struct AddrofExprIr* ir) {
    struct ExprIr* addrof_operand = ir_addrof_expr_operand_as_expr(ir);
    if (!addrof_operand) return NULL;

    struct LoadExprIr* load_expr = ir_expr_as_load(addrof_operand);
    if (!load_expr)
        assert(false);  // ToDo: maybe, this is invalid situation (?)

    struct ExprIr* load_addr = ir_load_expr_addr(load_expr);
    struct ExprIr* new_load_addr =
        visitor2_visit_expr(as_visitor(visitor), load_addr);
    return new_load_addr ? new_load_addr : load_addr;
}

static struct ExprIr* visit_load_expr2(struct SimplifyVisitor2* visitor,
                                       struct LoadExprIr* ir) {
    struct ExprIr* addr = ir_load_expr_addr(ir);
    struct ExprIr* new_addr = visitor2_visit_expr(as_visitor(visitor), addr);
    if (new_addr) {
        ir_load_expr_set_addr(ir, new_addr);
    }
    return NULL;
}

static struct ExprIr* visit_store_expr2(struct SimplifyVisitor2* visitor,
                                        struct StoreExprIr* ir) {
    struct ExprIr* addr = ir_store_expr_addr(ir);
    struct ExprIr* new_addr = visitor2_visit_expr(as_visitor(visitor), addr);
    if (new_addr) {
        ir_store_expr_set_addr(ir, new_addr);
    }

    struct ExprIr* value = ir_store_expr_value(ir);
    struct ExprIr* new_value = visitor2_visit_expr(as_visitor(visitor), value);
    if (new_value) {
        ir_store_expr_set_value(ir, new_value);
    }

    return NULL;
}

static struct BlockIr* visit_block2(struct SimplifyVisitor2* visitor,
                                    struct BlockIr* ir) {
    struct BlockIterator* it = ir_block_new_iterator(ir);
    for (;;) {
        struct Ir* stmt = ir_block_iterator_next(it);
        if (!stmt) break;

        struct Ir* new_stmt = visitor2_visit_ir(as_visitor(visitor), stmt);
        if (new_stmt) ir_block_iterator_swap_at(it, new_stmt);
    }
    return NULL;
}

static struct FunctionIr* visit_function2(struct SimplifyVisitor2* visitor,
                                          struct FunctionIr* ir) {
    struct BlockIr* body = ir_function_body(ir);
    visitor2_visit_block(as_visitor(visitor), body);
    return NULL;
}

struct SimplifyVisitor2* new_simplify_visitor2(struct Context* context) {
    struct SimplifyVisitor2* visitor = malloc(sizeof(struct SimplifyVisitor2));
    visitor2_initialize(as_visitor(visitor));

    register_visitor(visitor->as_visitor, visit_const_expr, visit_const_expr2);
    register_visitor(visitor->as_visitor, visit_binop_expr, visit_binop_expr2);
    register_visitor(visitor->as_visitor, visit_addrof_expr,
                     visit_addrof_expr2);
    register_visitor(visitor->as_visitor, visit_load_expr, visit_load_expr2);
    register_visitor(visitor->as_visitor, visit_store_expr, visit_store_expr2);
    register_visitor(visitor->as_visitor, visit_block, visit_block2);
    register_visitor(visitor->as_visitor, visit_function, visit_function2);

    visitor->context = context;
    return visitor;
}

void simplify2_apply(struct SimplifyVisitor2* visitor, struct BlockIr* ir) {
    visitor2_visit_block(as_visitor(visitor), ir);
}
