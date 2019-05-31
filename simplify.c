#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

struct SimplifyVisitor {
    struct Visitor as_visitor;
    struct Context* context;
};

static struct Visitor* as_visitor(struct SimplifyVisitor* visitor) {
    return &visitor->as_visitor;
}

static struct ExprIr* visit_const_expr(struct SimplifyVisitor* visitor,
                                       struct ConstExprIr* ir) {
    (void)visitor;
    (void)ir;
    return NULL;
}

static struct ExprIr* visit_binop_expr(struct SimplifyVisitor* visitor,
                                       struct BinopExprIr* ir) {
    struct ExprIr* lhs = ir_binop_expr_lhs(ir);
    struct ExprIr* new_lhs = visitor_visit_expr(as_visitor(visitor), lhs);
    if (new_lhs) {
        ir_binop_expr_set_lhs(ir, new_lhs);
        lhs = new_lhs;
    }

    struct ExprIr* rhs = ir_binop_expr_rhs(ir);
    struct ExprIr* new_rhs = visitor_visit_expr(as_visitor(visitor), rhs);
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

    struct ExprIr* new_ir =
        ir_const_expr_cast(ir_new_integer_const_expr(value));
    ir_expr_set_type(new_ir, ir_expr_type(ir_binop_expr_cast(ir)));
    return new_ir;
}

static struct ExprIr* visit_call_expr(struct SimplifyVisitor* visitor,
                                      struct CallExprIr* ir) {
    struct VarExprIr* func_name = ir_expr_as_var(ir_call_expr_function(ir));
    assert(func_name);

    for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                           *eit = list_end(ir_call_expr_args(ir));
         it != eit; it = list_next(it)) {
        struct ListItem* item = (struct ListItem*)it;
        struct ExprIr* arg = item->item;
        struct ExprIr* new_arg = visitor_visit_expr(as_visitor(visitor), arg);
        if (new_arg) item->item = new_arg;
    }
    return NULL;
}

static struct ExprIr* visit_var_expr(struct SimplifyVisitor* visitor,
                                     struct VarExprIr* ir) {
    (void)visitor;
    (void)ir;
    return NULL;
}

static struct ExprIr* visit_unop_expr(struct SimplifyVisitor* visitor,
                                      struct UnopExprIr* ir) {
    (void)visitor;
    (void)ir;
    assert(false);
    return NULL;
}

static struct ExprIr* visit_subst_expr(struct SimplifyVisitor* visitor,
                                       struct SubstExprIr* ir) {
    struct ExprIr* addr = ir_subst_expr_addr(ir);
    struct ExprIr* new_addr = visitor_visit_expr(as_visitor(visitor), addr);
    if (new_addr) {
        ir_subst_expr_set_addr(ir, new_addr);
    }

    struct ExprIr* value = ir_subst_expr_value(ir);
    struct ExprIr* new_value = visitor_visit_expr(as_visitor(visitor), value);
    if (new_value) {
        ir_subst_expr_set_value(ir, new_value);
    }

    return NULL;
}

static struct ExprIr* visit_member_expr(struct SimplifyVisitor* visitor,
                                        struct MemberExprIr* ir) {
    struct ExprIr* base = ir_member_expr_base(ir);
    struct ExprIr* new_base = visitor_visit_expr(as_visitor(visitor), base);
    if (new_base) {
        ir_member_expr_set_base(ir, new_base);
    }
    return NULL;
}

static struct ExprIr* visit_deref_expr(struct SimplifyVisitor* visitor,
                                       struct DerefExprIr* ir) {
    struct ExprIr* new_operand =
        visitor_visit_expr(as_visitor(visitor), ir_deref_expr_operand(ir));
    if (new_operand) {
        ir_deref_expr_set_operand(ir, new_operand);
    }
    return NULL;
}

static struct ExprIr* visit_addrof_expr(struct SimplifyVisitor* visitor,
                                        struct AddrofExprIr* ir) {
    struct DerefExprIr* deref = ir_expr_as_deref(ir_addrof_expr_operand(ir));
    assert(deref);
    struct ExprIr* operand = ir_deref_expr_operand(deref);
    struct ExprIr* new_operand =
        visitor_visit_expr(as_visitor(visitor), operand);
    return new_operand ? new_operand : operand;
}

static struct BlockIr* visit_block(struct SimplifyVisitor* visitor,
                                   struct BlockIr* ir) {
    struct BlockIterator* it = ir_block_new_iterator(ir);
    for (;;) {
        struct Ir* stmt = ir_block_iterator_next(it);
        if (!stmt) break;

        struct Ir* new_stmt = visitor_visit_ir(as_visitor(visitor), stmt);
        if (new_stmt) ir_block_iterator_swap_at(it, new_stmt);
    }
    return NULL;
}

static struct FunctionIr* visit_function(struct SimplifyVisitor* visitor,
                                         struct FunctionIr* ir) {
    struct BlockIr* body = ir_function_body(ir);
    visitor_visit_block(as_visitor(visitor), body);
    return NULL;
}

static struct CfIr* visit_branch_cf(struct SimplifyVisitor* visitor,
                                    struct BranchCfIr* ir) {
    struct ExprIr* cond_expr =
        visitor_visit_expr(as_visitor(visitor), ir_branch_cf_cond_expr(ir));
    if (cond_expr) {
        ir_branch_cf_set_cond_expr(ir, cond_expr);
    }

    visitor_visit_block(as_visitor(visitor), ir_branch_cf_true_block(ir));
    visitor_visit_block(as_visitor(visitor), ir_branch_cf_false_block(ir));

    return NULL;
}

static struct CfIr* visit_return_cf(struct SimplifyVisitor* visitor,
                                    struct ReturnCfIr* ir) {
    struct ExprIr* expr = ir_return_cf_expr(ir);
    if (expr) {
        struct ExprIr* new_expr = visitor_visit_expr(as_visitor(visitor), expr);
        if (new_expr) ir_return_cf_set_expr(ir, new_expr);
    }
    return NULL;
}

struct SimplifyVisitor* new_simplify_visitor(struct Context* context) {
    struct SimplifyVisitor* visitor = malloc(sizeof(struct SimplifyVisitor));
    visitor_initialize(as_visitor(visitor));

    register_visitor(visitor->as_visitor, visit_const_expr, visit_const_expr);
    register_visitor(visitor->as_visitor, visit_binop_expr, visit_binop_expr);
    register_visitor(visitor->as_visitor, visit_call_expr, visit_call_expr);
    register_visitor(visitor->as_visitor, visit_var_expr, visit_var_expr);
    register_visitor(visitor->as_visitor, visit_unop_expr, visit_unop_expr);
    register_visitor(visitor->as_visitor, visit_subst_expr, visit_subst_expr);
    register_visitor(visitor->as_visitor, visit_member_expr, visit_member_expr);
    register_visitor(visitor->as_visitor, visit_deref_expr, visit_deref_expr);
    register_visitor(visitor->as_visitor, visit_addrof_expr, visit_addrof_expr);
    register_visitor(visitor->as_visitor, visit_block, visit_block);
    register_visitor(visitor->as_visitor, visit_function, visit_function);
    register_visitor(visitor->as_visitor, visit_branch_cf, visit_branch_cf);
    register_visitor(visitor->as_visitor, visit_return_cf, visit_return_cf);

    visitor->context = context;
    return visitor;
}

void simplify_apply(struct SimplifyVisitor* visitor, struct BlockIr* ir) {
    visitor_visit_block(as_visitor(visitor), ir);
}
