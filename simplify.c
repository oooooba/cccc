#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

struct SimplifyVisitor {
    struct Visitor2 as_visitor;
    struct Context* context;
};

static struct Visitor2* as_visitor(struct SimplifyVisitor* visitor) {
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

static struct ExprIr* visit_addrof_expr(struct SimplifyVisitor* visitor,
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

static struct ExprIr* visit_load_expr(struct SimplifyVisitor* visitor,
                                      struct LoadExprIr* ir) {
    struct ExprIr* addr = ir_load_expr_addr(ir);
    struct ExprIr* new_addr = visitor2_visit_expr(as_visitor(visitor), addr);
    if (new_addr) {
        ir_load_expr_set_addr(ir, new_addr);
    }
    return NULL;
}

static struct ExprIr* visit_store_expr(struct SimplifyVisitor* visitor,
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

static struct ExprIr* visit_call_expr(struct SimplifyVisitor* visitor,
                                      struct CallExprIr* ir) {
    assert(ir_call_expr_tag(ir) == AddrTag_Var);

    for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                           *eit = list_end(ir_call_expr_args(ir));
         it != eit; it = list_next(it)) {
        struct ListItem* item = (struct ListItem*)it;
        struct ExprIr* arg = item->item;
        struct ExprIr* new_arg = visitor2_visit_expr(as_visitor(visitor), arg);
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
    if (ir_unop_expr_op(ir) == UnopExprIrTag_Addrof) {
        struct UnopExprIr* operand = ir_expr_as_unop(ir_unop_expr_operand(ir));
        if (operand && ir_unop_expr_op(operand) == UnopExprIrTag_Deref) {
            struct ExprIr* expr = ir_unop_expr_operand(operand);
            struct ExprIr* new_expr =
                visitor2_visit_expr(as_visitor(visitor), expr);
            return new_expr ? new_expr : expr;
        }
    }

    struct ExprIr* operand = ir_unop_expr_operand(ir);
    struct ExprIr* new_operand =
        visitor2_visit_expr(as_visitor(visitor), operand);
    if (new_operand) {
        ir_unop_expr_set_operand(ir, new_operand);
    }

    return NULL;
}

static struct ExprIr* visit_subst_expr(struct SimplifyVisitor* visitor,
                                       struct SubstExprIr* ir) {
    struct ExprIr* addr = ir_subst_expr_addr(ir);
    struct ExprIr* new_addr = visitor2_visit_expr(as_visitor(visitor), addr);
    if (new_addr) {
        ir_subst_expr_set_addr(ir, new_addr);
    }

    struct ExprIr* value = ir_subst_expr_value(ir);
    struct ExprIr* new_value = visitor2_visit_expr(as_visitor(visitor), value);
    if (new_value) {
        ir_subst_expr_set_value(ir, new_value);
    }

    return NULL;
}

static struct BlockIr* visit_block(struct SimplifyVisitor* visitor,
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

static struct FunctionIr* visit_function(struct SimplifyVisitor* visitor,
                                         struct FunctionIr* ir) {
    struct BlockIr* body = ir_function_body(ir);
    visitor2_visit_block(as_visitor(visitor), body);
    return NULL;
}

static struct CfIr* visit_branch_cf(struct SimplifyVisitor* visitor,
                                    struct BranchCfIr* ir) {
    struct ExprIr* cond_expr =
        visitor2_visit_expr(as_visitor(visitor), ir_branch_cf_cond_expr(ir));
    if (cond_expr) {
        ir_branch_cf_set_cond_expr(ir, cond_expr);
    }

    visitor2_visit_block(as_visitor(visitor), ir_branch_cf_true_block(ir));
    visitor2_visit_block(as_visitor(visitor), ir_branch_cf_false_block(ir));

    return NULL;
}

static struct CfIr* visit_return_cf(struct SimplifyVisitor* visitor,
                                    struct ReturnCfIr* ir) {
    struct ExprIr* expr = ir_return_cf_expr(ir);
    if (expr) {
        struct ExprIr* new_expr =
            visitor2_visit_expr(as_visitor(visitor), expr);
        if (new_expr) ir_return_cf_set_expr(ir, new_expr);
    }
    return NULL;
}

struct SimplifyVisitor* new_simplify_visitor(struct Context* context) {
    struct SimplifyVisitor* visitor = malloc(sizeof(struct SimplifyVisitor));
    visitor2_initialize(as_visitor(visitor));

    register_visitor(visitor->as_visitor, visit_const_expr, visit_const_expr);
    register_visitor(visitor->as_visitor, visit_binop_expr, visit_binop_expr);
    register_visitor(visitor->as_visitor, visit_addrof_expr, visit_addrof_expr);
    register_visitor(visitor->as_visitor, visit_load_expr, visit_load_expr);
    register_visitor(visitor->as_visitor, visit_store_expr, visit_store_expr);
    register_visitor(visitor->as_visitor, visit_call_expr, visit_call_expr);
    register_visitor(visitor->as_visitor, visit_var_expr, visit_var_expr);
    register_visitor(visitor->as_visitor, visit_unop_expr, visit_unop_expr);
    register_visitor(visitor->as_visitor, visit_subst_expr, visit_subst_expr);
    register_visitor(visitor->as_visitor, visit_block, visit_block);
    register_visitor(visitor->as_visitor, visit_function, visit_function);
    register_visitor(visitor->as_visitor, visit_branch_cf, visit_branch_cf);
    register_visitor(visitor->as_visitor, visit_return_cf, visit_return_cf);

    visitor->context = context;
    return visitor;
}

void simplify_apply(struct SimplifyVisitor* visitor, struct BlockIr* ir) {
    visitor2_visit_block(as_visitor(visitor), ir);
}
