#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct TypingVisitor {
    struct Visitor as_visitor;
    struct Context* context;
    struct FunctionIr* function;
};

static struct Visitor* as_visitor(struct TypingVisitor* visitor) {
    return &visitor->as_visitor;
}

static void check_type(struct TypingVisitor* visitor,
                       struct TypeIr* expected_type,
                       struct TypeIr* actually_type) {
    (void)visitor;
    assert(type_equal(expected_type, actually_type));
}

static struct ExprIr* visit_const_expr(struct TypingVisitor* visitor,
                                       struct ConstExprIr* ir) {
    (void)visitor;
    ir_expr_set_type(ir_const_expr_cast(ir), type_new_int2());
    return ir_const_expr_cast(ir);
}

static struct ExprIr* visit_binop_expr(struct TypingVisitor* visitor,
                                       struct BinopExprIr* ir) {
    struct ExprIr* lhs = ir_binop_expr_lhs(ir);
    lhs = visitor_visit_expr(as_visitor(visitor), lhs);
    struct ExprIr* rhs = ir_binop_expr_rhs(ir);
    rhs = visitor_visit_expr(as_visitor(visitor), rhs);

    check_type(visitor, ir_expr_type(lhs), ir_expr_type(rhs));

    ir_binop_expr_set_lhs(ir, lhs);
    ir_binop_expr_set_rhs(ir, rhs);
    ir_expr_set_type(ir_binop_expr_cast(ir), ir_expr_type(lhs));
    return ir_binop_expr_cast(ir);
}

static struct ExprIr* visit_call_expr(struct TypingVisitor* visitor,
                                      struct CallExprIr* ir) {
    struct ExprIr* func_expr = ir_call_expr_function(ir);
    func_expr = visitor_visit_expr(as_visitor(visitor), func_expr);
    ir_call_expr_set_function(ir, func_expr);

    struct VarExprIr* func_var_expr = ir_expr_as_var(func_expr);
    assert(func_var_expr);
    struct FunctionTypeIr* func_type =
        type_as_function(ir_expr_type(ir_var_expr_cast(func_var_expr)));
    assert(func_type);

    struct List* arg_exprs = ir_call_expr_args(ir);
    struct List* param_types = type_function_param_types(func_type);
    assert(list_size(arg_exprs) == list_size(param_types));

    struct ListHeader* ita = list_begin(arg_exprs);
    struct ListHeader* itp = list_begin(param_types);
    struct ListHeader* eita = list_end(arg_exprs);
    struct ListHeader* eitp = list_end(param_types);
    while (ita != eita && itp != eitp) {
        struct ExprIr* arg_expr = ((struct ListItem*)ita)->item;
        struct TypeIr* param_type = ((struct ListItem*)itp)->item;

        arg_expr = visitor_visit_expr(as_visitor(visitor), arg_expr);
        ((struct ListItem*)ita)->item = arg_expr;
        assert(type_equal(param_type, ir_expr_type(arg_expr)));

        ita = list_next(ita);
        itp = list_next(itp);
    }

    struct TypeIr* result_type = type_function_result_type(func_type);
    ir_expr_set_type(ir_call_expr_cast(ir), result_type);

    return ir_call_expr_cast(ir);
}

static struct ExprIr* visit_var_expr(struct TypingVisitor* visitor,
                                     struct VarExprIr* ir) {
    (void)visitor;
    struct TypeIr* type = ir_var_expr_type(ir);
    if (ir_var_expr_is_function(ir)) {
        ir_expr_set_type(ir_var_expr_cast(ir), type);
    } else {
        ir_expr_set_type(ir_var_expr_cast(ir), type_new_pointer2(type));
    }
    return ir_var_expr_cast(ir);
}

static struct ExprIr* visit_unop_expr(struct TypingVisitor* visitor,
                                      struct UnopExprIr* ir) {
    (void)visitor;
    (void)ir;
    assert(false);
    return NULL;
}

static struct ExprIr* visit_subst_expr(struct TypingVisitor* visitor,
                                       struct SubstExprIr* ir) {
    struct ExprIr* value = ir_subst_expr_value(ir);
    value = visitor_visit_expr(as_visitor(visitor), value);
    struct ExprIr* addr = ir_subst_expr_addr(ir);
    addr = visitor_visit_expr(as_visitor(visitor), addr);

    check_type(visitor, type_new_pointer2(ir_expr_type(value)),
               ir_expr_type(addr));

    ir_subst_expr_set_value(ir, value);
    ir_subst_expr_set_addr(ir, addr);
    ir_expr_set_type(ir_subst_expr_cast(ir), ir_expr_type(value));
    return ir_subst_expr_cast(ir);
}

static struct ExprIr* visit_member_expr(struct TypingVisitor* visitor,
                                        struct MemberExprIr* ir) {
    struct ExprIr* base = ir_member_expr_base(ir);
    base = visitor_visit_expr(as_visitor(visitor), base);
    ir_member_expr_set_base(ir, base);

    struct PointerTypeIr* ptr_struct_type = type_as_pointer(ir_expr_type(base));
    assert(ptr_struct_type);

    struct StructTypeIr* struct_type =
        type_as_struct(type_pointer_elem_type(ptr_struct_type));
    assert(struct_type);

    struct MemberEntry* entry =
        type_struct_find_member(struct_type, ir_member_expr_name_index(ir));
    assert(entry);
    struct TypeIr* type = type_member_entry_type(entry);
    ir_member_expr_set_offset(ir, type_member_entry_offset(entry));

    ir_expr_set_type(ir_member_expr_cast(ir), type_new_pointer2(type));
    return ir_member_expr_cast(ir);
}

static struct ExprIr* visit_deref_expr(struct TypingVisitor* visitor,
                                       struct DerefExprIr* ir) {
    struct ExprIr* operand = ir_deref_expr_operand(ir);
    operand = visitor_visit_expr(as_visitor(visitor), operand);
    ir_deref_expr_set_operand(ir, operand);
    struct PointerTypeIr* operand_type = type_as_pointer(ir_expr_type(operand));
    assert(operand_type);
    ir_expr_set_type(ir_deref_expr_cast(ir),
                     type_pointer_elem_type(operand_type));
    return ir_deref_expr_cast(ir);
}

static struct ExprIr* visit_addrof_expr(struct TypingVisitor* visitor,
                                        struct AddrofExprIr* ir) {
    struct ExprIr* operand = ir_addrof_expr_operand(ir);
    operand = visitor_visit_expr(as_visitor(visitor), operand);
    ir_addrof_expr_set_operand(ir, operand);
    ir_expr_set_type(ir_addrof_expr_cast(ir),
                     type_new_pointer2(ir_expr_type(operand)));
    return ir_addrof_expr_cast(ir);
}

static struct BlockIr* visit_block(struct TypingVisitor* visitor,
                                   struct BlockIr* ir) {
    struct BlockIterator* it = ir_block_new_iterator(ir);
    for (;;) {
        struct Ir* stmt = ir_block_iterator_next(it);
        if (!stmt) break;

        stmt = visitor_visit_ir(as_visitor(visitor), stmt);
        ir_block_iterator_swap_at(it, stmt);
    }
    return ir;
}

static struct FunctionIr* visit_function(struct TypingVisitor* visitor,
                                         struct FunctionIr* ir) {
    visitor->function = ir;

    for (struct ListHeader *it = list_begin(ir_function_params(ir)),
                           *eit = list_end(ir_function_params(ir));
         it != eit; it = list_next(it)) {
        struct ListItem* item = (struct ListItem*)it;
        struct VarExprIr* param = item->item;
        param = ir_expr_as_var(visit_var_expr(visitor, param));
        assert(param);
        item->item = param;
    }

    struct BlockIr* body = ir_function_body(ir);
    body = visitor_visit_block(as_visitor(visitor), body);
    ir_function_set_body(ir, body);

    return ir;
}

static struct CfIr* visit_branch_cf(struct TypingVisitor* visitor,
                                    struct BranchCfIr* ir) {
    struct ExprIr* cond_expr = ir_branch_cf_cond_expr(ir);
    cond_expr = visitor_visit_expr(as_visitor(visitor), cond_expr);
    ir_branch_cf_set_cond_expr(ir, cond_expr);

    struct BlockIr* true_block = ir_branch_cf_true_block(ir);
    true_block = visitor_visit_block(as_visitor(visitor), true_block);
    ir_branch_cf_set_true_block(ir, true_block);

    struct BlockIr* false_block = ir_branch_cf_false_block(ir);
    false_block = visitor_visit_block(as_visitor(visitor), false_block);
    ir_branch_cf_set_false_block(ir, false_block);

    return ir_branch_cf_cast(ir);
}

static struct CfIr* visit_return_cf(struct TypingVisitor* visitor,
                                    struct ReturnCfIr* ir) {
    struct ExprIr* expr = ir_return_cf_expr(ir);
    if (expr) {
        expr = visitor_visit_expr(as_visitor(visitor), expr);
        ir_return_cf_set_expr(ir, expr);

        check_type(visitor, ir_function_result_type(visitor->function),
                   ir_expr_type(expr));
    } else
        assert(false && "unimplemented");  // ToDo: compare to void
    return ir_return_cf_cast(ir);
}

struct TypingVisitor* new_typing_visitor(struct Context* context) {
    struct TypingVisitor* visitor = malloc(sizeof(struct TypingVisitor));
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

void typing_apply(struct TypingVisitor* visitor, struct BlockIr* ir) {
    visitor_visit_block(as_visitor(visitor), ir);
}
