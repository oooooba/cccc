#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct TypingVisitor {
    struct Visitor as_visitor;
    struct FunctionIr* function;
};

static struct Visitor* as_visitor(struct TypingVisitor* visitor) {
    return &visitor->as_visitor;
}

static struct ExprIr* visit_const_expr(struct TypingVisitor* visitor,
                                       struct ConstExprIr* ir) {
    (void)visitor;
    struct TypeIr* type = NULL;
    switch (ir_const_expr_tag(ir)) {
        case ConstExprIrTag_Integer:
            type = type_int_super(type_new_int());
            break;
        case ConstExprIrTag_String:
            type = type_char_super(type_new_char());
            type = type_pointer_super(type_new_pointer(type));
            break;
        default:
            assert(false);
    }
    ir_expr_set_type(ir_const_expr_cast(ir), type);
    return ir_const_expr_cast(ir);
}

static struct ExprIr* visit_binop_expr(struct TypingVisitor* visitor,
                                       struct BinopExprIr* ir) {
    visitor_visit_binop_expr(as_visitor(visitor), ir);
    struct ExprIr* lhs = ir_binop_expr_lhs(ir);
    struct ExprIr* rhs = ir_binop_expr_rhs(ir);

    struct TypeIr* lhs_type = ir_expr_type(lhs);
    struct TypeIr* rhs_type = ir_expr_type(rhs);

    if (!type_equal(lhs_type, rhs_type)) {
        if (type_tag(lhs_type) == Type_Int && type_tag(rhs_type) == Type_Char)
            rhs = ir_cast_expr_cast(ir_new_cast_expr(rhs, lhs_type));
        else if (type_tag(lhs_type) == Type_Char &&
                 type_tag(rhs_type) == Type_Int)
            lhs = ir_cast_expr_cast(ir_new_cast_expr(lhs, rhs_type));
        else if (type_tag(lhs_type) == Type_Long &&
                 type_tag(rhs_type) == Type_Int)
            rhs = ir_cast_expr_cast(ir_new_cast_expr(rhs, lhs_type));
        else if (type_tag(lhs_type) == Type_Int &&
                 type_tag(rhs_type) == Type_Long)
            lhs = ir_cast_expr_cast(ir_new_cast_expr(lhs, rhs_type));
        else if (ir_binop_expr_op(ir) == BinopExprIrTag_Add ||
                 ir_binop_expr_op(ir) == BinopExprIrTag_Sub) {
            if (type_tag(lhs_type) == Type_Int &&
                type_tag(rhs_type) == Type_Pointer) {
                struct CastExprIr* new_rhs =
                    ir_new_cast_expr(lhs, type_long_super(type_new_long()));
                lhs = rhs;
                rhs = ir_cast_expr_cast(new_rhs);
            } else if (type_tag(lhs_type) == Type_Pointer &&
                       type_tag(rhs_type) == Type_Int)
                rhs = ir_cast_expr_cast(
                    ir_new_cast_expr(rhs, type_long_super(type_new_long())));
        } else if (ir_binop_expr_op(ir) == BinopExprIrTag_Equal &&
                   type_tag(lhs_type) == Type_Pointer &&
                   type_tag(rhs_type) == Type_Pointer) {
        } else
            assert(false);
    }

    ir_binop_expr_set_lhs(ir, lhs);
    ir_binop_expr_set_rhs(ir, rhs);
    ir_expr_set_type(ir_binop_expr_cast(ir), ir_expr_type(lhs));
    return ir_binop_expr_cast(ir);
}

static struct ExprIr* visit_unop_expr(struct TypingVisitor* visitor,
                                      struct UnopExprIr* ir) {
    visitor_visit_unop_expr(as_visitor(visitor), ir);
    struct TypeIr* operand_type = ir_expr_type(ir_unop_expr_operand(ir));
    ir_expr_set_type(ir_unop_expr_cast(ir), operand_type);
    return ir_unop_expr_cast(ir);
}

static struct ExprIr* visit_call_expr(struct TypingVisitor* visitor,
                                      struct CallExprIr* ir) {
    struct ExprIr* func_expr = ir_call_expr_function(ir);
    func_expr = visitor_visit_expr(as_visitor(visitor), func_expr);
    ir_call_expr_set_function(ir, func_expr);
    struct FunctionTypeIr* func_type =
        type_as_function(ir_expr_type(func_expr));
    if (!func_type) {
        struct PointerTypeIr* pointer_type =
            type_as_pointer(ir_expr_type(func_expr));
        assert(pointer_type);
        func_type = type_as_function(type_pointer_elem_type(pointer_type));
    }

    struct List* arg_exprs = ir_call_expr_args(ir);
    struct List* param_types = type_function_param_types(func_type);
    assert(list_size(arg_exprs) >= list_size(param_types));

    struct ListHeader* ita = list_begin(arg_exprs);
    struct ListHeader* itp = list_begin(param_types);
    struct ListHeader* eita = list_end(arg_exprs);
    struct ListHeader* eitp = list_end(param_types);
    while (itp != eitp) {
        assert(ita != eita);

        struct ExprIr* arg_expr = ((struct ListItem*)ita)->item;
        struct TypeIr* param_type = ((struct ListItem*)itp)->item;

        if (!type_equal(param_type, ir_expr_type(arg_expr)))
            arg_expr =
                ir_cast_expr_cast(ir_new_cast_expr(arg_expr, param_type));

        arg_expr = visitor_visit_expr(as_visitor(visitor), arg_expr);
        ((struct ListItem*)ita)->item = arg_expr;

        ita = list_next(ita);
        itp = list_next(itp);
    }

    // for variable length parameters
    while (ita != eita) {
        struct ExprIr* arg_expr = ((struct ListItem*)ita)->item;

        arg_expr = visitor_visit_expr(as_visitor(visitor), arg_expr);
        ((struct ListItem*)ita)->item = arg_expr;

        ita = list_next(ita);
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
        ir_expr_set_type(ir_var_expr_cast(ir),
                         type_pointer_super(type_new_pointer(type)));
    }
    return ir_var_expr_cast(ir);
}

static struct ExprIr* visit_subst_expr(struct TypingVisitor* visitor,
                                       struct SubstExprIr* ir) {
    visitor_visit_subst_expr(as_visitor(visitor), ir);
    struct ExprIr* value = ir_subst_expr_value(ir);
    struct ExprIr* addr = ir_subst_expr_addr(ir);

    struct TypeIr* value_type = ir_expr_type(value);
    struct TypeIr* loc_type =
        type_pointer_elem_type(type_as_pointer(ir_expr_type(addr)));

    if (!type_equal(value_type, loc_type)) {
        // apply implicit conversion
        struct CastExprIr* cast = ir_new_cast_expr(value, loc_type);
        value = ir_cast_expr_cast(cast);
    }

    ir_subst_expr_set_value(ir, value);
    ir_subst_expr_set_addr(ir, addr);
    ir_expr_set_type(ir_subst_expr_cast(ir), ir_expr_type(value));
    return ir_subst_expr_cast(ir);
}

static struct ExprIr* visit_member_expr(struct TypingVisitor* visitor,
                                        struct MemberExprIr* ir) {
    visitor_visit_member_expr(as_visitor(visitor), ir);

    struct PointerTypeIr* ptr_struct_type =
        type_as_pointer(ir_expr_type(ir_member_expr_base(ir)));
    assert(ptr_struct_type);

    struct StructTypeIr* struct_type =
        type_as_struct(type_pointer_elem_type(ptr_struct_type));
    assert(struct_type);

    struct MemberEntry* entry =
        type_struct_find_member(struct_type, ir_member_expr_name_index(ir));
    assert(entry);
    struct TypeIr* type = type_member_entry_type(entry);
    ir_member_expr_set_offset(ir, type_member_entry_offset(entry));

    ir_expr_set_type(ir_member_expr_cast(ir),
                     type_pointer_super(type_new_pointer(type)));
    return ir_member_expr_cast(ir);
}

static struct ExprIr* visit_deref_expr(struct TypingVisitor* visitor,
                                       struct DerefExprIr* ir) {
    visitor_visit_deref_expr(as_visitor(visitor), ir);
    struct TypeIr* operand_type = ir_expr_type(ir_deref_expr_operand(ir));
    if (type_as_pointer(operand_type)) {
        struct TypeIr* elem_type =
            type_pointer_elem_type(type_as_pointer(operand_type));
        ir_expr_set_type(ir_deref_expr_cast(ir), elem_type);
    } else if (type_as_function(operand_type)) {
        return ir_deref_expr_operand(ir);
    } else
        assert(false);
    return ir_deref_expr_cast(ir);
}

static struct ExprIr* visit_addrof_expr(struct TypingVisitor* visitor,
                                        struct AddrofExprIr* ir) {
    visitor_visit_addrof_expr(as_visitor(visitor), ir);
    struct ExprIr* operand = ir_addrof_expr_operand(ir);
    ir_expr_set_type(
        ir_addrof_expr_cast(ir),
        type_pointer_super(type_new_pointer(ir_expr_type(operand))));
    return ir_addrof_expr_cast(ir);
}

static struct ExprIr* visit_cond_expr(struct TypingVisitor* visitor,
                                      struct CondExprIr* ir) {
    visitor_visit_cond_expr(as_visitor(visitor), ir);
    ir_expr_set_type(ir_cond_expr_cast(ir),
                     ir_expr_type(ir_cond_expr_true_expr(ir)));
    return ir_cond_expr_cast(ir);
}

static struct StmtIr* visit_if_stmt(struct TypingVisitor* visitor,
                                    struct IfStmtIr* ir) {
    visitor_visit_if_stmt(as_visitor(visitor), ir);
    struct ExprIr* cond_expr = ir_if_stmt_cond_expr(ir);
    cond_expr = visitor_visit_expr(as_visitor(visitor), cond_expr);
    ir_if_stmt_set_cond_expr(ir, cond_expr);
    return ir_if_stmt_super(ir);
}

static struct StmtIr* visit_while_stmt(struct TypingVisitor* visitor,
                                       struct WhileStmtIr* ir) {
    visitor_visit_while_stmt(as_visitor(visitor), ir);
    struct ExprIr* cond_expr = ir_while_stmt_cond_expr(ir);
    cond_expr = visitor_visit_expr(as_visitor(visitor), cond_expr);
    ir_while_stmt_set_cond_expr(ir, cond_expr);
    return ir_while_stmt_super(ir);
}

static struct StmtIr* visit_return_stmt(struct TypingVisitor* visitor,
                                        struct ReturnStmtIr* ir) {
    visitor_visit_return_stmt(as_visitor(visitor), ir);
    struct ExprIr* expr = ir_return_stmt_expr(ir);
    if (expr) {
        expr = visitor_visit_expr(as_visitor(visitor), expr);
        struct TypeIr* result_type = ir_function_result_type(visitor->function);
        if (!type_equal(ir_expr_type(expr), result_type))
            expr = ir_cast_expr_cast(ir_new_cast_expr(expr, result_type));
        ir_return_stmt_set_expr(ir, expr);
    } else {
        struct TypeIr* return_type = ir_function_result_type(visitor->function);
        assert(type_as_void(return_type));
    }
    return ir_return_stmt_super(ir);
}

static struct StmtIr* visit_decl_stmt(struct TypingVisitor* visitor,
                                      struct DeclStmtIr* ir) {
    struct VarExprIr* var = ir_decl_stmt_var(ir);
    struct VarExprIr* new_var = ir_expr_as_var(visit_var_expr(visitor, var));
    assert(var == new_var);
    return ir_decl_stmt_super(ir);
}

static struct FunctionIr* visit_function(struct TypingVisitor* visitor,
                                         struct FunctionIr* ir) {
    visitor->function = ir;
    visitor_visit_block_stmt(as_visitor(visitor), ir_function_body2(ir));
    return ir;
}

struct TypingVisitor* new_typing_visitor(struct Context* context) {
    struct TypingVisitor* visitor = malloc(sizeof(struct TypingVisitor));
    visitor_initialize(as_visitor(visitor), context);

    register_visitor(visitor->as_visitor, visit_const_expr, visit_const_expr);
    register_visitor(visitor->as_visitor, visit_binop_expr, visit_binop_expr);
    register_visitor(visitor->as_visitor, visit_unop_expr, visit_unop_expr);
    register_visitor(visitor->as_visitor, visit_call_expr, visit_call_expr);
    register_visitor(visitor->as_visitor, visit_var_expr, visit_var_expr);
    register_visitor(visitor->as_visitor, visit_subst_expr, visit_subst_expr);
    register_visitor(visitor->as_visitor, visit_member_expr, visit_member_expr);
    register_visitor(visitor->as_visitor, visit_deref_expr, visit_deref_expr);
    register_visitor(visitor->as_visitor, visit_addrof_expr, visit_addrof_expr);
    register_visitor(visitor->as_visitor, visit_cond_expr, visit_cond_expr);

    register_visitor(visitor->as_visitor, visit_if_stmt, visit_if_stmt);
    register_visitor(visitor->as_visitor, visit_while_stmt, visit_while_stmt);
    register_visitor(visitor->as_visitor, visit_return_stmt, visit_return_stmt);
    register_visitor(visitor->as_visitor, visit_decl_stmt, visit_decl_stmt);

    register_visitor(visitor->as_visitor, visit_function, visit_function);

    return visitor;
}
