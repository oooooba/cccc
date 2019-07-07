#include "visitor.h"
#include "ir.h"

#include <assert.h>
#include <stdbool.h>

struct Ir* visitor_visit_ir(struct Visitor* visitor, struct Ir* ir) {
    switch (ir_tag(ir)) {
        case IrTag_Expr:
            return ir_expr_cast(visitor_visit_expr(visitor, ir_as_expr(ir)));
        case IrTag_Function:
            return ir_function_cast(
                visitor_visit_function(visitor, ir_as_function(ir)));
        default:
            assert(false);
    }
    return NULL;
}

struct ExprIr* visitor_visit_expr(struct Visitor* visitor, struct ExprIr* ir) {
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
        case ExprIrTag_Member:
            return visitor->visit_member_expr(visitor, ir_expr_as_member(ir));
        case ExprIrTag_Deref:
            return visitor->visit_deref_expr(visitor, ir_expr_as_deref(ir));
        case ExprIrTag_Addrof:
            return visitor->visit_addrof_expr(visitor, ir_expr_as_addrof(ir));
        case ExprIrTag_Cast:
            return visitor->visit_cast_expr(visitor, ir_expr_as_cast(ir));
        default:
            assert(false);
    }
    return NULL;
}

struct StmtIr* visitor_visit_stmt_pre(struct Visitor* visitor,
                                      struct StmtIr* ir) {
    (void)visitor;
    (void)ir;
    return NULL;
}

struct StmtIr* visitor_visit_stmt(struct Visitor* visitor, struct StmtIr* ir) {
    struct StmtIr* stmt;
    struct StmtIr* new_ir = visitor->visit_stmt_pre(visitor, ir);
    if (new_ir) ir = new_ir;
    switch (ir_stmt_tag(ir)) {
        case StmtIrTag_Expr:
            stmt = visitor->visit_expr_stmt(visitor, ir_stmt_as_expr(ir));
            break;
        case StmtIrTag_Block:
            stmt = visitor->visit_block_stmt(visitor, ir_stmt_as_block(ir));
            break;
        case StmtIrTag_If:
            stmt = visitor->visit_if_stmt(visitor, ir_stmt_as_if(ir));
            break;
        case StmtIrTag_Return:
            stmt = visitor->visit_return_stmt(visitor, ir_stmt_as_return(ir));
            break;
        case StmtIrTag_Push:
            stmt = visitor->visit_push_stmt(visitor, ir_stmt_as_push(ir));
            break;
        case StmtIrTag_Pop:
            stmt = visitor->visit_pop_stmt(visitor, ir_stmt_as_pop(ir));
            break;
        case StmtIrTag_Decl:
            stmt = visitor->visit_decl_stmt(visitor, ir_stmt_as_decl(ir));
            break;
        default:
            assert(false);
            stmt = NULL;
    }
    return stmt;
}

struct FunctionIr* visitor_visit_function(struct Visitor* visitor,
                                          struct FunctionIr* ir) {
    return visitor->visit_function(visitor, ir);
}

struct ExprIr* visitor_visit_const_expr(struct Visitor* visitor,
                                        struct ConstExprIr* ir) {
    (void)visitor;
    return ir_const_expr_cast(ir);
}

struct ExprIr* visitor_visit_unop_expr(struct Visitor* visitor,
                                       struct UnopExprIr* ir) {
    (void)visitor;
    assert(false);
    return ir_unop_expr_cast(ir);
}

struct ExprIr* visitor_visit_binop_expr(struct Visitor* visitor,
                                        struct BinopExprIr* ir) {
    struct ExprIr* lhs = visitor_visit_expr(visitor, ir_binop_expr_lhs(ir));
    ir_binop_expr_set_lhs(ir, lhs);

    struct ExprIr* rhs = visitor_visit_expr(visitor, ir_binop_expr_rhs(ir));
    ir_binop_expr_set_rhs(ir, rhs);

    return ir_binop_expr_cast(ir);
}

struct ExprIr* visitor_visit_call_expr(struct Visitor* visitor,
                                       struct CallExprIr* ir) {
    struct ExprIr* expr =
        visitor_visit_expr(visitor, ir_call_expr_function(ir));
    ir_call_expr_set_function(ir, expr);

    for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                           *eit = list_end(ir_call_expr_args(ir));
         it != eit; it = list_next(it)) {
        struct ListItem* item = (struct ListItem*)it;
        struct ExprIr* arg = item->item;
        struct ExprIr* new_arg = visitor_visit_expr(visitor, arg);
        item->item = new_arg;
    }
    return ir_call_expr_cast(ir);
}

struct ExprIr* visitor_visit_var_expr(struct Visitor* visitor,
                                      struct VarExprIr* ir) {
    (void)visitor;
    return ir_var_expr_cast(ir);
}

struct ExprIr* visitor_visit_subst_expr(struct Visitor* visitor,
                                        struct SubstExprIr* ir) {
    struct ExprIr* value = visitor_visit_expr(visitor, ir_subst_expr_value(ir));
    ir_subst_expr_set_value(ir, value);

    struct ExprIr* addr = visitor_visit_expr(visitor, ir_subst_expr_addr(ir));
    ir_subst_expr_set_addr(ir, addr);

    return ir_subst_expr_cast(ir);
}

struct ExprIr* visitor_visit_member_expr(struct Visitor* visitor,
                                         struct MemberExprIr* ir) {
    struct ExprIr* base = visitor_visit_expr(visitor, ir_member_expr_base(ir));
    ir_member_expr_set_base(ir, base);
    return ir_member_expr_cast(ir);
}

struct ExprIr* visitor_visit_deref_expr(struct Visitor* visitor,
                                        struct DerefExprIr* ir) {
    struct ExprIr* operand =
        visitor_visit_expr(visitor, ir_deref_expr_operand(ir));
    ir_deref_expr_set_operand(ir, operand);
    return ir_deref_expr_cast(ir);
}

struct ExprIr* visitor_visit_addrof_expr(struct Visitor* visitor,
                                         struct AddrofExprIr* ir) {
    struct ExprIr* operand =
        visitor_visit_expr(visitor, ir_addrof_expr_operand(ir));
    ir_addrof_expr_set_operand(ir, operand);
    return ir_addrof_expr_cast(ir);
}

struct ExprIr* visitor_visit_cast_expr(struct Visitor* visitor,
                                       struct CastExprIr* ir) {
    struct ExprIr* operand =
        visitor_visit_expr(visitor, ir_cast_expr_operand(ir));
    ir_cast_expr_set_operand(ir, operand);
    return ir_cast_expr_cast(ir);
}

struct StmtIr* visitor_visit_expr_stmt(struct Visitor* visitor,
                                       struct ExprStmtIr* ir) {
    struct ExprIr* expr = visitor_visit_expr(visitor, ir_expr_stmt_expr(ir));
    ir_expr_stmt_set_expr(ir, expr);
    return ir_expr_stmt_super(ir);
}

struct StmtIr* visitor_visit_block_stmt(struct Visitor* visitor,
                                        struct BlockStmtIr* ir) {
    struct List* stmts = ir_block_stmt_statements(ir);
    for (struct ListHeader *it = list_begin(stmts), *eit = list_end(stmts);
         it != eit; it = list_next(it)) {
        struct ListItem* list_item = (struct ListItem*)it;
        struct StmtIr* stmt = list_item->item;
        stmt = visitor_visit_stmt(visitor, stmt);
        list_item->item = stmt;
    }
    return ir_block_stmt_super(ir);
}

struct StmtIr* visitor_visit_if_stmt(struct Visitor* visitor,
                                     struct IfStmtIr* ir) {
    struct ExprIr* cond = visitor_visit_expr(visitor, ir_if_stmt_cond_expr(ir));
    ir_if_stmt_set_cond_expr(ir, cond);

    struct StmtIr* true_stmt =
        visitor_visit_stmt(visitor, ir_if_stmt_true_stmt(ir));
    ir_if_stmt_set_true_stmt(ir, true_stmt);

    struct StmtIr* false_stmt =
        visitor_visit_stmt(visitor, ir_if_stmt_false_stmt(ir));
    ir_if_stmt_set_false_stmt(ir, false_stmt);

    return ir_if_stmt_super(ir);
}

struct StmtIr* visitor_visit_return_stmt(struct Visitor* visitor,
                                         struct ReturnStmtIr* ir) {
    struct ExprIr* expr = ir_return_stmt_expr(ir);
    if (expr) {
        expr = visitor_visit_expr(visitor, expr);
        ir_return_stmt_set_expr(ir, expr);
    }
    return ir_return_stmt_super(ir);
}

struct StmtIr* visitor_visit_decl_stmt(struct Visitor* visitor,
                                       struct DeclStmtIr* ir) {
    (void)visitor;
    return ir_decl_stmt_super(ir);
}

void visitor_initialize(struct Visitor* visitor, struct Context* context) {
    visitor->context = context;

    register_visitor(*visitor, visit_const_expr, visitor_visit_const_expr);
    register_visitor(*visitor, visit_unop_expr, visitor_visit_unop_expr);
    register_visitor(*visitor, visit_binop_expr, visitor_visit_binop_expr);
    register_visitor(*visitor, visit_call_expr, visitor_visit_call_expr);
    register_visitor(*visitor, visit_var_expr, visitor_visit_var_expr);
    register_visitor(*visitor, visit_subst_expr, visitor_visit_subst_expr);
    register_visitor(*visitor, visit_member_expr, visitor_visit_member_expr);
    register_visitor(*visitor, visit_deref_expr, visitor_visit_deref_expr);
    register_visitor(*visitor, visit_addrof_expr, visitor_visit_addrof_expr);
    register_visitor(*visitor, visit_cast_expr, visitor_visit_cast_expr);

    register_visitor(*visitor, visit_stmt_pre, visitor_visit_stmt_pre);

    register_visitor(*visitor, visit_expr_stmt, visitor_visit_expr_stmt);
    register_visitor(*visitor, visit_block_stmt, visitor_visit_block_stmt);
    register_visitor(*visitor, visit_if_stmt, visitor_visit_if_stmt);
    register_visitor(*visitor, visit_return_stmt, visitor_visit_return_stmt);
    register_visitor(*visitor, visit_push_stmt, NULL);
    register_visitor(*visitor, visit_pop_stmt, NULL);
    register_visitor(*visitor, visit_decl_stmt, visitor_visit_decl_stmt);

    register_visitor(*visitor, visit_function, NULL);
}

struct Context* visitor_context(struct Visitor* visitor) {
    return visitor->context;
}

void visitor_apply(struct Visitor* visitor) {
    for (struct ListHeader *
             it = context_function_definition_begin(visitor->context),
            *eit = context_function_definition_end(visitor->context);
         it != eit; it = list_next(it)) {
        struct MapEntry* map_entry = (struct MapEntry*)it;
        struct FunctionIr* func = map_entry_value(map_entry);
        func = visitor_visit_function(visitor, func);
        map_entry_set_value(map_entry, func);
    }
}
