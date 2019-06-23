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
        case IrTag_Cf:
            return ir_cf_cast(visitor_visit_cf(visitor, ir_as_cf(ir)));
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
        case StmtIrTag_Cf:
            stmt = visitor->visit_cf_stmt(visitor, ir_stmt_as_cf(ir));
            break;
        case StmtIrTag_Push:
            stmt = visitor->visit_push_stmt(visitor, ir_stmt_as_push(ir));
            break;
        case StmtIrTag_Pop:
            stmt = visitor->visit_pop_stmt(visitor, ir_stmt_as_pop(ir));
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

struct CfIr* visitor_visit_cf(struct Visitor* visitor, struct CfIr* ir) {
    switch (ir_cf_tag(ir)) {
        case CfIrTag_Return:
            return visitor->visit_return_cf(visitor, ir_cf_as_return(ir));
        default:
            assert(false);
    }
    return NULL;
}

struct StmtIr* visitor_visit_expr_stmt(struct Visitor* visitor,
                                       struct ExprStmtIr* ir) {
    struct ExprIr* expr = visitor_visit_expr(visitor, ir_expr_stmt_expr(ir));
    if (expr) {
        ir_expr_stmt_set_expr(ir, expr);
        return ir_expr_stmt_super(ir);
    } else
        return NULL;
}

struct StmtIr* visitor_visit_block_stmt(struct Visitor* visitor,
                                        struct BlockStmtIr* ir) {
    bool changed = false;
    struct List* stmts = ir_block_stmt_statements(ir);
    for (struct ListHeader *it = list_begin(stmts), *eit = list_end(stmts);
         it != eit; it = list_next(it)) {
        struct ListItem* list_item = (struct ListItem*)it;
        struct StmtIr* stmt = list_item->item;
        stmt = visitor_visit_stmt(visitor, stmt);
        if (stmt) {
            list_item->item = stmt;
            changed = true;
        }
    }
    return changed ? ir_block_stmt_super(ir) : NULL;
}

struct StmtIr* visitor_visit_if_stmt(struct Visitor* visitor,
                                     struct IfStmtIr* ir) {
    bool changed = false;
    struct ExprIr* cond = visitor_visit_expr(visitor, ir_if_stmt_cond_expr(ir));
    if (cond) {
        ir_if_stmt_set_cond_expr(ir, cond);
        changed = true;
    }
    struct StmtIr* true_stmt =
        visitor_visit_stmt(visitor, ir_if_stmt_true_stmt(ir));
    if (true_stmt) {
        ir_if_stmt_set_true_stmt(ir, true_stmt);
        changed = true;
    }
    struct StmtIr* false_stmt =
        visitor_visit_stmt(visitor, ir_if_stmt_false_stmt(ir));
    if (false_stmt) {
        ir_if_stmt_set_false_stmt(ir, false_stmt);
        changed = true;
    }
    return changed ? ir_if_stmt_super(ir) : NULL;
}

struct StmtIr* visitor_visit_return_stmt(struct Visitor* visitor,
                                         struct ReturnStmtIr* ir) {
    struct ExprIr* expr = ir_return_stmt_expr(ir);
    if (!expr) return NULL;
    expr = visitor_visit_expr(visitor, expr);
    if (expr) {
        ir_return_stmt_set_expr(ir, expr);
        return ir_return_stmt_super(ir);
    } else
        return NULL;
}

// ToDo: for refactoring
struct StmtIr* visitor_visit_cf_stmt(struct Visitor* visitor,
                                     struct CfStmtIr* ir) {
    struct CfIr* cf = visitor_visit_cf(visitor, ir_cf_stmt_cf(ir));
    if (cf) {
        ir_cf_stmt_set_cf(ir, cf);
        return ir_cf_stmt_super(ir);
    } else
        return NULL;
}

void visitor_initialize(struct Visitor* visitor, struct Context* context) {
    visitor->context = context;

    register_visitor(*visitor, visit_const_expr, NULL);
    register_visitor(*visitor, visit_binop_expr, NULL);
    register_visitor(*visitor, visit_call_expr, NULL);
    register_visitor(*visitor, visit_var_expr, NULL);
    register_visitor(*visitor, visit_unop_expr, NULL);
    register_visitor(*visitor, visit_subst_expr, NULL);
    register_visitor(*visitor, visit_member_expr, NULL);
    register_visitor(*visitor, visit_deref_expr, NULL);
    register_visitor(*visitor, visit_addrof_expr, NULL);
    register_visitor(*visitor, visit_cast_expr, NULL);

    register_visitor(*visitor, visit_stmt_pre, visitor_visit_stmt_pre);

    register_visitor(*visitor, visit_expr_stmt, visitor_visit_expr_stmt);
    register_visitor(*visitor, visit_block_stmt, visitor_visit_block_stmt);
    register_visitor(*visitor, visit_if_stmt, visitor_visit_if_stmt);
    register_visitor(*visitor, visit_return_stmt, visitor_visit_return_stmt);
    register_visitor(*visitor, visit_cf_stmt, visitor_visit_cf_stmt);
    register_visitor(*visitor, visit_push_stmt, NULL);
    register_visitor(*visitor, visit_pop_stmt, NULL);

    register_visitor(*visitor, visit_function, NULL);
    register_visitor(*visitor, visit_return_cf, NULL);
}

struct Context* visitor_context(struct Visitor* visitor) {
    return visitor->context;
}

void visitor_apply(struct Visitor* visitor) {
    for (struct ListHeader *
             it = context_function_declaration_begin(visitor->context),
            *eit = context_function_declaration_end(visitor->context);
         it != eit; it = list_next(it)) {
        struct MapEntry* map_entry = (struct MapEntry*)it;
        struct FunctionIr* func = map_entry_value(map_entry);
        func = visitor_visit_function(visitor, func);
        if (func) map_entry_set_value(map_entry, func);
    }
}
