#include "visitor.h"
#include "ir.h"

#include <assert.h>
#include <stdbool.h>

struct Ir* visitor_visit_ir(struct Visitor* visitor, struct Ir* ir) {
    switch (ir_tag(ir)) {
        case IrTag_Expr:
            return ir_expr_cast(visitor_visit_expr(visitor, ir_as_expr(ir)));
        case IrTag_Block:
            return ir_block_cast(visitor_visit_block(visitor, ir_as_block(ir)));
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

struct StmtIr* visitor_visit_stmt(struct Visitor* visitor, struct StmtIr* ir) {
    struct StmtIr* stmt;
    switch (ir_stmt_tag(ir)) {
        case StmtIrTag_Expr:
            stmt = visitor->visit_expr_stmt(visitor, ir_stmt_as_expr(ir));
            break;
        case StmtIrTag_Block:
            stmt = visitor->visit_block_stmt(visitor, ir_stmt_as_block(ir));
            break;
        case StmtIrTag_Cf:
            stmt = visitor->visit_cf_stmt(visitor, ir_stmt_as_cf(ir));
            break;
        default:
            assert(false);
            stmt = NULL;
    }
    return stmt;
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
        list_item->item = visitor_visit_stmt(visitor, stmt);
    }
    return ir_block_stmt_super(ir);
}

// ToDo: for refactoring
struct StmtIr* visitor_visit_cf_stmt(struct Visitor* visitor,
                                     struct CfStmtIr* ir) {
    struct CfIr* cf = visitor_visit_cf(visitor, ir_cf_stmt_cf(ir));
    ir_cf_stmt_set_cf(ir, cf);
    return ir_cf_stmt_super(ir);
}

void visitor_initialize(struct Visitor* visitor) {
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

    register_visitor(*visitor, visit_expr_stmt, visitor_visit_expr_stmt);
    register_visitor(*visitor, visit_block_stmt, visitor_visit_block_stmt);
    register_visitor(*visitor, visit_cf_stmt, visitor_visit_cf_stmt);

    register_visitor(*visitor, visit_block, NULL);
    register_visitor(*visitor, visit_function, NULL);
    register_visitor(*visitor, visit_branch_cf, NULL);
    register_visitor(*visitor, visit_return_cf, NULL);
    register_visitor(*visitor, visit_label_cf, NULL);
    register_visitor(*visitor, visit_push_cf, NULL);
    register_visitor(*visitor, visit_pop_cf, NULL);
}
