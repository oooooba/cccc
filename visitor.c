#include "visitor.h"
#include "ast.h"
#include "ir.h"

#include <assert.h>
#include <stdbool.h>

struct ExprNode* visitor_visit_expr(struct Visitor* visitor,
                                    struct ExprNode* node) {
    switch (ast_expr_tag(node)) {
        case ExprNodeTag_Binop:
            return visitor->visit_binop_expr(visitor,
                                             (struct BinopExprNode*)node);
        case ExprNodeTag_Uniop:
            return visitor->visit_uniop_expr(visitor,
                                             (struct UniopExprNode*)node);
        case ExprNodeTag_Literal:
            return visitor->visit_literal_expr(visitor,
                                               (struct LiteralExprNode*)node);
        case ExprNodeTag_Var:
            return visitor->visit_var_expr(visitor, (struct VarExprNode*)node);
        default:
            assert(false);
    }
}

struct StmtNode* visitor_visit_stmt(struct Visitor* visitor,
                                    struct StmtNode* node) {
    switch (ast_stmt_tag(node)) {
        case StmtNodeTag_Expr:
            return visitor->visit_expr_stmt(visitor,
                                            (struct ExprStmtNode*)node);
        case StmtNodeTag_Block:
            return visitor->visit_block_stmt(visitor,
                                             (struct BlockStmtNode*)node);
        case StmtNodeTag_If:
            return visitor->visit_if_stmt(visitor, (struct IfStmtNode*)node);
        case StmtNodeTag_Decl:
            return visitor->visit_decl_stmt(visitor,
                                            (struct DeclStmtNode*)node);
        default:
            assert(false);
    }
}

void visitor_initialize(struct Visitor* visitor) {
    register_visitor(*visitor, visit_literal_expr, NULL);
    register_visitor(*visitor, visit_binop_expr, NULL);
    register_visitor(*visitor, visit_var_expr, NULL);
    register_visitor(*visitor, visit_expr_stmt, NULL);
    register_visitor(*visitor, visit_block_stmt, NULL);
    register_visitor(*visitor, visit_if_stmt, NULL);
    register_visitor(*visitor, visit_decl_stmt, NULL);
}

struct ExprIr* visitor2_visit_expr(struct Visitor2* visitor,
                                   struct ExprIr* ir) {
    switch (ir_expr_tag(ir)) {
        case ExprIrTag_Const:
            return visitor->visit_const_expr(visitor, ir_expr_as_const(ir));
        case ExprIrTag_Binop:
            return visitor->visit_binop_expr(visitor, ir_expr_as_binop(ir));
        default:
            assert(false);
    }
    return NULL;
}

struct BlockIr* visitor2_visit_block(struct Visitor2* visitor,
                                     struct BlockIr* block) {
    struct BlockIterator* it = ir_block_new_iterator(block);
    bool modified = false;
    for (;;) {
        struct Ir* stmt = ir_block_iterator_next(it);
        if (!stmt) break;
        struct Ir* new_stmt;
        switch (ir_tag(stmt)) {
            case IrTag_Expr:
                new_stmt = ir_expr_cast(
                    visitor2_visit_expr(visitor, ir_as_expr(stmt)));
                if (visitor->visit_block_iterate_post)
                    new_stmt = visitor->visit_block_iterate_post(visitor, block,
                                                                 new_stmt);
                break;
            case IrTag_Block:
                new_stmt = ir_block_cast(
                    visitor2_visit_block(visitor, ir_as_block(stmt)));
                break;
            default:
                assert(false);
        }
        if (new_stmt) {
            ir_block_iterator_swap_at(it, new_stmt);
            modified = true;
        }
    }
    return modified ? block : NULL;
}

void visitor2_initialize(struct Visitor2* visitor) {
    register_visitor(*visitor, visit_const_expr, NULL);
    register_visitor(*visitor, visit_binop_expr, NULL);
    register_visitor(*visitor, visit_block_iterate_post, NULL);
}
