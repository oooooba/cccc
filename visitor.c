#include "visitor.h"
#include "ast.h"

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
