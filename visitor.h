#ifndef VISITOR_H
#define VISITOR_H

#include "ast.h"

struct Visitor {
    struct ExprNode* (*visit_literal_expr)(struct Visitor* visitor,
                                           struct LiteralExprNode* node);
    struct ExprNode* (*visit_binop_expr)(struct Visitor* visitor,
                                         struct BinopExprNode* node);
    struct ExprNode* (*visit_uniop_expr)(struct Visitor* visitor,
                                         struct UniopExprNode* node);
    struct ExprNode* (*visit_var_expr)(struct Visitor* visitor,
                                       struct VarExprNode* node);
    struct StmtNode* (*visit_expr_stmt)(struct Visitor* visitor,
                                        struct ExprStmtNode* node);
    struct StmtNode* (*visit_block_stmt)(struct Visitor* visitor,
                                         struct BlockStmtNode* node);
    struct StmtNode* (*visit_if_stmt)(struct Visitor* visitor,
                                      struct IfStmtNode* node);
    struct StmtNode* (*visit_decl_stmt)(struct Visitor* visitor,
                                        struct DeclStmtNode* node);
};

struct ExprNode* visitor_visit_expr(struct Visitor* visitor,
                                    struct ExprNode* node);
struct StmtNode* visitor_visit_stmt(struct Visitor* visitor,
                                    struct StmtNode* node);

void visitor_initialize(struct Visitor* visitor);

#define register_visitor(obj, member, proc) \
    do {                                    \
        (obj).member = (void*)(proc);       \
    } while (0)

#endif
