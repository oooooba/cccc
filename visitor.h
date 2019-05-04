#ifndef VISITOR_H
#define VISITOR_H

#include "ast.h"
#include "ir.h"

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

struct Visitor2 {
    struct ExprIr* (*visit_const_expr)(struct Visitor2* visitor,
                                       struct ConstExprIr* ir);
    struct ExprIr* (*visit_binop_expr)(struct Visitor2* visitor,
                                       struct BinopExprIr* ir);
    struct ExprIr* (*visit_addrof_expr)(struct Visitor2* visitor,
                                        struct AddrofExprIr* ir);
    struct ExprIr* (*visit_load_expr)(struct Visitor2* visitor,
                                      struct LoadExprIr* ir);
    struct ExprIr* (*visit_store_expr)(struct Visitor2* visitor,
                                       struct StoreExprIr* ir);
    struct BlockIr* (*visit_block)(struct Visitor2* visitor,
                                   struct BlockIr* ir);
    struct FunctionIr* (*visit_function)(struct Visitor2* visitor,
                                         struct FunctionIr* ir);
    struct CfIr* (*visit_branch_cf)(struct Visitor2* visitor,
                                    struct BranchCfIr* ir);
};

struct Ir* visitor2_visit_ir(struct Visitor2* visitor, struct Ir* ir);

struct ExprIr* visitor2_visit_expr(struct Visitor2* visitor, struct ExprIr* ir);

struct BlockIr* visitor2_visit_block(struct Visitor2* visitor,
                                     struct BlockIr* ir);

struct FunctionIr* visitor2_visit_function(struct Visitor2* visitor,
                                           struct FunctionIr* ir);

struct CfIr* visitor2_visit_cf(struct Visitor2* visitor, struct CfIr* ir);

void visitor2_initialize(struct Visitor2* visitor);

#define register_visitor(obj, member, proc) \
    do {                                    \
        (obj).member = (void*)(proc);       \
    } while (0)

#endif
