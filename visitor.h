#ifndef VISITOR_H
#define VISITOR_H

#include "context.h"
#include "ir.h"

struct Visitor {
    struct Context* context;

    struct ExprIr* (*visit_const_expr)(struct Visitor* visitor,
                                       struct ConstExprIr* ir);
    struct ExprIr* (*visit_binop_expr)(struct Visitor* visitor,
                                       struct BinopExprIr* ir);
    struct ExprIr* (*visit_call_expr)(struct Visitor* visitor,
                                      struct CallExprIr* ir);
    struct ExprIr* (*visit_var_expr)(struct Visitor* visitor,
                                     struct VarExprIr* ir);
    struct ExprIr* (*visit_unop_expr)(struct Visitor* visitor,
                                      struct UnopExprIr* ir);
    struct ExprIr* (*visit_subst_expr)(struct Visitor* visitor,
                                       struct SubstExprIr* ir);
    struct ExprIr* (*visit_member_expr)(struct Visitor* visitor,
                                        struct MemberExprIr* ir);
    struct ExprIr* (*visit_deref_expr)(struct Visitor* visitor,
                                       struct DerefExprIr* ir);
    struct ExprIr* (*visit_addrof_expr)(struct Visitor* visitor,
                                        struct AddrofExprIr* ir);
    struct ExprIr* (*visit_cast_expr)(struct Visitor* visitor,
                                      struct CastExprIr* ir);

    struct StmtIr* (*visit_stmt_pre)(struct Visitor* visitor,
                                     struct StmtIr* ir);

    struct StmtIr* (*visit_expr_stmt)(struct Visitor* visitor,
                                      struct ExprStmtIr* ir);
    struct StmtIr* (*visit_block_stmt)(struct Visitor* visitor,
                                       struct BlockStmtIr* ir);
    struct StmtIr* (*visit_if_stmt)(struct Visitor* visitor,
                                    struct IfStmtIr* ir);
    struct StmtIr* (*visit_return_stmt)(struct Visitor* visitor,
                                        struct ReturnStmtIr* ir);

    struct StmtIr* (*visit_push_stmt)(struct Visitor* visitor,
                                      struct PushStmtIr* ir);
    struct StmtIr* (*visit_pop_stmt)(struct Visitor* visitor,
                                     struct PopStmtIr* ir);

    struct FunctionIr* (*visit_function)(struct Visitor* visitor,
                                         struct FunctionIr* ir);
};

struct Ir* visitor_visit_ir(struct Visitor* visitor, struct Ir* ir);

struct ExprIr* visitor_visit_expr(struct Visitor* visitor, struct ExprIr* ir);

struct StmtIr* visitor_visit_stmt(struct Visitor* visitor, struct StmtIr* ir);

struct FunctionIr* visitor_visit_function(struct Visitor* visitor,
                                          struct FunctionIr* ir);

struct StmtIr* visitor_visit_expr_stmt(struct Visitor* visitor,
                                       struct ExprStmtIr* ir);
struct StmtIr* visitor_visit_block_stmt(struct Visitor* visitor,
                                        struct BlockStmtIr* ir);

void visitor_initialize(struct Visitor* visitor, struct Context* context);
struct Context* visitor_context(struct Visitor* visitor);
void visitor_apply(struct Visitor* visitor);

#define register_visitor(obj, member, proc) \
    do {                                    \
        (obj).member = (void*)(proc);       \
    } while (0)

#endif
