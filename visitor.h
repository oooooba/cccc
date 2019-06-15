#ifndef VISITOR_H
#define VISITOR_H

#include "ir.h"

struct Visitor {
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
    struct BlockIr* (*visit_block)(struct Visitor* visitor, struct BlockIr* ir);

    struct StmtIr* (*visit_expr_stmt)(struct Visitor* visitor,
                                      struct ExprStmtIr* ir);
    struct StmtIr* (*visit_block_stmt)(struct Visitor* visitor,
                                       struct BlockStmtIr* ir);
    // ToDo: for refactoring
    struct StmtIr* (*visit_cf_stmt)(struct Visitor* visitor,
                                    struct CfStmtIr* ir);

    struct FunctionIr* (*visit_function)(struct Visitor* visitor,
                                         struct FunctionIr* ir);
    struct CfIr* (*visit_branch_cf)(struct Visitor* visitor,
                                    struct BranchCfIr* ir);
    struct CfIr* (*visit_return_cf)(struct Visitor* visitor,
                                    struct ReturnCfIr* ir);
    struct CfIr* (*visit_label_cf)(struct Visitor* visitor,
                                   struct LabelCfIr* ir);
    struct CfIr* (*visit_push_cf)(struct Visitor* visitor, struct PushCfIr* ir);
    struct CfIr* (*visit_pop_cf)(struct Visitor* visitor, struct PopCfIr* ir);
};

struct Ir* visitor_visit_ir(struct Visitor* visitor, struct Ir* ir);

struct ExprIr* visitor_visit_expr(struct Visitor* visitor, struct ExprIr* ir);

struct BlockIr* visitor_visit_block(struct Visitor* visitor,
                                    struct BlockIr* ir);

struct StmtIr* visitor_visit_stmt(struct Visitor* visitor, struct StmtIr* ir);

struct FunctionIr* visitor_visit_function(struct Visitor* visitor,
                                          struct FunctionIr* ir);

struct CfIr* visitor_visit_cf(struct Visitor* visitor, struct CfIr* ir);

struct StmtIr* visitor_visit_expr_stmt(struct Visitor* visitor,
                                       struct ExprStmtIr* ir);
struct StmtIr* visitor_visit_block_stmt(struct Visitor* visitor,
                                        struct BlockStmtIr* ir);

struct BlockStmtIr*
visitor_convert_block_to_block_stmt_only_used_for_refactoring(
    struct BlockIr* ir);

void visitor_initialize(struct Visitor* visitor);

#define register_visitor(obj, member, proc) \
    do {                                    \
        (obj).member = (void*)(proc);       \
    } while (0)

#endif
