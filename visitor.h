#ifndef VISITOR_H
#define VISITOR_H

#include "ir.h"

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
    struct ExprIr* (*visit_call_expr)(struct Visitor2* visitor,
                                      struct CallExprIr* ir);
    struct BlockIr* (*visit_block)(struct Visitor2* visitor,
                                   struct BlockIr* ir);
    struct FunctionIr* (*visit_function)(struct Visitor2* visitor,
                                         struct FunctionIr* ir);
    struct CfIr* (*visit_branch_cf)(struct Visitor2* visitor,
                                    struct BranchCfIr* ir);
    struct CfIr* (*visit_push_cf)(struct Visitor2* visitor,
                                  struct PushCfIr* ir);
    struct CfIr* (*visit_pop_cf)(struct Visitor2* visitor, struct PopCfIr* ir);
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
