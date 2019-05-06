#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct DumpVisitor {
    struct Visitor2 as_visitor;
    struct Context* context;
    FILE* stream;
};

static struct Visitor2* as_visitor(struct DumpVisitor* visitor) {
    return &visitor->as_visitor;
}

static struct ExprIr* visit_const_expr2(struct DumpVisitor* visitor,
                                        struct ConstExprIr* ir) {
    fprintf(visitor->stream, "v%p = %ld\n", ir,
            ir_const_expr_integer_value(ir));
    return NULL;
}

static struct ExprIr* visit_binop_expr2(struct DumpVisitor* visitor,
                                        struct BinopExprIr* ir) {
    struct ExprIr* lhs = ir_binop_expr_lhs(ir);
    struct ExprIr* rhs = ir_binop_expr_rhs(ir);
    visitor2_visit_expr(as_visitor(visitor), lhs);
    visitor2_visit_expr(as_visitor(visitor), rhs);
    const char* op;
    switch (ir_binop_expr_op(ir)) {
        case BinopExprIrTag_Add:
            op = "add";
            break;
        case BinopExprIrTag_Sub:
            op = "sub";
            break;
        case BinopExprIrTag_Mul:
            op = "mul";
            break;
        default:
            assert(false);
    }
    fprintf(visitor->stream, "v%p = %s v%p, v%p\n", ir, op, lhs, rhs);
    return NULL;
}

static struct ExprIr* visit_addrof_expr2(struct DumpVisitor* visitor,
                                         struct AddrofExprIr* ir) {
    if (ir_addrof_expr_tag(ir) == AddrTag_Var) {
        fprintf(visitor->stream, "v%p = addrof [%s]\n", ir,
                strtable_at(&visitor->context->strtable,
                            ir_var_index(ir_addrof_expr_operand_as_var(ir))));
    } else {
        struct ExprIr* expr = ir_addrof_expr_operand_as_expr(ir);
        visitor2_visit_expr(as_visitor(visitor), expr);
        fprintf(visitor->stream, "v%p = addrof [v%p]\n", ir, expr);
    }
    return NULL;
}

static struct ExprIr* visit_load_expr2(struct DumpVisitor* visitor,
                                       struct LoadExprIr* ir) {
    struct ExprIr* addr = ir_load_expr_addr(ir);
    visitor2_visit_expr(as_visitor(visitor), addr);
    fprintf(visitor->stream, "v%p = load [v%p]\n", ir, addr);
    return NULL;
}

static struct ExprIr* visit_store_expr2(struct DumpVisitor* visitor,
                                        struct StoreExprIr* ir) {
    struct ExprIr* addr = ir_store_expr_addr(ir);
    struct ExprIr* value = ir_store_expr_value(ir);
    visitor2_visit_expr(as_visitor(visitor), addr);
    visitor2_visit_expr(as_visitor(visitor), value);
    fprintf(visitor->stream, "v%p = store [v%p] v%p\n", ir, addr, value);
    return NULL;
}

static struct ExprIr* visit_call_expr2(struct DumpVisitor* visitor,
                                       struct CallExprIr* ir) {
    assert(ir_call_expr_tag(ir) == AddrTag_Var);

    for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                           *eit = list_end(ir_call_expr_args(ir));
         it != eit; it = list_next(it)) {
        struct ExprIr* arg = ((struct ListItem*)it)->item;
        visitor2_visit_expr(as_visitor(visitor), arg);
    }

    strtable_id name_id = ir_var_index(ir_call_expr_var(ir));
    const char* name = strtable_at(&visitor->context->strtable, name_id);
    fprintf(visitor->stream, "v%p = call %s (", ir, name);
    bool first = true;
    for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                           *eit = list_end(ir_call_expr_args(ir));
         it != eit; it = list_next(it)) {
        struct ExprIr* arg = ((struct ListItem*)it)->item;
        if (first)
            first = false;
        else
            fprintf(visitor->stream, ", ");
        fprintf(visitor->stream, "v%p", arg);
    }
    fprintf(visitor->stream, ")\n");
    return NULL;
}

static struct BlockIr* visit_block2(struct DumpVisitor* visitor,
                                    struct BlockIr* ir) {
    fprintf(visitor->stream, "[@%p]{\n", ir);
    struct BlockIterator* it = ir_block_new_iterator(ir);
    for (;;) {
        struct Ir* stmt = ir_block_iterator_next(it);
        if (!stmt) break;

        visitor2_visit_ir(as_visitor(visitor), stmt);
    }
    fprintf(visitor->stream, "}\n");
    return NULL;
}

static struct FunctionIr* visit_function2(struct DumpVisitor* visitor,
                                          struct FunctionIr* ir) {
    const char* name =
        strtable_at(&visitor->context->strtable, ir_function_name_index(ir));
    struct BlockIr* body = ir_function_body(ir);
    fprintf(visitor->stream, "function %s (", name);
    bool first = true;
    for (struct ListHeader *it = list_begin(ir_function_params(ir)),
                           *eit = list_end(ir_function_params(ir));
         it != eit; it = list_next(it)) {
        struct VarIr* var = ((struct ListItem*)it)->item;
        const char* var_name =
            strtable_at(&visitor->context->strtable, ir_var_index(var));
        if (first)
            first = false;
        else
            fprintf(visitor->stream, ", ");
        fprintf(visitor->stream, "%s", var_name);
    }
    fprintf(visitor->stream, ") ");
    visitor2_visit_block(as_visitor(visitor), body);
    return NULL;
}

static struct CfIr* visit_branch_cf2(struct DumpVisitor* visitor,
                                     struct BranchCfIr* ir) {
    struct ExprIr* cond_expr = ir_branch_cf_cond_expr(ir);
    visitor2_visit_expr(as_visitor(visitor), cond_expr);
    fprintf(visitor->stream, "if (v%p) ", cond_expr);

    struct BlockIr* true_block = ir_branch_cf_true_block(ir);
    visitor2_visit_block(as_visitor(visitor), true_block);

    struct BlockIr* false_block = ir_branch_cf_false_block(ir);
    if (false_block) visitor2_visit_block(as_visitor(visitor), false_block);

    return NULL;
}

struct DumpVisitor* new_dump_visitor(struct Context* context, FILE* stream) {
    struct DumpVisitor* visitor = malloc(sizeof(struct DumpVisitor));
    visitor2_initialize(as_visitor(visitor));

    register_visitor(visitor->as_visitor, visit_const_expr, visit_const_expr2);
    register_visitor(visitor->as_visitor, visit_binop_expr, visit_binop_expr2);
    register_visitor(visitor->as_visitor, visit_addrof_expr,
                     visit_addrof_expr2);
    register_visitor(visitor->as_visitor, visit_load_expr, visit_load_expr2);
    register_visitor(visitor->as_visitor, visit_store_expr, visit_store_expr2);
    register_visitor(visitor->as_visitor, visit_call_expr, visit_call_expr2);
    register_visitor(visitor->as_visitor, visit_block, visit_block2);
    register_visitor(visitor->as_visitor, visit_function, visit_function2);
    register_visitor(visitor->as_visitor, visit_branch_cf, visit_branch_cf2);

    visitor->context = context;
    visitor->stream = stream;
    return visitor;
}

void dump_apply(struct DumpVisitor* visitor, struct BlockIr* ir) {
    visitor2_visit_block(as_visitor(visitor), ir);
}
