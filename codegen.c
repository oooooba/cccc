#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct CodegenVisitor2 {
    struct Visitor2 as_visitor;
    struct Context* context;
    FILE* stream;
};

static struct Visitor2* as_visitor(struct CodegenVisitor2* visitor) {
    return &visitor->as_visitor;
}

static const char* register_name(struct CodegenVisitor2* visitor,
                                 strtable_id index) {
    const char* reg = strtable_at(&visitor->context->strtable, index);
    assert(reg[0] == '%');
    return reg + 1;
}

static struct ExprIr* visit_const_expr2(struct CodegenVisitor2* visitor,
                                        struct ConstExprIr* ir) {
    strtable_id dst_reg_id = ir_expr_reg_id(ir_const_expr_cast(ir));
    const char* dst_reg = register_name(visitor, dst_reg_id);

    switch (ir_const_expr_tag(ir)) {
        case ConstExprIrTag_Integer:
            fprintf(visitor->stream, "\tmov\t%s, %ld\n", dst_reg,
                    ir_const_expr_integer_value(ir));
            break;
        case ConstExprIrTag_Register: {
            strtable_id src_reg_id = ir_const_expr_register_id(ir);
            if (src_reg_id == dst_reg_id) break;
            const char* src_reg = register_name(visitor, src_reg_id);
            fprintf(visitor->stream, "\tmov\t%s, %s\n", dst_reg, src_reg);
        } break;
        default:
            assert(false);
    }

    return NULL;
}

static struct ExprIr* visit_binop_expr2(struct CodegenVisitor2* visitor,
                                        struct BinopExprIr* ir) {
    struct ExprIr* lhs = ir_binop_expr_lhs(ir);
    struct ExprIr* rhs = ir_binop_expr_rhs(ir);
    visitor2_visit_expr(as_visitor(visitor), lhs);
    visitor2_visit_expr(as_visitor(visitor), rhs);

    strtable_id lhs_reg_id = ir_expr_reg_id(lhs);
    strtable_id rhs_reg_id = ir_expr_reg_id(rhs);
    strtable_id result_reg_id = ir_expr_reg_id(ir_binop_expr_cast(ir));
    assert(lhs_reg_id == result_reg_id);

    const char* rhs_reg = register_name(visitor, rhs_reg_id);
    const char* result_reg = register_name(visitor, result_reg_id);

    const char* op;
    switch (ir_binop_expr_op(ir)) {
        case BinopExprIrTag_Add:
            op = "add";
            break;
        case BinopExprIrTag_Sub:
            op = "sub";
            break;
        case BinopExprIrTag_Mul:
            op = "imul";
            break;
        default:
            assert(false);
    }

    fprintf(visitor->stream, "\t%s\t%s, %s\n", op, result_reg, rhs_reg);
    return NULL;
}

static struct ExprIr* visit_addrof_expr2(struct CodegenVisitor2* visitor,
                                         struct AddrofExprIr* ir) {
    assert(ir_addrof_expr_tag(ir) == AddrTag_Var);
    struct VarIr* var = ir_addrof_expr_operand_as_var(ir);
    size_t offset = ir_var_offset(var);
    strtable_id reg_id = ir_expr_reg_id(ir_addrof_expr_cast(ir));
    const char* reg = register_name(visitor, reg_id);
    fprintf(visitor->stream, "\tlea\t%s, [rbp - %ld]\n", reg, offset);
    return NULL;
}

static struct ExprIr* visit_load_expr2(struct CodegenVisitor2* visitor,
                                       struct LoadExprIr* ir) {
    struct ExprIr* addr = ir_load_expr_addr(ir);
    visitor2_visit_expr(as_visitor(visitor), addr);

    strtable_id addr_reg_id = ir_expr_reg_id(addr);
    strtable_id result_reg_id = ir_expr_reg_id(ir_load_expr_cast(ir));

    const char* addr_reg = register_name(visitor, addr_reg_id);
    const char* result_reg = register_name(visitor, result_reg_id);

    fprintf(visitor->stream, "\tmov\t%s, [%s]\n", result_reg, addr_reg);
    return NULL;
}

static struct ExprIr* visit_store_expr2(struct CodegenVisitor2* visitor,
                                        struct StoreExprIr* ir) {
    struct ExprIr* addr = ir_store_expr_addr(ir);
    struct ExprIr* value = ir_store_expr_value(ir);
    visitor2_visit_expr(as_visitor(visitor), addr);
    visitor2_visit_expr(as_visitor(visitor), value);

    strtable_id addr_reg_id = ir_expr_reg_id(addr);
    strtable_id value_reg_id = ir_expr_reg_id(value);

    const char* addr_reg = register_name(visitor, addr_reg_id);
    const char* value_reg = register_name(visitor, value_reg_id);

    fprintf(visitor->stream, "\tmov\t[%s], %s\n", addr_reg, value_reg);
    return NULL;
}

static struct BlockIr* visit_block2(struct CodegenVisitor2* visitor,
                                    struct BlockIr* ir) {
    fprintf(visitor->stream, "lab_%p:\n", ir);
    struct BlockIterator* it = ir_block_new_iterator(ir);
    for (;;) {
        struct Ir* stmt = ir_block_iterator_next(it);
        if (!stmt) break;

        visitor2_visit_ir(as_visitor(visitor), stmt);
    }
    return NULL;
}

static struct FunctionIr* visit_function2(struct CodegenVisitor2* visitor,
                                          struct FunctionIr* ir) {
    fprintf(visitor->stream, "\n");

    const char* name =
        strtable_at(&visitor->context->strtable, ir_function_name_index(ir));
    fprintf(visitor->stream, ".global %s\n", name);
    fprintf(visitor->stream, "%s:\n", name);

    fprintf(stderr, "codegen: 0x%p: %s\n", ir, name);

    fprintf(visitor->stream, "\tpush\trbp\n");

    struct BlockIr* body = ir_function_body(ir);
    visitor2_visit_block(as_visitor(visitor), body);

    fprintf(visitor->stream, "\tpop\trbp\n");
    fprintf(visitor->stream, "\tret\n");
    return NULL;
}

static struct CfIr* visit_branch_cf2(struct CodegenVisitor2* visitor,
                                     struct BranchCfIr* ir) {
    struct ExprIr* cond_expr = ir_branch_cf_cond_expr(ir);
    visitor2_visit_expr(as_visitor(visitor), cond_expr);

    strtable_id cond_reg_id = ir_expr_reg_id(cond_expr);
    const char* cond_reg = register_name(visitor, cond_reg_id);

    fprintf(visitor->stream, "\tand\t%s, %s\n", cond_reg, cond_reg);
    fprintf(visitor->stream, "\tjz\tlab_%p_else\n", ir);

    visitor2_visit_block(as_visitor(visitor), ir_branch_cf_true_block(ir));
    fprintf(visitor->stream, "\tjmp\tlab_%p_cont\n", ir);

    fprintf(visitor->stream, "lab_%p_else:\n", ir);
    visitor2_visit_block(as_visitor(visitor), ir_branch_cf_false_block(ir));

    fprintf(visitor->stream, "lab_%p_cont:\n", ir);
    return NULL;
}

struct CodegenVisitor2* new_codegen_visitor(struct Context* context,
                                            FILE* stream) {
    struct CodegenVisitor2* visitor = malloc(sizeof(struct CodegenVisitor2));
    visitor2_initialize(as_visitor(visitor));

    register_visitor(visitor->as_visitor, visit_const_expr, visit_const_expr2);
    register_visitor(visitor->as_visitor, visit_binop_expr, visit_binop_expr2);
    register_visitor(visitor->as_visitor, visit_addrof_expr,
                     visit_addrof_expr2);
    register_visitor(visitor->as_visitor, visit_load_expr, visit_load_expr2);
    register_visitor(visitor->as_visitor, visit_store_expr, visit_store_expr2);
    register_visitor(visitor->as_visitor, visit_block, visit_block2);
    register_visitor(visitor->as_visitor, visit_function, visit_function2);
    register_visitor(visitor->as_visitor, visit_branch_cf, visit_branch_cf2);

    visitor->context = context;
    visitor->stream = stream;

    fprintf(stream, ".intel_syntax noprefix\n");
    fprintf(stream, ".text\n");

    return visitor;
}

void codegen_apply(struct CodegenVisitor2* visitor, struct BlockIr* ir) {
    visitor2_visit_block(as_visitor(visitor), ir);
}
