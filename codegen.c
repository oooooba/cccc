#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct CodegenVisitor {
    struct Visitor as_visitor;
    FILE* stream;
    struct FunctionIr* function;
};

static struct Visitor* as_visitor(struct CodegenVisitor* visitor) {
    return &visitor->as_visitor;
}

static struct Context* ctx(struct CodegenVisitor* visitor) {
    return visitor_context(as_visitor(visitor));
}

static const char* register_name(struct CodegenVisitor* visitor,
                                 strtable_id index) {
    const char* reg = strtable_at(&ctx(visitor)->strtable, index);
    assert(reg[0] == '%');
    return reg + 1;
}

static struct ExprIr* visit_const_expr2(struct CodegenVisitor* visitor,
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

static struct ExprIr* visit_binop_expr2(struct CodegenVisitor* visitor,
                                        struct BinopExprIr* ir) {
    struct ExprIr* lhs = ir_binop_expr_lhs(ir);
    struct ExprIr* rhs = ir_binop_expr_rhs(ir);
    visitor_visit_expr(as_visitor(visitor), lhs);
    visitor_visit_expr(as_visitor(visitor), rhs);

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

    if (ir_expr_type(lhs) == NULL ||
        type_tag(ir_expr_type(lhs)) != Type_Pointer)
        fprintf(visitor->stream, "\t%s\t%s, %s\n", op, result_reg, rhs_reg);
    else {
        struct TypeIr* elem_type =
            type_pointer_elem_type(type_as_pointer(ir_expr_type(lhs)));
        fprintf(visitor->stream, "\timul\t%s, %ld\n", rhs_reg,
                type_size(elem_type));
        fprintf(visitor->stream, "\t%s\t%s, %s\n", op, result_reg, rhs_reg);
    }
    return NULL;
}

static struct ExprIr* visit_call_expr2(struct CodegenVisitor* visitor,
                                       struct CallExprIr* ir) {
    struct VarExprIr* func_name = ir_expr_as_var(ir_call_expr_function(ir));
    assert(func_name);

    for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                           *eit = list_end(ir_call_expr_args(ir));
         it != eit; it = list_next(it)) {
        struct ExprIr* arg = ((struct ListItem*)it)->item;
        visitor_visit_expr(as_visitor(visitor), arg);
    }

    visitor_visit_stmt(as_visitor(visitor),
                       ir_block_stmt_super(ir_call_expr_pre_expr_block(ir)));

    strtable_id name_id = ir_var_expr_index(func_name);
    const char* name = strtable_at(&ctx(visitor)->strtable, name_id);
    fprintf(visitor->stream, "\tcall\t%s", name);

    // debug code
    {
        fprintf(visitor->stream, "\t# ( ");
        for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                               *eit = list_end(ir_call_expr_args(ir));
             it != eit; it = list_next(it)) {
            struct ExprIr* arg = ((struct ListItem*)it)->item;
            fprintf(visitor->stream, "%s ",
                    strtable_at(&ctx(visitor)->strtable, ir_expr_reg_id(arg)));
        }
        fprintf(visitor->stream, ") => %s",
                strtable_at(&ctx(visitor)->strtable,
                            ir_expr_reg_id(ir_call_expr_cast(ir))));
        fprintf(visitor->stream, "\n");
    }

    visitor_visit_stmt(as_visitor(visitor),
                       ir_block_stmt_super(ir_call_expr_post_expr_block(ir)));

    return NULL;
}

static struct ExprIr* visit_var_expr2(struct CodegenVisitor* visitor,
                                      struct VarExprIr* ir) {
    size_t offset = ir_var_expr_offset(ir);
    strtable_id reg_id = ir_expr_reg_id(ir_var_expr_cast(ir));
    const char* reg = register_name(visitor, reg_id);
    fprintf(visitor->stream, "\tlea\t%s, [rbp - %ld]\n", reg, offset);
    return NULL;
}

static struct ExprIr* visit_unop_expr2(struct CodegenVisitor* visitor,
                                       struct UnopExprIr* ir) {
    (void)visitor;
    (void)ir;
    assert(false);
    return NULL;
}

static struct ExprIr* visit_member_expr2(struct CodegenVisitor* visitor,
                                         struct MemberExprIr* ir) {
    struct ExprIr* base = ir_member_expr_base(ir);
    visitor_visit_expr(as_visitor(visitor), base);

    strtable_id base_reg_id = ir_expr_reg_id(base);
    strtable_id result_reg_id = ir_expr_reg_id(ir_member_expr_cast(ir));
    assert(base_reg_id == result_reg_id);

    const char* result_reg = register_name(visitor, result_reg_id);
    size_t offset = ir_member_expr_offset(ir);

    fprintf(visitor->stream, "\tadd\t%s, %ld\n", result_reg, offset);
    return NULL;
}

static struct ExprIr* visit_subst_expr2(struct CodegenVisitor* visitor,
                                        struct SubstExprIr* ir) {
    struct ExprIr* value = ir_subst_expr_value(ir);
    visitor_visit_expr(as_visitor(visitor), value);
    struct ExprIr* addr = ir_subst_expr_addr(ir);
    visitor_visit_expr(as_visitor(visitor), addr);

    strtable_id value_reg_id = ir_expr_reg_id(value);
    strtable_id addr_reg_id = ir_expr_reg_id(addr);
    assert(ir_expr_reg_id(ir_subst_expr_cast(ir)) == value_reg_id);

    const char* value_reg = register_name(visitor, value_reg_id);
    const char* addr_reg = register_name(visitor, addr_reg_id);
    fprintf(visitor->stream, "\tmov\t[%s], %s\n", addr_reg, value_reg);

    return NULL;
}

static struct ExprIr* visit_deref_expr2(struct CodegenVisitor* visitor,
                                        struct DerefExprIr* ir) {
    struct ExprIr* operand = ir_deref_expr_operand(ir);
    visitor_visit_expr(as_visitor(visitor), operand);

    strtable_id operand_reg_id = ir_expr_reg_id(operand);
    strtable_id result_reg_id = ir_expr_reg_id(ir_deref_expr_cast(ir));

    const char* operand_reg = register_name(visitor, operand_reg_id);
    const char* result_reg = register_name(visitor, result_reg_id);
    fprintf(visitor->stream, "\tmov\t%s, [%s]\n", result_reg, operand_reg);

    return NULL;
}

static struct ExprIr* visit_cast_expr2(struct CodegenVisitor* visitor,
                                       struct CastExprIr* ir) {
    struct ExprIr* operand = ir_cast_expr_operand(ir);
    visitor_visit_expr(as_visitor(visitor), operand);

    strtable_id operand_reg_id = ir_expr_reg_id(operand);
    strtable_id result_reg_id = ir_expr_reg_id(ir_cast_expr_cast(ir));

    const char* operand_reg = register_name(visitor, operand_reg_id);
    const char* result_reg = register_name(visitor, result_reg_id);

    size_t operand_reg_size = type_size(ir_expr_type(operand));
    size_t result_reg_size = type_size(ir_expr_type(ir_cast_expr_cast(ir)));
    if (operand_reg_size < result_reg_size)
        fprintf(visitor->stream, "\tmovsx\t%s, %s\n", result_reg, operand_reg);

    return NULL;
}

static struct StmtIr* visit_stmt_pre(struct CodegenVisitor* visitor,
                                     struct StmtIr* ir) {
    strtable_id label_index = ir_stmt_label_index(ir);
    if (label_index == (strtable_id)-1)
        fprintf(visitor->stream, "lab_%p_end:\n", visitor->function);
    return NULL;
}

static struct StmtIr* visit_if_stmt(struct CodegenVisitor* visitor,
                                    struct IfStmtIr* ir) {
    struct ExprIr* cond_expr = ir_if_stmt_cond_expr(ir);
    visitor_visit_expr(as_visitor(visitor), cond_expr);

    strtable_id cond_reg_id = ir_expr_reg_id(cond_expr);
    const char* cond_reg = register_name(visitor, cond_reg_id);

    fprintf(visitor->stream, "\tand\t%s, %s\n", cond_reg, cond_reg);
    fprintf(visitor->stream, "\tjz\tlab_%p_else\n", ir);

    visitor_visit_stmt(as_visitor(visitor), ir_if_stmt_true_stmt(ir));
    fprintf(visitor->stream, "\tjmp\tlab_%p_cont\n", ir);

    fprintf(visitor->stream, "lab_%p_else:\n", ir);
    visitor_visit_stmt(as_visitor(visitor), ir_if_stmt_false_stmt(ir));

    fprintf(visitor->stream, "lab_%p_cont:\n", ir);
    return NULL;
}

static struct StmtIr* visit_push_stmt(struct CodegenVisitor* visitor,
                                      struct PushStmtIr* ir) {
    strtable_id reg_id = ir_push_stmt_reg_id(ir);
    const char* reg = register_name(visitor, reg_id);
    fprintf(visitor->stream, "\tpush\t%s\n", reg);
    return NULL;
}

static struct StmtIr* visit_pop_stmt(struct CodegenVisitor* visitor,
                                     struct PopStmtIr* ir) {
    strtable_id reg_id = ir_pop_stmt_reg_id(ir);
    const char* reg = register_name(visitor, reg_id);
    fprintf(visitor->stream, "\tpop\t%s\n", reg);
    return NULL;
}

static struct FunctionIr* visit_function2(struct CodegenVisitor* visitor,
                                          struct FunctionIr* ir) {
    fprintf(visitor->stream, "\n");
    visitor->function = ir;

    const char* name =
        strtable_at(&ctx(visitor)->strtable, ir_function_name_index(ir));
    fprintf(visitor->stream, ".global %s\n", name);
    fprintf(visitor->stream, "%s:\n", name);

    struct BlockStmtIr* body = ir_function_body2(ir);
    visitor_visit_stmt(as_visitor(visitor), ir_block_stmt_super(body));

    fprintf(visitor->stream, "\tret\n");
    return NULL;
}

static struct CfIr* visit_return_cf2(struct CodegenVisitor* visitor,
                                     struct ReturnCfIr* ir) {
    struct ExprIr* expr = ir_return_cf_expr(ir);
    if (expr) {
        visitor_visit_expr(as_visitor(visitor), expr);
    }
    fprintf(visitor->stream, "\tjmp\tlab_%p_end\n", visitor->function);
    return NULL;
}

static struct CfIr* visit_label_cf2(struct CodegenVisitor* visitor,
                                    struct LabelCfIr* ir) {
    strtable_id index = ir_label_cf_index(ir);
    if (index == STRTABLE_INVALID_ID)
        fprintf(visitor->stream, "lab_%p_end:\n", visitor->function);
    else
        assert(false);
    return NULL;
}

struct CodegenVisitor* new_codegen_visitor(struct Context* context,
                                           FILE* stream) {
    struct CodegenVisitor* visitor = malloc(sizeof(struct CodegenVisitor));
    visitor_initialize(as_visitor(visitor), context);

    register_visitor(visitor->as_visitor, visit_const_expr, visit_const_expr2);
    register_visitor(visitor->as_visitor, visit_binop_expr, visit_binop_expr2);
    register_visitor(visitor->as_visitor, visit_call_expr, visit_call_expr2);
    register_visitor(visitor->as_visitor, visit_var_expr, visit_var_expr2);
    register_visitor(visitor->as_visitor, visit_unop_expr, visit_unop_expr2);
    register_visitor(visitor->as_visitor, visit_subst_expr, visit_subst_expr2);
    register_visitor(visitor->as_visitor, visit_member_expr,
                     visit_member_expr2);
    register_visitor(visitor->as_visitor, visit_deref_expr, visit_deref_expr2);
    register_visitor(visitor->as_visitor, visit_cast_expr, visit_cast_expr2);

    register_visitor(visitor->as_visitor, visit_stmt_pre, visit_stmt_pre);

    register_visitor(visitor->as_visitor, visit_if_stmt, visit_if_stmt);
    register_visitor(visitor->as_visitor, visit_push_stmt, visit_push_stmt);
    register_visitor(visitor->as_visitor, visit_pop_stmt, visit_pop_stmt);

    register_visitor(visitor->as_visitor, visit_function, visit_function2);
    register_visitor(visitor->as_visitor, visit_return_cf, visit_return_cf2);
    register_visitor(visitor->as_visitor, visit_label_cf, visit_label_cf2);

    visitor->stream = stream;

    fprintf(stream, ".intel_syntax noprefix\n");
    fprintf(stream, ".text\n");

    return visitor;
}
