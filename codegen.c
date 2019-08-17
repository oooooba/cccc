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
    struct StmtIr* break_dst;
    struct StmtIr* continue_dst;
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

static struct ExprIr* visit_const_expr(struct CodegenVisitor* visitor,
                                       struct ConstExprIr* ir) {
    strtable_id dst_reg_id = ir_expr_reg_id(ir_const_expr_cast(ir));
    const char* dst_reg = register_name(visitor, dst_reg_id);

    switch (ir_const_expr_tag(ir)) {
        case ConstExprIrTag_Integer:
            fprintf(visitor->stream, "\tmov\t%s, %ld\n", dst_reg,
                    ir_const_expr_integer_value(ir));
            break;
        case ConstExprIrTag_String:
            fprintf(visitor->stream, "\tlea\t%s, strlab_%ld[rip]\n", dst_reg,
                    ir_const_expr_string_literal_id(ir));
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

    return ir_const_expr_cast(ir);
}

static void print_relational_instr(struct CodegenVisitor* visitor, void* label,
                                   const char* op, const char* lhs_reg,
                                   const char* rhs_reg) {
    fprintf(visitor->stream, "\tcmp\t%s, %s\n", lhs_reg, rhs_reg);
    fprintf(visitor->stream, "\t%s\tlab_%p_else\n", op, label);
    fprintf(visitor->stream, "\tmov\t%s, 1\n", lhs_reg);
    fprintf(visitor->stream, "\tjmp\tlab_%p_cont\n", label);
    fprintf(visitor->stream, "lab_%p_else:\n", label);
    fprintf(visitor->stream, "\tmov\t%s, 0\n", lhs_reg);
    fprintf(visitor->stream, "lab_%p_cont:\n", label);
}

static void print_logical_op_instr(struct CodegenVisitor* visitor,
                                   struct BinopExprIr* ir) {
    const char* opcode;
    const char* jmp_opcode;
    int shortcut_value;
    int final_value;
    switch (ir_binop_expr_op(ir)) {
        case BinopExprIrTag_LogicalAnd:
            opcode = "and";
            jmp_opcode = "jz";
            shortcut_value = 0;
            final_value = 1;
            break;
        case BinopExprIrTag_LogicalOr:
            opcode = "or";
            jmp_opcode = "jnz";
            shortcut_value = 1;
            final_value = 0;
            break;
        default:
            assert(false);
    }

    struct ExprIr* lhs = ir_binop_expr_lhs(ir);
    visitor_visit_expr(as_visitor(visitor), lhs);

    strtable_id lhs_reg_id = ir_expr_reg_id(lhs);
    strtable_id result_reg_id = ir_expr_reg_id(ir_binop_expr_cast(ir));
    assert(lhs_reg_id == result_reg_id);
    const char* result_reg = register_name(visitor, result_reg_id);

    fprintf(visitor->stream, "\t%s\t%s, %s\n", opcode, result_reg, result_reg);
    fprintf(visitor->stream, "\t%s\tlab_%p_shortcut\n", jmp_opcode, ir);

    struct ExprIr* rhs = ir_binop_expr_rhs(ir);
    visitor_visit_expr(as_visitor(visitor), rhs);

    strtable_id rhs_reg_id = ir_expr_reg_id(rhs);
    const char* rhs_reg = register_name(visitor, rhs_reg_id);

    fprintf(visitor->stream, "\t%s\t%s, %s\n", opcode, rhs_reg, rhs_reg);
    fprintf(visitor->stream, "\t%s\tlab_%p_shortcut\n", jmp_opcode, ir);

    fprintf(visitor->stream, "\tmov\t%s, %d\n", result_reg, final_value);
    fprintf(visitor->stream, "\tjmp\tlab_%p_cont\n", ir);

    fprintf(visitor->stream, "lab_%p_shortcut:\n", ir);
    fprintf(visitor->stream, "\tmov\t%s, %d\n", result_reg, shortcut_value);
    fprintf(visitor->stream, "lab_%p_cont:\n", ir);
}

static struct ExprIr* visit_binop_expr(struct CodegenVisitor* visitor,
                                       struct BinopExprIr* ir) {
    enum BinopExprIrTag op = ir_binop_expr_op(ir);
    if (op == BinopExprIrTag_LogicalAnd || op == BinopExprIrTag_LogicalOr) {
        print_logical_op_instr(visitor, ir);
        return ir_binop_expr_cast(ir);
    }

    visitor_visit_binop_expr(as_visitor(visitor), ir);
    struct ExprIr* lhs = ir_binop_expr_lhs(ir);
    struct ExprIr* rhs = ir_binop_expr_rhs(ir);

    strtable_id lhs_reg_id = ir_expr_reg_id(lhs);
    strtable_id rhs_reg_id = ir_expr_reg_id(rhs);
    strtable_id result_reg_id = ir_expr_reg_id(ir_binop_expr_cast(ir));
    assert(lhs_reg_id == result_reg_id);

    const char* rhs_reg = register_name(visitor, rhs_reg_id);
    const char* result_reg = register_name(visitor, result_reg_id);

    const char* opcode;
    switch (op) {
        case BinopExprIrTag_Add:
            opcode = "add";
            break;
        case BinopExprIrTag_Sub:
            opcode = "sub";
            break;
        case BinopExprIrTag_Mul:
            opcode = "imul";
            break;
        case BinopExprIrTag_Equal:
            print_relational_instr(visitor, ir, "jnz", result_reg, rhs_reg);
            return ir_binop_expr_cast(ir);
        case BinopExprIrTag_Lt:
            print_relational_instr(visitor, ir, "jge", result_reg, rhs_reg);
            return ir_binop_expr_cast(ir);
        case BinopExprIrTag_Le:
            print_relational_instr(visitor, ir, "jg", result_reg, rhs_reg);
            return ir_binop_expr_cast(ir);
        case BinopExprIrTag_Gt:
            print_relational_instr(visitor, ir, "jle", result_reg, rhs_reg);
            return ir_binop_expr_cast(ir);
        case BinopExprIrTag_Ge:
            print_relational_instr(visitor, ir, "jl", result_reg, rhs_reg);
            return ir_binop_expr_cast(ir);
        case BinopExprIrTag_LogicalAnd:
            print_relational_instr(visitor, ir, "jl", result_reg, rhs_reg);
            return ir_binop_expr_cast(ir);
        default:
            assert(false);
    }

    if (ir_expr_type(lhs) == NULL ||
        type_tag(ir_expr_type(lhs)) != Type_Pointer)
        fprintf(visitor->stream, "\t%s\t%s, %s\n", opcode, result_reg, rhs_reg);
    else {
        struct TypeIr* elem_type =
            type_pointer_elem_type(type_as_pointer(ir_expr_type(lhs)));
        fprintf(visitor->stream, "\timul\t%s, %ld\n", rhs_reg,
                type_size(elem_type));
        fprintf(visitor->stream, "\t%s\t%s, %s\n", opcode, result_reg, rhs_reg);
    }
    return ir_binop_expr_cast(ir);
}

static struct ExprIr* visit_unop_expr(struct CodegenVisitor* visitor,
                                      struct UnopExprIr* ir) {
    visitor_visit_unop_expr(as_visitor(visitor), ir);
    struct ExprIr* operand = ir_unop_expr_operand(ir);

    strtable_id operand_reg_id = ir_expr_reg_id(operand);
    strtable_id result_reg_id = ir_expr_reg_id(ir_unop_expr_cast(ir));
    assert(operand_reg_id == result_reg_id);

    const char* result_reg = register_name(visitor, result_reg_id);

    const char* op;
    switch (ir_unop_expr_op(ir)) {
        case UnopExprIrTag_Neg:
            op = "neg";
            break;
        case UnopExprIrTag_Not:
            fprintf(visitor->stream, "\tand\t%s, %s\n", result_reg, result_reg);
            fprintf(visitor->stream, "\tjz\tlab_%p_else\n", ir);
            fprintf(visitor->stream, "\tmov\t%s, 0\n", result_reg);
            fprintf(visitor->stream, "\tjmp\tlab_%p_cont\n", ir);
            fprintf(visitor->stream, "lab_%p_else:\n", ir);
            fprintf(visitor->stream, "\tmov\t%s, 1\n", result_reg);
            fprintf(visitor->stream, "lab_%p_cont:\n", ir);
            return ir_unop_expr_cast(ir);
        default:
            assert(false);
    }

    fprintf(visitor->stream, "\t%s\t%s\n", op, result_reg);
    return ir_unop_expr_cast(ir);
}

static struct ExprIr* visit_call_expr(struct CodegenVisitor* visitor,
                                      struct CallExprIr* ir) {
    struct VarExprIr* func_name = ir_expr_as_var(ir_call_expr_function(ir));
    strtable_id name_id;
    if (func_name)
        name_id = ir_var_expr_index(func_name);
    else {
        visitor_visit_expr(as_visitor(visitor), ir_call_expr_function(ir));
        name_id = ir_expr_reg_id(ir_call_expr_function(ir));
    }

    for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                           *eit = list_end(ir_call_expr_args(ir));
         it != eit; it = list_next(it)) {
        struct ExprIr* arg = ((struct ListItem*)it)->item;
        visitor_visit_expr(as_visitor(visitor), arg);
    }

    visitor_visit_stmt(as_visitor(visitor),
                       ir_block_stmt_super(ir_call_expr_pre_expr_block(ir)));

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

    return ir_call_expr_cast(ir);
}

static struct ExprIr* visit_var_expr(struct CodegenVisitor* visitor,
                                     struct VarExprIr* ir) {
    strtable_id reg_id = ir_expr_reg_id(ir_var_expr_cast(ir));
    const char* reg = register_name(visitor, reg_id);
    if (ir_var_expr_is_function(ir)) {
        strtable_id index = ir_var_expr_index(ir);
        const char* name = strtable_at(&ctx(visitor)->strtable, index);
        fprintf(visitor->stream, "\tlea\t%s, %s[rip]\n", reg, name);
    } else {
        size_t offset =
            ir_function_region_size(visitor->function) - ir_var_expr_offset(ir);
        fprintf(visitor->stream, "\tlea\t%s, [rbp - %ld]\n", reg, offset);
    }
    return ir_var_expr_cast(ir);
}

static struct ExprIr* visit_member_expr(struct CodegenVisitor* visitor,
                                        struct MemberExprIr* ir) {
    visitor_visit_member_expr(as_visitor(visitor), ir);

    strtable_id base_reg_id = ir_expr_reg_id(ir_member_expr_base(ir));
    strtable_id result_reg_id = ir_expr_reg_id(ir_member_expr_cast(ir));
    assert(base_reg_id == result_reg_id);

    const char* result_reg = register_name(visitor, result_reg_id);
    size_t offset = ir_member_expr_offset(ir);

    fprintf(visitor->stream, "\tadd\t%s, %ld\n", result_reg, offset);
    return ir_member_expr_cast(ir);
}

static struct ExprIr* visit_subst_expr(struct CodegenVisitor* visitor,
                                       struct SubstExprIr* ir) {
    visitor_visit_subst_expr(as_visitor(visitor), ir);

    strtable_id value_reg_id = ir_expr_reg_id(ir_subst_expr_value(ir));
    strtable_id addr_reg_id = ir_expr_reg_id(ir_subst_expr_addr(ir));
    assert(ir_expr_reg_id(ir_subst_expr_cast(ir)) == value_reg_id);

    const char* value_reg = register_name(visitor, value_reg_id);
    const char* addr_reg = register_name(visitor, addr_reg_id);
    fprintf(visitor->stream, "\tmov\t[%s], %s\n", addr_reg, value_reg);

    return ir_subst_expr_cast(ir);
}

static struct ExprIr* visit_deref_expr(struct CodegenVisitor* visitor,
                                       struct DerefExprIr* ir) {
    visitor_visit_deref_expr(as_visitor(visitor), ir);

    strtable_id operand_reg_id = ir_expr_reg_id(ir_deref_expr_operand(ir));
    strtable_id result_reg_id = ir_expr_reg_id(ir_deref_expr_cast(ir));

    const char* operand_reg = register_name(visitor, operand_reg_id);
    const char* result_reg = register_name(visitor, result_reg_id);
    fprintf(visitor->stream, "\tmov\t%s, [%s]\n", result_reg, operand_reg);

    return ir_deref_expr_cast(ir);
}

static struct ExprIr* visit_cast_expr(struct CodegenVisitor* visitor,
                                      struct CastExprIr* ir) {
    visitor_visit_cast_expr(as_visitor(visitor), ir);
    struct ExprIr* operand = ir_cast_expr_operand(ir);

    strtable_id operand_reg_id = ir_expr_reg_id(operand);
    strtable_id result_reg_id = ir_expr_reg_id(ir_cast_expr_cast(ir));

    const char* operand_reg = register_name(visitor, operand_reg_id);
    const char* result_reg = register_name(visitor, result_reg_id);

    enum RegisterSizeKind operand_reg_size =
        context_type_to_register_size_kind(ir_expr_type(operand));
    enum RegisterSizeKind result_reg_size =
        context_type_to_register_size_kind(ir_expr_type(ir_cast_expr_cast(ir)));
    if (operand_reg_size < result_reg_size)
        fprintf(visitor->stream, "\tmovsx\t%s, %s\n", result_reg, operand_reg);

    return ir_cast_expr_cast(ir);
}

static struct ExprIr* visit_cond_expr(struct CodegenVisitor* visitor,
                                      struct CondExprIr* ir) {
    struct ExprIr* cond = ir_cond_expr_cond(ir);
    visitor_visit_expr(as_visitor(visitor), cond);

    strtable_id cond_reg_id = ir_expr_reg_id(cond);
    const char* cond_reg = register_name(visitor, cond_reg_id);

    fprintf(visitor->stream, "\tand\t%s, %s\n", cond_reg, cond_reg);
    fprintf(visitor->stream, "\tjz\tlab_%p_else\n", ir);

    visitor_visit_expr(as_visitor(visitor), ir_cond_expr_true_expr(ir));
    fprintf(visitor->stream, "\tjmp\tlab_%p_cont\n", ir);

    fprintf(visitor->stream, "lab_%p_else:\n", ir);
    visitor_visit_expr(as_visitor(visitor), ir_cond_expr_false_expr(ir));

    fprintf(visitor->stream, "lab_%p_cont:\n", ir);
    return ir_cond_expr_cast(ir);
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
    return ir_if_stmt_super(ir);
}

static struct StmtIr* visit_switch_stmt(struct CodegenVisitor* visitor,
                                        struct SwitchStmtIr* ir) {
    struct StmtIr* prev_break_dst = visitor->break_dst;
    visitor->break_dst = ir_switch_stmt_super(ir);

    struct ExprIr* cond_expr = ir_switch_stmt_cond_expr(ir);
    visitor_visit_expr(as_visitor(visitor), cond_expr);

    strtable_id cond_reg_id = ir_expr_reg_id(cond_expr);
    const char* cond_reg = register_name(visitor, cond_reg_id);

    struct List* branches = ir_switch_stmt_branches(ir);
    for (struct ListHeader *it = list_begin(branches),
                           *eit = list_end(branches);
         it != eit; it = list_next(it)) {
        struct SwitchStmtBranch* branch = ((struct ListItem*)it)->item;

        fprintf(visitor->stream, "\tcmp\t%s, %ld\n", cond_reg,
                ir_switch_branch_case_value(branch));
        fprintf(visitor->stream, "\tjz\tlab_%p_case\n", branch);
    }
    fprintf(visitor->stream, "\tjmp\tlab_%p_default\n", ir);

    for (struct ListHeader *it = list_begin(branches),
                           *eit = list_end(branches);
         it != eit; it = list_next(it)) {
        struct SwitchStmtBranch* branch = ((struct ListItem*)it)->item;

        fprintf(visitor->stream, "lab_%p_case:\n", branch);
        visitor_visit_stmt(as_visitor(visitor), ir_switch_branch_stmt(branch));
    }

    fprintf(visitor->stream, "lab_%p_default:\n", ir);
    visitor_visit_stmt(as_visitor(visitor), ir_switch_stmt_default_stmt(ir));

    fprintf(visitor->stream, "lab_%p_cont:\n", ir);

    visitor->break_dst = prev_break_dst;

    return ir_switch_stmt_super(ir);
}

static struct StmtIr* visit_while_stmt(struct CodegenVisitor* visitor,
                                       struct WhileStmtIr* ir) {
    struct StmtIr* prev_break_dst = visitor->break_dst;
    visitor->break_dst = ir_while_stmt_super(ir);
    struct StmtIr* prev_continue_dst = visitor->continue_dst;
    visitor->continue_dst = ir_while_stmt_super(ir);

    fprintf(visitor->stream, "lab_%p_head:\n", ir);

    struct ExprIr* cond_expr = ir_while_stmt_cond_expr(ir);
    visitor_visit_expr(as_visitor(visitor), cond_expr);

    strtable_id cond_reg_id = ir_expr_reg_id(cond_expr);
    const char* cond_reg = register_name(visitor, cond_reg_id);

    fprintf(visitor->stream, "\tand\t%s, %s\n", cond_reg, cond_reg);
    fprintf(visitor->stream, "\tjz\tlab_%p_cont\n", ir);

    visitor_visit_stmt(as_visitor(visitor), ir_while_stmt_body_stmt(ir));

    fprintf(visitor->stream, "lab_%p_update:\n", ir);
    if (ir_while_stmt_update_expr(ir)) {
        visitor_visit_expr(as_visitor(visitor), ir_while_stmt_update_expr(ir));
    }

    fprintf(visitor->stream, "\tjmp\tlab_%p_head\n", ir);

    fprintf(visitor->stream, "lab_%p_cont:\n", ir);

    visitor->break_dst = prev_break_dst;
    visitor->continue_dst = prev_continue_dst;

    return ir_while_stmt_super(ir);
}

static struct StmtIr* visit_return_stmt(struct CodegenVisitor* visitor,
                                        struct ReturnStmtIr* ir) {
    visitor_visit_return_stmt(as_visitor(visitor), ir);
    fprintf(visitor->stream, "\tjmp\tlab_%p_end\n", visitor->function);
    return ir_return_stmt_super(ir);
}

static struct StmtIr* visit_break_stmt(struct CodegenVisitor* visitor,
                                       struct BreakStmtIr* ir) {
    (void)visitor;
    fprintf(visitor->stream, "\tjmp\tlab_%p_cont\n", visitor->break_dst);
    return ir_break_stmt_super(ir);
}

static struct StmtIr* visit_continue_stmt(struct CodegenVisitor* visitor,
                                          struct ContinueStmtIr* ir) {
    (void)visitor;
    fprintf(visitor->stream, "\tjmp\tlab_%p_update\n", visitor->continue_dst);
    return ir_continue_stmt_super(ir);
}

static struct StmtIr* visit_push_stmt(struct CodegenVisitor* visitor,
                                      struct PushStmtIr* ir) {
    strtable_id reg_id = ir_push_stmt_reg_id(ir);
    const char* reg = register_name(visitor, reg_id);
    fprintf(visitor->stream, "\tpush\t%s\n", reg);
    return ir_push_stmt_super(ir);
}

static struct StmtIr* visit_pop_stmt(struct CodegenVisitor* visitor,
                                     struct PopStmtIr* ir) {
    strtable_id reg_id = ir_pop_stmt_reg_id(ir);
    const char* reg = register_name(visitor, reg_id);
    fprintf(visitor->stream, "\tpop\t%s\n", reg);
    return ir_pop_stmt_super(ir);
}

static struct GlobalIr* visit_global(struct CodegenVisitor* visitor,
                                     struct GlobalIr* ir) {
    if (ir_global_is_public(ir)) {
        struct FunctionIr* function = ir_global_function(ir);
        const char* name = strtable_at(&ctx(visitor)->strtable,
                                       ir_function_name_index(function));
        fprintf(visitor->stream, ".global %s\n", name);
    }
    return ir;
}

static struct FunctionIr* visit_function(struct CodegenVisitor* visitor,
                                         struct FunctionIr* ir) {
    fprintf(visitor->stream, "\n");
    visitor->function = ir;

    const char* name =
        strtable_at(&ctx(visitor)->strtable, ir_function_name_index(ir));
    fprintf(visitor->stream, "%s:\n", name);

    struct BlockStmtIr* body = ir_function_body2(ir);
    visitor_visit_stmt(as_visitor(visitor), ir_block_stmt_super(body));

    fprintf(visitor->stream, "\tret\n");
    return ir;
}

static void emit_string_literals(struct CodegenVisitor* visitor) {
    for (size_t i = 1, len = strtable_len(&ctx(visitor)->strtable); i < len;
         ++i) {
        const char* str = strtable_at(&ctx(visitor)->strtable, i);
        if (str[0] != '"') continue;
        fprintf(visitor->stream, "strlab_%ld:\n", i);
        fprintf(visitor->stream, ".string\t%s\n", str);
    }
}

struct CodegenVisitor* new_codegen_visitor(struct Context* context,
                                           FILE* stream) {
    struct CodegenVisitor* visitor = malloc(sizeof(struct CodegenVisitor));
    visitor_initialize(as_visitor(visitor), context);

    register_visitor(visitor->as_visitor, visit_const_expr, visit_const_expr);
    register_visitor(visitor->as_visitor, visit_binop_expr, visit_binop_expr);
    register_visitor(visitor->as_visitor, visit_unop_expr, visit_unop_expr);
    register_visitor(visitor->as_visitor, visit_call_expr, visit_call_expr);
    register_visitor(visitor->as_visitor, visit_var_expr, visit_var_expr);
    register_visitor(visitor->as_visitor, visit_subst_expr, visit_subst_expr);
    register_visitor(visitor->as_visitor, visit_member_expr, visit_member_expr);
    register_visitor(visitor->as_visitor, visit_deref_expr, visit_deref_expr);
    register_visitor(visitor->as_visitor, visit_addrof_expr, NULL);
    register_visitor(visitor->as_visitor, visit_cast_expr, visit_cast_expr);
    register_visitor(visitor->as_visitor, visit_cond_expr, visit_cond_expr);

    register_visitor(visitor->as_visitor, visit_stmt_pre, visit_stmt_pre);

    register_visitor(visitor->as_visitor, visit_if_stmt, visit_if_stmt);
    register_visitor(visitor->as_visitor, visit_switch_stmt, visit_switch_stmt);
    register_visitor(visitor->as_visitor, visit_while_stmt, visit_while_stmt);
    register_visitor(visitor->as_visitor, visit_return_stmt, visit_return_stmt);
    register_visitor(visitor->as_visitor, visit_break_stmt, visit_break_stmt);
    register_visitor(visitor->as_visitor, visit_continue_stmt,
                     visit_continue_stmt);
    register_visitor(visitor->as_visitor, visit_push_stmt, visit_push_stmt);
    register_visitor(visitor->as_visitor, visit_pop_stmt, visit_pop_stmt);

    register_visitor(visitor->as_visitor, visit_function, visit_function);

    register_visitor(visitor->as_visitor, visit_global, visit_global);

    visitor->stream = stream;
    visitor->break_dst = NULL;
    visitor->continue_dst = NULL;

    fprintf(stream, ".intel_syntax noprefix\n");
    emit_string_literals(visitor);
    fprintf(stream, ".text\n");

    return visitor;
}
