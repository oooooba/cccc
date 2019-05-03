#include "ast.h"
#include "context.h"
#include "ir.h"
#include "list.h"
#include "visitor.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

struct CodegenVisitor {
    struct Visitor super_type;
    struct Context* context;
    FILE* output_stream;
    struct FunInfo* current_fun_info;
};

static struct Visitor* super_type(struct CodegenVisitor* visitor) {
    return &visitor->super_type;
}

static struct IdNode* visit_id(struct CodegenVisitor* visitor,
                               struct IdNode* node) {
    const char* str = ast_id_str(node, &visitor->context->strtable);
    fprintf(visitor->output_stream, "%s", str + (str[0] == '%'));
    return NULL;
}

static struct ExprNode* visit_literal_expr(struct CodegenVisitor* visitor,
                                           struct LiteralExprNode* node) {
    intptr_t value = ast_integer_literal_expr_value(node);
    fprintf(visitor->output_stream, "%ld", value);
    return NULL;
}

static bool is_register(struct CodegenVisitor* visitor,
                        struct VarExprNode* node) {
    return ast_id_str(ast_var_expr_id(node), &visitor->context->strtable)[0] ==
           '%';
}

static struct ExprNode* visit_var_expr(struct CodegenVisitor* visitor,
                                       struct VarExprNode* node) {
    if (is_register(visitor, node)) {
        visit_id(visitor, ast_var_expr_id(node));
    } else {
        struct VarInfo* info = context_find_local_var_info_by_strtable_id(
            visitor->current_fun_info, ast_var_expr_index(node));
        size_t offset = context_var_info_offset(info);
        fprintf(visitor->output_stream, "rbp - %ld", offset);
    }
    return NULL;
}

static const char* dispatch_uniop_opcode(enum UniopExprNodeTag op) {
    switch (op) {
        case UniopExprNodeTag_Addrof:
            return "lea";
        case UniopExprNodeTag_Deref:
            return "mov";
        default:
            assert(false);
    }
    return NULL;
}

static const char* dispatch_binop_opcode(enum BinopExprNodeTag op) {
    switch (op) {
        case BinopExprNodeTag_Add:
            return "add";
        case BinopExprNodeTag_Sub:
            return "sub";
        case BinopExprNodeTag_Mul:
            return "imul";
        case BinopExprNodeTag_Subst:
            assert(false);
        default:
            assert(false);
    }
    return NULL;
}

static void emit_assembly_stmt(struct CodegenVisitor* visitor,
                               struct ExprNode* dst,
                               struct ExprNode* operation) {
    fprintf(visitor->output_stream, "\t");
    if (ast_expr_tag(dst) == ExprNodeTag_Var) {
        struct VarExprNode* dst_reg = (struct VarExprNode*)dst;
        assert(is_register(visitor, dst_reg));
        switch (ast_expr_tag(operation)) {
            case ExprNodeTag_Literal:
                fprintf(visitor->output_stream, "mov\t");
                visit_var_expr(visitor, dst_reg);
                fprintf(visitor->output_stream, ", ");
                visitor_visit_expr(super_type(visitor), operation);
                break;
            case ExprNodeTag_Uniop: {
                struct UniopExprNode* uniop = (struct UniopExprNode*)operation;
                enum UniopExprNodeTag tag = ast_uniop_expr_op(uniop);
                fprintf(visitor->output_stream, "%s\t",
                        dispatch_uniop_opcode(tag));
                visit_var_expr(visitor, dst_reg);
                fprintf(visitor->output_stream, ", ");
                switch (tag) {
                    case UniopExprNodeTag_Addrof:
                    case UniopExprNodeTag_Deref:
                        fprintf(visitor->output_stream, "[");
                        visitor_visit_expr(super_type(visitor),
                                           ast_uniop_expr_expr(uniop));
                        fprintf(visitor->output_stream, "]");
                        break;
                    default:
                        visitor_visit_expr(super_type(visitor),
                                           ast_uniop_expr_expr(uniop));
                }
            } break;
            case ExprNodeTag_Binop: {
                struct BinopExprNode* binop = (struct BinopExprNode*)operation;
                fprintf(visitor->output_stream, "%s\t",
                        dispatch_binop_opcode(ast_binop_expr_op(binop)));
                visit_var_expr(visitor, dst_reg);
                fprintf(visitor->output_stream, ", ");
                visitor_visit_expr(super_type(visitor),
                                   ast_binop_expr_rhs(binop));
            } break;
            default:
                assert(false);
        }
    } else {
        assert(ast_expr_tag(dst) == ExprNodeTag_Uniop);
        struct UniopExprNode* uniop = (struct UniopExprNode*)dst;
        assert(ast_uniop_expr_op(uniop) == UniopExprNodeTag_Deref);

        fprintf(visitor->output_stream, "mov\t");
        fprintf(visitor->output_stream, "[");
        visitor_visit_expr(super_type(visitor), ast_uniop_expr_expr(uniop));
        fprintf(visitor->output_stream, "]");
        fprintf(visitor->output_stream, ", ");
        visitor_visit_expr(super_type(visitor), operation);
    }
}

static struct ExprNode* visit_binop_expr(struct CodegenVisitor* visitor,
                                         struct BinopExprNode* node) {
    assert(ast_binop_expr_op(node) == BinopExprNodeTag_Subst);
    struct ExprNode* dst = ast_binop_expr_lhs(node);
    struct ExprNode* operation = ast_binop_expr_rhs(node);
    emit_assembly_stmt(visitor, dst, operation);
    return NULL;
}

static struct StmtNode* visit_expr_stmt(struct CodegenVisitor* visitor,
                                        struct ExprStmtNode* node) {
    visitor_visit_expr(super_type(visitor), ast_expr_stmt_expr(node));
    fprintf(visitor->output_stream, "\n");
    return NULL;
}

static struct StmtNode* visit_block_stmt(struct CodegenVisitor* visitor,
                                         struct BlockStmtNode* node) {
    fprintf(visitor->output_stream, "lab_%p:\n", node);
    struct BlockStmtNodeIterator it;
    ast_block_stmt_iterator(node, &it);
    for (;;) {
        struct StmtNode* stmt = ast_block_stmt_iterator_next(&it);
        if (!stmt) break;
        visitor_visit_stmt(super_type(visitor), stmt);
    }
    return NULL;
}

static struct StmtNode* visit_if_stmt(struct CodegenVisitor* visitor,
                                      struct IfStmtNode* node) {
    assert(ast_expr_tag(ast_if_stmt_cond_expr(node)) == ExprNodeTag_Var);
    struct VarExprNode* cond_reg =
        (struct VarExprNode*)ast_if_stmt_cond_expr(node);
    fprintf(visitor->output_stream, "\tand\t");
    visit_var_expr(visitor, cond_reg);
    fprintf(visitor->output_stream, ", ");
    visit_var_expr(visitor, cond_reg);
    fprintf(visitor->output_stream, "\n");

    fprintf(visitor->output_stream, "\tjz\tlab_%p_else\n", node);

    visit_block_stmt(visitor, ast_if_stmt_then_block(node));
    fprintf(visitor->output_stream, "\tjmp\tlab_%p_cont\n", node);

    fprintf(visitor->output_stream, "lab_%p_else:\n", node);
    visit_block_stmt(visitor, ast_if_stmt_else_block(node));

    fprintf(visitor->output_stream, "lab_%p_cont:\n", node);

    return NULL;
}

static struct StmtNode* visit_decl_stmt(struct CodegenVisitor* visitor,
                                        struct DeclStmtNode* node) {
    (void)visitor;
    (void)node;
    return NULL;
}

static struct FundefNode* visit_fundef(struct CodegenVisitor* visitor,
                                       struct FundefNode* node) {
    fprintf(visitor->output_stream, "\n");

    visitor->current_fun_info =
        context_find_fun_info_by_fundef_node(visitor->context, node);

    fprintf(visitor->output_stream, ".global ");
    visit_id(visitor, ast_fundef_name(node));
    fprintf(visitor->output_stream, "\n");

    visit_id(visitor, ast_fundef_name(node));
    fprintf(visitor->output_stream, ":\n");

    fprintf(visitor->output_stream, "\tpush\trbp\n");
    fprintf(visitor->output_stream, "\tmov\trbp, rsp\n");
    fprintf(visitor->output_stream, "\tsub\trsp, %ld\n",
            context_fun_info_stack_slot_size(visitor->current_fun_info));

    // ToDo: setup arguments

    visit_block_stmt(visitor, ast_fundef_body(node));

    fprintf(visitor->output_stream, "\tleave\n");
    fprintf(visitor->output_stream, "\tret\n");

    return NULL;
}

struct CodegenVisitor* new_codegen_visitor(struct Context* context,
                                           FILE* output_stream) {
    struct CodegenVisitor* visitor = malloc(sizeof(struct CodegenVisitor));
    visitor_initialize(super_type(visitor));

    register_visitor(visitor->super_type, visit_literal_expr,
                     visit_literal_expr);
    register_visitor(visitor->super_type, visit_binop_expr, visit_binop_expr);
    register_visitor(visitor->super_type, visit_var_expr, visit_var_expr);
    register_visitor(visitor->super_type, visit_block_stmt, visit_block_stmt);
    register_visitor(visitor->super_type, visit_expr_stmt, visit_expr_stmt);
    register_visitor(visitor->super_type, visit_if_stmt, visit_if_stmt);
    register_visitor(visitor->super_type, visit_decl_stmt, visit_decl_stmt);

    visitor->context = context;
    visitor->output_stream = output_stream;
    return visitor;
}

void codegen_apply(struct CodegenVisitor* visitor, struct FundefNode* node) {
    fprintf(visitor->output_stream, ".intel_syntax noprefix\n");
    fprintf(visitor->output_stream, ".text\n");

    visit_fundef(visitor, node);
}

struct CodegenVisitor2 {
    struct Visitor2 as_visitor;
    struct Context* context;
    FILE* stream;
};

static struct Visitor2* as_visitor(struct CodegenVisitor2* visitor) {
    return &visitor->as_visitor;
}

static struct ExprIr* visit_const_expr2(struct CodegenVisitor2* visitor,
                                        struct ConstExprIr* ir) {
    strtable_id reg_id = ir_expr_reg_id(ir_const_expr_cast(ir));
    const char* reg = strtable_at(&visitor->context->strtable, reg_id);
    fprintf(visitor->stream, "\tmov\t%s, %ld\n", reg,
            ir_const_expr_integer_value(ir));
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

    const char* rhs_reg = strtable_at(&visitor->context->strtable, rhs_reg_id);
    const char* result_reg =
        strtable_at(&visitor->context->strtable, result_reg_id);

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
    const char* reg = strtable_at(&visitor->context->strtable, reg_id);
    fprintf(visitor->stream, "\tlea\t%s, [ebp - %ld]\n", reg, offset);
    return NULL;
}

static struct ExprIr* visit_load_expr2(struct CodegenVisitor2* visitor,
                                       struct LoadExprIr* ir) {
    struct ExprIr* addr = ir_load_expr_addr(ir);
    visitor2_visit_expr(as_visitor(visitor), addr);

    strtable_id addr_reg_id = ir_expr_reg_id(addr);
    strtable_id result_reg_id = ir_expr_reg_id(ir_load_expr_cast(ir));

    const char* addr_reg =
        strtable_at(&visitor->context->strtable, addr_reg_id);
    const char* result_reg =
        strtable_at(&visitor->context->strtable, result_reg_id);

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

    const char* addr_reg =
        strtable_at(&visitor->context->strtable, addr_reg_id);
    const char* value_reg =
        strtable_at(&visitor->context->strtable, value_reg_id);

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
    struct BlockIr* body = ir_function_body(ir);
    visitor2_visit_block(as_visitor(visitor), body);

    fprintf(visitor->stream, "\tret\n");
    return NULL;
}

struct CodegenVisitor2* new_codegen_visitor2(struct Context* context,
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

    visitor->context = context;
    visitor->stream = stream;

    fprintf(stream, ".intel_syntax noprefix\n");
    fprintf(stream, ".text\n");

    return visitor;
}

void codegen2_apply(struct CodegenVisitor2* visitor, struct FunctionIr* ir) {
    visitor2_visit_function(as_visitor(visitor), ir);
}
