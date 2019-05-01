#include "ast.h"
#include "context.h"
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
