#include "ast.h"
#include "context.h"
#include "ir.h"
#include "list.h"
#include "type.h"
#include "visitor.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

struct DumpVisitor {
    struct Visitor super_type;
    struct Context* context;
    FILE* output_stream;
    bool require_location;
};

static struct Visitor* super_type(struct DumpVisitor* visitor) {
    return &visitor->super_type;
}

static struct IdNode* visit_id(struct DumpVisitor* visitor,
                               struct IdNode* node) {
    fprintf(visitor->output_stream, "%s",
            ast_id_str(node, &visitor->context->strtable));
    return NULL;
}

static struct Type* visit_type(struct DumpVisitor* visitor,
                               struct TypeNode* node) {
    const char* type;
    switch (type_tag(node)) {
        case Type_Int:
            type = "int";
            break;
        default:
            assert(false);
    }
    fprintf(visitor->output_stream, "%s", type);
    return NULL;
}

static struct DeclNode* visit_decl(struct DumpVisitor* visitor,
                                   struct DeclNode* node) {
    visit_type(visitor, ast_decl_type(node));
    fprintf(visitor->output_stream, " ");
    visit_id(visitor, ast_decl_name(node));
    if (ast_decl_initial_expr(node) != NULL) {
        fprintf(visitor->output_stream, " = ");
        visitor_visit_expr(super_type(visitor), ast_decl_initial_expr(node));
    }
    return NULL;
}

static struct ExprNode* visit_literal_expr(struct DumpVisitor* visitor,
                                           struct LiteralExprNode* node) {
    intptr_t value = ast_integer_literal_expr_value(node);
    fprintf(visitor->output_stream, "%ld", value);
    return NULL;
}

static struct ExprNode* visit_var_expr(struct DumpVisitor* visitor,
                                       struct VarExprNode* node) {
    visit_id(visitor, ast_var_expr_id(node));
    return NULL;
}

static struct ExprNode* visit_uniop_expr(struct DumpVisitor* visitor,
                                         struct UniopExprNode* node) {
    fprintf(visitor->output_stream, "(");
    const char* op;
    switch (ast_uniop_expr_op(node)) {
        case UniopExprNodeTag_Addrof:
            op = "&";
            break;
        case UniopExprNodeTag_Deref:
            op = "*";
            break;
        default:
            assert(false);
    }
    fprintf(visitor->output_stream, "%s", op);
    visitor_visit_expr(super_type(visitor), ast_uniop_expr_expr(node));
    fprintf(visitor->output_stream, ")");

    return NULL;
}

static struct ExprNode* visit_binop_expr(struct DumpVisitor* visitor,
                                         struct BinopExprNode* node) {
    fprintf(visitor->output_stream, "(");
    visitor_visit_expr(super_type(visitor), ast_binop_expr_lhs(node));
    const char* op;
    switch (ast_binop_expr_op(node)) {
        case BinopExprNodeTag_Add:
            op = "+";
            break;
        case BinopExprNodeTag_Sub:
            op = "-";
            break;
        case BinopExprNodeTag_Mul:
            op = "*";
            break;
        case BinopExprNodeTag_Subst:
            op = "=";
            break;
        default:
            assert(false);
    }
    fprintf(visitor->output_stream, " %s ", op);
    visitor_visit_expr(super_type(visitor), ast_binop_expr_rhs(node));
    fprintf(visitor->output_stream, ")");

    return NULL;
}

static struct StmtNode* visit_expr_stmt(struct DumpVisitor* visitor,
                                        struct ExprStmtNode* node) {
    visitor_visit_expr(super_type(visitor), ast_expr_stmt_expr(node));
    fprintf(visitor->output_stream, ";\n");
    return NULL;
}

static struct StmtNode* visit_decl_stmt(struct DumpVisitor* visitor,
                                        struct DeclStmtNode* node) {
    visit_decl(visitor, ast_decl_stmt_decl(node));
    fprintf(visitor->output_stream, ";\n");
    return NULL;
}

static struct StmtNode* visit_block_stmt(struct DumpVisitor* visitor,
                                         struct BlockStmtNode* node) {
    fprintf(visitor->output_stream, "{\n");
    struct BlockStmtNodeIterator it;
    ast_block_stmt_iterator(node, &it);
    for (;;) {
        struct StmtNode* stmt = ast_block_stmt_iterator_next(&it);
        if (!stmt) break;
        visitor_visit_stmt(super_type(visitor), stmt);
    }
    fprintf(visitor->output_stream, "}\n");
    return NULL;
}

static struct StmtNode* visit_if_stmt(struct DumpVisitor* visitor,
                                      struct IfStmtNode* node) {
    fprintf(visitor->output_stream, "if (");
    visitor_visit_expr(super_type(visitor), ast_if_stmt_cond_expr(node));
    fprintf(visitor->output_stream, ")");
    visit_block_stmt(visitor, ast_if_stmt_then_block(node));
    fprintf(visitor->output_stream, "else ");
    visit_block_stmt(visitor, ast_if_stmt_else_block(node));
    return NULL;
}

static struct FundefNode* visit_fundef(struct DumpVisitor* visitor,
                                       struct FundefNode* node) {
    visit_type(visitor, ast_fundef_return_type(node));
    fprintf(visitor->output_stream, " ");
    visit_id(visitor, ast_fundef_name(node));
    fprintf(visitor->output_stream, "(void)");
    visit_block_stmt(visitor, ast_fundef_body(node));
    return NULL;
}

static struct DumpVisitor* new_dump_visitor(struct Context* context,
                                            FILE* output_stream) {
    struct DumpVisitor* visitor = malloc(sizeof(struct DumpVisitor));
    visitor_initialize(super_type(visitor));

    register_visitor(visitor->super_type, visit_literal_expr,
                     visit_literal_expr);
    register_visitor(visitor->super_type, visit_binop_expr, visit_binop_expr);
    register_visitor(visitor->super_type, visit_uniop_expr, visit_uniop_expr);
    register_visitor(visitor->super_type, visit_var_expr, visit_var_expr);
    register_visitor(visitor->super_type, visit_block_stmt, visit_block_stmt);
    register_visitor(visitor->super_type, visit_expr_stmt, visit_expr_stmt);
    register_visitor(visitor->super_type, visit_if_stmt, visit_if_stmt);
    register_visitor(visitor->super_type, visit_decl_stmt, visit_decl_stmt);

    visitor->context = context;
    visitor->output_stream = output_stream;
    visitor->require_location = false;
    return visitor;
}

void dump_apply(struct FundefNode* node, struct Context* context,
                FILE* output_stream) {
    struct DumpVisitor* visitor = new_dump_visitor(context, output_stream);
    visit_fundef(visitor, node);
}

struct DumpVisitor2 {
    struct Visitor2 as_visitor;
    struct Context* context;
    FILE* stream;
};

static struct Visitor2* as_visitor(struct DumpVisitor2* visitor) {
    return &visitor->as_visitor;
}

static struct ExprIr* visit_const_expr2(struct DumpVisitor2* visitor,
                                        struct ConstExprIr* ir) {
    fprintf(visitor->stream, "v%p = %ld\n", ir,
            ir_const_expr_integer_value(ir));
    return NULL;
}

static struct ExprIr* visit_binop_expr2(struct DumpVisitor2* visitor,
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
        default:
            assert(false);
    }
    fprintf(visitor->stream, "v%p = %s v%p, v%p\n", ir, op, lhs, rhs);
    return NULL;
}

static struct ExprIr* visit_addrof_expr2(struct DumpVisitor2* visitor,
                                         struct AddrofExprIr* ir) {
    assert(ir_addrof_expr_tag(ir) == AddrTag_Var);
    fprintf(visitor->stream, "v%p = addrof [%s]\n", ir,
            strtable_at(&visitor->context->strtable,
                        ir_var_index(ir_addrof_expr_var(ir))));
    return NULL;
}

static struct ExprIr* visit_load_expr2(struct DumpVisitor2* visitor,
                                       struct LoadExprIr* ir) {
    struct ExprIr* addr = ir_load_expr_addr(ir);
    visitor2_visit_expr(as_visitor(visitor), addr);
    fprintf(visitor->stream, "v%p = load [v%p]\n", ir, addr);
    return NULL;
}

static struct ExprIr* visit_store_expr2(struct DumpVisitor2* visitor,
                                        struct StoreExprIr* ir) {
    struct ExprIr* addr = ir_store_expr_addr(ir);
    struct ExprIr* value = ir_store_expr_value(ir);
    visitor2_visit_expr(as_visitor(visitor), addr);
    visitor2_visit_expr(as_visitor(visitor), value);
    fprintf(visitor->stream, "v%p = store [v%p] v%p\n", ir, addr, value);
    return NULL;
}

static struct BlockIr* visit_block_pre(struct DumpVisitor2* visitor,
                                       struct BlockIr* block) {
    fprintf(visitor->stream, "(@%p){\n", block);
    return NULL;
}

static struct BlockIr* visit_block_post(struct DumpVisitor2* visitor,
                                        struct BlockIr* target_block,
                                        struct BlockIr* result_block) {
    (void)result_block;
    fprintf(visitor->stream, "}(@%p)\n", target_block);
    return NULL;
}

static struct FunctionIr* visit_function(struct DumpVisitor2* visitor,
                                         struct FunctionIr* ir) {
    const char* name =
        strtable_at(&visitor->context->strtable, ir_function_name_index(ir));
    struct BlockIr* body = ir_function_body(ir);
    fprintf(visitor->stream, "function %s () ", name);
    visitor2_visit_block(as_visitor(visitor), body);
    return NULL;
}

struct DumpVisitor2* new_dump_visitor2(struct Context* context, FILE* stream) {
    struct DumpVisitor2* visitor = malloc(sizeof(struct DumpVisitor2));
    visitor2_initialize(as_visitor(visitor));

    register_visitor(visitor->as_visitor, visit_const_expr, visit_const_expr2);
    register_visitor(visitor->as_visitor, visit_binop_expr, visit_binop_expr2);
    register_visitor(visitor->as_visitor, visit_addrof_expr,
                     visit_addrof_expr2);
    register_visitor(visitor->as_visitor, visit_load_expr, visit_load_expr2);
    register_visitor(visitor->as_visitor, visit_store_expr, visit_store_expr2);
    register_visitor(visitor->as_visitor, visit_block_pre, visit_block_pre);
    register_visitor(visitor->as_visitor, visit_block_post, visit_block_post);

    visitor->context = context;
    visitor->stream = stream;
    return visitor;
}

void dump2_apply(struct DumpVisitor2* visitor, struct FunctionIr* ir) {
    visit_function(visitor, ir);
}
