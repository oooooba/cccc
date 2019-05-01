#include "ast.h"
#include "list.h"
#include "strtable.h"
#include "type.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

////////// id //////////

struct IdNode {
    strtable_id index;
};

struct IdNode* ast_new_id(strtable_id index) {
    struct IdNode* node = malloc(sizeof(struct IdNode));
    node->index = index;
    return node;
}

strtable_id ast_id_index(struct IdNode* node) { return node->index; }

const char* ast_id_str(struct IdNode* node, struct Strtable* strtable) {
    return strtable_at(strtable, ast_id_index(node));
}

////////// expr //////////

struct ExprNode {
    enum ExprNodeTag tag;
};

struct LiteralExprNode {
    struct ExprNode super_type;
    enum LiteralExprNodeTag tag;
    union {
        intptr_t integer;
    };
};

struct BinopExprNode {
    struct ExprNode super_type;
    enum BinopExprNodeTag op;
    struct ExprNode* lhs;
    struct ExprNode* rhs;
};

struct UniopExprNode {
    struct ExprNode super_type;
    enum UniopExprNodeTag op;
    struct ExprNode* expr;
};

struct VarExprNode {
    struct ExprNode super_type;
    struct IdNode* id;
};

static void initialize_expr_ast(struct ExprNode* node, enum ExprNodeTag tag) {
    node->tag = tag;
}

enum ExprNodeTag ast_expr_tag(struct ExprNode* node) { return node->tag; }

struct ExprNode* ast_literal_expr_super_type(struct LiteralExprNode* node) {
    return &node->super_type;
}

enum LiteralExprNodeTag ast_literal_expr_tag(struct LiteralExprNode* node) {
    return node->tag;
}

intptr_t ast_integer_literal_expr_value(struct LiteralExprNode* node) {
    return node->integer;
}

struct LiteralExprNode* ast_new_integer_literal_expr(intptr_t value) {
    struct LiteralExprNode* node = malloc(sizeof(struct LiteralExprNode));
    initialize_expr_ast(ast_literal_expr_super_type(node),
                        LiteralExprNodeTag_Integer);
    node->integer = value;
    return node;
}

struct ExprNode* ast_binop_expr_super_type(struct BinopExprNode* node) {
    return &node->super_type;
}

enum BinopExprNodeTag ast_binop_expr_op(struct BinopExprNode* node) {
    return node->op;
}

struct ExprNode* ast_binop_expr_lhs(struct BinopExprNode* node) {
    return node->lhs;
}

void ast_binop_expr_set_lhs(struct BinopExprNode* node, struct ExprNode* lhs) {
    node->lhs = lhs;
}

struct ExprNode* ast_binop_expr_rhs(struct BinopExprNode* node) {
    return node->rhs;
}

void ast_binop_expr_set_rhs(struct BinopExprNode* node, struct ExprNode* rhs) {
    node->rhs = rhs;
}

struct BinopExprNode* ast_new_binop_expr(enum BinopExprNodeTag op,
                                         struct ExprNode* lhs,
                                         struct ExprNode* rhs) {
    struct BinopExprNode* node = malloc(sizeof(struct BinopExprNode));
    initialize_expr_ast(ast_binop_expr_super_type(node), ExprNodeTag_Binop);
    node->op = op;
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

struct ExprNode* ast_uniop_expr_super_type(struct UniopExprNode* node) {
    return &node->super_type;
}

enum UniopExprNodeTag ast_uniop_expr_op(struct UniopExprNode* node) {
    return node->op;
}

struct ExprNode* ast_uniop_expr_expr(struct UniopExprNode* node) {
    return node->expr;
}

void ast_uniop_expr_set_expr(struct UniopExprNode* node,
                             struct ExprNode* expr) {
    node->expr = expr;
}

struct UniopExprNode* ast_new_uniop_expr(enum UniopExprNodeTag op,
                                         struct ExprNode* expr) {
    struct UniopExprNode* node = malloc(sizeof(struct UniopExprNode));
    initialize_expr_ast(ast_uniop_expr_super_type(node), ExprNodeTag_Uniop);
    node->op = op;
    node->expr = expr;
    return node;
}

struct ExprNode* ast_var_expr_super_type(struct VarExprNode* node) {
    return &node->super_type;
}

struct VarExprNode* ast_new_var_expr(struct IdNode* id) {
    struct VarExprNode* node = malloc(sizeof(struct VarExprNode));
    initialize_expr_ast(ast_var_expr_super_type(node), ExprNodeTag_Var);
    node->id = id;
    return node;
}

struct IdNode* ast_var_expr_id(struct VarExprNode* node) {
    return node->id;
}

strtable_id ast_var_expr_index(struct VarExprNode* node) {
    return node->id->index;
}

////////// stmt //////////

struct StmtNode {
    enum StmtNodeTag tag;
};

struct ExprStmtNode {
    struct StmtNode super_type;
    struct ExprNode* expr;
};

struct BlockStmtNode {
    struct StmtNode super_type;
    struct List stmts;
};

struct IfStmtNode {
    struct StmtNode super_type;
    struct ExprNode* cond_expr;
    struct BlockStmtNode* then_block;
    struct BlockStmtNode* else_block;
};

struct DeclStmtNode {
    struct StmtNode super_type;
    struct DeclNode* decl;
};

static void initialize_stmt_ast(struct StmtNode* node, enum StmtNodeTag tag) {
    node->tag = tag;
}

enum StmtNodeTag ast_stmt_tag(struct StmtNode* node) { return node->tag; }

struct StmtNode* ast_expr_stmt_super_type(struct ExprStmtNode* node) {
    return &node->super_type;
}

struct ExprNode* ast_expr_stmt_expr(struct ExprStmtNode* node) {
    return node->expr;
}

struct ExprStmtNode* ast_new_expr_stmt(struct ExprNode* expr) {
    struct ExprStmtNode* node = malloc(sizeof(struct ExprStmtNode));
    initialize_stmt_ast(ast_expr_stmt_super_type(node), StmtNodeTag_Expr);
    node->expr = expr;
    return node;
}

struct StmtNode* ast_block_stmt_super_type(struct BlockStmtNode* node) {
    return &node->super_type;
}

struct BlockStmtNode* ast_new_block_stmt(void) {
    struct BlockStmtNode* node = malloc(sizeof(struct BlockStmtNode));
    initialize_stmt_ast(ast_block_stmt_super_type(node), StmtNodeTag_Block);
    list_initialize(&node->stmts);
    return node;
}

void ast_block_stmt_iterator(struct BlockStmtNode* node,
                             struct BlockStmtNodeIterator* it) {
    it->node = node;
    it->current = list_end(&node->stmts);
}

struct StmtNode* ast_block_stmt_iterator_next(
    struct BlockStmtNodeIterator* it) {
    it->current = list_next(it->current);
    struct ListHeader* current = it->current;
    if (current == list_end(&it->node->stmts)) return NULL;
    return ((struct ListItem*)current)->item;
}

struct StmtNode* ast_block_stmt_swap_at(struct BlockStmtNodeIterator* it,
                                        struct StmtNode* stmt) {
    struct ListItem* item = (struct ListItem*)it->current;
    struct StmtNode* prev_stmt = item->item;
    item->item = stmt;
    return prev_stmt;
}

void ast_block_stmt_insert_at_end(struct BlockStmtNode* node,
                                  struct StmtNode* stmt) {
    struct ListItem* item = malloc(sizeof(struct ListItem));
    item->item = stmt;
    list_insert_at_end(&node->stmts, &item->header);
}

struct StmtNode* ast_if_stmt_super_type(struct IfStmtNode* node) {
    return &node->super_type;
}

struct IfStmtNode* ast_new_if_stmt(struct ExprNode* cond_expr,
                                   struct BlockStmtNode* then_block,
                                   struct BlockStmtNode* else_block) {
    struct IfStmtNode* node = malloc(sizeof(struct IfStmtNode));
    initialize_stmt_ast(ast_if_stmt_super_type(node), StmtNodeTag_If);
    node->cond_expr = cond_expr;
    node->then_block = then_block;
    node->else_block = else_block;
    return node;
}

struct ExprNode* ast_if_stmt_cond_expr(struct IfStmtNode* node) {
    return node->cond_expr;
}

void ast_if_stmt_set_cond_expr(struct IfStmtNode* node,
                               struct ExprNode* cond_expr) {
    node->cond_expr = cond_expr;
}

struct BlockStmtNode* ast_if_stmt_then_block(struct IfStmtNode* node) {
    return node->then_block;
}

struct BlockStmtNode* ast_if_stmt_else_block(struct IfStmtNode* node) {
    return node->else_block;
}

struct StmtNode* ast_decl_stmt_super_type(struct DeclStmtNode* node) {
    return &node->super_type;
}

struct DeclNode* ast_decl_stmt_decl(struct DeclStmtNode* node) {
    return node->decl;
}

struct DeclStmtNode* ast_new_decl_stmt(struct DeclNode* decl) {
    struct DeclStmtNode* node = malloc(sizeof(struct DeclStmtNode));
    initialize_stmt_ast(ast_decl_stmt_super_type(node), StmtNodeTag_Decl);
    node->decl = decl;
    return node;
}

////////// decl //////////

struct DeclNode {
    struct IdNode* name;
    struct TypeNode* type;
    struct ExprNode* initial_expr;
};

struct DeclNode* ast_new_decl(struct IdNode* name, struct TypeNode* type,
                              struct ExprNode* initial_expr) {
    struct DeclNode* node = malloc(sizeof(struct DeclNode));
    node->name = name;
    node->type = type;
    node->initial_expr = initial_expr;
    return node;
}

struct IdNode* ast_decl_name(struct DeclNode* node) {
    return node->name;
}

struct TypeNode* ast_decl_type(struct DeclNode* node) {
    return node->type;
}

struct ExprNode* ast_decl_initial_expr(struct DeclNode* node) {
    return node->initial_expr;
}

////////// fundef //////////

struct FundefNode {
    struct IdNode* name;
    struct TypeNode* return_type;
    struct Vector* params;
    struct BlockStmtNode* body;
};

struct FundefNode* ast_new_fundef(struct IdNode* name,
                                  struct TypeNode* return_type,
                                  struct Vector* params,
                                  struct BlockStmtNode* body) {
    struct FundefNode* node = malloc(sizeof(struct FundefNode));
    node->name = name;
    node->return_type = return_type;
    node->params = params;
    node->body = body;
    return node;
}

struct IdNode* ast_fundef_name(struct FundefNode* node) {
    return node->name;
}

struct TypeNode* ast_fundef_return_type(struct FundefNode* node) {
    return node->return_type;
}

struct BlockStmtNode* ast_fundef_body(struct FundefNode* node) {
    return node->body;
}
