#ifndef AST_H
#define AST_H

#include "strtable.h"
#include "type.h"

#include <stdbool.h>
#include <stdint.h>

struct IdNode;
struct IdNode* ast_new_id(strtable_id index);

strtable_id ast_id_index(struct IdNode* node);
const char* ast_id_str(struct IdNode* node, struct Strtable* strtable);

struct ExprNode;
struct LiteralExprNode;
struct UniopExprNode;
struct BinopExprNode;
struct VarExprNode;

enum ExprNodeTag {
    ExprNodeTag_Literal,
    ExprNodeTag_Uniop,
    ExprNodeTag_Binop,
    ExprNodeTag_Var,
};

enum LiteralExprNodeTag {
    LiteralExprNodeTag_Integer,
};

enum UniopExprNodeTag {
    UniopExprNodeTag_Addrof,
    UniopExprNodeTag_Deref,
};

enum BinopExprNodeTag {
    BinopExprNodeTag_Add,
    BinopExprNodeTag_Sub,
    BinopExprNodeTag_Mul,
    BinopExprNodeTag_Subst,
};

enum ExprNodeTag ast_expr_tag(struct ExprNode* node);

struct ExprNode* ast_literal_expr_super_type(struct LiteralExprNode* node);

enum LiteralExprNodeTag ast_literal_expr_tag(struct LiteralExprNode* node);
intptr_t ast_integer_literal_expr_value(struct LiteralExprNode* node);
struct LiteralExprNode* ast_new_integer_literal_expr(intptr_t value);

struct ExprNode* ast_binop_expr_super_type(struct BinopExprNode* node);
enum BinopExprNodeTag ast_binop_expr_op(struct BinopExprNode* node);
struct ExprNode* ast_binop_expr_lhs(struct BinopExprNode* node);
void ast_binop_expr_set_lhs(struct BinopExprNode* node, struct ExprNode* lhs);
struct ExprNode* ast_binop_expr_rhs(struct BinopExprNode* node);
void ast_binop_expr_set_rhs(struct BinopExprNode* node, struct ExprNode* rhs);
struct BinopExprNode* ast_new_binop_expr(enum BinopExprNodeTag op,
                                         struct ExprNode* lhs,
                                         struct ExprNode* rhs);

struct ExprNode* ast_uniop_expr_super_type(struct UniopExprNode* node);
enum UniopExprNodeTag ast_uniop_expr_op(struct UniopExprNode* node);
struct ExprNode* ast_uniop_expr_expr(struct UniopExprNode* node);
void ast_uniop_expr_set_expr(struct UniopExprNode* node, struct ExprNode* expr);
struct UniopExprNode* ast_new_uniop_expr(enum UniopExprNodeTag op,
                                         struct ExprNode* expr);

struct ExprNode* ast_var_expr_super_type(struct VarExprNode* node);
struct VarExprNode* ast_new_var_expr(struct IdNode* id);
struct IdNode* ast_var_expr_id(struct VarExprNode* node);
strtable_id ast_var_expr_index(struct VarExprNode* node);

struct StmtNode;
struct ExprStmtNode;
struct BlockStmtNode;
struct IfStmtNode;
struct DeclStmtNode;

enum StmtNodeTag {
    StmtNodeTag_Expr,
    StmtNodeTag_Block,
    StmtNodeTag_If,
    StmtNodeTag_Decl,
};

enum StmtNodeTag ast_stmt_tag(struct StmtNode* node);

struct StmtNode* ast_expr_stmt_super_type(struct ExprStmtNode* node);
struct ExprStmtNode* ast_new_expr_stmt(struct ExprNode* expr);
struct ExprNode* ast_expr_stmt_expr(struct ExprStmtNode* node);

struct StmtNode* ast_block_stmt_super_type(struct BlockStmtNode* node);
struct BlockStmtNode* ast_new_block_stmt(void);

struct BlockStmtNodeIterator {
    struct BlockStmtNode* node;
    struct ListHeader* current;
};

void ast_block_stmt_iterator(struct BlockStmtNode* node,
                             struct BlockStmtNodeIterator* it);
struct StmtNode* ast_block_stmt_iterator_next(struct BlockStmtNodeIterator* it);
void ast_block_stmt_insert_at_end(struct BlockStmtNode* node,
                                  struct StmtNode* stmt);
struct StmtNode* ast_block_stmt_swap_at(struct BlockStmtNodeIterator* it,
                                        struct StmtNode* stmt);

struct StmtNode* ast_if_stmt_super_type(struct IfStmtNode* node);
struct IfStmtNode* ast_new_if_stmt(struct ExprNode* cond_expr,
                                   struct BlockStmtNode* then_block,
                                   struct BlockStmtNode* else_block);
struct ExprNode* ast_if_stmt_cond_expr(struct IfStmtNode* node);
void ast_if_stmt_set_cond_expr(struct IfStmtNode* node,
                               struct ExprNode* cond_expr);
struct BlockStmtNode* ast_if_stmt_then_block(struct IfStmtNode* node);
struct BlockStmtNode* ast_if_stmt_else_block(struct IfStmtNode* node);

struct StmtNode* ast_decl_stmt_super_type(struct DeclStmtNode* node);
struct DeclNode* ast_decl_stmt_decl(struct DeclStmtNode* node);
struct DeclStmtNode* ast_new_decl_stmt(struct DeclNode* decl);

struct DeclNode;

struct DeclNode* ast_new_decl(struct IdNode* name, struct TypeNode* type,
                              struct ExprNode* initial_expr);
struct IdNode* ast_decl_name(struct DeclNode* node);
struct TypeNode* ast_decl_type(struct DeclNode* node);
struct ExprNode* ast_decl_initial_expr(struct DeclNode* node);

struct FundefNode;

struct FundefNode* ast_new_fundef(struct IdNode* name,
                                  struct TypeNode* return_type,
                                  struct Vector* params,
                                  struct BlockStmtNode* body);
struct IdNode* ast_fundef_name(struct FundefNode* node);
struct TypeNode* ast_fundef_return_type(struct FundefNode* node);
struct BlockStmtNode* ast_fundef_body(struct FundefNode* node);

#endif
