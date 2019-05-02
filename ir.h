#ifndef IR_H
#define IR_H

#include "strtable.h"

#include <stddef.h>
#include <stdint.h>

struct Ir;

struct BlockIr;
struct BlockIterator;

struct ExprIr;
struct ConstExprIr;
struct BinopExprIr;

enum IrTag {
    IrTag_Expr,
    IrTag_Block,
};

enum ExprIrTag {
    ExprIrTag_Const,
    ExprIrTag_Binop,
};

enum BinopExprIrTag {
    BinopExprIrTag_Add,
    BinopExprIrTag_Sub,
};

struct ExprIr* ir_as_expr(struct Ir* ir);
struct BlockIr* ir_as_block(struct Ir* ir);
enum IrTag ir_tag(struct Ir* ir);

struct BlockIr* ir_new_block(void);
struct Ir* ir_block_cast(struct BlockIr* ir);
struct BlockIterator* ir_block_new_iterator(struct BlockIr* ir);
struct Ir* ir_block_iterator_next(struct BlockIterator* it);
struct Ir* ir_block_iterator_swap_at(struct BlockIterator* it,
                                     struct Ir* statement);
void ir_block_insert_expr_at_end(struct BlockIr* ir, struct ExprIr* expr);

struct Ir* ir_expr_cast(struct ExprIr* ir);
struct ConstExprIr* ir_expr_as_const(struct ExprIr* ir);
struct BinopExprIr* ir_expr_as_binop(struct ExprIr* ir);
enum ExprIrTag ir_expr_tag(struct ExprIr* ir);
strtable_id ir_expr_reg_id(struct ExprIr* ir);
void ir_expr_set_reg_id(struct ExprIr* ir, strtable_id id);

struct ConstExprIr* ir_new_integer_const_expr(intptr_t value);
struct ExprIr* ir_const_expr_cast(struct ConstExprIr* ir);
intptr_t ir_const_expr_integer_value(struct ConstExprIr* ir);

struct BinopExprIr* ir_new_binop_expr(enum BinopExprIrTag op,
                                      struct ExprIr* lhs, struct ExprIr* rhs);
struct ExprIr* ir_binop_expr_cast(struct BinopExprIr* ir);
enum BinopExprIrTag ir_binop_expr_op(struct BinopExprIr* ir);
struct ExprIr* ir_binop_expr_lhs(struct BinopExprIr* ir);
struct ExprIr* ir_binop_expr_rhs(struct BinopExprIr* ir);

#endif
