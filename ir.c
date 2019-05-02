#include "ir.h"
#include "list.h"
#include "strtable.h"

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

struct Ir {
    enum IrTag tag;
};

static void initialize_ir(struct Ir* ir, enum IrTag tag) { ir->tag = tag; }

struct ExprIr* ir_as_expr(struct Ir* ir) {
    return ir->tag == IrTag_Expr ? (struct ExprIr*)ir : NULL;
}

struct BlockIr* ir_as_block(struct Ir* ir) {
    return ir->tag == IrTag_Block ? (struct BlockIr*)ir : NULL;
}

enum IrTag ir_tag(struct Ir* ir) { return ir->tag; }

struct BlockIr {
    struct Ir as_ir;
    struct List statemetnts;
};

struct BlockIterator {
    struct BlockIr* block;
    struct ListHeader* current;
};

struct BlockIr* ir_new_block(void) {
    struct BlockIr* ir = malloc(sizeof(struct BlockIr));
    initialize_ir(ir_block_cast(ir), IrTag_Block);
    list_initialize(&ir->statemetnts);
    return ir;
}

struct Ir* ir_block_cast(struct BlockIr* ir) {
    return &ir->as_ir;
}

struct BlockIterator* ir_block_new_iterator(struct BlockIr* ir) {
    struct BlockIterator* it = malloc(sizeof(struct BlockIterator));
    it->block = ir;
    it->current = list_end(&ir->statemetnts);
    return it;
}

struct Ir* ir_block_iterator_next(struct BlockIterator* it) {
    it->current = list_next(it->current);
    struct ListHeader* current = it->current;
    if (current == list_end(&it->block->statemetnts)) return NULL;
    return ((struct ListItem*)current)->item;
}

struct Ir* ir_block_iterator_swap_at(struct BlockIterator* it,
                                     struct Ir* statement) {
    struct ListItem* item = (struct ListItem*)it->current;
    struct Ir* prev_statement = item->item;
    item->item = statement;
    return prev_statement;
}

static void ir_block_insert_at_end(struct BlockIr* ir, struct Ir* statement) {
    struct ListItem* item = malloc(sizeof(struct ListItem));
    item->item = statement;
    list_insert_at_end(&ir->statemetnts, &item->header);
}

void ir_block_insert_expr_at_end(struct BlockIr* ir, struct ExprIr* expr) {
    ir_block_insert_at_end(ir, ir_expr_cast(expr));
}

struct ExprIr {
    struct Ir as_ir;
    enum ExprIrTag tag;
    strtable_id reg_id;
};

static void initialize_expr(struct ExprIr* ir, enum ExprIrTag tag) {
    initialize_ir(ir_expr_cast(ir), IrTag_Expr);
    ir->tag = tag;
    ir->reg_id = STRTABLE_INVALID_ID;
}

struct Ir* ir_expr_cast(struct ExprIr* ir) {
    return &ir->as_ir;
}

struct ConstExprIr* ir_expr_as_const(struct ExprIr* ir) {
    return ir->tag == ExprIrTag_Const ? (struct ConstExprIr*)ir : NULL;
}

struct BinopExprIr* ir_expr_as_binop(struct ExprIr* ir) {
    return ir->tag == ExprIrTag_Binop ? (struct BinopExprIr*)ir : NULL;
}

enum ExprIrTag ir_expr_tag(struct ExprIr* ir) { return ir->tag; }

strtable_id ir_expr_reg_id(struct ExprIr* ir) { return ir->reg_id; }

void ir_expr_set_reg_id(struct ExprIr* ir, strtable_id id) { ir->reg_id = id; }

struct ConstExprIr {
    struct ExprIr as_expr;
    union {
        intptr_t integer;
    };
};

struct ConstExprIr* ir_new_integer_const_expr(intptr_t value) {
    struct ConstExprIr* ir = malloc(sizeof(struct ConstExprIr));
    initialize_expr(ir_const_expr_cast(ir), ExprIrTag_Const);
    ir->integer = value;
    return ir;
}

struct ExprIr* ir_const_expr_cast(struct ConstExprIr* ir) {
    return &ir->as_expr;
}

intptr_t ir_const_expr_integer_value(struct ConstExprIr* ir) {
    return ir->integer;
}

struct BinopExprIr {
    struct ExprIr as_expr;
    enum BinopExprIrTag op;
    struct ExprIr* lhs;
    struct ExprIr* rhs;
};

struct BinopExprIr* ir_new_binop_expr(enum BinopExprIrTag op,
                                      struct ExprIr* lhs, struct ExprIr* rhs) {
    struct BinopExprIr* ir = malloc(sizeof(struct BinopExprIr));
    initialize_expr(ir_binop_expr_cast(ir), ExprIrTag_Binop);
    ir->op = op;
    ir->lhs = lhs;
    ir->rhs = rhs;
    return ir;
}

struct ExprIr* ir_binop_expr_cast(struct BinopExprIr* ir) {
    return &ir->as_expr;
}

enum BinopExprIrTag ir_binop_expr_op(struct BinopExprIr* ir) { return ir->op; }

struct ExprIr* ir_binop_expr_lhs(struct BinopExprIr* ir) {
    return ir->lhs;
}

struct ExprIr* ir_binop_expr_rhs(struct BinopExprIr* ir) {
    return ir->rhs;
}
