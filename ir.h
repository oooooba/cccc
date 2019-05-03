#ifndef IR_H
#define IR_H

#include "strtable.h"

#include <stddef.h>
#include <stdint.h>

struct Ir;

struct FunctionIr;

struct BlockIr;
struct BlockIterator;
struct VarIr;

struct ExprIr;
struct ConstExprIr;
struct BinopExprIr;
struct AddrofExprIr;
struct LoadExprIr;
struct StoreExprIr;

enum IrTag {
    IrTag_Block,
    IrTag_Expr,
    IrTag_Var,
    IrTag_Function,
};

enum ExprIrTag {
    ExprIrTag_Const,
    ExprIrTag_Binop,
    ExprIrTag_Addrof,
    ExprIrTag_Load,
    ExprIrTag_Store,
};

enum BinopExprIrTag {
    BinopExprIrTag_Add,
    BinopExprIrTag_Sub,
};

enum AddrTag {
    AddrTag_Var,
    AddrTag_Expr,
};

struct ExprIr* ir_as_expr(struct Ir* ir);
struct BlockIr* ir_as_block(struct Ir* ir);
struct FunctionIr* ir_as_function(struct Ir* ir);
enum IrTag ir_tag(struct Ir* ir);

struct FunctionIr* ir_new_function(strtable_id name_index,
                                   struct BlockIr* body);
struct Ir* ir_function_cast(struct FunctionIr* ir);
strtable_id ir_function_name_index(struct FunctionIr* ir);
struct BlockIr* ir_function_body(struct FunctionIr* ir);
size_t ir_function_region_size(struct FunctionIr* ir);
void ir_function_set_region_size(struct FunctionIr* ir, size_t region_size);

struct BlockIr* ir_new_block(void);
struct Ir* ir_block_cast(struct BlockIr* ir);
struct BlockIterator* ir_block_new_iterator(struct BlockIr* ir);
struct Ir* ir_block_iterator_next(struct BlockIterator* it);
struct Ir* ir_block_iterator_swap_at(struct BlockIterator* it,
                                     struct Ir* statement);
void ir_block_insert_expr_at_end(struct BlockIr* ir, struct ExprIr* expr);
void ir_block_insert_block_at_end(struct BlockIr* ir, struct BlockIr* block);
struct VarIr* ir_block_new_var(struct BlockIr* ir, strtable_id index);
void ir_block_commit_region_status(struct BlockIr* ir, size_t region_base);
size_t ir_block_region_size(struct BlockIr* ir);

struct Ir* ir_var_cast(struct VarIr* ir);
strtable_id ir_var_index(struct VarIr* ir);
size_t ir_var_offset(struct VarIr* ir);

struct Ir* ir_expr_cast(struct ExprIr* ir);
struct ConstExprIr* ir_expr_as_const(struct ExprIr* ir);
struct BinopExprIr* ir_expr_as_binop(struct ExprIr* ir);
struct AddrofExprIr* ir_expr_as_addrof(struct ExprIr* ir);
struct LoadExprIr* ir_expr_as_load(struct ExprIr* ir);
struct StoreExprIr* ir_expr_as_store(struct ExprIr* ir);
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
void ir_binop_expr_set_lhs(struct BinopExprIr* ir, struct ExprIr* lhs);
void ir_binop_expr_set_rhs(struct BinopExprIr* ir, struct ExprIr* rhs);

struct AddrofExprIr* ir_new_addrof_expr_with_expr(struct ExprIr* expr);
struct AddrofExprIr* ir_new_addrof_expr_with_var(struct VarIr* var);
struct ExprIr* ir_addrof_expr_cast(struct AddrofExprIr* ir);
enum AddrTag ir_addrof_expr_tag(struct AddrofExprIr* ir);
struct VarIr* ir_addrof_expr_operand_as_var(struct AddrofExprIr* ir);
struct ExprIr* ir_addrof_expr_operand_as_expr(struct AddrofExprIr* ir);

struct LoadExprIr* ir_new_load_expr(struct ExprIr* addr);
struct ExprIr* ir_load_expr_cast(struct LoadExprIr* ir);
struct ExprIr* ir_load_expr_addr(struct LoadExprIr* ir);
void ir_load_expr_set_addr(struct LoadExprIr* ir, struct ExprIr* addr);

struct StoreExprIr* ir_new_store_expr(struct ExprIr* addr,
                                      struct ExprIr* value);
struct ExprIr* ir_store_expr_cast(struct StoreExprIr* ir);
struct ExprIr* ir_store_expr_addr(struct StoreExprIr* ir);
void ir_store_expr_set_addr(struct StoreExprIr* ir, struct ExprIr* addr);
struct ExprIr* ir_store_expr_value(struct StoreExprIr* ir);
void ir_store_expr_set_value(struct StoreExprIr* ir, struct ExprIr* value);

#endif
