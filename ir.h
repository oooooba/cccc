#ifndef IR_H
#define IR_H

#include "list.h"
#include "strtable.h"
#include "type.h"

#include <stddef.h>
#include <stdint.h>

struct Ir;

struct FunctionIr;

struct BlockIr;
struct BlockIterator;
struct VarIr;

struct CfIr;
struct BranchCfIr;
struct ReturnCfIr;
struct LabelCfIr;
struct PushCfIr;
struct PopCfIr;

struct ExprIr;
struct ConstExprIr;
struct BinopExprIr;
struct AddrofExprIr;
struct LoadExprIr;
struct StoreExprIr;
struct CallExprIr;

enum IrTag {
    IrTag_Block,
    IrTag_Expr,
    IrTag_Var,
    IrTag_Function,
    IrTag_Cf,
};

enum CfIrTag {
    CfIrTag_Branch,
    CfIrTag_Return,
    CfIrTag_Label,
    CfIrTag_Push,
    CfIrTag_Pop,
};

enum ExprIrTag {
    ExprIrTag_Const,
    ExprIrTag_Binop,
    ExprIrTag_Addrof,
    ExprIrTag_Load,
    ExprIrTag_Store,
    ExprIrTag_Call,
};

enum ConstExprIrTag {
    ConstExprIrTag_Integer,
    ConstExprIrTag_Register,
};

enum BinopExprIrTag {
    BinopExprIrTag_Add,
    BinopExprIrTag_Sub,
    BinopExprIrTag_Mul,
};

enum AddrTag {
    AddrTag_Var,
    AddrTag_Expr,
};

struct ExprIr* ir_as_expr(struct Ir* ir);
struct BlockIr* ir_as_block(struct Ir* ir);
struct FunctionIr* ir_as_function(struct Ir* ir);
struct CfIr* ir_as_cf(struct Ir* ir);
enum IrTag ir_tag(struct Ir* ir);

struct FunctionIr* ir_new_function(strtable_id name_index, struct List* params,
                                   struct BlockIr* body);
struct Ir* ir_function_cast(struct FunctionIr* ir);
strtable_id ir_function_name_index(struct FunctionIr* ir);
struct BlockIr* ir_function_body(struct FunctionIr* ir);
size_t ir_function_region_size(struct FunctionIr* ir);
void ir_function_set_region_size(struct FunctionIr* ir, size_t region_size);
struct List* ir_function_params(struct FunctionIr* ir);

struct BlockIr* ir_new_block(void);
struct Ir* ir_block_cast(struct BlockIr* ir);
struct BlockIterator* ir_block_new_iterator(struct BlockIr* ir);
struct Ir* ir_block_iterator_next(struct BlockIterator* it);
struct Ir* ir_block_iterator_swap_at(struct BlockIterator* it,
                                     struct Ir* statement);
void ir_block_insert_at(struct BlockIterator* it, struct Ir* statement);
void ir_block_insert_expr_at(struct BlockIterator* it, struct ExprIr* expr);
void ir_block_insert_at_end(struct BlockIr* ir, struct Ir* statement);
void ir_block_insert_expr_at_end(struct BlockIr* ir, struct ExprIr* expr);
void ir_block_insert_block_at_end(struct BlockIr* ir, struct BlockIr* block);
struct VarIr* ir_new_var_for_func(strtable_id index);
struct VarIr* ir_block_new_var(struct BlockIr* ir, strtable_id index,
                               struct TypeIr* type);
void ir_block_commit_region_status(struct BlockIr* ir, size_t region_base);
size_t ir_block_region_size(struct BlockIr* ir);

struct Ir* ir_var_cast(struct VarIr* ir);
strtable_id ir_var_index(struct VarIr* ir);
size_t ir_var_offset(struct VarIr* ir);

struct Ir* ir_cf_cast(struct CfIr* ir);
struct BranchCfIr* ir_cf_as_branch(struct CfIr* ir);
struct ReturnCfIr* ir_cf_as_return(struct CfIr* ir);
struct LabelCfIr* ir_cf_as_label(struct CfIr* ir);
struct PushCfIr* ir_cf_as_push(struct CfIr* ir);
struct PopCfIr* ir_cf_as_pop(struct CfIr* ir);
enum CfIrTag ir_cf_tag(struct CfIr* ir);

struct BranchCfIr* ir_new_branch_cf(struct ExprIr* cond_expr,
                                    struct BlockIr* true_block,
                                    struct BlockIr* false_block);
struct CfIr* ir_branch_cf_cast(struct BranchCfIr* ir);
struct ExprIr* ir_branch_cf_cond_expr(struct BranchCfIr* ir);
void ir_branch_cf_set_cond_expr(struct BranchCfIr* ir,
                                struct ExprIr* cond_expr);
struct BlockIr* ir_branch_cf_true_block(struct BranchCfIr* ir);
struct BlockIr* ir_branch_cf_false_block(struct BranchCfIr* ir);

struct ReturnCfIr* ir_new_return_cf(struct ExprIr* expr);
struct CfIr* ir_return_cf_cast(struct ReturnCfIr* ir);
struct ExprIr* ir_return_cf_expr(struct ReturnCfIr* ir);
void ir_return_cf_set_expr(struct ReturnCfIr* ir, struct ExprIr* expr);

struct LabelCfIr* ir_new_label_cf(strtable_id index);
struct CfIr* ir_label_cf_cast(struct LabelCfIr* ir);
strtable_id ir_label_cf_index(struct LabelCfIr* ir);

struct PushCfIr* ir_new_push_cf(strtable_id reg_id);
struct CfIr* ir_push_cf_cast(struct PushCfIr* ir);
strtable_id ir_push_cf_reg_id(struct PushCfIr* ir);

struct PopCfIr* ir_new_pop_cf(strtable_id reg_id);
struct CfIr* ir_pop_cf_cast(struct PopCfIr* ir);
strtable_id ir_pop_cf_reg_id(struct PopCfIr* ir);

struct Ir* ir_expr_cast(struct ExprIr* ir);
struct ConstExprIr* ir_expr_as_const(struct ExprIr* ir);
struct BinopExprIr* ir_expr_as_binop(struct ExprIr* ir);
struct AddrofExprIr* ir_expr_as_addrof(struct ExprIr* ir);
struct LoadExprIr* ir_expr_as_load(struct ExprIr* ir);
struct StoreExprIr* ir_expr_as_store(struct ExprIr* ir);
struct CallExprIr* ir_expr_as_call(struct ExprIr* ir);
enum ExprIrTag ir_expr_tag(struct ExprIr* ir);
strtable_id ir_expr_reg_id(struct ExprIr* ir);
void ir_expr_set_reg_id(struct ExprIr* ir, strtable_id id);

struct ConstExprIr* ir_new_integer_const_expr(intptr_t value);
struct ConstExprIr* ir_new_register_const_expr(strtable_id register_id);
struct ExprIr* ir_const_expr_cast(struct ConstExprIr* ir);
enum ConstExprIrTag ir_const_expr_tag(struct ConstExprIr* ir);
intptr_t ir_const_expr_integer_value(struct ConstExprIr* ir);
strtable_id ir_const_expr_register_id(struct ConstExprIr* ir);

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

struct CallExprIr* ir_new_call_expr_with_var(struct VarIr* var,
                                             struct List* args);
struct ExprIr* ir_call_expr_cast(struct CallExprIr* ir);
struct List* ir_call_expr_args(struct CallExprIr* ir);
enum AddrTag ir_call_expr_tag(struct CallExprIr* ir);
struct VarIr* ir_call_expr_var(struct CallExprIr* ir);
struct BlockIr* ir_call_expr_pre_expr_block(struct CallExprIr* ir);
struct BlockIr* ir_call_expr_post_expr_block(struct CallExprIr* ir);

#endif
