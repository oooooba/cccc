#ifndef IR_H
#define IR_H

#include "list.h"
#include "strtable.h"
#include "type.h"

#include <stddef.h>
#include <stdint.h>

struct Ir;

struct FunctionIr;

struct CfIr;
struct ReturnCfIr;
struct LabelCfIr;

struct ExprIr;
struct ConstExprIr;
struct BinopExprIr;
struct CallExprIr;
struct VarExprIr;
struct UnopExprIr;
struct SubstExprIr;
struct MemberExprIr;
struct DerefExprIr;
struct AddrofExprIr;
struct CastExprIr;

struct StmtIr;
struct ExprStmt;
struct BlockStmt;
struct IfStmt;
struct CfStmt;  // for refactoring
struct PushStmt;
struct PopStmt;

enum IrTag {
    IrTag_Expr,
    IrTag_Var,
    IrTag_Function,
    IrTag_Cf,
    IrTag_Stmt,
};

enum CfIrTag {
    CfIrTag_Return,
    CfIrTag_Label,
};

enum ExprIrTag {
    ExprIrTag_Const,
    ExprIrTag_Binop,
    ExprIrTag_Call,
    ExprIrTag_Var,
    ExprIrTag_Unop,
    ExprIrTag_Subst,
    ExprIrTag_Member,
    ExprIrTag_Deref,
    ExprIrTag_Addrof,
    ExprIrTag_Cast,
};

enum StmtIrTag {
    StmtIrTag_Expr,
    StmtIrTag_Block,
    StmtIrTag_If,
    StmtIrTag_Cf,  // ToDo: for refactoring
    StmtIrTag_Push,
    StmtIrTag_Pop,
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

enum UnopExprIrTag {
    UnopExprIrTag_Notused,
};

struct ExprIr* ir_as_expr(struct Ir* ir);
struct FunctionIr* ir_as_function(struct Ir* ir);
struct CfIr* ir_as_cf(struct Ir* ir);
enum IrTag ir_tag(struct Ir* ir);

struct FunctionIr* ir_new_function(strtable_id name_index,
                                   struct FunctionTypeIr* type);
struct Ir* ir_function_cast(struct FunctionIr* ir);
strtable_id ir_function_name_index(struct FunctionIr* ir);
struct BlockStmtIr* ir_function_body2(struct FunctionIr* ir);
void ir_function_set_body2(struct FunctionIr* ir, struct BlockStmtIr* body);
size_t ir_function_region_size(struct FunctionIr* ir);
void ir_function_set_region_size(struct FunctionIr* ir, size_t region_size);
struct List* ir_function_params(struct FunctionIr* ir);
bool ir_function_has_defined(struct FunctionIr* ir);
void ir_function_define(struct FunctionIr* ir, struct List* params,
                        struct BlockStmtIr* body);
struct TypeIr* ir_function_type(struct FunctionIr* ir);
struct TypeIr* ir_function_result_type(struct FunctionIr* ir);
struct TypeIr* ir_function_result_type(struct FunctionIr* ir);
struct List* ir_function_param_types(struct FunctionIr* ir);

struct Ir* ir_cf_cast(struct CfIr* ir);
struct ReturnCfIr* ir_cf_as_return(struct CfIr* ir);
struct LabelCfIr* ir_cf_as_label(struct CfIr* ir);
enum CfIrTag ir_cf_tag(struct CfIr* ir);

struct ReturnCfIr* ir_new_return_cf(struct ExprIr* expr);
struct CfIr* ir_return_cf_cast(struct ReturnCfIr* ir);
struct ExprIr* ir_return_cf_expr(struct ReturnCfIr* ir);
void ir_return_cf_set_expr(struct ReturnCfIr* ir, struct ExprIr* expr);

struct LabelCfIr* ir_new_label_cf(strtable_id index);
struct CfIr* ir_label_cf_cast(struct LabelCfIr* ir);
strtable_id ir_label_cf_index(struct LabelCfIr* ir);

struct Ir* ir_expr_cast(struct ExprIr* ir);
struct ConstExprIr* ir_expr_as_const(struct ExprIr* ir);
struct BinopExprIr* ir_expr_as_binop(struct ExprIr* ir);
struct VarExprIr* ir_expr_as_var(struct ExprIr* ir);
struct CallExprIr* ir_expr_as_call(struct ExprIr* ir);
struct UnopExprIr* ir_expr_as_unop(struct ExprIr* ir);
struct SubstExprIr* ir_expr_as_subst(struct ExprIr* ir);
struct MemberExprIr* ir_expr_as_member(struct ExprIr* ir);
struct DerefExprIr* ir_expr_as_deref(struct ExprIr* ir);
struct AddrofExprIr* ir_expr_as_addrof(struct ExprIr* ir);
struct CastExprIr* ir_expr_as_cast(struct ExprIr* ir);

enum ExprIrTag ir_expr_tag(struct ExprIr* ir);
struct TypeIr* ir_expr_type(struct ExprIr* ir);
void ir_expr_set_type(struct ExprIr* ir, struct TypeIr* type);
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

struct CallExprIr* ir_new_call_expr(struct ExprIr* function, struct List* args);
struct ExprIr* ir_call_expr_cast(struct CallExprIr* ir);
struct ExprIr* ir_call_expr_function(struct CallExprIr* ir);
void ir_call_expr_set_function(struct CallExprIr* ir, struct ExprIr* function);
struct List* ir_call_expr_args(struct CallExprIr* ir);
struct BlockStmtIr* ir_call_expr_pre_expr_block(struct CallExprIr* ir);
struct BlockStmtIr* ir_call_expr_post_expr_block(struct CallExprIr* ir);

struct ExprIr* ir_var_expr_cast(struct VarExprIr* ir);
struct VarExprIr* ir_var_expr_clone(struct VarExprIr* ir);
struct VarExprIr* ir_var_expr_from_function(struct FunctionIr* function);
bool ir_var_expr_is_function(struct VarExprIr* ir);
size_t ir_var_expr_offset(struct VarExprIr* ir);
strtable_id ir_var_expr_index(struct VarExprIr* ir);
struct TypeIr* ir_var_expr_type(struct VarExprIr* ir);

struct UnopExprIr* ir_new_unop_expr(enum UnopExprIrTag op,
                                    struct ExprIr* operand);
struct ExprIr* ir_unop_expr_cast(struct UnopExprIr* ir);
enum UnopExprIrTag ir_unop_expr_op(struct UnopExprIr* ir);
struct ExprIr* ir_unop_expr_operand(struct UnopExprIr* ir);
void ir_unop_expr_set_operand(struct UnopExprIr* ir, struct ExprIr* operand);

struct SubstExprIr* ir_new_subst_expr(struct ExprIr* addr,
                                      struct ExprIr* value);
struct ExprIr* ir_subst_expr_cast(struct SubstExprIr* ir);
struct ExprIr* ir_subst_expr_addr(struct SubstExprIr* ir);
void ir_subst_expr_set_addr(struct SubstExprIr* ir, struct ExprIr* addr);
struct ExprIr* ir_subst_expr_value(struct SubstExprIr* ir);
void ir_subst_expr_set_value(struct SubstExprIr* ir, struct ExprIr* value);

struct MemberExprIr* ir_new_member_expr(struct ExprIr* base,
                                        strtable_id name_index);
struct ExprIr* ir_member_expr_cast(struct MemberExprIr* ir);
struct ExprIr* ir_member_expr_base(struct MemberExprIr* ir);
void ir_member_expr_set_base(struct MemberExprIr* ir, struct ExprIr* base);
strtable_id ir_member_expr_name_index(struct MemberExprIr* ir);
size_t ir_member_expr_offset(struct MemberExprIr* ir);
void ir_member_expr_set_offset(struct MemberExprIr* ir, size_t offset);

struct DerefExprIr* ir_new_deref_expr(struct ExprIr* operand);
struct ExprIr* ir_deref_expr_cast(struct DerefExprIr* ir);
struct ExprIr* ir_deref_expr_operand(struct DerefExprIr* ir);
void ir_deref_expr_set_operand(struct DerefExprIr* ir, struct ExprIr* operand);

struct AddrofExprIr* ir_new_addrof_expr(struct ExprIr* operand);
struct ExprIr* ir_addrof_expr_cast(struct AddrofExprIr* ir);
struct ExprIr* ir_addrof_expr_operand(struct AddrofExprIr* ir);
void ir_addrof_expr_set_operand(struct AddrofExprIr* ir,
                                struct ExprIr* operand);

struct CastExprIr* ir_new_cast_expr(struct ExprIr* operand,
                                    struct TypeIr* type);
struct ExprIr* ir_cast_expr_cast(struct CastExprIr* ir);
struct ExprIr* ir_cast_expr_operand(struct CastExprIr* ir);
void ir_cast_expr_set_operand(struct CastExprIr* ir, struct ExprIr* operand);

enum StmtIrTag ir_stmt_tag(struct StmtIr* ir);
strtable_id ir_stmt_label_index(struct StmtIr* ir);
void ir_stmt_set_label_index(struct StmtIr* ir, strtable_id label_index);
struct Ir* ir_stmt_cast(struct StmtIr* ir);
struct ExprStmtIr* ir_stmt_as_expr(struct StmtIr* ir);
struct BlockStmtIr* ir_stmt_as_block(struct StmtIr* ir);
struct IfStmtIr* ir_stmt_as_if(struct StmtIr* ir);
struct CfStmtIr* ir_stmt_as_cf(struct StmtIr* ir);  // ToDo: for refactoring
struct PushStmtIr* ir_stmt_as_push(struct StmtIr* ir);
struct PopStmtIr* ir_stmt_as_pop(struct StmtIr* ir);

struct ExprStmtIr* ir_new_expr_stmt(struct ExprIr* expr);
struct StmtIr* ir_expr_stmt_super(struct ExprStmtIr* ir);
struct ExprIr* ir_expr_stmt_expr(struct ExprStmtIr* ir);
void ir_expr_stmt_set_expr(struct ExprStmtIr* ir, struct ExprIr* expr);

struct BlockStmtIr* ir_new_block_stmt(void);
struct StmtIr* ir_block_stmt_super(struct BlockStmtIr* ir);
struct List* ir_block_stmt_statements(struct BlockStmtIr* ir);
void ir_block_stmt_insert_at_end(struct BlockStmtIr* ir, struct StmtIr* stmt);
struct VarExprIr* ir_block_stmt_allocate_variable(struct BlockStmtIr* ir,
                                                  strtable_id name_index,
                                                  struct TypeIr* type);
void ir_block_stmt_commit_region_status(struct BlockStmtIr* ir,
                                        size_t region_base);
size_t ir_block_stmt_region_size(struct BlockStmtIr* ir);

struct IfStmtIr* ir_new_if_stmt(struct ExprIr* cond_expr,
                                struct StmtIr* true_stmt,
                                struct StmtIr* false_stmt);
struct StmtIr* ir_if_stmt_super(struct IfStmtIr* ir);
struct ExprIr* ir_if_stmt_cond_expr(struct IfStmtIr* ir);
void ir_if_stmt_set_cond_expr(struct IfStmtIr* ir, struct ExprIr* cond_expr);
struct StmtIr* ir_if_stmt_true_stmt(struct IfStmtIr* ir);
void ir_if_stmt_set_true_stmt(struct IfStmtIr* ir, struct StmtIr* true_stmt);
struct StmtIr* ir_if_stmt_false_stmt(struct IfStmtIr* ir);
void ir_if_stmt_set_false_stmt(struct IfStmtIr* ir, struct StmtIr* false_stmt);

// ToDo: for refactoring
struct CfStmtIr* ir_new_cf_stmt(struct CfIr* cf);
struct StmtIr* ir_cf_stmt_super(struct CfStmtIr* ir);
struct CfIr* ir_cf_stmt_cf(struct CfStmtIr* ir);
void ir_cf_stmt_set_cf(struct CfStmtIr* ir, struct CfIr* cf);

struct PushStmtIr* ir_new_push_stmt(strtable_id reg_id);
struct StmtIr* ir_push_stmt_super(struct PushStmtIr* ir);
strtable_id ir_push_stmt_reg_id(struct PushStmtIr* ir);

struct PopStmtIr* ir_new_pop_stmt(strtable_id reg_id);
struct StmtIr* ir_pop_stmt_super(struct PopStmtIr* ir);
strtable_id ir_pop_stmt_reg_id(struct PopStmtIr* ir);

#endif
