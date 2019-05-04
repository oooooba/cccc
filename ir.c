#include "ir.h"
#include "list.h"
#include "strtable.h"

#include <assert.h>
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

struct FunctionIr* ir_as_function(struct Ir* ir) {
    return ir->tag == IrTag_Function ? (struct FunctionIr*)ir : NULL;
}

struct CfIr* ir_as_cf(struct Ir* ir) {
    return ir->tag == IrTag_Cf ? (struct CfIr*)ir : NULL;
}

enum IrTag ir_tag(struct Ir* ir) { return ir->tag; }

struct FunctionIr {
    struct Ir as_ir;
    strtable_id name_index;
    struct BlockIr* body;
    size_t region_size;
};

struct FunctionIr* ir_new_function(strtable_id name_index,
                                   struct BlockIr* body) {
    struct FunctionIr* ir = malloc(sizeof(struct FunctionIr));
    initialize_ir(ir_function_cast(ir), IrTag_Function);
    ir->name_index = name_index;
    ir->body = body;
    ir->region_size = (size_t)-1;
    return ir;
}

struct Ir* ir_function_cast(struct FunctionIr* ir) {
    return &ir->as_ir;
}

strtable_id ir_function_name_index(struct FunctionIr* ir) {
    return ir->name_index;
}

struct BlockIr* ir_function_body(struct FunctionIr* ir) {
    return ir->body;
}

size_t ir_function_region_size(struct FunctionIr* ir) {
    assert(ir->region_size != (size_t)-1);
    return ir->region_size;
}

void ir_function_set_region_size(struct FunctionIr* ir, size_t region_size) {
    assert(region_size % sizeof(void*) == 0);
    assert(ir->region_size == (size_t)-1);
    ir->region_size = region_size;
}

struct VarIr {
    struct Ir as_ir;
    strtable_id index;
    size_t region_offset;
    struct BlockIr* block;
};

struct BlockIr {
    struct Ir as_ir;
    struct List statemetnts;
    size_t region_size;
    size_t region_base;
};

struct BlockIterator {
    struct BlockIr* block;
    struct ListHeader* current;
};

struct BlockIr* ir_new_block(void) {
    struct BlockIr* ir = malloc(sizeof(struct BlockIr));
    initialize_ir(ir_block_cast(ir), IrTag_Block);
    list_initialize(&ir->statemetnts);
    ir->region_size = 0;
    ir->region_base = (size_t)-1;
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

void ir_block_insert_at_end(struct BlockIr* ir, struct Ir* statement) {
    struct ListItem* item = malloc(sizeof(struct ListItem));
    item->item = statement;
    list_insert_at_end(&ir->statemetnts, &item->header);
}

void ir_block_insert_expr_at_end(struct BlockIr* ir, struct ExprIr* expr) {
    ir_block_insert_at_end(ir, ir_expr_cast(expr));
}

void ir_block_insert_block_at_end(struct BlockIr* ir, struct BlockIr* block) {
    ir_block_insert_at_end(ir, ir_block_cast(block));
}

static struct VarIr* ir_new_var(struct BlockIr* block, strtable_id index,
                                size_t region_offset) {
    struct VarIr* ir = malloc(sizeof(struct VarIr));
    initialize_ir(ir_var_cast(ir), IrTag_Var);
    ir->index = index;
    ir->region_offset = region_offset;
    ir->block = block;
    return ir;
}

struct VarIr* ir_block_new_var(struct BlockIr* ir, strtable_id index) {
    assert(ir->region_base == (size_t)-1);
    size_t var_size = sizeof(void*);  // ToDo: fix to refer size of type
    size_t region_offset = ir->region_size;
    if (region_offset % var_size) {
        ir->region_size =
            (ir->region_size + var_size - 1) / var_size * var_size;
        region_offset = ir->region_size;
    }
    ir->region_size += var_size;
    return ir_new_var(ir, index, region_offset);
}

static size_t ir_block_region_base(struct BlockIr* ir) {
    assert(ir->region_base != (size_t)-1);
    return ir->region_base;
}

void ir_block_commit_region_status(struct BlockIr* ir, size_t region_base) {
    assert(ir->region_base == (size_t)-1);
    size_t alignment = sizeof(void*);
    assert(region_base % alignment == 0);
    if (ir->region_size % alignment)
        ir->region_size =
            (ir->region_size + alignment - 1) / alignment * alignment;
    ir->region_base = region_base;
}

size_t ir_block_region_size(struct BlockIr* ir) {
    assert(ir->region_base != (size_t)-1);
    return ir->region_size;
}

struct Ir* ir_var_cast(struct VarIr* ir) {
    return &ir->as_ir;
}

size_t ir_var_offset(struct VarIr* ir) {
    return ir_block_region_base(ir->block) + ir->region_offset;
}

strtable_id ir_var_index(struct VarIr* ir) { return ir->index; }

struct CfIr {
    struct Ir as_ir;
    enum CfIrTag tag;
};

static void initialize_cf(struct CfIr* ir, enum CfIrTag tag) {
    initialize_ir(ir_cf_cast(ir), IrTag_Cf);
    ir->tag = tag;
}

enum CfIrTag ir_cf_tag(struct CfIr* ir) { return ir->tag; }

struct Ir* ir_cf_cast(struct CfIr* ir) {
    return &ir->as_ir;
}

struct BranchCfIr* ir_cf_as_branch(struct CfIr* ir) {
    return ir->tag == CfIrTag_Branch ? (struct BranchCfIr*)ir : NULL;
}

struct BranchCfIr {
    struct CfIr as_cf;
    struct ExprIr* cond_expr;
    struct BlockIr* true_block;
    struct BlockIr* false_block;
};

struct BranchCfIr* ir_new_branch_cf(struct ExprIr* cond_expr,
                                    struct BlockIr* true_block,
                                    struct BlockIr* false_block) {
    struct BranchCfIr* ir = malloc(sizeof(struct BranchCfIr));
    initialize_cf(ir_branch_cf_cast(ir), CfIrTag_Branch);
    ir->cond_expr = cond_expr;
    ir->true_block = true_block;
    ir->false_block = false_block;
    return ir;
}

struct CfIr* ir_branch_cf_cast(struct BranchCfIr* ir) {
    return &ir->as_cf;
}

struct ExprIr* ir_branch_cf_cond_expr(struct BranchCfIr* ir) {
    return ir->cond_expr;
}

struct BlockIr* ir_branch_cf_true_block(struct BranchCfIr* ir) {
    return ir->true_block;
}

struct BlockIr* ir_branch_cf_false_block(struct BranchCfIr* ir) {
    return ir->false_block;
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

struct AddrofExprIr* ir_expr_as_addrof(struct ExprIr* ir) {
    return ir->tag == ExprIrTag_Addrof ? (struct AddrofExprIr*)ir : NULL;
}

struct LoadExprIr* ir_expr_as_load(struct ExprIr* ir) {
    return ir->tag == ExprIrTag_Load ? (struct LoadExprIr*)ir : NULL;
}

struct StoreExprIr* ir_expr_as_store(struct ExprIr* ir) {
    return ir->tag == ExprIrTag_Store ? (struct StoreExprIr*)ir : NULL;
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

void ir_binop_expr_set_lhs(struct BinopExprIr* ir, struct ExprIr* lhs) {
    ir->lhs = lhs;
}

void ir_binop_expr_set_rhs(struct BinopExprIr* ir, struct ExprIr* rhs) {
    ir->rhs = rhs;
}

struct AddrofExprIr {
    struct ExprIr as_expr;
    enum AddrTag tag;
    union {
        struct ExprIr* expr;
        struct VarIr* var;
    };
};

struct AddrofExprIr* ir_new_addrof_expr_with_expr(struct ExprIr* expr) {
    struct AddrofExprIr* ir = malloc(sizeof(struct AddrofExprIr));
    initialize_expr(ir_addrof_expr_cast(ir), ExprIrTag_Addrof);
    ir->tag = AddrTag_Expr;
    ir->expr = expr;
    return ir;
}

struct AddrofExprIr* ir_new_addrof_expr_with_var(struct VarIr* var) {
    struct AddrofExprIr* ir = malloc(sizeof(struct AddrofExprIr));
    initialize_expr(ir_addrof_expr_cast(ir), ExprIrTag_Addrof);
    ir->tag = AddrTag_Var;
    ir->var = var;
    return ir;
}

struct ExprIr* ir_addrof_expr_cast(struct AddrofExprIr* ir) {
    return &ir->as_expr;
}

enum AddrTag ir_addrof_expr_tag(struct AddrofExprIr* ir) { return ir->tag; }

struct VarIr* ir_addrof_expr_operand_as_var(struct AddrofExprIr* ir) {
    return ir->tag == AddrTag_Var ? ir->var : NULL;
}

struct ExprIr* ir_addrof_expr_operand_as_expr(struct AddrofExprIr* ir) {
    return ir->tag == AddrTag_Expr ? ir->expr : NULL;
}

struct LoadExprIr {
    struct ExprIr as_expr;
    struct ExprIr* addr;
};

struct LoadExprIr* ir_new_load_expr(struct ExprIr* addr) {
    struct LoadExprIr* ir = malloc(sizeof(struct LoadExprIr));
    initialize_expr(ir_load_expr_cast(ir), ExprIrTag_Load);
    ir->addr = addr;
    return ir;
}

struct ExprIr* ir_load_expr_cast(struct LoadExprIr* ir) {
    return &ir->as_expr;
}

struct ExprIr* ir_load_expr_addr(struct LoadExprIr* ir) {
    return ir->addr;
}

void ir_load_expr_set_addr(struct LoadExprIr* ir, struct ExprIr* addr) {
    ir->addr = addr;
}

struct StoreExprIr {
    struct ExprIr as_expr;
    struct ExprIr* addr;
    struct ExprIr* value;
};

struct StoreExprIr* ir_new_store_expr(struct ExprIr* addr,
                                      struct ExprIr* value) {
    struct StoreExprIr* ir = malloc(sizeof(struct StoreExprIr));
    initialize_expr(ir_store_expr_cast(ir), ExprIrTag_Store);
    ir->addr = addr;
    ir->value = value;
    return ir;
}

struct ExprIr* ir_store_expr_cast(struct StoreExprIr* ir) {
    return &ir->as_expr;
}

struct ExprIr* ir_store_expr_addr(struct StoreExprIr* ir) {
    return ir->addr;
}

void ir_store_expr_set_addr(struct StoreExprIr* ir, struct ExprIr* addr) {
    ir->addr = addr;
}

struct ExprIr* ir_store_expr_value(struct StoreExprIr* ir) {
    return ir->value;
}

void ir_store_expr_set_value(struct StoreExprIr* ir, struct ExprIr* value) {
    ir->value = value;
}
