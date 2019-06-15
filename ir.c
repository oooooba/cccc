#include "ir.h"
#include "list.h"
#include "strtable.h"
#include "type.h"

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
    struct List* params;  // VarExprIr* list
    struct FunctionTypeIr* type;
};

struct FunctionIr* ir_new_function(strtable_id name_index,
                                   struct FunctionTypeIr* type,
                                   struct List* params, struct BlockIr* body) {
    // ToDo: insert assertion code to validate equality between type and type of
    // params

    struct FunctionIr* ir = malloc(sizeof(struct FunctionIr));
    initialize_ir(ir_function_cast(ir), IrTag_Function);
    ir->name_index = name_index;
    ir->body = body;
    ir->region_size = (size_t)-1;
    ir->params = params;
    ir->type = type;
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

void ir_function_set_body(struct FunctionIr* ir, struct BlockIr* body) {
    ir->body = body;
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

struct List* ir_function_params(struct FunctionIr* ir) {
    return ir->params;
}

struct TypeIr* ir_function_result_type(struct FunctionIr* ir) {
    return type_function_result_type(ir->type);
}

struct List* ir_function_param_types(struct FunctionIr* ir) {
    return type_function_param_types(ir->type);
}

struct Location {
    bool is_function;
    union {
        struct BlockIr* block;                   // location for a variable
        struct FunctionIr* function_definition;  // function definition
    };
    strtable_id name_index;
    struct TypeIr* type;
    size_t region_offset;
};

static struct Location* ir_new_location(struct BlockIr* block,
                                        strtable_id name_index,
                                        struct TypeIr* type,
                                        size_t region_offset) {
    struct Location* loc = malloc(sizeof(struct Location));
    loc->is_function = false;
    loc->block = block;
    loc->name_index = name_index;
    loc->type = type;
    loc->region_offset = region_offset;
    return loc;
}

struct Location* ir_declare_function(strtable_id name_index,
                                     struct FunctionTypeIr* type) {
    struct Location* loc = malloc(sizeof(struct Location));
    loc->is_function = true;
    loc->block = NULL;
    loc->name_index = name_index;
    loc->type = type_function_super(type);
    loc->region_offset = (size_t)-1;
    return loc;
}

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

void ir_block_insert_at(struct BlockIterator* it, struct Ir* statement) {
    struct ListItem* item = malloc(sizeof(struct ListItem));
    item->item = statement;
    list_insert_at(&it->block->statemetnts, it->current, &item->header);
}

void ir_block_insert_expr_at(struct BlockIterator* it, struct ExprIr* expr) {
    ir_block_insert_at(it, ir_expr_cast(expr));
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

struct Location* ir_block_allocate_location(struct BlockIr* ir,
                                            strtable_id name_index,
                                            struct TypeIr* type) {
    assert(ir->region_base == (size_t)-1);
    size_t var_size = type_size(type);
    ir->region_size = (ir->region_size + var_size - 1) / var_size * var_size;
    size_t region_offset = ir->region_size;
    ir->region_size += var_size;
    return ir_new_location(ir, name_index, type, region_offset);
}

static size_t ir_block_region_base(struct BlockIr* ir) {
    assert(ir->region_base != (size_t)-1);
    return ir->region_base;
}

void ir_block_commit_region_status(struct BlockIr* ir, size_t region_base) {
    assert(ir->region_base == (size_t)-1);
    size_t alignment = sizeof(void*);
    assert(region_base % alignment == 0);
    ir->region_size = (ir->region_size + alignment - 1) / alignment * alignment;
    ir->region_base = region_base;
}

size_t ir_block_region_size(struct BlockIr* ir) {
    assert(ir->region_base != (size_t)-1);
    return ir->region_size;
}

strtable_id ir_location_name_index(struct Location* loc) {
    return loc->name_index;
}

static struct TypeIr* ir_location_type(struct Location* loc) {
    return loc->type;
}

static size_t ir_location_offset(struct Location* loc) {
    return ir_block_region_base(loc->block) + loc->region_offset;
}

static bool ir_location_is_function(struct Location* loc) {
    return loc->is_function;
}

struct FunctionIr* ir_location_function_definition(struct Location* loc) {
    assert(ir_location_is_function(loc));
    return loc->function_definition;
}

void ir_location_set_function_definition(struct Location* loc,
                                         struct FunctionIr* function) {
    assert(ir_location_is_function(loc));
    loc->function_definition = function;
}

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

struct ReturnCfIr* ir_cf_as_return(struct CfIr* ir) {
    return ir->tag == CfIrTag_Return ? (struct ReturnCfIr*)ir : NULL;
}

struct LabelCfIr* ir_cf_as_label(struct CfIr* ir) {
    return ir->tag == CfIrTag_Label ? (struct LabelCfIr*)ir : NULL;
}

struct PushCfIr* ir_cf_as_push(struct CfIr* ir) {
    return ir->tag == CfIrTag_Push ? (struct PushCfIr*)ir : NULL;
}

struct PopCfIr* ir_cf_as_pop(struct CfIr* ir) {
    return ir->tag == CfIrTag_Pop ? (struct PopCfIr*)ir : NULL;
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

void ir_branch_cf_set_cond_expr(struct BranchCfIr* ir,
                                struct ExprIr* cond_expr) {
    ir->cond_expr = cond_expr;
}

struct BlockIr* ir_branch_cf_true_block(struct BranchCfIr* ir) {
    return ir->true_block;
}

void ir_branch_cf_set_true_block(struct BranchCfIr* ir,
                                 struct BlockIr* true_block) {
    ir->true_block = true_block;
}

struct BlockIr* ir_branch_cf_false_block(struct BranchCfIr* ir) {
    return ir->false_block;
}

void ir_branch_cf_set_false_block(struct BranchCfIr* ir,
                                  struct BlockIr* false_block) {
    ir->false_block = false_block;
}

struct ReturnCfIr {
    struct CfIr as_cf;
    struct ExprIr* expr;
};

struct ReturnCfIr* ir_new_return_cf(struct ExprIr* expr) {
    struct ReturnCfIr* ir = malloc(sizeof(struct ReturnCfIr));
    initialize_cf(ir_return_cf_cast(ir), CfIrTag_Return);
    ir->expr = expr;
    return ir;
}

struct CfIr* ir_return_cf_cast(struct ReturnCfIr* ir) {
    return &ir->as_cf;
}

struct ExprIr* ir_return_cf_expr(struct ReturnCfIr* ir) {
    return ir->expr;
}

void ir_return_cf_set_expr(struct ReturnCfIr* ir, struct ExprIr* expr) {
    ir->expr = expr;
}

struct LabelCfIr {
    struct CfIr as_cf;
    strtable_id index;
};

struct LabelCfIr* ir_new_label_cf(strtable_id index) {
    struct LabelCfIr* ir = malloc(sizeof(struct LabelCfIr));
    initialize_cf(ir_label_cf_cast(ir), CfIrTag_Label);
    ir->index = index;
    return ir;
}

struct CfIr* ir_label_cf_cast(struct LabelCfIr* ir) {
    return &ir->as_cf;
}

strtable_id ir_label_cf_index(struct LabelCfIr* ir) { return ir->index; }

struct PushCfIr {
    struct CfIr as_cf;
    strtable_id reg_id;
};

struct PushCfIr* ir_new_push_cf(strtable_id reg_id) {
    struct PushCfIr* ir = malloc(sizeof(struct PushCfIr));
    initialize_cf(ir_push_cf_cast(ir), CfIrTag_Push);
    ir->reg_id = reg_id;
    return ir;
}

struct CfIr* ir_push_cf_cast(struct PushCfIr* ir) {
    return &ir->as_cf;
}

strtable_id ir_push_cf_reg_id(struct PushCfIr* ir) { return ir->reg_id; }

struct PopCfIr {
    struct CfIr as_cf;
    strtable_id reg_id;
};

struct PopCfIr* ir_new_pop_cf(strtable_id reg_id) {
    struct PopCfIr* ir = malloc(sizeof(struct PopCfIr));
    initialize_cf(ir_pop_cf_cast(ir), CfIrTag_Pop);
    ir->reg_id = reg_id;
    return ir;
}

struct CfIr* ir_pop_cf_cast(struct PopCfIr* ir) {
    return &ir->as_cf;
}

strtable_id ir_pop_cf_reg_id(struct PopCfIr* ir) { return ir->reg_id; }

struct ExprIr {
    struct Ir as_ir;
    enum ExprIrTag tag;
    struct TypeIr* type;
    strtable_id reg_id;
};

static void initialize_expr(struct ExprIr* ir, enum ExprIrTag tag) {
    initialize_ir(ir_expr_cast(ir), IrTag_Expr);
    ir->tag = tag;
    ir->type = NULL;
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

struct CallExprIr* ir_expr_as_call(struct ExprIr* ir) {
    return ir->tag == ExprIrTag_Call ? (struct CallExprIr*)ir : NULL;
}

struct VarExprIr* ir_expr_as_var(struct ExprIr* ir) {
    return ir->tag == ExprIrTag_Var ? (struct VarExprIr*)ir : NULL;
}

struct UnopExprIr* ir_expr_as_unop(struct ExprIr* ir) {
    return ir->tag == ExprIrTag_Unop ? (struct UnopExprIr*)ir : NULL;
}

struct SubstExprIr* ir_expr_as_subst(struct ExprIr* ir) {
    return ir->tag == ExprIrTag_Subst ? (struct SubstExprIr*)ir : NULL;
}

struct MemberExprIr* ir_expr_as_member(struct ExprIr* ir) {
    return ir->tag == ExprIrTag_Member ? (struct MemberExprIr*)ir : NULL;
}

struct DerefExprIr* ir_expr_as_deref(struct ExprIr* ir) {
    return ir->tag == ExprIrTag_Deref ? (struct DerefExprIr*)ir : NULL;
}

struct AddrofExprIr* ir_expr_as_addrof(struct ExprIr* ir) {
    return ir->tag == ExprIrTag_Addrof ? (struct AddrofExprIr*)ir : NULL;
}

struct CastExprIr* ir_expr_as_cast(struct ExprIr* ir) {
    return ir->tag == ExprIrTag_Cast ? (struct CastExprIr*)ir : NULL;
}

enum ExprIrTag ir_expr_tag(struct ExprIr* ir) { return ir->tag; }

struct TypeIr* ir_expr_type(struct ExprIr* ir) {
    return ir->type;
}

void ir_expr_set_type(struct ExprIr* ir, struct TypeIr* type) {
    ir->type = type;
}

strtable_id ir_expr_reg_id(struct ExprIr* ir) { return ir->reg_id; }

void ir_expr_set_reg_id(struct ExprIr* ir, strtable_id id) { ir->reg_id = id; }

struct ConstExprIr {
    struct ExprIr as_expr;
    enum ConstExprIrTag tag;
    union {
        intptr_t integer;
        strtable_id register_id;
    };
};

struct ConstExprIr* ir_new_integer_const_expr(intptr_t value) {
    struct ConstExprIr* ir = malloc(sizeof(struct ConstExprIr));
    initialize_expr(ir_const_expr_cast(ir), ExprIrTag_Const);
    ir->tag = ConstExprIrTag_Integer;
    ir->integer = value;
    return ir;
}

struct ConstExprIr* ir_new_register_const_expr(strtable_id register_id) {
    struct ConstExprIr* ir = malloc(sizeof(struct ConstExprIr));
    initialize_expr(ir_const_expr_cast(ir), ExprIrTag_Const);
    ir->tag = ConstExprIrTag_Register;
    ir->register_id = register_id;
    return ir;
}

struct ExprIr* ir_const_expr_cast(struct ConstExprIr* ir) {
    return &ir->as_expr;
}

enum ConstExprIrTag ir_const_expr_tag(struct ConstExprIr* ir) {
    return ir->tag;
}

intptr_t ir_const_expr_integer_value(struct ConstExprIr* ir) {
    assert(ir->tag == ConstExprIrTag_Integer);
    return ir->integer;
}

strtable_id ir_const_expr_register_id(struct ConstExprIr* ir) {
    assert(ir->tag == ConstExprIrTag_Register);
    return ir->register_id;
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

struct CallExprIr {
    struct ExprIr as_expr;
    struct ExprIr* function;
    struct List* args;  // ExprIr* list
    struct BlockIr* pre_expr_block;
    struct BlockIr* post_expr_block;
};

struct CallExprIr* ir_new_call_expr(struct ExprIr* function,
                                    struct List* args) {
    struct CallExprIr* ir = malloc(sizeof(struct CallExprIr));
    initialize_expr(ir_call_expr_cast(ir), ExprIrTag_Call);
    ir->function = function;
    ir->args = args;
    ir->pre_expr_block = ir_new_block();
    ir->post_expr_block = ir_new_block();
    return ir;
}

struct ExprIr* ir_call_expr_cast(struct CallExprIr* ir) {
    return &ir->as_expr;
}

struct ExprIr* ir_call_expr_function(struct CallExprIr* ir) {
    return ir->function;
}

void ir_call_expr_set_function(struct CallExprIr* ir, struct ExprIr* function) {
    ir->function = function;
}

struct List* ir_call_expr_args(struct CallExprIr* ir) {
    return ir->args;
}

struct BlockIr* ir_call_expr_pre_expr_block(struct CallExprIr* ir) {
    return ir->pre_expr_block;
}

struct BlockIr* ir_call_expr_post_expr_block(struct CallExprIr* ir) {
    return ir->post_expr_block;
}

struct VarExprIr {
    struct ExprIr as_expr;
    struct Location* location;
};

struct VarExprIr* ir_new_var_expr(struct Location* location) {
    struct VarExprIr* ir = malloc(sizeof(struct VarExprIr));
    initialize_expr(ir_var_expr_cast(ir), ExprIrTag_Var);
    ir->location = location;
    return ir;
}

struct ExprIr* ir_var_expr_cast(struct VarExprIr* ir) {
    return &ir->as_expr;
}

struct Location* ir_var_expr_location(struct VarExprIr* ir) {
    return ir->location;
}

size_t ir_var_expr_offset(struct VarExprIr* ir) {
    return ir_location_offset(ir->location);
}

strtable_id ir_var_expr_index(struct VarExprIr* ir) {
    return ir_location_name_index(ir->location);
}

struct TypeIr* ir_var_expr_type(struct VarExprIr* ir) {
    return ir_location_type(ir->location);
}

bool ir_var_expr_is_function(struct VarExprIr* ir) {
    return ir_location_is_function(ir->location);
}

struct UnopExprIr {
    struct ExprIr as_expr;
    enum UnopExprIrTag op;
    struct ExprIr* operand;
};

struct UnopExprIr* ir_new_unop_expr(enum UnopExprIrTag op,
                                    struct ExprIr* operand) {
    struct UnopExprIr* ir = malloc(sizeof(struct UnopExprIr));
    initialize_expr(ir_unop_expr_cast(ir), ExprIrTag_Unop);
    ir->op = op;
    ir->operand = operand;
    return ir;
}

struct ExprIr* ir_unop_expr_cast(struct UnopExprIr* ir) {
    return &ir->as_expr;
}

enum UnopExprIrTag ir_unop_expr_op(struct UnopExprIr* ir) { return ir->op; }

struct ExprIr* ir_unop_expr_operand(struct UnopExprIr* ir) {
    return ir->operand;
}

void ir_unop_expr_set_operand(struct UnopExprIr* ir, struct ExprIr* operand) {
    ir->operand = operand;
}

struct SubstExprIr {
    struct ExprIr as_expr;
    struct ExprIr* addr;
    struct ExprIr* value;
};

struct SubstExprIr* ir_new_subst_expr(struct ExprIr* addr,
                                      struct ExprIr* value) {
    struct SubstExprIr* ir = malloc(sizeof(struct SubstExprIr));
    initialize_expr(ir_subst_expr_cast(ir), ExprIrTag_Subst);
    ir->addr = addr;
    ir->value = value;
    return ir;
}

struct ExprIr* ir_subst_expr_cast(struct SubstExprIr* ir) {
    return &ir->as_expr;
}

struct ExprIr* ir_subst_expr_addr(struct SubstExprIr* ir) {
    return ir->addr;
}

void ir_subst_expr_set_addr(struct SubstExprIr* ir, struct ExprIr* addr) {
    ir->addr = addr;
}

struct ExprIr* ir_subst_expr_value(struct SubstExprIr* ir) {
    return ir->value;
}

void ir_subst_expr_set_value(struct SubstExprIr* ir, struct ExprIr* value) {
    ir->value = value;
}

struct MemberExprIr {
    struct ExprIr as_expr;
    struct ExprIr* base;
    strtable_id name_index;
    size_t offset;
};

struct MemberExprIr* ir_new_member_expr(struct ExprIr* base,
                                        strtable_id name_index) {
    struct MemberExprIr* ir = malloc(sizeof(struct MemberExprIr));
    initialize_expr(ir_member_expr_cast(ir), ExprIrTag_Member);
    ir->base = base;
    ir->name_index = name_index;
    ir->offset = (size_t)-1;
    return ir;
}

struct ExprIr* ir_member_expr_cast(struct MemberExprIr* ir) {
    return &ir->as_expr;
}

struct ExprIr* ir_member_expr_base(struct MemberExprIr* ir) {
    return ir->base;
}

void ir_member_expr_set_base(struct MemberExprIr* ir, struct ExprIr* base) {
    ir->base = base;
}

strtable_id ir_member_expr_name_index(struct MemberExprIr* ir) {
    return ir->name_index;
}

size_t ir_member_expr_offset(struct MemberExprIr* ir) { return ir->offset; }

void ir_member_expr_set_offset(struct MemberExprIr* ir, size_t offset) {
    ir->offset = offset;
}

struct DerefExprIr {
    struct ExprIr as_expr;
    struct ExprIr* operand;
};

struct DerefExprIr* ir_new_deref_expr(struct ExprIr* operand) {
    struct DerefExprIr* ir = malloc(sizeof(struct DerefExprIr));
    initialize_expr(ir_deref_expr_cast(ir), ExprIrTag_Deref);
    ir->operand = operand;
    return ir;
}

struct ExprIr* ir_deref_expr_cast(struct DerefExprIr* ir) {
    return &ir->as_expr;
}

struct ExprIr* ir_deref_expr_operand(struct DerefExprIr* ir) {
    return ir->operand;
}

void ir_deref_expr_set_operand(struct DerefExprIr* ir, struct ExprIr* operand) {
    ir->operand = operand;
}

struct AddrofExprIr {
    struct ExprIr as_expr;
    struct ExprIr* operand;
};

struct AddrofExprIr* ir_new_addrof_expr(struct ExprIr* operand) {
    struct AddrofExprIr* ir = malloc(sizeof(struct AddrofExprIr));
    initialize_expr(ir_addrof_expr_cast(ir), ExprIrTag_Addrof);
    ir->operand = operand;
    return ir;
}

struct ExprIr* ir_addrof_expr_cast(struct AddrofExprIr* ir) {
    return &ir->as_expr;
}

struct ExprIr* ir_addrof_expr_operand(struct AddrofExprIr* ir) {
    return ir->operand;
}

void ir_addrof_expr_set_operand(struct AddrofExprIr* ir,
                                struct ExprIr* operand) {
    ir->operand = operand;
}

struct CastExprIr {
    struct ExprIr as_expr;
    struct ExprIr* operand;
};

struct CastExprIr* ir_new_cast_expr(struct ExprIr* operand,
                                    struct TypeIr* type) {
    struct CastExprIr* ir = malloc(sizeof(struct CastExprIr));
    initialize_expr(ir_cast_expr_cast(ir), ExprIrTag_Cast);
    ir->operand = operand;
    ir_expr_set_type(ir_cast_expr_cast(ir), type);
    return ir;
}

struct ExprIr* ir_cast_expr_cast(struct CastExprIr* ir) {
    return &ir->as_expr;
}

struct ExprIr* ir_cast_expr_operand(struct CastExprIr* ir) {
    return ir->operand;
}

void ir_cast_expr_set_operand(struct CastExprIr* ir, struct ExprIr* operand) {
    ir->operand = operand;
}

struct StmtIr {
    struct Ir as_ir;
    enum StmtIrTag tag;
};

static void initialize_stmt(struct StmtIr* ir, enum StmtIrTag tag) {
    initialize_ir(ir_stmt_cast(ir), IrTag_Stmt);
    ir->tag = tag;
}

enum StmtIrTag ir_stmt_tag(struct StmtIr* ir) { return ir->tag; }

struct Ir* ir_stmt_cast(struct StmtIr* ir) {
    return &ir->as_ir;
}

struct ExprStmtIr* ir_stmt_as_expr(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Expr ? (struct ExprStmtIr*)ir : NULL;
}

struct BlockStmtIr* ir_stmt_as_block(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Block ? (struct BlockStmtIr*)ir : NULL;
}

// ToDo: for refactoring
struct CfStmtIr* ir_stmt_as_cf(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Cf ? (struct CfStmtIr*)ir : NULL;
}

struct ExprStmtIr {
    struct StmtIr super;
    struct ExprIr* expr;
};

struct ExprStmtIr* ir_new_expr_stmt(struct ExprIr* expr) {
    struct ExprStmtIr* ir = malloc(sizeof(struct ExprStmtIr));
    initialize_stmt(ir_expr_stmt_super(ir), StmtIrTag_Expr);
    ir->expr = expr;
    return ir;
}

struct StmtIr* ir_expr_stmt_super(struct ExprStmtIr* ir) {
    return &ir->super;
}

struct ExprIr* ir_expr_stmt_expr(struct ExprStmtIr* ir) {
    return ir->expr;
}

void ir_expr_stmt_set_expr(struct ExprStmtIr* ir, struct ExprIr* expr) {
    ir->expr = expr;
}

struct BlockStmtIr {
    struct StmtIr super;
    struct List
        statemetnts;  // elem type: struct ListItem, item type: struct StmtIr*
    size_t region_size;
    size_t region_base;
};

struct BlockStmtIr* ir_new_block_stmt(void) {
    struct BlockStmtIr* ir = malloc(sizeof(struct BlockStmtIr));
    initialize_stmt(ir_block_stmt_super(ir), StmtIrTag_Block);
    list_initialize(&ir->statemetnts);
    ir->region_size = 0;
    ir->region_base = (size_t)-1;
    return ir;
}

struct StmtIr* ir_block_stmt_super(struct BlockStmtIr* ir) {
    return &ir->super;
}

struct List* ir_block_stmt_statements(struct BlockStmtIr* ir) {
    return &ir->statemetnts;
}

struct CfStmtIr {
    struct StmtIr super;
    struct CfIr* cf;
};

struct CfStmtIr* ir_new_cf_stmt(struct CfIr* cf) {
    struct CfStmtIr* ir = malloc(sizeof(struct CfStmtIr));
    initialize_stmt(ir_cf_stmt_super(ir), StmtIrTag_Cf);
    ir->cf = cf;
    return ir;
}

struct StmtIr* ir_cf_stmt_super(struct CfStmtIr* ir) {
    return &ir->super;
}

struct CfIr* ir_cf_stmt_cf(struct CfStmtIr* ir) {
    return ir->cf;
}

void ir_cf_stmt_set_cf(struct CfStmtIr* ir, struct CfIr* cf) { ir->cf = cf; }

struct BlockStmtIr* ir_block_stmt_convert_for_refactoring(struct BlockIr* ir) {
    struct BlockStmtIr* block = ir_new_block_stmt();
    struct List* stmts = ir_block_stmt_statements(block);

    struct BlockIterator* it = ir_block_new_iterator(ir);
    for (;;) {
        struct Ir* stmt = ir_block_iterator_next(it);
        if (!stmt) break;

        struct ListItem* list_item = malloc(sizeof(struct ListItem));
        if (ir_tag(stmt) == IrTag_Expr)
            list_item->item = ir_new_expr_stmt(ir_as_expr(stmt));
        else if (ir_tag(stmt) == IrTag_Cf)
            list_item->item = ir_new_cf_stmt(ir_as_cf(stmt));
        else if (ir_tag(stmt) == IrTag_Block)
            list_item->item = ir_block_stmt_super(
                ir_block_stmt_convert_for_refactoring(ir_as_block(stmt)));
        else
            assert(false);

        list_insert_at_end(stmts, &list_item->header);
    }
    return block;
}
