#include "ir.h"
#include "list.h"
#include "strtable.h"
#include "type.h"

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#define INVALID_VALUE ((size_t)-1)

struct Region {
    size_t base;
    size_t size;
};

static struct Region* ir_new_region(void) {
    struct Region* region = malloc(sizeof(struct Region));
    *region = (struct Region){.base = INVALID_VALUE, .size = 0};
    return region;
}

static size_t ir_region_base(struct Region* region) {
    assert(region->base != INVALID_VALUE);
    return region->base;
}

static size_t ir_region_size(struct Region* region) {
    assert(region->base != INVALID_VALUE);
    return region->size;
}

static size_t ir_region_allocate(struct Region* region, size_t size) {
    assert(region->base == INVALID_VALUE);
    size_t offset = region->size;
    region->size += size;
    return offset;
}

static size_t ir_region_align(struct Region* region, size_t alignment) {
    assert(region->base == INVALID_VALUE);
    size_t old_size = region->size;
    size_t new_size = (old_size + alignment - 1) / alignment * alignment;
    region->size = new_size;
    return new_size - old_size;
}

static size_t ir_region_freeze(struct Region* region, size_t base,
                               size_t alignment) {
    assert(region->base == INVALID_VALUE);
    assert(base % alignment == 0);
    size_t diff = ir_region_align(region, alignment);
    region->base = base;
    return diff;
}

struct Ir {
    enum IrTag tag;
};

static void initialize_ir(struct Ir* ir, enum IrTag tag) { ir->tag = tag; }

struct ExprIr* ir_as_expr(struct Ir* ir) {
    return ir->tag == IrTag_Expr ? (struct ExprIr*)ir : NULL;
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
    struct BlockStmtIr* body;
    size_t region_size;
    struct List* params;  // VarExprIr* list
    struct FunctionTypeIr* type;
};

struct FunctionIr* ir_new_function(strtable_id name_index,
                                   struct FunctionTypeIr* type) {
    struct FunctionIr* ir = malloc(sizeof(struct FunctionIr));
    initialize_ir(ir_function_cast(ir), IrTag_Function);
    ir->name_index = name_index;
    ir->body = NULL;
    ir->region_size = (size_t)-1;
    ir->params = NULL;
    ir->type = type;
    return ir;
}

struct Ir* ir_function_cast(struct FunctionIr* ir) {
    return &ir->as_ir;
}

strtable_id ir_function_name_index(struct FunctionIr* ir) {
    return ir->name_index;
}

struct BlockStmtIr* ir_function_body2(struct FunctionIr* ir) {
    assert(ir->body);
    return ir->body;
}

void ir_function_set_body2(struct FunctionIr* ir, struct BlockStmtIr* body) {
    assert(ir->body);
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
    assert(ir->params);
    return ir->params;
}

bool ir_function_has_defined(struct FunctionIr* ir) { return ir->body != NULL; }

void ir_function_define(struct FunctionIr* ir, struct List* params,
                        struct BlockStmtIr* body) {
    // ToDo: insert equality check between type and type of params
    assert(!ir->params);
    assert(!ir->body);
    ir->params = params;
    ir->body = body;
}

struct TypeIr* ir_function_type(struct FunctionIr* ir) {
    return type_function_super(ir->type);
}

struct TypeIr* ir_function_result_type(struct FunctionIr* ir) {
    return type_function_result_type(ir->type);
}

struct List* ir_function_param_types(struct FunctionIr* ir) {
    return type_function_param_types(ir->type);
}

struct Location {
    struct Region* region;
    strtable_id name_index;
    struct TypeIr* type;
    size_t offset;
};

static struct Location* ir_new_location(struct Region* region,
                                        strtable_id name_index,
                                        struct TypeIr* type) {
    struct Location* loc = malloc(sizeof(struct Location));
    loc->region = region;
    loc->name_index = name_index;
    loc->type = type;
    size_t size = type_size(type);
    ir_region_align(region, size);
    loc->offset = ir_region_allocate(region, size);
    return loc;
}

static strtable_id ir_location_name_index(struct Location* loc) {
    return loc->name_index;
}

static struct TypeIr* ir_location_type(struct Location* loc) {
    return loc->type;
}

static size_t ir_location_offset(struct Location* loc) {
    return ir_region_base(loc->region) + loc->offset;
}

struct CfIr {
    struct Ir as_ir;
    enum CfIrTag tag;
};

enum CfIrTag ir_cf_tag(struct CfIr* ir) { return ir->tag; }

struct Ir* ir_cf_cast(struct CfIr* ir) {
    return &ir->as_ir;
}

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
    struct BlockStmtIr* pre_expr_block;
    struct BlockStmtIr* post_expr_block;
};

struct CallExprIr* ir_new_call_expr(struct ExprIr* function,
                                    struct List* args) {
    struct CallExprIr* ir = malloc(sizeof(struct CallExprIr));
    initialize_expr(ir_call_expr_cast(ir), ExprIrTag_Call);
    ir->function = function;
    ir->args = args;
    ir->pre_expr_block = ir_new_block_stmt();
    ir->post_expr_block = ir_new_block_stmt();
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

struct BlockStmtIr* ir_call_expr_pre_expr_block(struct CallExprIr* ir) {
    return ir->pre_expr_block;
}

struct BlockStmtIr* ir_call_expr_post_expr_block(struct CallExprIr* ir) {
    return ir->post_expr_block;
}

struct VarExprIr {
    struct ExprIr as_expr;
    bool is_function;
    union {
        struct Location* location;
        struct FunctionIr* function;
    };
};

static struct VarExprIr* ir_new_var_expr(struct Location* location) {
    struct VarExprIr* ir = malloc(sizeof(struct VarExprIr));
    initialize_expr(ir_var_expr_cast(ir), ExprIrTag_Var);
    ir->is_function = false;
    ir->location = location;
    return ir;
}

struct ExprIr* ir_var_expr_cast(struct VarExprIr* ir) {
    return &ir->as_expr;
}

struct VarExprIr* ir_var_expr_clone(struct VarExprIr* ir) {
    struct VarExprIr* new_ir = ir_new_var_expr(NULL);
    new_ir->is_function = ir->is_function;
    new_ir->location = ir->location;
    return new_ir;
}

struct VarExprIr* ir_var_expr_from_function(struct FunctionIr* function) {
    struct VarExprIr* new_ir = ir_new_var_expr(NULL);
    new_ir->is_function = true;
    new_ir->function = function;
    return new_ir;
}

bool ir_var_expr_is_function(struct VarExprIr* ir) { return ir->is_function; }

struct FunctionIr* ir_var_expr_function(struct VarExprIr* ir) {
    assert(ir->is_function);
    return ir->function;
}

size_t ir_var_expr_offset(struct VarExprIr* ir) {
    assert(!ir->is_function);
    return ir_location_offset(ir->location);
}

strtable_id ir_var_expr_index(struct VarExprIr* ir) {
    if (ir->is_function)
        return ir_function_name_index(ir->function);
    else
        return ir_location_name_index(ir->location);
}

struct TypeIr* ir_var_expr_type(struct VarExprIr* ir) {
    if (ir->is_function)
        return ir_function_type(ir->function);
    else
        return ir_location_type(ir->location);
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
    strtable_id label_index;
};

static void initialize_stmt(struct StmtIr* ir, enum StmtIrTag tag) {
    initialize_ir(ir_stmt_cast(ir), IrTag_Stmt);
    ir->tag = tag;
    ir->label_index = STRTABLE_INVALID_ID;
}

enum StmtIrTag ir_stmt_tag(struct StmtIr* ir) { return ir->tag; }

strtable_id ir_stmt_label_index(struct StmtIr* ir) { return ir->label_index; }

void ir_stmt_set_label_index(struct StmtIr* ir, strtable_id label_index) {
    ir->label_index = label_index;
}

struct Ir* ir_stmt_cast(struct StmtIr* ir) {
    return &ir->as_ir;
}

struct ExprStmtIr* ir_stmt_as_expr(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Expr ? (struct ExprStmtIr*)ir : NULL;
}

struct BlockStmtIr* ir_stmt_as_block(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Block ? (struct BlockStmtIr*)ir : NULL;
}

struct IfStmtIr* ir_stmt_as_if(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_If ? (struct IfStmtIr*)ir : NULL;
}

struct ReturnStmtIr* ir_stmt_as_return(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Return ? (struct ReturnStmtIr*)ir : NULL;
}

// ToDo: for refactoring
struct CfStmtIr* ir_stmt_as_cf(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Cf ? (struct CfStmtIr*)ir : NULL;
}

struct PushStmtIr* ir_stmt_as_push(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Push ? (struct PushStmtIr*)ir : NULL;
}

struct PopStmtIr* ir_stmt_as_pop(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Pop ? (struct PopStmtIr*)ir : NULL;
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
    struct Region* region;
};

struct BlockStmtIr* ir_new_block_stmt(void) {
    struct BlockStmtIr* ir = malloc(sizeof(struct BlockStmtIr));
    initialize_stmt(ir_block_stmt_super(ir), StmtIrTag_Block);
    list_initialize(&ir->statemetnts);
    ir->region = ir_new_region();
    return ir;
}

struct StmtIr* ir_block_stmt_super(struct BlockStmtIr* ir) {
    return &ir->super;
}

struct List* ir_block_stmt_statements(struct BlockStmtIr* ir) {
    return &ir->statemetnts;
}

void ir_block_stmt_insert_at_end(struct BlockStmtIr* ir, struct StmtIr* stmt) {
    struct ListItem* list_item = malloc(sizeof(struct ListItem));
    list_item->item = stmt;
    list_insert_at_end(&ir->statemetnts, list_from(list_item));
}

struct VarExprIr* ir_block_stmt_allocate_variable(struct BlockStmtIr* ir,
                                                  strtable_id name_index,
                                                  struct TypeIr* type) {
    struct Location* loc = ir_new_location(ir->region, name_index, type);
    return ir_new_var_expr(loc);
}

void ir_block_stmt_commit_region_status(struct BlockStmtIr* ir,
                                        size_t region_base) {
    size_t alignment = sizeof(void*);
    ir_region_freeze(ir->region, region_base, alignment);
}

size_t ir_block_stmt_region_size(struct BlockStmtIr* ir) {
    return ir_region_size(ir->region);
}

struct IfStmtIr {
    struct StmtIr super;
    struct ExprIr* cond_expr;
    struct StmtIr* true_stmt;
    struct StmtIr* false_stmt;
};

struct IfStmtIr* ir_new_if_stmt(struct ExprIr* cond_expr,
                                struct StmtIr* true_stmt,
                                struct StmtIr* false_stmt) {
    struct IfStmtIr* ir = malloc(sizeof(struct IfStmtIr));
    initialize_stmt(ir_if_stmt_super(ir), StmtIrTag_If);
    ir->cond_expr = cond_expr;
    ir->true_stmt = true_stmt;
    ir->false_stmt = false_stmt;
    return ir;
}

struct StmtIr* ir_if_stmt_super(struct IfStmtIr* ir) {
    return &ir->super;
}

struct ExprIr* ir_if_stmt_cond_expr(struct IfStmtIr* ir) {
    return ir->cond_expr;
}

void ir_if_stmt_set_cond_expr(struct IfStmtIr* ir, struct ExprIr* cond_expr) {
    ir->cond_expr = cond_expr;
}

struct StmtIr* ir_if_stmt_true_stmt(struct IfStmtIr* ir) {
    return ir->true_stmt;
}

void ir_if_stmt_set_true_stmt(struct IfStmtIr* ir, struct StmtIr* true_stmt) {
    ir->true_stmt = true_stmt;
}

struct StmtIr* ir_if_stmt_false_stmt(struct IfStmtIr* ir) {
    return ir->false_stmt;
}

void ir_if_stmt_set_false_stmt(struct IfStmtIr* ir, struct StmtIr* false_stmt) {
    ir->false_stmt = false_stmt;
}

struct ReturnStmtIr {
    struct StmtIr super;
    struct ExprIr* expr;
};

struct ReturnStmtIr* ir_new_return_stmt(struct ExprIr* expr) {
    struct ReturnStmtIr* ir = malloc(sizeof(struct ReturnStmtIr));
    initialize_stmt(ir_return_stmt_super(ir), StmtIrTag_Return);
    ir->expr = expr;
    return ir;
}

struct StmtIr* ir_return_stmt_super(struct ReturnStmtIr* ir) {
    return &ir->super;
}

struct ExprIr* ir_return_stmt_expr(struct ReturnStmtIr* ir) {
    return ir->expr;
}

void ir_return_stmt_set_expr(struct ReturnStmtIr* ir, struct ExprIr* expr) {
    ir->expr = expr;
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

struct PushStmtIr {
    struct StmtIr super;
    strtable_id reg_id;
};

struct PushStmtIr* ir_new_push_stmt(strtable_id reg_id) {
    struct PushStmtIr* ir = malloc(sizeof(struct PushStmtIr));
    initialize_stmt(ir_push_stmt_super(ir), StmtIrTag_Push);
    ir->reg_id = reg_id;
    return ir;
}

struct StmtIr* ir_push_stmt_super(struct PushStmtIr* ir) {
    return &ir->super;
}

strtable_id ir_push_stmt_reg_id(struct PushStmtIr* ir) { return ir->reg_id; }

struct PopStmtIr {
    struct StmtIr super;
    strtable_id reg_id;
};

struct PopStmtIr* ir_new_pop_stmt(strtable_id reg_id) {
    struct PopStmtIr* ir = malloc(sizeof(struct PopStmtIr));
    initialize_stmt(ir_pop_stmt_super(ir), StmtIrTag_Pop);
    ir->reg_id = reg_id;
    return ir;
}

struct StmtIr* ir_pop_stmt_super(struct PopStmtIr* ir) {
    return &ir->super;
}

strtable_id ir_pop_stmt_reg_id(struct PopStmtIr* ir) { return ir->reg_id; }
