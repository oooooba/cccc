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
    region->base = INVALID_VALUE;
    region->size = 0;
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
    region->size = region->size + size;
    return offset;
}

static size_t roundup(size_t x, size_t y) {
    // to avoid division operation, ToDo: fix to handle div instruction
    long ix = x;
    size_t z = 0;
    while (ix > 0) {
        ix = ix - y;
        ++z;
    }
    return z * y;
}

static size_t ir_region_align(struct Region* region, size_t alignment) {
    assert(region->base == INVALID_VALUE);
    size_t old_size = region->size;
    size_t new_size = roundup(old_size, alignment);
    region->size = new_size;
    return new_size - old_size;
}

static size_t ir_region_freeze(struct Region* region, size_t base,
                               size_t alignment) {
    assert(region->base == INVALID_VALUE);
    assert(roundup(base, alignment) == base);
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

enum IrTag ir_tag(struct Ir* ir) { return ir->tag; }

struct GlobalIr {
    struct Ir as_ir;
    bool has_definition;
    bool is_public;
    struct FunctionIr* function;
};

struct GlobalIr* ir_new_global_from_function(struct FunctionIr* function,
                                             bool has_definition,
                                             bool is_public) {
    struct GlobalIr* ir = malloc(sizeof(struct GlobalIr));
    initialize_ir(ir_global_cast(ir), IrTag_Global);
    ir->has_definition = has_definition;
    ir->is_public = is_public;
    ir->function = function;
    return ir;
}

struct Ir* ir_global_cast(struct GlobalIr* ir) {
    return &ir->as_ir;
}

bool ir_global_has_definition(struct GlobalIr* ir) {
    return ir->has_definition;
}

bool ir_global_is_public(struct GlobalIr* ir) { return ir->is_public; }

struct FunctionIr* ir_global_function(struct GlobalIr* ir) {
    return ir->function;
}

struct FunctionIr {
    struct Ir as_ir;
    strtable_id name_index;
    struct BlockStmtIr* body;
    size_t region_size;
    struct List* param_decl_list;  // DeclStmtIr* list
    struct FunctionTypeIr* type;
};

struct FunctionIr* ir_new_function(strtable_id name_index,
                                   struct FunctionTypeIr* type) {
    struct FunctionIr* ir = malloc(sizeof(struct FunctionIr));
    initialize_ir(ir_function_cast(ir), IrTag_Function);
    ir->name_index = name_index;
    ir->body = NULL;
    ir->region_size = (size_t)-1;
    ir->param_decl_list = NULL;
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
    assert(roundup(region_size, sizeof(void*)) == region_size);
    assert(ir->region_size == (size_t)-1);
    ir->region_size = region_size;
}

struct List* ir_function_param_decl_list(struct FunctionIr* ir) {
    assert(ir->param_decl_list);
    return ir->param_decl_list;
}

bool ir_function_has_defined(struct FunctionIr* ir) { return ir->body != NULL; }

void ir_function_define(struct FunctionIr* ir, struct List* param_decl_list,
                        struct BlockStmtIr* body) {
    // ToDo: insert equality check between type and type of param_decl_list
    assert(!ir->param_decl_list);
    assert(!ir->body);
    ir->param_decl_list = param_decl_list;
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

static struct TypeIr* ir_location_type(struct Location* loc) {
    return loc->type;
}

static size_t ir_location_offset(struct Location* loc) {
    return ir_region_base(loc->region) + loc->offset;
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

struct CondExprIr* ir_expr_as_cond(struct ExprIr* ir) {
    return ir->tag == ExprIrTag_Cond ? (struct CondExprIr*)ir : NULL;
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
        strtable_id string_literal_id;
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

struct ConstExprIr* ir_new_string_const_expr(strtable_id string_literal_id) {
    struct ConstExprIr* ir = malloc(sizeof(struct ConstExprIr));
    initialize_expr(ir_const_expr_cast(ir), ExprIrTag_Const);
    ir->tag = ConstExprIrTag_String;
    ir->string_literal_id = string_literal_id;
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

strtable_id ir_const_expr_string_literal_id(struct ConstExprIr* ir) {
    assert(ir->tag == ConstExprIrTag_String);
    return ir->string_literal_id;
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

enum VarExprTag {
    VarExprTag_Invalid,
    VarExprTag_Location,
    VarExprTag_Function,
};

struct VarExprIr {
    struct ExprIr as_expr;
    strtable_id id;
    enum VarExprTag tag;
    union {
        struct Location* location;
        struct FunctionIr* function;
    };
};

static struct VarExprIr* ir_new_var_expr_inner(strtable_id id) {
    struct VarExprIr* ir = malloc(sizeof(struct VarExprIr));
    initialize_expr(ir_var_expr_cast(ir), ExprIrTag_Var);
    ir->id = id;
    return ir;
}

struct VarExprIr* ir_new_var_expr(strtable_id id) {
    struct VarExprIr* ir = ir_new_var_expr_inner(id);
    ir->tag = VarExprTag_Invalid;
    ir->location = NULL;
    return ir;
}

static struct VarExprIr* ir_new_var_expr_from_location(
    strtable_id id, struct Location* location) {
    struct VarExprIr* ir = ir_new_var_expr_inner(id);
    ir->tag = VarExprTag_Location;
    ir->location = location;
    return ir;
}

struct VarExprIr* ir_new_var_expr_from_function(strtable_id id,
                                                struct FunctionIr* function) {
    struct VarExprIr* ir = ir_new_var_expr_inner(id);
    ir->tag = VarExprTag_Function;
    ir->function = function;
    return ir;
}

struct ExprIr* ir_var_expr_cast(struct VarExprIr* ir) {
    return &ir->as_expr;
}

void ir_var_expr_copy(struct VarExprIr* ir, struct VarExprIr* src) {
    assert(src->tag != VarExprTag_Invalid);
    ir->tag = src->tag;
    ir->location = src->location;
}

bool ir_var_expr_is_invalid(struct VarExprIr* ir) {
    return ir->tag == VarExprTag_Invalid;
}

bool ir_var_expr_is_function(struct VarExprIr* ir) {
    return ir->tag == VarExprTag_Function;
}

struct FunctionIr* ir_var_expr_function(struct VarExprIr* ir) {
    assert(ir->tag == VarExprTag_Function);
    return ir->function;
}

size_t ir_var_expr_offset(struct VarExprIr* ir) {
    assert(ir->tag == VarExprTag_Location);
    return ir_location_offset(ir->location);
}

strtable_id ir_var_expr_index(struct VarExprIr* ir) { return ir->id; }

struct TypeIr* ir_var_expr_type(struct VarExprIr* ir) {
    switch (ir->tag) {
        case VarExprTag_Location:
            return ir_location_type(ir->location);
        case VarExprTag_Function:
            return ir_function_type(ir->function);
        default:
            assert(false);
            return NULL;
    }
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

struct ExprIr* ir_expr_clone(struct ExprIr* ir) {
    switch (ir->tag) {
        case ExprIrTag_Const: {
            struct ConstExprIr* expr = ir_expr_as_const(ir);
            intptr_t value = ir_const_expr_integer_value(expr);
            return ir_const_expr_cast(ir_new_integer_const_expr(value));
        }
        case ExprIrTag_Binop: {
            struct BinopExprIr* expr = ir_expr_as_binop(ir);
            enum BinopExprIrTag op = ir_binop_expr_op(expr);
            struct ExprIr* lhs = ir_expr_clone(ir_binop_expr_lhs(expr));
            struct ExprIr* rhs = ir_expr_clone(ir_binop_expr_rhs(expr));
            return ir_binop_expr_cast(ir_new_binop_expr(op, lhs, rhs));
        }
        case ExprIrTag_Var: {
            struct VarExprIr* expr = ir_expr_as_var(ir);
            strtable_id id = ir_var_expr_index(expr);
            return ir_var_expr_cast(ir_new_var_expr(id));
        }
        case ExprIrTag_Member: {
            struct MemberExprIr* expr = ir_expr_as_member(ir);
            struct ExprIr* base = ir_expr_clone(ir_member_expr_base(expr));
            strtable_id name_index = ir_member_expr_name_index(expr);
            return ir_member_expr_cast(ir_new_member_expr(base, name_index));
        }
        case ExprIrTag_Deref: {
            struct DerefExprIr* expr = ir_expr_as_deref(ir);
            struct ExprIr* operand = ir_expr_clone(ir_deref_expr_operand(expr));
            return ir_deref_expr_cast(ir_new_deref_expr(operand));
        }
        case ExprIrTag_Addrof: {
            struct AddrofExprIr* expr = ir_expr_as_addrof(ir);
            struct ExprIr* operand =
                ir_expr_clone(ir_addrof_expr_operand(expr));
            return ir_addrof_expr_cast(ir_new_addrof_expr(operand));
        }
        default:
            assert(false);
            return NULL;
    }
}

struct CondExprIr {
    struct ExprIr as_expr;
    struct ExprIr* cond;
    struct ExprIr* true_expr;
    struct ExprIr* false_expr;
};

struct CondExprIr* ir_new_cond_expr(struct ExprIr* cond,
                                    struct ExprIr* true_expr,
                                    struct ExprIr* false_expr) {
    struct CondExprIr* ir = malloc(sizeof(struct CondExprIr));
    initialize_expr(ir_cond_expr_cast(ir), ExprIrTag_Cond);
    ir->cond = cond;
    ir->true_expr = true_expr;
    ir->false_expr = false_expr;
    return ir;
}

struct ExprIr* ir_cond_expr_cast(struct CondExprIr* ir) {
    return &ir->as_expr;
}

struct ExprIr* ir_cond_expr_cond(struct CondExprIr* ir) {
    return ir->cond;
}

struct ExprIr* ir_cond_expr_true_expr(struct CondExprIr* ir) {
    return ir->true_expr;
}

struct ExprIr* ir_cond_expr_false_expr(struct CondExprIr* ir) {
    return ir->false_expr;
}

void ir_cond_expr_set_cond(struct CondExprIr* ir, struct ExprIr* cond) {
    ir->cond = cond;
}

void ir_cond_expr_set_true_expr(struct CondExprIr* ir,
                                struct ExprIr* true_expr) {
    ir->true_expr = true_expr;
}

void ir_cond_expr_set_false_expr(struct CondExprIr* ir,
                                 struct ExprIr* false_expr) {
    ir->false_expr = false_expr;
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

struct SwitchStmtIr* ir_stmt_as_switch(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Switch ? (struct SwitchStmtIr*)ir : NULL;
}

struct WhileStmtIr* ir_stmt_as_while(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_While ? (struct WhileStmtIr*)ir : NULL;
}

struct ReturnStmtIr* ir_stmt_as_return(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Return ? (struct ReturnStmtIr*)ir : NULL;
}

struct BreakStmtIr* ir_stmt_as_break(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Break ? (struct BreakStmtIr*)ir : NULL;
}

struct ContinueStmtIr* ir_stmt_as_continue(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Continue ? (struct ContinueStmtIr*)ir : NULL;
}

struct PushStmtIr* ir_stmt_as_push(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Push ? (struct PushStmtIr*)ir : NULL;
}

struct PopStmtIr* ir_stmt_as_pop(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Pop ? (struct PopStmtIr*)ir : NULL;
}

struct DeclStmtIr* ir_stmt_as_decl(struct StmtIr* ir) {
    return ir->tag == StmtIrTag_Decl ? (struct DeclStmtIr*)ir : NULL;
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

struct SwitchStmtBranch {
    intptr_t case_value;
    struct StmtIr* stmt;
};

struct SwitchStmtBranch* ir_new_switch_branch(intptr_t case_value,
                                              struct StmtIr* stmt) {
    struct SwitchStmtBranch* branch = malloc(sizeof(struct SwitchStmtBranch));
    branch->case_value = case_value;
    branch->stmt = stmt;
    return branch;
}

intptr_t ir_switch_branch_case_value(struct SwitchStmtBranch* branch) {
    return branch->case_value;
}

struct StmtIr* ir_switch_branch_stmt(struct SwitchStmtBranch* branch) {
    return branch->stmt;
}

void ir_switch_branch_set_stmt(struct SwitchStmtBranch* branch,
                               struct StmtIr* stmt) {
    branch->stmt = stmt;
}

struct SwitchStmtIr {
    struct StmtIr super;
    struct ExprIr* cond_expr;
    struct List* branches;  // ListItem list, item=SwitchStmtBranch
    struct StmtIr* default_stmt;
};

struct SwitchStmtIr* ir_new_switch_stmt(struct ExprIr* cond_expr,
                                        struct List* branches,
                                        struct StmtIr* default_stmt) {
    struct SwitchStmtIr* ir = malloc(sizeof(struct SwitchStmtIr));
    initialize_stmt(ir_switch_stmt_super(ir), StmtIrTag_Switch);
    ir->cond_expr = cond_expr;
    ir->branches = branches;
    ir->default_stmt = default_stmt;
    return ir;
}

struct StmtIr* ir_switch_stmt_super(struct SwitchStmtIr* ir) {
    return &ir->super;
}

struct ExprIr* ir_switch_stmt_cond_expr(struct SwitchStmtIr* ir) {
    return ir->cond_expr;
}

void ir_switch_stmt_set_cond_expr(struct SwitchStmtIr* ir,
                                  struct ExprIr* cond_expr) {
    ir->cond_expr = cond_expr;
}

struct List* ir_switch_stmt_branches(struct SwitchStmtIr* ir) {
    return ir->branches;
}

struct StmtIr* ir_switch_stmt_default_stmt(struct SwitchStmtIr* ir) {
    return ir->default_stmt;
}

void ir_switch_stmt_set_default_stmt(struct SwitchStmtIr* ir,
                                     struct StmtIr* default_stmt) {
    ir->default_stmt = default_stmt;
}

struct WhileStmtIr {
    struct StmtIr super;
    struct ExprIr* cond_expr;
    struct ExprIr* update_expr;
    struct StmtIr* body_stmt;
};

struct WhileStmtIr* ir_new_while_stmt(struct ExprIr* cond_expr,
                                      struct ExprIr* update_expr,
                                      struct StmtIr* body_stmt) {
    struct WhileStmtIr* ir = malloc(sizeof(struct WhileStmtIr));
    initialize_stmt(ir_while_stmt_super(ir), StmtIrTag_While);
    ir->cond_expr = cond_expr;
    ir->update_expr = update_expr;
    ir->body_stmt = body_stmt;
    return ir;
}

struct StmtIr* ir_while_stmt_super(struct WhileStmtIr* ir) {
    return &ir->super;
}

struct ExprIr* ir_while_stmt_cond_expr(struct WhileStmtIr* ir) {
    return ir->cond_expr;
}

void ir_while_stmt_set_cond_expr(struct WhileStmtIr* ir,
                                 struct ExprIr* cond_expr) {
    ir->cond_expr = cond_expr;
}

struct ExprIr* ir_while_stmt_update_expr(struct WhileStmtIr* ir) {
    return ir->update_expr;
}

void ir_while_stmt_set_update_expr(struct WhileStmtIr* ir,
                                   struct ExprIr* update_expr) {
    ir->update_expr = update_expr;
}

struct StmtIr* ir_while_stmt_body_stmt(struct WhileStmtIr* ir) {
    return ir->body_stmt;
}

void ir_while_stmt_set_body_stmt(struct WhileStmtIr* ir,
                                 struct StmtIr* body_stmt) {
    ir->body_stmt = body_stmt;
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

struct BreakStmtIr {
    struct StmtIr super;
};

struct BreakStmtIr* ir_new_break_stmt(void) {
    struct BreakStmtIr* ir = malloc(sizeof(struct BreakStmtIr));
    initialize_stmt(ir_break_stmt_super(ir), StmtIrTag_Break);
    return ir;
}

struct StmtIr* ir_break_stmt_super(struct BreakStmtIr* ir) {
    return &ir->super;
}

struct ContinueStmtIr {
    struct StmtIr super;
};

struct ContinueStmtIr* ir_new_continue_stmt(void) {
    struct ContinueStmtIr* ir = malloc(sizeof(struct ContinueStmtIr));
    initialize_stmt(ir_continue_stmt_super(ir), StmtIrTag_Continue);
    return ir;
}

struct StmtIr* ir_continue_stmt_super(struct ContinueStmtIr* ir) {
    return &ir->super;
}

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

struct DeclStmtIr {
    struct StmtIr super;
    strtable_id var_id;
    struct TypeIr* type;
    struct BlockStmtIr* block;
    struct VarExprIr* var;
};

struct DeclStmtIr* ir_new_decl_stmt(strtable_id var_id, struct TypeIr* type,
                                    struct BlockStmtIr* block) {
    struct DeclStmtIr* ir = malloc(sizeof(struct DeclStmtIr));
    initialize_stmt(ir_decl_stmt_super(ir), StmtIrTag_Decl);
    ir->var_id = var_id;
    ir->type = type;
    ir->block = block;
    ir->var = NULL;
    return ir;
}

struct StmtIr* ir_decl_stmt_super(struct DeclStmtIr* ir) {
    return &ir->super;
}

strtable_id ir_decl_stmt_var_id(struct DeclStmtIr* ir) { return ir->var_id; }

struct TypeIr* ir_decl_stmt_type(struct DeclStmtIr* ir) {
    return ir->type;
}

struct VarExprIr* ir_decl_stmt_var(struct DeclStmtIr* ir) {
    assert(ir->var);
    return ir->var;
}

struct VarExprIr* ir_decl_stmt_instantiate(struct DeclStmtIr* ir) {
    struct Location* loc =
        ir_new_location(ir->block->region, ir->var_id, ir->type);
    struct VarExprIr* var = ir_new_var_expr_from_location(ir->var_id, loc);
    ir->var = var;
    return var;
}
