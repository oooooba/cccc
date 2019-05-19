#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct DumpVisitor {
    struct Visitor as_visitor;
    struct Context* context;
    FILE* stream;
};

static struct Visitor* as_visitor(struct DumpVisitor* visitor) {
    return &visitor->as_visitor;
}

static struct ExprIr* visit_const_expr2(struct DumpVisitor* visitor,
                                        struct ConstExprIr* ir) {
    fprintf(visitor->stream, "v%p = %ld\n", ir,
            ir_const_expr_integer_value(ir));
    return NULL;
}

static struct ExprIr* visit_binop_expr2(struct DumpVisitor* visitor,
                                        struct BinopExprIr* ir) {
    struct ExprIr* lhs = ir_binop_expr_lhs(ir);
    struct ExprIr* rhs = ir_binop_expr_rhs(ir);
    visitor_visit_expr(as_visitor(visitor), lhs);
    visitor_visit_expr(as_visitor(visitor), rhs);
    const char* op;
    switch (ir_binop_expr_op(ir)) {
        case BinopExprIrTag_Add:
            op = "add";
            break;
        case BinopExprIrTag_Sub:
            op = "sub";
            break;
        case BinopExprIrTag_Mul:
            op = "mul";
            break;
        default:
            assert(false);
    }
    fprintf(visitor->stream, "v%p = %s v%p, v%p\n", ir, op, lhs, rhs);
    return NULL;
}

static struct ExprIr* visit_call_expr2(struct DumpVisitor* visitor,
                                       struct CallExprIr* ir) {
    struct VarExprIr* func_name = ir_expr_as_var(ir_call_expr_function(ir));
    assert(func_name);

    for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                           *eit = list_end(ir_call_expr_args(ir));
         it != eit; it = list_next(it)) {
        struct ExprIr* arg = ((struct ListItem*)it)->item;
        visitor_visit_expr(as_visitor(visitor), arg);
    }

    strtable_id name_id = ir_var_expr_index(func_name);
    const char* name = strtable_at(&visitor->context->strtable, name_id);
    fprintf(visitor->stream, "v%p = call %s (", ir, name);
    bool first = true;
    for (struct ListHeader *it = list_begin(ir_call_expr_args(ir)),
                           *eit = list_end(ir_call_expr_args(ir));
         it != eit; it = list_next(it)) {
        struct ExprIr* arg = ((struct ListItem*)it)->item;
        if (first)
            first = false;
        else
            fprintf(visitor->stream, ", ");
        fprintf(visitor->stream, "v%p", arg);
    }
    fprintf(visitor->stream, ")\n");
    return NULL;
}

static struct ExprIr* visit_var_expr2(struct DumpVisitor* visitor,
                                      struct VarExprIr* ir) {
    strtable_id index = ir_var_expr_index(ir);
    const char* name = strtable_at(&visitor->context->strtable, index);
    fprintf(visitor->stream, "v%p = %s\n", ir, name);
    return NULL;
}

static struct ExprIr* visit_unop_expr2(struct DumpVisitor* visitor,
                                       struct UnopExprIr* ir) {
    struct ExprIr* operand = ir_unop_expr_operand(ir);
    enum UnopExprIrTag op = ir_unop_expr_op(ir);

    if (op == UnopExprIrTag_Addrof) {
        struct VarExprIr* var = ir_expr_as_var(operand);
        if (var) {
            strtable_id index = ir_var_expr_index(var);
            const char* name = strtable_at(&visitor->context->strtable, index);
            fprintf(visitor->stream, "v%p = addrof %s\n", ir, name);
            return NULL;
        }
    }

    visitor_visit_expr(as_visitor(visitor), operand);

    const char* ope;
    switch (op) {
        case UnopExprIrTag_Deref:
            ope = "deref";
            break;
        case UnopExprIrTag_Addrof:
            ope = "addrof";
            break;
        default:
            assert(false);
    }
    fprintf(visitor->stream, "v%p = %s v%p\n", ir, ope, operand);

    return NULL;
}

static struct ExprIr* visit_subst_expr2(struct DumpVisitor* visitor,
                                        struct SubstExprIr* ir) {
    struct ExprIr* value = ir_subst_expr_value(ir);
    visitor_visit_expr(as_visitor(visitor), value);
    struct ExprIr* addr = ir_subst_expr_addr(ir);
    visitor_visit_expr(as_visitor(visitor), addr);
    fprintf(visitor->stream, "v%p = subst v%p, v%p\n", ir, addr, value);
    return NULL;
}

static struct BlockIr* visit_block2(struct DumpVisitor* visitor,
                                    struct BlockIr* ir) {
    fprintf(visitor->stream, "[@%p]{\n", ir);
    struct BlockIterator* it = ir_block_new_iterator(ir);
    for (;;) {
        struct Ir* stmt = ir_block_iterator_next(it);
        if (!stmt) break;

        visitor_visit_ir(as_visitor(visitor), stmt);
    }
    fprintf(visitor->stream, "}\n");
    return NULL;
}

static struct FunctionIr* visit_function2(struct DumpVisitor* visitor,
                                          struct FunctionIr* ir) {
    const char* name =
        strtable_at(&visitor->context->strtable, ir_function_name_index(ir));
    struct BlockIr* body = ir_function_body(ir);
    fprintf(visitor->stream, "function %s (", name);
    bool first = true;
    for (struct ListHeader *it = list_begin(ir_function_params(ir)),
                           *eit = list_end(ir_function_params(ir));
         it != eit; it = list_next(it)) {
        struct VarExprIr* var = ((struct ListItem*)it)->item;
        const char* var_name =
            strtable_at(&visitor->context->strtable, ir_var_expr_index(var));
        if (first)
            first = false;
        else
            fprintf(visitor->stream, ", ");
        fprintf(visitor->stream, "%s", var_name);
    }
    fprintf(visitor->stream, ") ");
    visitor_visit_block(as_visitor(visitor), body);
    return NULL;
}

static struct CfIr* visit_branch_cf2(struct DumpVisitor* visitor,
                                     struct BranchCfIr* ir) {
    struct ExprIr* cond_expr = ir_branch_cf_cond_expr(ir);
    visitor_visit_expr(as_visitor(visitor), cond_expr);
    fprintf(visitor->stream, "if (v%p) ", cond_expr);

    struct BlockIr* true_block = ir_branch_cf_true_block(ir);
    visitor_visit_block(as_visitor(visitor), true_block);

    struct BlockIr* false_block = ir_branch_cf_false_block(ir);
    if (false_block) visitor_visit_block(as_visitor(visitor), false_block);

    return NULL;
}

static struct CfIr* visit_return_cf2(struct DumpVisitor* visitor,
                                     struct ReturnCfIr* ir) {
    struct ExprIr* expr = ir_return_cf_expr(ir);
    if (expr) {
        visitor_visit_expr(as_visitor(visitor), expr);
        fprintf(visitor->stream, "return v%p", expr);
    } else
        fprintf(visitor->stream, "return");
    fprintf(visitor->stream, "\n");
    return NULL;
}

struct DumpVisitor* new_dump_visitor(struct Context* context, FILE* stream) {
    struct DumpVisitor* visitor = malloc(sizeof(struct DumpVisitor));
    visitor_initialize(as_visitor(visitor));

    register_visitor(visitor->as_visitor, visit_const_expr, visit_const_expr2);
    register_visitor(visitor->as_visitor, visit_binop_expr, visit_binop_expr2);
    register_visitor(visitor->as_visitor, visit_call_expr, visit_call_expr2);
    register_visitor(visitor->as_visitor, visit_var_expr, visit_var_expr2);
    register_visitor(visitor->as_visitor, visit_unop_expr, visit_unop_expr2);
    register_visitor(visitor->as_visitor, visit_subst_expr, visit_subst_expr2);
    register_visitor(visitor->as_visitor, visit_block, visit_block2);
    register_visitor(visitor->as_visitor, visit_function, visit_function2);
    register_visitor(visitor->as_visitor, visit_branch_cf, visit_branch_cf2);
    register_visitor(visitor->as_visitor, visit_return_cf, visit_return_cf2);

    visitor->context = context;
    visitor->stream = stream;
    return visitor;
}

static void dump_type(struct DumpVisitor* visitor, struct TypeIr* type) {
    switch (type_tag2(type)) {
        case Type_Int:
            fprintf(visitor->stream, "int");
            break;
        case Type_Pointer: {
            struct PointerTypeIr* p = type_as_pointer(type);
            dump_type(visitor, type_pointer_elem_type(p));
            fprintf(visitor->stream, "*");
        } break;
        case Type_Struct: {
            struct StructTypeIr* s = type_as_struct(type);
            fprintf(visitor->stream, "struct");
            strtable_id name_index = type_struct_name_index(s);
            if (name_index != STRTABLE_INVALID_ID)
                fprintf(visitor->stream, " %s",
                        strtable_at(&visitor->context->strtable, name_index));
            struct List* elem_types = type_struct_elem_types(s);
            if (!elem_types) break;
            fprintf(visitor->stream, " { ");

            for (struct ListHeader *it = list_begin(type_struct_elem_types(s)),
                                   *eit = list_end(type_struct_elem_types(s));
                 it != eit; it = list_next(it)) {
                struct MemberEntry* entry = (struct MemberEntry*)it;
                dump_type(visitor, type_member_entry_type(entry));
                const char* name =
                    strtable_at(&visitor->context->strtable,
                                type_member_entry_name_index(entry));
                fprintf(visitor->stream, " %s; ", name);
            }

            fprintf(visitor->stream, "}");
        } break;
        default:
            assert(false);
    }
}

static void dump_user_defined_types(struct DumpVisitor* visitor) {
    for (struct ListHeader *
             it = context_user_defined_type_begin(visitor->context),
            *eit = context_user_defined_type_end(visitor->context);
         it != eit; it = list_next(it)) {
        struct MapEntry* map_entry = (struct MapEntry*)it;
        struct TypeIr* type = map_entry_value(map_entry);
        dump_type(visitor, type);
        fprintf(visitor->stream, ";\n");
    }
}

void dump_apply(struct DumpVisitor* visitor, struct BlockIr* ir) {
    dump_user_defined_types(visitor);
    visitor_visit_block(as_visitor(visitor), ir);
}
