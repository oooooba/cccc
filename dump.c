#include "context.h"
#include "ir.h"
#include "visitor.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct DumpVisitor {
    struct Visitor as_visitor;
    FILE* stream;
};

static struct Visitor* as_visitor(struct DumpVisitor* visitor) {
    return &visitor->as_visitor;
}

static struct Context* ctx(struct DumpVisitor* visitor) {
    return visitor_context(as_visitor(visitor));
}

static struct ExprIr* visit_const_expr(struct DumpVisitor* visitor,
                                       struct ConstExprIr* ir) {
    fprintf(visitor->stream, "v%p = %ld :: ", ir,
            ir_const_expr_integer_value(ir));
    context_dump_type(ctx(visitor), visitor->stream,
                      ir_expr_type(ir_const_expr_cast(ir)));
    fprintf(visitor->stream, "\n");
    return ir_const_expr_cast(ir);
}

static struct ExprIr* visit_binop_expr(struct DumpVisitor* visitor,
                                       struct BinopExprIr* ir) {
    visitor_visit_binop_expr(as_visitor(visitor), ir);
    struct ExprIr* lhs = ir_binop_expr_lhs(ir);
    struct ExprIr* rhs = ir_binop_expr_rhs(ir);
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
        case BinopExprIrTag_Equal:
            op = "equal";
            break;
        case BinopExprIrTag_Lt:
            op = "lt";
            break;
        case BinopExprIrTag_Le:
            op = "le";
            break;
        case BinopExprIrTag_Gt:
            op = "gt";
            break;
        case BinopExprIrTag_Ge:
            op = "ge";
            break;
        default:
            assert(false);
    }
    fprintf(visitor->stream, "v%p = %s v%p, v%p :: ", ir, op, lhs, rhs);
    context_dump_type(ctx(visitor), visitor->stream,
                      ir_expr_type(ir_binop_expr_cast(ir)));
    fprintf(visitor->stream, "\n");
    return ir_binop_expr_cast(ir);
}

static struct ExprIr* visit_unop_expr(struct DumpVisitor* visitor,
                                      struct UnopExprIr* ir) {
    visitor_visit_unop_expr(as_visitor(visitor), ir);
    struct ExprIr* operand = ir_unop_expr_operand(ir);
    const char* op;
    switch (ir_unop_expr_op(ir)) {
        case UnopExprIrTag_Not:
            op = "not";
            break;
        default:
            assert(false);
    }
    fprintf(visitor->stream, "v%p = %s v%p :: ", ir, op, operand);
    context_dump_type(ctx(visitor), visitor->stream,
                      ir_expr_type(ir_unop_expr_cast(ir)));
    fprintf(visitor->stream, "\n");
    return ir_unop_expr_cast(ir);
}

static struct ExprIr* visit_call_expr(struct DumpVisitor* visitor,
                                      struct CallExprIr* ir) {
    visitor_visit_call_expr(as_visitor(visitor), ir);
    fprintf(visitor->stream, "v%p = call %p (", ir, ir_call_expr_function(ir));
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
    fprintf(visitor->stream, ") :: ");
    context_dump_type(ctx(visitor), visitor->stream,
                      ir_expr_type(ir_call_expr_cast(ir)));
    fprintf(visitor->stream, "\n");
    return ir_call_expr_cast(ir);
}

static struct ExprIr* visit_var_expr(struct DumpVisitor* visitor,
                                     struct VarExprIr* ir) {
    strtable_id index = ir_var_expr_index(ir);
    const char* name = strtable_at(&ctx(visitor)->strtable, index);
    fprintf(visitor->stream, "v%p = address of %s :: ", ir, name);
    context_dump_type(ctx(visitor), visitor->stream,
                      ir_expr_type(ir_var_expr_cast(ir)));
    fprintf(visitor->stream, "\n");
    return ir_var_expr_cast(ir);
}

static struct ExprIr* visit_subst_expr(struct DumpVisitor* visitor,
                                       struct SubstExprIr* ir) {
    visitor_visit_subst_expr(as_visitor(visitor), ir);
    struct ExprIr* value = ir_subst_expr_value(ir);
    struct ExprIr* addr = ir_subst_expr_addr(ir);
    fprintf(visitor->stream, "v%p = subst v%p, v%p :: ", ir, addr, value);
    context_dump_type(ctx(visitor), visitor->stream,
                      ir_expr_type(ir_subst_expr_cast(ir)));
    fprintf(visitor->stream, "\n");
    return ir_subst_expr_cast(ir);
}

static struct ExprIr* visit_member_expr(struct DumpVisitor* visitor,
                                        struct MemberExprIr* ir) {
    visitor_visit_member_expr(as_visitor(visitor), ir);
    struct ExprIr* base = ir_member_expr_base(ir);
    strtable_id member_index = ir_member_expr_name_index(ir);
    const char* member_name =
        strtable_at(&ctx(visitor)->strtable, member_index);
    fprintf(visitor->stream, "v%p = member <v%p + offset(%s)> :: ", ir, base,
            member_name);
    context_dump_type(ctx(visitor), visitor->stream,
                      ir_expr_type(ir_member_expr_cast(ir)));
    fprintf(visitor->stream, "\n");
    return ir_member_expr_cast(ir);
}

static struct ExprIr* visit_deref_expr(struct DumpVisitor* visitor,
                                       struct DerefExprIr* ir) {
    visitor_visit_deref_expr(as_visitor(visitor), ir);
    struct ExprIr* operand = ir_deref_expr_operand(ir);
    fprintf(visitor->stream, "v%p = deref v%p :: ", ir, operand);
    context_dump_type(ctx(visitor), visitor->stream,
                      ir_expr_type(ir_deref_expr_cast(ir)));
    fprintf(visitor->stream, "\n");
    return ir_deref_expr_cast(ir);
}

static struct ExprIr* visit_addrof_expr(struct DumpVisitor* visitor,
                                        struct AddrofExprIr* ir) {
    visitor_visit_addrof_expr(as_visitor(visitor), ir);
    struct ExprIr* operand = ir_addrof_expr_operand(ir);
    fprintf(visitor->stream, "v%p = addrof <v%p> :: ", ir, operand);
    context_dump_type(ctx(visitor), visitor->stream,
                      ir_expr_type(ir_addrof_expr_cast(ir)));
    fprintf(visitor->stream, "\n");
    return ir_addrof_expr_cast(ir);
}

static struct ExprIr* visit_cast_expr(struct DumpVisitor* visitor,
                                      struct CastExprIr* ir) {
    visitor_visit_cast_expr(as_visitor(visitor), ir);
    struct ExprIr* operand = ir_cast_expr_operand(ir);
    fprintf(visitor->stream, "v%p = cast v%p :: ", ir, operand);
    context_dump_type(ctx(visitor), visitor->stream,
                      ir_expr_type(ir_cast_expr_cast(ir)));
    fprintf(visitor->stream, "\n");
    return ir_cast_expr_cast(ir);
}

static struct StmtIr* visit_block_stmt(struct DumpVisitor* visitor,
                                       struct BlockStmtIr* ir) {
    fprintf(visitor->stream, "[@%p]{\n", ir);
    visitor_visit_block_stmt(as_visitor(visitor), ir);
    fprintf(visitor->stream, "}\n");
    return ir_block_stmt_super(ir);
}

static struct StmtIr* visit_if_stmt(struct DumpVisitor* visitor,
                                    struct IfStmtIr* ir) {
    struct ExprIr* cond_expr = ir_if_stmt_cond_expr(ir);
    visitor_visit_expr(as_visitor(visitor), cond_expr);
    fprintf(visitor->stream, "if (v%p) ", cond_expr);

    struct StmtIr* true_stmt = ir_if_stmt_true_stmt(ir);
    visitor_visit_stmt(as_visitor(visitor), true_stmt);

    fprintf(visitor->stream, "else ");
    struct StmtIr* false_stmt = ir_if_stmt_false_stmt(ir);
    visitor_visit_stmt(as_visitor(visitor), false_stmt);

    return ir_if_stmt_super(ir);
}

static struct StmtIr* visit_switch_stmt(struct DumpVisitor* visitor,
                                        struct SwitchStmtIr* ir) {
    struct ExprIr* cond_expr = ir_switch_stmt_cond_expr(ir);
    visitor_visit_expr(as_visitor(visitor), cond_expr);
    fprintf(visitor->stream, "switch (v%p) {\n", cond_expr);

    struct List* branches = ir_switch_stmt_branches(ir);
    for (struct ListHeader *it = list_begin(branches),
                           *eit = list_end(branches);
         it != eit; it = list_next(it)) {
        struct SwitchStmtBranch* branch = ((struct ListItem*)it)->item;
        fprintf(visitor->stream, "case %ld:\n",
                ir_switch_branch_case_value(branch));
        visitor_visit_stmt(as_visitor(visitor), ir_switch_branch_stmt(branch));
    }

    fprintf(visitor->stream, "default:\n");
    visitor_visit_stmt(as_visitor(visitor), ir_switch_stmt_default_stmt(ir));

    fprintf(visitor->stream, "}\n");

    return ir_switch_stmt_super(ir);
}

static struct StmtIr* visit_while_stmt(struct DumpVisitor* visitor,
                                       struct WhileStmtIr* ir) {
    struct ExprIr* cond_expr = ir_while_stmt_cond_expr(ir);
    visitor_visit_expr(as_visitor(visitor), cond_expr);
    fprintf(visitor->stream, "while (v%p) ", cond_expr);

    struct StmtIr* body_stmt = ir_while_stmt_body_stmt(ir);
    visitor_visit_stmt(as_visitor(visitor), body_stmt);

    return ir_while_stmt_super(ir);
}

static struct StmtIr* visit_return_stmt(struct DumpVisitor* visitor,
                                        struct ReturnStmtIr* ir) {
    struct ExprIr* expr = ir_return_stmt_expr(ir);
    if (expr) {
        visitor_visit_expr(as_visitor(visitor), expr);
        fprintf(visitor->stream, "return v%p", expr);
    } else
        fprintf(visitor->stream, "return");
    fprintf(visitor->stream, "\n");
    return ir_return_stmt_super(ir);
}

static struct StmtIr* visit_break_stmt(struct DumpVisitor* visitor,
                                       struct BreakStmtIr* ir) {
    (void)visitor;
    fprintf(visitor->stream, "break\n");
    return ir_break_stmt_super(ir);
}

static struct StmtIr* visit_decl_stmt(struct DumpVisitor* visitor,
                                      struct DeclStmtIr* ir) {
    const char* name =
        strtable_at(&ctx(visitor)->strtable, ir_decl_stmt_var_id(ir));
    fprintf(visitor->stream, "decl %s :: ", name);
    context_dump_type(ctx(visitor), visitor->stream, ir_decl_stmt_type(ir));
    fprintf(visitor->stream, "\n");
    return ir_decl_stmt_super(ir);
}

static struct FunctionIr* visit_function2(struct DumpVisitor* visitor,
                                          struct FunctionIr* ir) {
    const char* name =
        strtable_at(&ctx(visitor)->strtable, ir_function_name_index(ir));
    fprintf(visitor->stream, "function %s (", name);
    bool first = true;
    for (struct ListHeader *it = list_begin(ir_function_param_decl_list(ir)),
                           *eit = list_end(ir_function_param_decl_list(ir));
         it != eit; it = list_next(it)) {
        struct DeclStmtIr* decl = ((struct ListItem*)it)->item;
        const char* var_name =
            strtable_at(&ctx(visitor)->strtable, ir_decl_stmt_var_id(decl));
        if (first)
            first = false;
        else
            fprintf(visitor->stream, ", ");
        fprintf(visitor->stream, "%s :: ", var_name);
        context_dump_type(ctx(visitor), visitor->stream,
                          ir_decl_stmt_type(decl));
    }
    fprintf(visitor->stream, ") :: ");
    context_dump_type(ctx(visitor), visitor->stream,
                      ir_function_result_type(ir));
    fprintf(visitor->stream, " ");
    visit_block_stmt(visitor, ir_function_body2(ir));
    return ir;
}

struct DumpVisitor* new_dump_visitor(struct Context* context, FILE* stream) {
    struct DumpVisitor* visitor = malloc(sizeof(struct DumpVisitor));
    visitor_initialize(as_visitor(visitor), context);

    register_visitor(visitor->as_visitor, visit_const_expr, visit_const_expr);
    register_visitor(visitor->as_visitor, visit_binop_expr, visit_binop_expr);
    register_visitor(visitor->as_visitor, visit_unop_expr, visit_unop_expr);
    register_visitor(visitor->as_visitor, visit_call_expr, visit_call_expr);
    register_visitor(visitor->as_visitor, visit_var_expr, visit_var_expr);
    register_visitor(visitor->as_visitor, visit_subst_expr, visit_subst_expr);
    register_visitor(visitor->as_visitor, visit_member_expr, visit_member_expr);
    register_visitor(visitor->as_visitor, visit_deref_expr, visit_deref_expr);
    register_visitor(visitor->as_visitor, visit_addrof_expr, visit_addrof_expr);
    register_visitor(visitor->as_visitor, visit_cast_expr, visit_cast_expr);

    register_visitor(visitor->as_visitor, visit_block_stmt, visit_block_stmt);
    register_visitor(visitor->as_visitor, visit_if_stmt, visit_if_stmt);
    register_visitor(visitor->as_visitor, visit_switch_stmt, visit_switch_stmt);
    register_visitor(visitor->as_visitor, visit_while_stmt, visit_while_stmt);
    register_visitor(visitor->as_visitor, visit_return_stmt, visit_return_stmt);
    register_visitor(visitor->as_visitor, visit_break_stmt, visit_break_stmt);
    register_visitor(visitor->as_visitor, visit_decl_stmt, visit_decl_stmt);

    register_visitor(visitor->as_visitor, visit_function, visit_function2);

    visitor->stream = stream;
    return visitor;
}
