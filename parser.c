#include "parser.h"
#include "context.h"
#include "ir.h"
#include "list.h"
#include "map.h"
#include "token.h"
#include "type.h"
#include "vector.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct Env;

struct Parser {
    struct Context* context;
    struct List* tokens;
    struct ListHeader* current_token;
    struct Env* current_env;
};

/***** helper functions *****/

struct Env {
    struct Map var_map;  // key: strtable_id, value: VarIr*
    struct Env* outer_env;
};

static struct Env* env_new(struct Env* outer_env) {
    struct Env* env = malloc(sizeof(struct Env));
    map_initialize(&env->var_map);
    env->outer_env = outer_env;
    return env;
}

static struct VarIr* env_find(struct Env* env, strtable_id index) {
    if (!env) return NULL;
    struct VarIr* var = (struct VarIr*)map_find(&env->var_map, (void*)index);
    return var ? var : env_find(env->outer_env, index);
}

static void env_insert(struct Env* env, strtable_id index, struct VarIr* var) {
    map_insert(&env->var_map, (void*)index, var);
}

static struct Token* peek_k(struct Parser* parser, size_t k) {
    struct ListHeader* it = parser->current_token;
    for (; k; --k) {
        it = list_next(it);
    }
    return (struct Token*)(it);
}

static struct Token* peek(struct Parser* parser) { return peek_k(parser, 0); }

static void advance(struct Parser* parser) {
    parser->current_token = list_next(parser->current_token);
}

static bool acceptable(struct Parser* parser, enum TokenTag expected) {
    if (peek(parser)->tag == expected)
        return true;
    else
        return false;
}

static bool acceptable_type(struct Parser* parser) {
    return acceptable(parser, Token_KeywordInt);
}

static void expect(struct Parser* parser, enum TokenTag expected) {
    if (!acceptable(parser, expected)) {
        fprintf(stderr, "expected %d, actual %d\n", expected,
                peek(parser)->tag);
        assert(false);
    }
    advance(parser);
}

static struct BlockIr* to_block(struct Ir* ir) {
    struct BlockIr* block = ir_as_block(ir);
    if (!block) {
        block = ir_new_block();
        ir_block_insert_at_end(block, ir);
    }
    return block;
}

/***** lexical elements *****/

static struct ExprIr* parse_integer_constant(struct Parser* parser) {
    assert(acceptable(parser, Token_Integer));
    struct Token* token = peek(parser);
    struct ConstExprIr* ir = ir_new_integer_const_expr(token->integer);
    advance(parser);
    return ir_const_expr_cast(ir);
}

static strtable_id parse_identifier(struct Parser* parser) {
    assert(acceptable(parser, Token_Id));
    strtable_id index = peek(parser)->strtable_index;
    advance(parser);
    return index;
}

static struct ExprIr* parse_constant(struct Parser* parser) {
    assert(acceptable(parser, Token_Integer));
    return parse_integer_constant(parser);
}

/***** expressions *****/

static struct ExprIr* parse_primary_expression(struct Parser* parser) {
    switch (peek(parser)->tag) {
        case Token_Integer:
            return parse_constant(parser);
        case Token_Id: {
            strtable_id index = parse_identifier(parser);
            struct VarIr* var = env_find(parser->current_env, index);
            if (!var) var = context_find_var_for_func(parser->context, index);
            assert(var);
            struct AddrofExprIr* addrof_var = ir_new_addrof_expr_with_var(var);
            struct LoadExprIr* load_addrof_var =
                ir_new_load_expr(ir_addrof_expr_cast(addrof_var));
            return ir_load_expr_cast(load_addrof_var);
        }
        default:
            assert(false);
            return NULL;
    }
}

static struct ExprIr* parse_assignment_expression(struct Parser* parser);

static struct ExprIr* parse_postfix_expression(struct Parser* parser) {
    struct ExprIr* expr = parse_primary_expression(parser);
    if (acceptable(parser, Token_LeftParen)) {
        advance(parser);
        struct VarIr* var;
        {
            // ToDo: this is ugly, fix
            struct LoadExprIr* le = ir_expr_as_load(expr);
            struct AddrofExprIr* ae = ir_expr_as_addrof(ir_load_expr_addr(le));
            var = ir_addrof_expr_operand_as_var(ae);
        }
        struct List* args = malloc(sizeof(struct List));
        list_initialize(args);
        while (!acceptable(parser, Token_RightParen)) {
            struct ExprIr* arg = parse_assignment_expression(parser);
            struct ListItem* item = malloc(sizeof(struct ListItem));
            item->item = arg;
            list_insert_at_end(args, list_from(item));
            if (acceptable(parser, Token_Comma)) {
                assert(peek_k(parser, 1)->tag != Token_RightParen);
                advance(parser);
            }
        }
        advance(parser);
        expr = ir_call_expr_cast(ir_new_call_expr_with_var(var, args));
    }
    return expr;
}

static struct ExprIr* parse_cast_expression(struct Parser* parser);

static struct ExprIr* parse_unary_expression(struct Parser* parser) {
    if (acceptable(parser, Token_Ampersand)) {
        advance(parser);
        struct ExprIr* expr = parse_cast_expression(parser);
        return ir_addrof_expr_cast(ir_new_addrof_expr_with_expr(expr));
    } else if (acceptable(parser, Token_Asterisk)) {
        advance(parser);
        struct ExprIr* expr = parse_cast_expression(parser);
        return ir_load_expr_cast(ir_new_load_expr(expr));
    } else
        return parse_postfix_expression(parser);
}

static struct ExprIr* parse_cast_expression(struct Parser* parser) {
    return parse_unary_expression(parser);
}

static struct ExprIr* parse_multiplicative_expression(struct Parser* parser) {
    struct ExprIr* lhs = parse_cast_expression(parser);
    for (;;) {
        enum BinopExprIrTag op;
        if (acceptable(parser, Token_Asterisk))
            op = BinopExprIrTag_Mul;
        else
            break;
        advance(parser);
        struct ExprIr* rhs = parse_cast_expression(parser);
        lhs = ir_binop_expr_cast(ir_new_binop_expr(op, lhs, rhs));
    }
    return lhs;
}

static struct ExprIr* parse_additive_expression(struct Parser* parser) {
    struct ExprIr* lhs = parse_multiplicative_expression(parser);
    for (;;) {
        enum BinopExprIrTag op;
        if (acceptable(parser, Token_Plus))
            op = BinopExprIrTag_Add;
        else if (acceptable(parser, Token_Minus))
            op = BinopExprIrTag_Sub;
        else
            break;
        advance(parser);
        struct ExprIr* rhs = parse_multiplicative_expression(parser);
        lhs = ir_binop_expr_cast(ir_new_binop_expr(op, lhs, rhs));
    }
    return lhs;
}

static struct ExprIr* parse_assignment_expression(struct Parser* parser) {
    struct ExprIr* lhs = parse_additive_expression(parser);
    if (acceptable(parser, Token_Equal)) {
        advance(parser);

        struct LoadExprIr* deref_expr = ir_expr_as_load(lhs);
        assert(deref_expr);
        struct ExprIr* addr_expr = ir_load_expr_addr(deref_expr);

        struct ExprIr* rhs = parse_additive_expression(parser);
        struct StoreExprIr* subst_expr = ir_new_store_expr(addr_expr, rhs);

        lhs = ir_store_expr_cast(subst_expr);
    }
    return lhs;
}

static struct ExprIr* parse_expression(struct Parser* parser) {
    return parse_assignment_expression(parser);
}

/***** declarations *****/

struct Declaration {
    struct ListHeader as_list;
    strtable_id name_index;
    struct TypeIr* type;
    struct ExprIr* initializer;
};

static struct ExprIr* parse_initializer(struct Parser* parser) {
    return parse_expression(parser);
}

static struct TypeIr* parse_type_specifier(struct Parser* parser) {
    assert(acceptable(parser, Token_KeywordInt));
    advance(parser);
    return type_new_int2();
}

static strtable_id parse_declarator(struct Parser* parser,
                                    struct TypeIr* base_type,
                                    struct TypeIr** result_type) {
    if (acceptable(parser, Token_Asterisk)) {
        advance(parser);
        *result_type = type_new_pointer2(base_type);
    } else
        *result_type = base_type;

    assert(acceptable(parser, Token_Id));
    struct Token* token = peek(parser);
    strtable_id name_index = token->strtable_index;
    advance(parser);

    return name_index;
}

static void parse_declaration(struct Parser* parser, struct List* result) {
    struct TypeIr* base_type = parse_type_specifier(parser);

    struct TypeIr* type;
    strtable_id var_index = parse_declarator(parser, base_type, &type);

    struct ExprIr* initializer = NULL;
    if (acceptable(parser, Token_Equal)) {
        advance(parser);
        initializer = parse_initializer(parser);
    }

    struct Declaration* decl = malloc(sizeof(struct Declaration));
    decl->name_index = var_index;
    decl->type = type;
    decl->initializer = initializer;
    list_insert_at_end(result, list_from(decl));
}

/***** statements and blocks *****/

static struct Ir* parse_statement(struct Parser* parser);

static struct BlockIr* parse_compound_statement(struct Parser* parser,
                                                struct BlockIr* block) {
    expect(parser, Token_LeftCurry);

    if (!block) block = ir_new_block();

    struct Env* env = env_new(parser->current_env);
    parser->current_env = env;

    while (!acceptable(parser, Token_RightCurry)) {
        if (acceptable_type(parser)) {
            struct List decls;
            list_initialize(&decls);
            parse_declaration(parser, &decls);
            expect(parser, Token_Semicolon);
            for (struct ListHeader *it = list_begin(&decls),
                                   *eit = list_end(&decls);
                 it != eit; it = list_next(it)) {
                struct Declaration* decl = (struct Declaration*)it;
                strtable_id var_index = decl->name_index;

                assert(!env_find(env, var_index));
                struct VarIr* var =
                    ir_block_new_var(block, var_index, decl->type);
                env_insert(env, var_index, var);

                if (!decl->initializer) continue;

                struct AddrofExprIr* addrof_var =
                    ir_new_addrof_expr_with_var(var);
                struct StoreExprIr* subst = ir_new_store_expr(
                    ir_addrof_expr_cast(addrof_var), decl->initializer);
                ir_block_insert_expr_at_end(block, ir_store_expr_cast(subst));
            }
        } else {
            struct Ir* item = parse_statement(parser);
            if (item) ir_block_insert_at_end(block, item);
        }
    }
    expect(parser, Token_RightCurry);

    parser->current_env = env->outer_env;

    return block;
}

static struct ExprIr* parse_expression_statement(struct Parser* parser) {
    struct ExprIr* expr = NULL;
    if (!acceptable(parser, Token_Semicolon)) expr = parse_expression(parser);
    expect(parser, Token_Semicolon);
    return expr;
}

static struct CfIr* parse_selection_statement(struct Parser* parser) {
    if (acceptable(parser, Token_KeywordIf)) {
        advance(parser);

        expect(parser, Token_LeftParen);
        struct ExprIr* cond_expr = parse_expression(parser);
        expect(parser, Token_RightParen);

        struct Ir* true_stmt = parse_statement(parser);
        struct BlockIr* true_block = to_block(true_stmt);

        struct BlockIr* false_block = NULL;
        if (acceptable(parser, Token_KeywordElse)) {
            advance(parser);
            struct Ir* false_stmt = parse_statement(parser);
            false_block = to_block(false_stmt);
        }

        return ir_branch_cf_cast(
            ir_new_branch_cf(cond_expr, true_block, false_block));
    }
    assert(false);
}

static struct CfIr* parse_jump_statement(struct Parser* parser) {
    if (acceptable(parser, Token_KeywordReturn)) {
        advance(parser);
        struct ExprIr* expr = NULL;
        if (!acceptable(parser, Token_Semicolon)) {
            expr = parse_expression(parser);
            expect(parser, Token_Semicolon);
        }
        return ir_return_cf_cast(ir_new_return_cf(expr));
    }
    assert(false);
}

static struct Ir* parse_statement(struct Parser* parser) {
    if (acceptable(parser, Token_LeftCurry))
        return ir_block_cast(parse_compound_statement(parser, NULL));
    else if (acceptable(parser, Token_KeywordIf))
        return ir_cf_cast(parse_selection_statement(parser));
    else if (acceptable(parser, Token_KeywordReturn))
        return ir_cf_cast(parse_jump_statement(parser));
    else
        return ir_expr_cast(parse_expression_statement(parser));
}

/***** external definitions *****/

static struct FunctionIr* parse_function_definition(struct Parser* parser) {
    struct List func_decls;
    list_initialize(&func_decls);
    parse_declaration(parser, &func_decls);
    assert(list_size(&func_decls) == 1);
    struct Declaration* func_decl =
        (struct Declaration*)list_begin(&func_decls);

    struct Env* env = env_new(parser->current_env);
    parser->current_env = env;

    // ToDo: this function must be registered to call recursively
    strtable_id name_index = func_decl->name_index;
    struct VarIr* var_for_func = ir_new_var_for_func(name_index);
    context_register_var_for_func(parser->context, name_index, var_for_func);

    struct BlockIr* body = ir_new_block();

    expect(parser, Token_LeftParen);
    struct List* params = malloc(sizeof(struct List));
    list_initialize(params);
    while (!acceptable(parser, Token_RightParen)) {
        struct List param_decls;
        list_initialize(&param_decls);
        parse_declaration(parser, &param_decls);
        assert(list_size(&param_decls) == 1);

        struct Declaration* param_decl =
            (struct Declaration*)list_begin(&param_decls);
        strtable_id var_index = param_decl->name_index;
        struct VarIr* var = ir_block_new_var(body, var_index, param_decl->type);
        env_insert(env, var_index, var);

        struct ListItem* param_item = malloc(sizeof(struct ListItem));
        param_item->item = var;
        list_insert_at_end(params, list_from(param_item));

        if (acceptable(parser, Token_Comma)) {
            assert(peek_k(parser, 1)->tag != Token_RightParen);
            advance(parser);
        }
    }
    expect(parser, Token_RightParen);

    body = parse_compound_statement(parser, body);

    parser->current_env = env->outer_env;

    return ir_new_function(name_index, params, body);
}

static struct BlockIr* parse_translation_unit(struct Parser* parser) {
    expect(parser, Token_PseudoFileBegin);
    struct BlockIr* translation_unit = ir_new_block();
    while (!acceptable(parser, Token_PseudoFileEnd)) {
        struct Ir* item = ir_function_cast(parse_function_definition(parser));
        ir_block_insert_at_end(translation_unit, item);
    }
    expect(parser, Token_PseudoFileEnd);
    return translation_unit;
}

struct BlockIr* parser_run(struct Parser* parser) {
    return parse_translation_unit(parser);
}

struct Parser* parser_new(struct Context* context, struct List* tokens) {
    struct Parser* parser = malloc(sizeof(struct Parser));
    parser->context = context;
    parser->tokens = tokens;
    parser->current_token = list_begin(tokens);
    parser->current_env = NULL;
    return parser;
}
