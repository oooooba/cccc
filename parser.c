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
    struct Map var_map;  // key: strtable_id, value: VarExprIr*
    struct Env* outer_env;
};

static struct Env* env_new(struct Env* outer_env) {
    struct Env* env = malloc(sizeof(struct Env));
    map_initialize(&env->var_map);
    env->outer_env = outer_env;
    return env;
}

static struct VarExprIr* env_find(struct Env* env, strtable_id index) {
    if (!env) return NULL;
    struct VarExprIr* var =
        (struct VarExprIr*)map_find(&env->var_map, (void*)index);
    if (var) {
        assert(ir_var_expr_index(var) == index);
        return var;
    } else
        return env_find(env->outer_env, index);
}

static void env_insert(struct Env* env, strtable_id index,
                       struct VarExprIr* var) {
    assert(ir_var_expr_index(var) == index);
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
    return acceptable(parser, Token_KeywordLong) ||
           acceptable(parser, Token_KeywordInt) ||
           acceptable(parser, Token_KeywordChar) ||
           acceptable(parser, Token_KeywordStruct);
}

static void expect(struct Parser* parser, enum TokenTag expected) {
    if (!acceptable(parser, expected)) {
        fprintf(stderr, "expected %d, actual %d\n", expected,
                peek(parser)->tag);
        assert(false);
    }
    advance(parser);
}

static struct StmtIr* to_block_stmt(struct StmtIr* ir) {
    struct BlockStmtIr* block = ir_stmt_as_block(ir);
    if (!block) {
        block = ir_new_block_stmt();
        ir_block_stmt_insert_at_end(block, ir);
    }
    return ir_block_stmt_super(block);
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

static struct ExprIr* parse_expression(struct Parser* parser);

static struct ExprIr* parse_primary_expression(struct Parser* parser) {
    switch (peek(parser)->tag) {
        case Token_Integer:
            return parse_constant(parser);
        case Token_Id: {
            strtable_id index = parse_identifier(parser);
            struct VarExprIr* var = env_find(parser->current_env, index);
            if (var) {
                // reference to memory location (variable)
                var = ir_var_expr_clone(var);
                struct DerefExprIr* deref_var =
                    ir_new_deref_expr(ir_var_expr_cast(var));
                return ir_deref_expr_cast(deref_var);
            } else {
                struct FunctionIr* function =
                    context_find_function_declaration(parser->context, index);
                assert(function);
                struct VarExprIr* var = ir_var_expr_from_function(function);
                return ir_var_expr_cast(var);
            }
        } break;
        default:
            assert(false);
            return NULL;
    }
}

static struct ExprIr* parse_assignment_expression(struct Parser* parser);

static struct ExprIr* parse_postfix_expression(struct Parser* parser) {
    struct ExprIr* expr = parse_primary_expression(parser);
    while (true) {
        if (acceptable(parser, Token_LeftBracket)) {
            advance(parser);
            struct ExprIr* index = parse_expression(parser);
            expect(parser, Token_RightBracket);
            struct BinopExprIr* add =
                ir_new_binop_expr(BinopExprIrTag_Add, expr, index);
            struct DerefExprIr* deref =
                ir_new_deref_expr(ir_binop_expr_cast(add));
            expr = ir_deref_expr_cast(deref);
        } else if (acceptable(parser, Token_LeftParen)) {
            advance(parser);
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
            expr = ir_call_expr_cast(ir_new_call_expr(expr, args));
        } else if (acceptable(parser, Token_Dot)) {
            advance(parser);
            strtable_id name_index = parse_identifier(parser);
            struct AddrofExprIr* base = ir_new_addrof_expr(expr);
            struct MemberExprIr* member =
                ir_new_member_expr(ir_addrof_expr_cast(base), name_index);
            struct DerefExprIr* deref =
                ir_new_deref_expr(ir_member_expr_cast(member));
            expr = ir_deref_expr_cast(deref);
        } else
            break;
    }
    return expr;
}

static struct ExprIr* parse_cast_expression(struct Parser* parser);

static struct ExprIr* parse_unary_expression(struct Parser* parser) {
    if (acceptable(parser, Token_Ampersand)) {
        advance(parser);
        struct ExprIr* operand = parse_cast_expression(parser);
        return ir_addrof_expr_cast(ir_new_addrof_expr(operand));
    } else if (acceptable(parser, Token_Asterisk)) {
        advance(parser);
        struct ExprIr* operand = parse_cast_expression(parser);
        return ir_deref_expr_cast(ir_new_deref_expr(operand));
    } else
        return parse_postfix_expression(parser);
}

static struct TypeIr* parse_type_name(struct Parser* parser);

static struct ExprIr* parse_cast_expression(struct Parser* parser) {
    if (!acceptable(parser, Token_LeftParen))
        return parse_unary_expression(parser);
    expect(parser, Token_LeftParen);
    struct TypeIr* type = parse_type_name(parser);
    expect(parser, Token_RightParen);
    struct ExprIr* expr = parse_cast_expression(parser);
    return ir_cast_expr_cast(ir_new_cast_expr(expr, type));
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
        struct AddrofExprIr* addrof = ir_new_addrof_expr(lhs);
        struct ExprIr* rhs = parse_additive_expression(parser);
        struct SubstExprIr* subst =
            ir_new_subst_expr(ir_addrof_expr_cast(addrof), rhs);
        lhs = ir_subst_expr_cast(subst);
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

static strtable_id parse_declarator(struct Parser* parser,
                                    struct TypeIr* base_type,
                                    struct TypeIr** result_type);
static struct TypeIr* parse_type_specifier(struct Parser* parser);

static struct ExprIr* parse_initializer(struct Parser* parser) {
    return parse_expression(parser);
}

static struct TypeIr* parse_struct_or_union_specifier(struct Parser* parser) {
    expect(parser, Token_KeywordStruct);

    // ToDo: currently, anonymous struct is not supported
    assert(acceptable(parser, Token_Id));
    strtable_id name_index = parse_identifier(parser);

    struct TypeIr* type =
        context_find_user_defined_type(parser->context, name_index);
    if (!type) {
        type = type_struct_super(type_new_struct(name_index, NULL));
        context_insert_user_defined_type(parser->context, name_index, type);
    }

    if (acceptable(parser, Token_LeftCurry)) {
        advance(parser);

        struct List* elem_types = malloc(sizeof(struct List));
        list_initialize(elem_types);

        while (!acceptable(parser, Token_RightCurry)) {
            struct TypeIr* base_type = parse_type_specifier(parser);
            struct TypeIr* type;
            strtable_id member_index =
                parse_declarator(parser, base_type, &type);

            struct MemberEntry* entry =
                type_new_member_entry(member_index, type);
            list_insert_at_end(elem_types,
                               type_member_entry_as_list_header(entry));

            expect(parser, Token_Semicolon);
        }
        expect(parser, Token_RightCurry);

        type_struct_set_elem_types(type_as_struct(type), elem_types);
    }

    return type;
}

static struct TypeIr* parse_type_specifier(struct Parser* parser) {
    struct TypeIr* type;
    switch (peek(parser)->tag) {
        case Token_KeywordLong:
            type = type_long_super(type_new_long());
            break;
        case Token_KeywordInt:
            type = type_int_super(type_new_int());
            break;
        case Token_KeywordChar:
            type = type_char_super(type_new_char());
            break;
        case Token_KeywordVoid:
            type = type_void_super(type_new_void());
            break;
        case Token_KeywordStruct:
            return parse_struct_or_union_specifier(parser);
        default:
            assert(false);
    }
    advance(parser);
    return type;
}

static strtable_id parse_declarator(struct Parser* parser,
                                    struct TypeIr* base_type,
                                    struct TypeIr** result_type) {
    if (acceptable(parser, Token_Asterisk)) {
        advance(parser);
        *result_type = type_pointer_super(type_new_pointer(base_type));
    } else
        *result_type = base_type;

    assert(acceptable(parser, Token_Id));
    struct Token* token = peek(parser);
    strtable_id name_index = token->strtable_index;
    advance(parser);

    return name_index;
}

static struct TypeIr* parse_type_name(struct Parser* parser) {
    struct TypeIr* type = parse_type_specifier(parser);
    if (acceptable(parser, Token_Asterisk)) {
        advance(parser);
        type = type_pointer_super(type_new_pointer(type));
    }
    return type;
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

static struct StmtIr* parse_statement(struct Parser* parser);

static struct BlockStmtIr* parse_compound_statement(struct Parser* parser,
                                                    struct BlockStmtIr* block) {
    expect(parser, Token_LeftCurry);

    if (!block) block = ir_new_block_stmt();

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
                strtable_id name_index = decl->name_index;

                assert(!env_find(env, name_index));
                struct VarExprIr* var = ir_block_stmt_allocate_variable(
                    block, name_index, decl->type);
                env_insert(env, name_index, var);

                if (!decl->initializer) continue;

                struct SubstExprIr* subst =
                    ir_new_subst_expr(ir_var_expr_cast(var), decl->initializer);
                struct ExprStmtIr* stmt =
                    ir_new_expr_stmt(ir_subst_expr_cast(subst));
                ir_block_stmt_insert_at_end(block, ir_expr_stmt_super(stmt));
            }
        } else {
            struct StmtIr* stmt = parse_statement(parser);
            if (stmt) ir_block_stmt_insert_at_end(block, stmt);
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

static struct StmtIr* parse_selection_statement(struct Parser* parser) {
    if (acceptable(parser, Token_KeywordIf)) {
        advance(parser);

        expect(parser, Token_LeftParen);
        struct ExprIr* cond_expr = parse_expression(parser);
        expect(parser, Token_RightParen);

        struct StmtIr* true_stmt = parse_statement(parser);
        true_stmt = to_block_stmt(true_stmt);

        struct StmtIr* false_stmt = NULL;
        if (acceptable(parser, Token_KeywordElse)) {
            advance(parser);
            false_stmt = parse_statement(parser);
            false_stmt = to_block_stmt(false_stmt);
        }

        return ir_if_stmt_super(
            ir_new_if_stmt(cond_expr, true_stmt, false_stmt));
    }
    assert(false);
}

static struct StmtIr* parse_jump_statement(struct Parser* parser) {
    if (acceptable(parser, Token_KeywordReturn)) {
        advance(parser);
        struct ExprIr* expr = NULL;
        if (!acceptable(parser, Token_Semicolon)) {
            expr = parse_expression(parser);
            expect(parser, Token_Semicolon);
        }
        return ir_return_stmt_super(ir_new_return_stmt(expr));
    }
    assert(false);
}

static struct StmtIr* parse_statement(struct Parser* parser) {
    if (acceptable(parser, Token_LeftCurry))
        return ir_block_stmt_super(parse_compound_statement(parser, NULL));
    else if (acceptable(parser, Token_KeywordIf))
        return parse_selection_statement(parser);
    else if (acceptable(parser, Token_KeywordReturn))
        return parse_jump_statement(parser);
    else
        return ir_expr_stmt_super(
            ir_new_expr_stmt(parse_expression_statement(parser)));
}

/***** external definitions *****/

static struct FunctionIr* parse_function_definition_or_declaration(
    struct Parser* parser) {
    struct List func_decls;
    list_initialize(&func_decls);
    parse_declaration(parser, &func_decls);
    assert(list_size(&func_decls) == 1);
    struct Declaration* func_decl =
        (struct Declaration*)list_begin(&func_decls);

    struct Env* env = env_new(parser->current_env);
    parser->current_env = env;

    struct BlockStmtIr* body = ir_new_block_stmt();

    expect(parser, Token_LeftParen);
    struct List* params = malloc(sizeof(struct List));
    list_initialize(params);
    struct List* param_types = malloc(sizeof(struct List));
    list_initialize(param_types);
    if (acceptable(parser, Token_KeywordVoid))
        advance(parser);
    else {
        while (!acceptable(parser, Token_RightParen)) {
            struct List param_decls;
            list_initialize(&param_decls);
            parse_declaration(parser, &param_decls);
            assert(list_size(&param_decls) == 1);

            struct Declaration* param_decl =
                (struct Declaration*)list_begin(&param_decls);
            strtable_id name_index = param_decl->name_index;
            struct VarExprIr* var = ir_block_stmt_allocate_variable(
                body, name_index, param_decl->type);
            env_insert(env, name_index, var);

            struct ListItem* param_item = malloc(sizeof(struct ListItem));
            param_item->item = var;
            list_insert_at_end(params, list_from(param_item));

            struct ListItem* param_type_item = malloc(sizeof(struct ListItem));
            param_type_item->item = param_decl->type;
            list_insert_at_end(param_types, list_from(param_type_item));

            if (acceptable(parser, Token_Comma)) {
                assert(peek_k(parser, 1)->tag != Token_RightParen);
                advance(parser);
            }
        }
    }
    expect(parser, Token_RightParen);

    bool has_func_def;
    if (acceptable(parser, Token_LeftCurry))
        has_func_def = true;
    else if (acceptable(parser, Token_Semicolon))
        has_func_def = false;
    else
        assert(false);

    struct FunctionTypeIr* func_type =
        type_new_function(func_decl->type, param_types);
    strtable_id name_index = func_decl->name_index;
    struct FunctionIr* function =
        context_find_function_declaration(parser->context, name_index);
    if (function) {
        // ToDo: check function type

        // prevent to insert dupulicate definitions
        if (has_func_def) assert(!ir_function_has_defined(function));
    } else {
        function = ir_new_function(name_index, func_type);
        context_insert_function_declaration(parser->context, name_index,
                                            function);
    }

    if (has_func_def) {
        body = parse_compound_statement(parser, body);
        assert(!ir_function_has_defined(function));
        ir_function_define(function, params, body);
    } else
        expect(parser, Token_Semicolon);

    parser->current_env = env->outer_env;
    return function;
}

static void parse_translation_unit(struct Parser* parser) {
    expect(parser, Token_PseudoFileBegin);
    while (!acceptable(parser, Token_PseudoFileEnd)) {
        if (acceptable(parser, Token_KeywordStruct)) {
            struct TypeIr* type = parse_type_specifier(parser);
            expect(parser, Token_Semicolon);
            (void)type;
        } else {
            parse_function_definition_or_declaration(parser);
        }
    }
    expect(parser, Token_PseudoFileEnd);
}

void parser_run(struct Parser* parser) { parse_translation_unit(parser); }

struct Parser* parser_new(struct Context* context, struct List* tokens) {
    struct Parser* parser = malloc(sizeof(struct Parser));
    parser->context = context;
    parser->tokens = tokens;
    parser->current_token = list_begin(tokens);
    parser->current_env = NULL;
    return parser;
}
