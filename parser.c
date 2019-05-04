#include "parser.h"
#include "ast.h"
#include "context.h"
#include "ir.h"
#include "list.h"
#include "token.h"
#include "type.h"
#include "vector.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

/***** helper functions *****/

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

static struct VarIr* parse_identifier(struct Parser* parser) {
    assert("wrong implementation" && false);
    assert(acceptable(parser, Token_Id));
    struct Token* token = peek(parser);
    return ir_block_new_var(parser->current_block, token->strtable_index, NULL);
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
            struct VarIr* var = parse_identifier(parser);
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

static struct ExprIr* parse_cast_expression(struct Parser* parser) {
    return parse_primary_expression(parser);
}

static struct ExprIr* parse_multiplicative_expression(struct Parser* parser) {
    struct ExprIr* lhs = parse_cast_expression(parser);
    for (;;) {
        enum BinopExprNodeTag op;
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
        enum BinopExprNodeTag op;
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

static struct ExprIr* parse_expression(struct Parser* parser) {
    return parse_additive_expression(parser);
}

/***** declarations *****/

static struct ExprIr* parse_initializer(struct Parser* parser) {
    return parse_expression(parser);
}

static struct TypeIr* parse_type_specifier(struct Parser* parser) {
    assert(acceptable(parser, Token_KeywordInt));
    advance(parser);
    return type_new_int2();
}

static strtable_id parse_declarator(struct Parser* parser) {
    assert(acceptable(parser, Token_Id));
    struct Token* token = peek(parser);
    strtable_id name_index = token->strtable_index;
    advance(parser);
    return name_index;
}

static struct Ir* parse_declaration(struct Parser* parser) {
    struct TypeIr* type = parse_type_specifier(parser);
    strtable_id var_index = parse_declarator(parser);
    struct VarIr* var =
        ir_block_new_var(parser->current_block, var_index, type);
    if (acceptable(parser, Token_Equal)) {
        advance(parser);

        struct ExprIr* initializer = parse_initializer(parser);

        struct AddrofExprIr* addrof_var = ir_new_addrof_expr_with_var(var);
        struct StoreExprIr* subst =
            ir_new_store_expr(ir_addrof_expr_cast(addrof_var), initializer);
        ir_block_insert_expr_at_end(parser->current_block,
                                    ir_store_expr_cast(subst));
    }
    expect(parser, Token_Semicolon);
    return NULL;
}

/***** statements and blocks *****/

static struct Ir* parse_statement(struct Parser* parser);

static struct BlockIr* parse_compound_statement(struct Parser* parser) {
    expect(parser, Token_LeftCurry);
    struct BlockIr* prev_block = parser->current_block;
    struct BlockIr* block = ir_new_block();
    parser->current_block = block;
    while (!acceptable(parser, Token_RightCurry)) {
        struct Ir* item = acceptable_type(parser) ? parse_declaration(parser)
                                                  : parse_statement(parser);
        if (item) ir_block_insert_at_end(block, item);
    }
    expect(parser, Token_RightCurry);
    parser->current_block = prev_block;
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

static struct Ir* parse_statement(struct Parser* parser) {
    if (acceptable(parser, Token_LeftCurry))
        return ir_block_cast(parse_compound_statement(parser));
    else if (acceptable(parser, Token_KeywordIf))
        return ir_cf_cast(parse_selection_statement(parser));
    else
        return ir_expr_cast(parse_expression_statement(parser));
}

/***** external definitions *****/

static struct FunctionIr* parse_function_definition(struct Parser* parser) {
    expect(parser, Token_KeywordInt);

    assert(acceptable(parser, Token_Id));
    struct Token* token = peek(parser);
    strtable_id name_index = token->strtable_index;
    advance(parser);

    expect(parser, Token_LeftParen);
    expect(parser, Token_RightParen);

    struct BlockIr* body = parse_compound_statement(parser);

    return ir_new_function(name_index, body);
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

void parser_initialize(struct Parser* parser, struct Context* context,
                       struct List* tokens) {
    parser->context = context;
    parser->tokens = tokens;
    parser->current_token = list_begin(tokens);
    parser->current_block = NULL;
}
