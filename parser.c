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

static struct Token* token_from(void* p) { return p; }

struct BinopInfoEntry {
    enum TokenTag token;
    enum BinopExprNodeTag op;
    int precedence;
};

static const struct BinopInfoEntry binop_info_table[] = {
    {
        .token = Token_Equal, .op = BinopExprNodeTag_Subst, .precedence = 1,
    },
    {
        .token = Token_Plus, .op = BinopExprNodeTag_Add, .precedence = 2,
    },
    {
        .token = Token_Minus, .op = BinopExprNodeTag_Sub, .precedence = 2,
    },
    {
        .token = Token_Asterisk, .op = BinopExprNodeTag_Mul, .precedence = 3,
    },
};

static const struct BinopInfoEntry* find_binop_table_entry(
    enum TokenTag token) {
    for (size_t i = 0;
         i < sizeof(binop_info_table) / sizeof(struct BinopInfoEntry); ++i) {
        if (binop_info_table[i].token == token) return &binop_info_table[i];
    }
    return NULL;
}

static int find_precedence(enum TokenTag token) {
    const struct BinopInfoEntry* entry = find_binop_table_entry(token);
    return entry ? entry->precedence : -1;
}

static enum BinopExprNodeTag find_op(enum TokenTag token) {
    const struct BinopInfoEntry* entry = find_binop_table_entry(token);
    if (entry)
        return entry->op;
    else {
        assert(false);
        return -1;
    }
}

static struct Token* peek_k(struct Parser* parser, size_t k) {
    struct ListHeader* it = parser->current_token;
    for (; k; --k) {
        it = list_next(it);
    }
    return token_from(it);
}

static struct Token* peek(struct Parser* parser) { return peek_k(parser, 0); }

static void advance(struct Parser* parser) {
    parser->current_token = list_next(parser->current_token);
}

static struct ListHeader* take_snapshot(struct Parser* parser) {
    return parser->current_token;
}

static struct ListHeader* restore_snapshot(struct Parser* parser,
                                           struct ListHeader* snapshot) {
    struct ListHeader* current = parser->current_token;
    parser->current_token = snapshot;
    return current;
}

static struct ExprNode* parse_integer_literal_expr(struct Parser* parser) {
    struct Token* token = peek(parser);
    assert(token->tag == Token_Integer);
    struct LiteralExprNode* node = ast_new_integer_literal_expr(token->integer);
    advance(parser);
    return ast_literal_expr_super_type(node);
}

static struct IdNode* parse_id(struct Parser* parser) {
    struct Token* token = peek(parser);
    assert(token->tag == Token_Id);
    struct IdNode* node = ast_new_id(token->strtable_index);
    advance(parser);
    return node;
}

static struct VarExprNode* parse_var_expr(struct Parser* parser) {
    assert(peek(parser)->tag == Token_Id);
    struct IdNode* id = parse_id(parser);
    return ast_new_var_expr(id);
}

static struct ExprNode* parse_primary_expr(struct Parser* parser) {
    switch (peek(parser)->tag) {
        case Token_Integer:
            return parse_integer_literal_expr(parser);
        case Token_Id: {
            struct VarExprNode* var = parse_var_expr(parser);
            struct UniopExprNode* addrof_var = ast_new_uniop_expr(
                UniopExprNodeTag_Addrof, ast_var_expr_super_type(var));
            struct UniopExprNode* deref_addrof_var = ast_new_uniop_expr(
                UniopExprNodeTag_Deref, ast_uniop_expr_super_type(addrof_var));
            return ast_uniop_expr_super_type(deref_addrof_var);
        }
        default:
            assert(false);
            return NULL;
    }
}

static struct ExprNode* parse_binop_expr_helper(struct Parser* parser,
                                                struct ExprNode* lhs,
                                                int left_prec) {
    for (;;) {
        struct Token* token = peek(parser);
        int current_prec = find_precedence(token->tag);
        if (current_prec < left_prec) return lhs;

        enum BinopExprNodeTag op = find_op(token->tag);
        advance(parser);
        struct ExprNode* rhs = parse_primary_expr(parser);

        int right_prec = find_precedence(peek(parser)->tag);
        if (current_prec < right_prec)
            rhs = parse_binop_expr_helper(parser, rhs, current_prec + 1);
        lhs = ast_binop_expr_super_type(ast_new_binop_expr(op, lhs, rhs));
    }
}

static struct ExprNode* parse_binop_expr(struct Parser* parser) {
    struct ExprNode* lhs = parse_primary_expr(parser);
    return parse_binop_expr_helper(parser, lhs, 0);
}

static struct ExprNode* parse_expr(struct Parser* parser) {
    return parse_binop_expr(parser);
}

static struct TypeNode* parse_type(struct Parser* parser) {
    struct Token* token = peek(parser);
    if (token->tag != Token_KeywordInt) return NULL;
    advance(parser);
    return type_new_int();
}

static struct DeclNode* parse_decl(struct Parser* parser,
                                   struct TypeNode* type) {
    assert(type);
    struct IdNode* name = parse_id(parser);
    struct ExprNode* initial_expr = NULL;
    if (peek(parser)->tag == Token_Equal) {
        advance(parser);
        initial_expr = parse_expr(parser);
    }
    return ast_new_decl(name, type, initial_expr);
}

static struct StmtNode* parse_stmt(struct Parser* parser);

static struct IfStmtNode* parse_if_stmt(struct Parser* parser) {
    assert(peek(parser)->tag == Token_KeywordIf);
    advance(parser);

    assert(peek(parser)->tag == Token_LeftParen);
    advance(parser);
    struct ExprNode* cond_expr = parse_expr(parser);
    assert(peek(parser)->tag == Token_RightParen);
    advance(parser);

    struct StmtNode* then_stmt = parse_stmt(parser);
    struct BlockStmtNode* then_block;
    if (ast_stmt_tag(then_stmt) == StmtNodeTag_Block)
        then_block = (struct BlockStmtNode*)then_stmt;
    else {
        then_block = ast_new_block_stmt();
        ast_block_stmt_insert_at_end(then_block, then_stmt);
    }

    struct BlockStmtNode* else_block;
    if (peek(parser)->tag == Token_KeywordElse) {
        advance(parser);
        struct StmtNode* else_stmt = parse_stmt(parser);
        if (ast_stmt_tag(else_stmt) == StmtNodeTag_Block)
            else_block = (struct BlockStmtNode*)else_stmt;
        else {
            else_block = ast_new_block_stmt();
            ast_block_stmt_insert_at_end(else_block, else_stmt);
        }
    } else
        else_block = ast_new_block_stmt();

    return ast_new_if_stmt(cond_expr, then_block, else_block);
}

static struct BlockStmtNode* parse_block_stmt(struct Parser* parser) {
    assert(peek(parser)->tag == Token_LeftCurry);
    advance(parser);
    struct BlockStmtNode* block = ast_new_block_stmt();
    while (peek(parser)->tag != Token_RightCurry) {
        struct StmtNode* stmt = parse_stmt(parser);
        ast_block_stmt_insert_at_end(block, stmt);
    }
    advance(parser);
    return block;
}

static struct StmtNode* parse_stmt(struct Parser* parser) {
    switch (peek(parser)->tag) {
        case Token_LeftCurry:
            return ast_block_stmt_super_type(parse_block_stmt(parser));
        case Token_KeywordIf:
            return ast_if_stmt_super_type(parse_if_stmt(parser));
        default: {
            struct ListHeader* snapshot = take_snapshot(parser);
            struct TypeNode* type = parse_type(parser);
            struct StmtNode* stmt;
            if (type) {
                struct DeclNode* decl = parse_decl(parser, type);
                stmt = ast_decl_stmt_super_type(ast_new_decl_stmt(decl));
            } else {
                restore_snapshot(parser, snapshot);
                struct ExprNode* expr = parse_expr(parser);
                stmt = ast_expr_stmt_super_type(ast_new_expr_stmt(expr));
            }
            assert(peek(parser)->tag == Token_Semicolon);
            advance(parser);
            return stmt;
        }
    }
}

static struct FundefNode* parse_fundef(struct Parser* parser) {
    assert(peek(parser)->tag == Token_KeywordInt);
    struct TypeNode* return_type = parse_type(parser);
    assert(peek(parser)->tag == Token_Id);
    struct IdNode* name = parse_id(parser);
    assert(peek(parser)->tag == Token_LeftParen);
    advance(parser);
    assert(peek(parser)->tag == Token_RightParen);
    advance(parser);
    struct Vector* params = malloc(sizeof(struct Vector));
    vector_initialize(params, sizeof(void*));  // ToDo: fix
    assert(peek(parser)->tag == Token_LeftCurry);
    struct BlockStmtNode* body = parse_block_stmt(parser);
    return ast_new_fundef(name, return_type, params, body);
}

struct List* parser_parse(struct Parser* parser) {
    assert(peek(parser)->tag == Token_PseudoFileBegin);
    advance(parser);
    struct List* top_levels = malloc(sizeof(struct List));
    list_initialize(top_levels);
    while (peek(parser)->tag != Token_PseudoFileEnd) {
        struct FundefNode* fundef = parse_fundef(parser);
        struct ListItem* item = malloc(sizeof(struct ListItem));
        item->item = fundef;
        list_insert_at_end(top_levels, list_from(item));  // ToDo: fix
    }
    advance(parser);
    return top_levels;
}

/////////////////////////////////////////////////////////////////////////////

/***** helper functions *****/

bool acceptable(struct Parser* parser, enum TokenTag expected) {
    if (peek(parser)->tag == expected)
        return true;
    else
        return false;
}

void expect(struct Parser* parser, enum TokenTag expected) {
    if (!acceptable(parser, expected)) {
        fprintf(stderr, "expected %d, actual %d\n", expected,
                peek(parser)->tag);
        assert(false);
    }
    advance(parser);
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
    assert(acceptable(parser, Token_Id));
    struct Token* token = peek(parser);
    return ir_block_new_var(parser->current_block, token->strtable_index);
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

/***** statements and blocks *****/

static struct Ir* parse_statement(struct Parser* parser);

static struct BlockIr* parse_compound_statement(struct Parser* parser) {
    expect(parser, Token_LeftCurry);
    struct BlockIr* prev_block = parser->current_block;
    struct BlockIr* block = ir_new_block();
    parser->current_block = block;
    while (!acceptable(parser, Token_RightCurry)) {
        struct Ir* item = parse_statement(parser);
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

static struct BlockIr* to_block(struct Ir* ir) {
    struct BlockIr* block = ir_as_block(ir);
    if (!block) {
        block = ir_new_block();
        ir_block_insert_at_end(block, ir);
    }
    return block;
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
