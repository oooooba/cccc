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

struct Parser {
    struct Context* context;
    struct List* tokens;
    struct ListHeader* current_token;
    // struct Env* current_env;
};

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

static bool acceptable_storage_class_specifier(struct Parser* parser) {
    return acceptable(parser, Token_KeywordTypedef);
}

static bool acceptable_type_specifier(struct Parser* parser) {
    return acceptable(parser, Token_KeywordLong) ||
           acceptable(parser, Token_KeywordInt) ||
           acceptable(parser, Token_KeywordChar) ||
           acceptable(parser, Token_KeywordVoid) ||
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

static void insert_at_end_as_list_item(struct List* list, void* item) {
    struct ListItem* list_item = malloc(sizeof(struct ListItem));
    list_item->item = item;
    list_insert_at_end(list, list_from(list_item));
}

struct Declarator;

enum DirectDeclaratorTag {
    DirectDeclaratorTag_Identifier,
    DirectDeclaratorTag_Declarator,
    DirectDeclaratorTag_Parameters,
};

struct DirectDeclarator {
    enum DirectDeclaratorTag tag;
    union {
        strtable_id identifier;
        struct Declarator* declarator;
        struct {
            struct DirectDeclarator* direct_declarator;
            struct List*
                list;  // struct ListItem list, item: struct Declaration*
        } parameters;
    };
};

static struct DirectDeclarator* new_direct_declarator(
    enum DirectDeclaratorTag tag) {
    struct DirectDeclarator* direct_declarator =
        malloc(sizeof(struct DirectDeclarator));
    *direct_declarator = (struct DirectDeclarator){
        .tag = tag,
    };
    return direct_declarator;
}

struct Declarator {
    bool has_pointer;  // ToDo: fix to handle precisely
    struct DirectDeclarator* direct_declarator;
};

static struct Declarator* new_declarator(
    bool has_pointer, struct DirectDeclarator* direct_declarator) {
    struct Declarator* declarator = malloc(sizeof(struct Declarator));
    *declarator = (struct Declarator){
        .has_pointer = has_pointer, .direct_declarator = direct_declarator,
    };
    return declarator;
}

struct InitDeclarator {
    struct ListHeader as_list;
    struct Declarator* declarator;
    struct ExprIr* assign_expression;
};

static struct InitDeclarator* new_init_declarator(
    struct Declarator* declarator, struct ExprIr* assign_expression) {
    struct InitDeclarator* init_declarator =
        malloc(sizeof(struct InitDeclarator));
    *init_declarator = (struct InitDeclarator){
        .declarator = declarator, .assign_expression = assign_expression,
    };
    return init_declarator;
}

enum StorageClassSpecifierTag {
    StorageClassSpecifierTag_Typedef,
};

enum DeclarationSpecifierTag {
    DeclarationSpecifierTag_StorageClass,
    DeclarationSpecifierTag_Type,
};

struct DeclarationSpecifier {
    struct DeclarationSpecifier* outer;
    enum DeclarationSpecifierTag tag;
    union {
        enum StorageClassSpecifierTag storageClassTag;
        struct TypeIr* type;
    };
};

static struct DeclarationSpecifier* new_declaration_specifier(
    struct DeclarationSpecifier* outer, enum DeclarationSpecifierTag tag) {
    struct DeclarationSpecifier* specifier =
        malloc(sizeof(struct DeclarationSpecifier));
    *specifier = (struct DeclarationSpecifier){
        .outer = outer, .tag = tag,
    };
    return specifier;
}

struct Declaration2 {
    struct DeclarationSpecifier* declaration_specifiers;
    struct List* init_declarator_list;  // struct InitDeclarator list
};

static struct Declaration2* new_declaration(
    struct DeclarationSpecifier* declaration_specifiers,
    struct List* init_declarator_list) {
    assert(declaration_specifiers);

    struct Declaration2* declaration = malloc(sizeof(struct Declaration2));
    *declaration = (struct Declaration2){
        .declaration_specifiers = declaration_specifiers,
        .init_declarator_list = init_declarator_list,
    };
    return declaration;
}

static struct List* normalize_declaration(struct Declaration2* declaration) {
    struct List* decl_list = malloc(sizeof(struct List));
    list_initialize(decl_list);

    if (list_size(declaration->init_declarator_list) == 1) {
        insert_at_end_as_list_item(decl_list, declaration);
        return decl_list;
    }

    while (list_size(declaration->init_declarator_list)) {
        struct ListHeader* it = list_begin(declaration->init_declarator_list);
        list_erase_at_begin(declaration->init_declarator_list);

        struct List* list = malloc(sizeof(struct List));
        list_initialize(list);
        list_insert_at_begin(list, it);

        struct Declaration2* decl =
            new_declaration(declaration->declaration_specifiers, list);
        insert_at_end_as_list_item(decl_list, decl);
    }

    return decl_list;
}

static struct DeclStmtIr* insert_normalized_declaration_statement(
    struct Declaration2* declaration, struct Context* context,
    struct BlockStmtIr* block) {
    (void)context;
    assert(block);
    assert(declaration->declaration_specifiers->tag ==
           DeclarationSpecifierTag_Type);
    struct TypeIr* base_type = declaration->declaration_specifiers->type;

    struct InitDeclarator* init_declarator = (struct InitDeclarator*)list_cast(
        list_begin(declaration->init_declarator_list));
    struct Declarator* declarator = init_declarator->declarator;
    struct DirectDeclarator* direct_declarator = declarator->direct_declarator;
    assert(direct_declarator->tag == DirectDeclaratorTag_Identifier);

    strtable_id name_index = direct_declarator->identifier;
    struct VarExprIr* var = ir_new_var_expr(name_index);
    struct TypeIr* type = declarator->has_pointer
                              ? type_pointer_super(type_new_pointer(base_type))
                              : base_type;
    struct DeclStmtIr* decl = ir_new_decl_stmt(name_index, type, block);
    ir_block_stmt_insert_at_end(block, ir_decl_stmt_super(decl));

    if (init_declarator->assign_expression) {
        struct SubstExprIr* subst = ir_new_subst_expr(
            ir_var_expr_cast(var), init_declarator->assign_expression);
        struct ExprStmtIr* stmt = ir_new_expr_stmt(ir_subst_expr_cast(subst));
        ir_block_stmt_insert_at_end(block, ir_expr_stmt_super(stmt));
    }

    return decl;
}

/***** prototypes *****/

static struct Declaration2* parse_declaration2(struct Parser* parser);
static struct DeclarationSpecifier* parse_declaration_specifiers(
    struct Parser* parser);
static struct List* parse_init_declarator_list(struct Parser* parser);
static struct InitDeclarator* parse_init_declarator(struct Parser* parser);
static enum StorageClassSpecifierTag parse_storage_class_specifier(
    struct Parser* parser);
static struct TypeIr* parse_type_specifier(struct Parser* parser);
static struct TypeIr* parse_struct_or_union_specifier(struct Parser* parser);
static struct Declarator* parse_declarator2(struct Parser* parser);
static struct DirectDeclarator* parse_direct_declarator(struct Parser* parser);
static struct List* parse_parameter_type_list(struct Parser* parser);
static struct List* parse_parameter_list(struct Parser* parser);
static struct Declaration2* parse_parameter_declaration(struct Parser* parser);
static struct TypeIr* parse_type_name(struct Parser* parser);

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
            struct VarExprIr* var = ir_new_var_expr(index);
            struct DerefExprIr* deref_var =
                ir_new_deref_expr(ir_var_expr_cast(var));
            return ir_deref_expr_cast(deref_var);
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
                insert_at_end_as_list_item(args, arg);
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
    } else if (acceptable(parser, Token_KeywordSizeof)) {
        struct ExprIr* const_expr = NULL;
        advance(parser);
        expect(parser, Token_LeftParen);
        if (acceptable_type_specifier(parser)) {
            struct TypeIr* type = parse_type_name(parser);
            const_expr =
                ir_const_expr_cast(ir_new_integer_const_expr(type_size(type)));
        } else {
            assert(false);
        }
        expect(parser, Token_RightParen);
        return const_expr;
    } else
        return parse_postfix_expression(parser);
}

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

static struct Declaration2* parse_declaration2(struct Parser* parser) {
    struct DeclarationSpecifier* declaration_specifiers =
        parse_declaration_specifiers(parser);
    if (!declaration_specifiers) return NULL;

    if (acceptable(parser, Token_Semicolon)) {
        expect(parser, Token_Semicolon);
        return new_declaration(declaration_specifiers, NULL);
    }

    struct List* init_declarator_list = parse_init_declarator_list(parser);
    if (!init_declarator_list) return NULL;
    if (acceptable(parser, Token_Semicolon)) {
        expect(parser, Token_Semicolon);
        return new_declaration(declaration_specifiers, init_declarator_list);
    }
    return NULL;
}

static struct DeclarationSpecifier* parse_declaration_specifiers(
    struct Parser* parser) {
    struct DeclarationSpecifier* specifiers = NULL;
    while (true) {
        if (acceptable_type_specifier(parser)) {
            struct TypeIr* type = parse_type_specifier(parser);
            if (!type) return NULL;
            specifiers = new_declaration_specifier(
                specifiers, DeclarationSpecifierTag_Type);
            specifiers->type = type;
        } else if (acceptable_storage_class_specifier(parser)) {
            assert(false);
            enum StorageClassSpecifierTag tag =
                parse_storage_class_specifier(parser);
            specifiers = new_declaration_specifier(
                specifiers, DeclarationSpecifierTag_StorageClass);
            specifiers->storageClassTag = tag;
        } else
            break;
    }
    return specifiers;
}

static struct List* parse_init_declarator_list(struct Parser* parser) {
    struct InitDeclarator* init_declarator = parse_init_declarator(parser);
    if (!init_declarator) return NULL;

    struct List* init_declarator_list = malloc(sizeof(struct List));
    list_initialize(init_declarator_list);
    list_insert_at_end(init_declarator_list, list_from(init_declarator));

    while (acceptable(parser, Token_Comma)) {
        advance(parser);
        init_declarator = parse_init_declarator(parser);
        if (!init_declarator) return NULL;
        list_insert_at_end(init_declarator_list, list_from(init_declarator));
    }
    return init_declarator_list;
}

static struct InitDeclarator* parse_init_declarator(struct Parser* parser) {
    struct Declarator* declarator = parse_declarator2(parser);
    if (!declarator) return NULL;

    struct ExprIr* assign_expression = NULL;
    if (acceptable(parser, Token_Equal)) {
        advance(parser);
        assign_expression = parse_assignment_expression(parser);
        if (!assign_expression) return NULL;
    }
    return new_init_declarator(declarator, assign_expression);
}

static enum StorageClassSpecifierTag parse_storage_class_specifier(
    struct Parser* parser) {
    enum StorageClassSpecifierTag tag;
    switch (peek(parser)->tag) {
        case Token_KeywordTypedef:
            tag = StorageClassSpecifierTag_Typedef;
            break;
        default:
            assert(false);
    }
    advance(parser);
    return tag;
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

        while (!acceptable(parser,
                           Token_RightCurry)) {  // ToDo: fix to parse precisely
            struct TypeIr* type = parse_type_specifier(parser);
            strtable_id member_index = parse_identifier(parser);

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

static struct Declarator* parse_declarator2(struct Parser* parser) {
    bool has_pointer = false;
    if (acceptable(parser, Token_Asterisk)) {
        advance(parser);
        has_pointer = true;
    }

    struct DirectDeclarator* direct_declarator =
        parse_direct_declarator(parser);
    return direct_declarator ? new_declarator(has_pointer, direct_declarator)
                             : NULL;
}

static struct DirectDeclarator* parse_direct_declarator(struct Parser* parser) {
    struct DirectDeclarator* direct_declarator = NULL;
    if (acceptable(parser, Token_Id)) {
        strtable_id id = parse_identifier(parser);
        if (id == STRTABLE_INVALID_ID) return NULL;

        direct_declarator =
            new_direct_declarator(DirectDeclaratorTag_Identifier);
        direct_declarator->identifier = id;
    } else if (acceptable(parser, Token_LeftParen)) {
        advance(parser);

        struct Declarator* declarator = parse_declarator2(parser);
        if (!declarator) return NULL;

        if (!acceptable(parser, Token_RightParen)) return NULL;
        advance(parser);

        direct_declarator =
            new_direct_declarator(DirectDeclaratorTag_Declarator);
        direct_declarator->declarator = declarator;
    } else
        return NULL;

    while (true) {
        if (acceptable(parser, Token_LeftParen)) {
            advance(parser);
            struct List* parameter_type_list =
                parse_parameter_type_list(parser);
            if (!parameter_type_list) return NULL;
            if (!acceptable(parser, Token_RightParen)) return NULL;
            expect(parser, Token_RightParen);

            struct DirectDeclarator* inner_direct_declarator =
                direct_declarator;
            direct_declarator =
                new_direct_declarator(DirectDeclaratorTag_Parameters);
            direct_declarator->parameters.direct_declarator =
                inner_direct_declarator;
            direct_declarator->parameters.list = parameter_type_list;
        } else
            break;
    }
    return direct_declarator;
}

static struct List* parse_parameter_type_list(struct Parser* parser) {
    struct List* parameter_list = parse_parameter_list(parser);
    return parameter_list;
}

static struct List* parse_parameter_list(struct Parser* parser) {
    struct Declaration2* parameter = parse_parameter_declaration(parser);
    if (!parameter) return NULL;

    struct List* parameter_list = malloc(sizeof(struct List));
    list_initialize(parameter_list);
    insert_at_end_as_list_item(parameter_list, parameter);

    // hold to check its type
    struct Declaration2* first_decl = parameter;

    while (acceptable(parser, Token_Comma)) {
        advance(parser);
        parameter = parse_parameter_declaration(parser);
        if (!parameter) return NULL;
        insert_at_end_as_list_item(parameter_list, parameter);
    }

    if (list_size(parameter_list) == 1) {
        struct DeclarationSpecifier* first_specifiers =
            first_decl->declaration_specifiers;
        assert(first_specifiers->tag == DeclarationSpecifierTag_Type);
        if (type_as_void(first_specifiers->type))
            list_initialize(parameter_list);
    }

    return parameter_list;
}

static struct Declaration2* parse_parameter_declaration(struct Parser* parser) {
    struct DeclarationSpecifier* declaration_specifiers =
        parse_declaration_specifiers(parser);
    if (!declaration_specifiers) return NULL;

    struct Declarator* declarator = parse_declarator2(parser);
    assert(declaration_specifiers->tag == DeclarationSpecifierTag_Type);
    if (!type_as_void(declaration_specifiers->type) && !declarator) return NULL;

    struct InitDeclarator* init_declarator =
        new_init_declarator(declarator, NULL);
    struct List* init_declarator_list = malloc(sizeof(struct List));
    list_initialize(init_declarator_list);
    list_insert_at_end(init_declarator_list, list_from(init_declarator));

    struct Declaration2* declaration =
        new_declaration(declaration_specifiers, init_declarator_list);
    return declaration;
}

static struct TypeIr* parse_type_name(
    struct Parser* parser) {  // ToDo: fix to handle precisely
    struct TypeIr* type = parse_type_specifier(parser);
    if (acceptable(parser, Token_Asterisk)) {
        advance(parser);
        type = type_pointer_super(type_new_pointer(type));
    }
    return type;
}

/***** statements and blocks *****/

static struct StmtIr* parse_statement(struct Parser* parser);

static struct BlockStmtIr* parse_compound_statement(struct Parser* parser,
                                                    struct BlockStmtIr* block) {
    expect(parser, Token_LeftCurry);

    if (!block) block = ir_new_block_stmt();

    while (!acceptable(parser, Token_RightCurry)) {
        struct Declaration2* declaration = parse_declaration2(parser);
        if (declaration) {
            struct List* decl_list = normalize_declaration(declaration);
            for (struct ListHeader *it = list_begin(decl_list),
                                   *eit = list_end(decl_list);
                 it != eit; it = list_next(it)) {
                struct ListItem* list_item = (struct ListItem*)it;
                struct Declaration2* decl = list_item->item;
                insert_normalized_declaration_statement(decl, parser->context,
                                                        block);
            }
            continue;
        }
        struct StmtIr* stmt = parse_statement(parser);
        if (stmt) ir_block_stmt_insert_at_end(block, stmt);
    }
    expect(parser, Token_RightCurry);

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

static struct FunctionIr* parse_function_definition(struct Parser* parser) {
    struct DeclarationSpecifier* specifiers =
        parse_declaration_specifiers(parser);
    if (!specifiers) return NULL;
    struct Declarator* declarator = parse_declarator2(parser);
    if (!declarator) return NULL;

    assert(specifiers->tag == DeclarationSpecifierTag_Type);
    struct TypeIr* return_type = specifiers->type;

    struct DirectDeclarator* direct_declarator = declarator->direct_declarator;
    assert(direct_declarator->tag == DirectDeclaratorTag_Parameters);

    struct DirectDeclarator* inner_direct_declarator =
        direct_declarator->parameters.direct_declarator;
    assert(inner_direct_declarator->tag == DirectDeclaratorTag_Identifier);
    strtable_id name_index = inner_direct_declarator->identifier;

    struct List* param_decl_list = direct_declarator->parameters.list;
    struct List* param_types = malloc(sizeof(struct List));
    list_initialize(param_types);
    struct List* param_decl_stmt_list = malloc(sizeof(struct List));
    list_initialize(param_decl_stmt_list);
    struct BlockStmtIr* body = ir_new_block_stmt();
    for (struct ListHeader *it = list_begin(param_decl_list),
                           *eit = list_end(param_decl_list);
         it != eit; it = list_next(it)) {
        struct Declaration2* decl = ((struct ListItem*)it)->item;
        struct DeclStmtIr* decl_stmt = insert_normalized_declaration_statement(
            decl, parser->context, body);

        struct TypeIr* param_type = ir_decl_stmt_type(decl_stmt);
        insert_at_end_as_list_item(param_types, param_type);
        insert_at_end_as_list_item(param_decl_stmt_list, decl_stmt);
    }
    struct FunctionTypeIr* func_type =
        type_new_function(return_type, param_types);

    struct FunctionIr* function =
        context_find_function_declaration(parser->context, name_index);
    if (function) {
        // ToDo: check function type

        // prevent to insert dupulicate definitions
        assert(!ir_function_has_defined(function));
    } else {
        function = ir_new_function(name_index, func_type);
        context_insert_function_declaration(parser->context, name_index,
                                            function);
    }

    struct BlockStmtIr* block = parse_compound_statement(parser, NULL);
    ir_block_stmt_insert_at_end(body, ir_block_stmt_super(block));
    assert(!ir_function_has_defined(function));
    ir_function_define(function, param_decl_stmt_list, body);

    return function;
}

static void parse_external_declaration(struct Parser* parser) {
    struct ListHeader* saved_current_token = parser->current_token;

    struct Declaration2* declaration = parse_declaration2(parser);
    if (declaration) return;

    // do backtracking
    parser->current_token = saved_current_token;
    if (parse_function_definition(parser)) return;

    assert(false);
}

static void parse_translation_unit(struct Parser* parser) {
    expect(parser, Token_PseudoFileBegin);
    while (!acceptable(parser, Token_PseudoFileEnd)) {
        parse_external_declaration(parser);
    }
    expect(parser, Token_PseudoFileEnd);
}

void parser_run(struct Parser* parser) { parse_translation_unit(parser); }

struct Parser* parser_new(struct Context* context, struct List* tokens) {
    struct Parser* parser = malloc(sizeof(struct Parser));
    parser->context = context;
    parser->tokens = tokens;
    parser->current_token = list_begin(tokens);
    return parser;
}
