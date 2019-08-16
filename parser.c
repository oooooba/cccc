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
    struct Map* enumeration_constant_map;
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
    return acceptable(parser, Token_KeywordStatic) ||
           acceptable(parser, Token_KeywordTypedef);
}

static bool acceptable_type_specifier_token(struct Parser* parser,
                                            struct Token* token) {
    if (token->tag == Token_Id) {
        strtable_id id = token->strtable_index;
        return context_find_user_defined_type(parser->context, id);
    }
    return token->tag == Token_KeywordLong || token->tag == Token_KeywordInt ||
           token->tag == Token_KeywordChar || token->tag == Token_KeywordVoid ||
           token->tag == Token_KeywordEnum ||
           token->tag == Token_KeywordStruct ||
           token->tag == Token_KeywordUnion;
}

static bool acceptable_type_specifier(struct Parser* parser) {
    return acceptable_type_specifier_token(parser, peek(parser));
}

static void expect(struct Parser* parser, enum TokenTag expected) {
    if (!acceptable(parser, expected)) {
        struct Token* token = peek(parser);
        fprintf(stderr, "expected %d, actual %d (line = %zu, pos = %zu)\n",
                expected, peek(parser)->tag, token->line, token->position);
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

static void* nth_list_item(struct List* list, size_t n) {
    assert(list_size(list));
    for (struct ListHeader *it = list_begin(list), *eit = list_end(list);
         it != eit; it = list_next(it)) {
        if (n) {
            --n;
            continue;
        }
        struct ListItem* list_item = (struct ListItem*)it;
        return list_item->item;
    }
    assert(false);
    return NULL;
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
            bool is_variable_length;
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
    StorageClassSpecifierTag_Static,
    StorageClassSpecifierTag_Typedef,
};

enum DeclarationSpecifierTag {
    DeclarationSpecifierTag_StorageClass,
    DeclarationSpecifierTag_Type,
};

struct DeclarationSpecifier {
    enum DeclarationSpecifierTag tag;
    union {
        enum StorageClassSpecifierTag storage_class_tag;
        struct TypeIr* type;
    };
};

static struct DeclarationSpecifier* new_declaration_specifier(
    enum DeclarationSpecifierTag tag) {
    struct DeclarationSpecifier* specifier =
        malloc(sizeof(struct DeclarationSpecifier));
    *specifier = (struct DeclarationSpecifier){
        .tag = tag,
    };
    return specifier;
}

struct Declaration2 {
    struct List* declaration_specifiers;  // struct ListItem list, elem: struct
                                          // DeclarationSpecifier*
    struct List* init_declarator_list;    // struct ListItem list, elem: struct
                                          // InitDeclarator*
};

static struct Declaration2* new_declaration(struct List* declaration_specifiers,
                                            struct List* init_declarator_list) {
    assert(declaration_specifiers);

    struct Declaration2* declaration = malloc(sizeof(struct Declaration2));
    *declaration = (struct Declaration2){
        .declaration_specifiers = declaration_specifiers,
        .init_declarator_list = init_declarator_list,
    };
    return declaration;
}

struct StructDeclaration {
    struct TypeIr* type_specifier;
    struct Declarator* declarator;
};

static struct StructDeclaration* new_struct_declaration(
    struct TypeIr* type_specifier, struct Declarator* declarator) {
    struct StructDeclaration* declaration =
        malloc(sizeof(struct StructDeclaration));
    declaration->type_specifier = type_specifier;
    declaration->declarator = declarator;
    return declaration;
}

struct Res {
    strtable_id id;
    struct FunctionTypeIr* type;
    struct BlockStmtIr* block;
    struct List* decl_stmt_list;
    bool is_static;
};

static struct DeclStmtIr* insert_normalized_declaration_statement(
    struct Declaration2* declaration, struct Context* context,
    struct BlockStmtIr* block);

static void construct_function_type(struct Context* context,
                                    struct TypeIr* return_type,
                                    struct DirectDeclarator* direct_declarator,
                                    struct Res* res) {
    assert(direct_declarator->tag == DirectDeclaratorTag_Parameters);

    bool is_variable_length = direct_declarator->parameters.is_variable_length;
    struct List* param_decl_list = direct_declarator->parameters.list;
    struct List* param_types = malloc(sizeof(struct List));
    list_initialize(param_types);
    struct List* param_decl_stmt_list = malloc(sizeof(struct List));
    list_initialize(param_decl_stmt_list);
    struct BlockStmtIr* block = ir_new_block_stmt();
    for (struct ListHeader *it = list_begin(param_decl_list),
                           *eit = list_end(param_decl_list);
         it != eit; it = list_next(it)) {
        struct Declaration2* decl = ((struct ListItem*)it)->item;
        struct DeclStmtIr* decl_stmt =
            insert_normalized_declaration_statement(decl, context, block);

        struct TypeIr* param_type = ir_decl_stmt_type(decl_stmt);
        insert_at_end_as_list_item(param_types, param_type);
        insert_at_end_as_list_item(param_decl_stmt_list, decl_stmt);
    }
    res->type = type_new_function(return_type, param_types, is_variable_length);
    res->block = block;
    res->decl_stmt_list = param_decl_stmt_list;
}

static struct List* normalize_declaration(struct Declaration2* declaration) {
    struct List* decl_list = malloc(sizeof(struct List));
    list_initialize(decl_list);

    if (list_size(declaration->init_declarator_list) == 1) {
        insert_at_end_as_list_item(decl_list, declaration);
        return decl_list;
    }

    while (list_size(declaration->init_declarator_list)) {
        struct InitDeclarator* init_declarator =
            nth_list_item(declaration->init_declarator_list, 0);
        list_erase_at_begin(declaration->init_declarator_list);

        struct List* list = malloc(sizeof(struct List));
        list_initialize(list);
        insert_at_end_as_list_item(list, init_declarator);

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

    struct DeclarationSpecifier* specifier =
        nth_list_item(declaration->declaration_specifiers, 0);
    assert(specifier->tag == DeclarationSpecifierTag_Type);
    struct TypeIr* base_type = specifier->type;

    struct InitDeclarator* init_declarator =
        nth_list_item(declaration->init_declarator_list, 0);
    struct Declarator* declarator = init_declarator->declarator;
    struct DirectDeclarator* direct_declarator = declarator->direct_declarator;

    strtable_id name_index;
    struct TypeIr* type = base_type;
    switch (direct_declarator->tag) {
        case DirectDeclaratorTag_Identifier:
            name_index = direct_declarator->identifier;
            type = declarator->has_pointer
                       ? type_pointer_super(type_new_pointer(base_type))
                       : base_type;
            break;
        case DirectDeclaratorTag_Declarator:
            assert(false);
            break;
        case DirectDeclaratorTag_Parameters: {  // function pointer
            struct DirectDeclarator* inner_direct_declarator =
                direct_declarator->parameters.direct_declarator;
            assert(inner_direct_declarator->tag ==
                   DirectDeclaratorTag_Declarator);
            struct Declarator* inner_declarator =
                inner_direct_declarator->declarator;
            assert(inner_declarator->has_pointer);
            assert(inner_declarator->direct_declarator->tag ==
                   DirectDeclaratorTag_Identifier);

            name_index = inner_declarator->direct_declarator->identifier;
            struct Res res;
            construct_function_type(context, type, direct_declarator, &res);
            type = type_function_super(res.type);
            type = type_pointer_super(type_new_pointer(type));
            break;
        }
        default:
            assert(false);
    }

    struct VarExprIr* var = ir_new_var_expr(name_index);
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

static void handle_declaration(struct Declaration2* declaration,
                               struct BlockStmtIr* block,
                               struct Context* context) {
    assert(block);
    struct List* decl_list = normalize_declaration(declaration);
    for (struct ListHeader *it = list_begin(decl_list),
                           *eit = list_end(decl_list);
         it != eit; it = list_next(it)) {
        struct ListItem* list_item = (struct ListItem*)it;
        struct Declaration2* decl = list_item->item;
        insert_normalized_declaration_statement(decl, context, block);
    }
}

/***** prototypes *****/

static struct Declaration2* parse_declaration2(struct Parser* parser);
static struct List* parse_declaration_specifiers(struct Parser* parser);
static struct List* parse_init_declarator_list(struct Parser* parser);
static struct InitDeclarator* parse_init_declarator(struct Parser* parser);
static enum StorageClassSpecifierTag parse_storage_class_specifier(
    struct Parser* parser);
static struct TypeIr* parse_type_specifier(struct Parser* parser);
static struct TypeIr* parse_enum_specifier(struct Parser* parser);
static struct TypeIr* parse_struct_or_union_specifier(struct Parser* parser);
static struct StructDeclaration* parse_struct_declaration(
    struct Parser* parser);
static struct Declarator* parse_declarator2(struct Parser* parser);
static struct DirectDeclarator* parse_direct_declarator(struct Parser* parser);
static struct List* parse_parameter_type_list(struct Parser* parser,
                                              bool* is_variable_length);
static struct List* parse_parameter_list(struct Parser* parser);
static struct Declaration2* parse_parameter_declaration(struct Parser* parser);
static struct TypeIr* parse_type_name(struct Parser* parser);

static struct Res* parse_function_declaration(
    struct Context* context, struct List* declaration_specifier_list,
    struct Declarator* declarator);

/***** lexical elements *****/

static struct ExprIr* parse_integer_constant(struct Parser* parser) {
    assert(acceptable(parser, Token_Integer));
    struct Token* token = peek(parser);
    struct ConstExprIr* ir = ir_new_integer_const_expr(token->integer);
    advance(parser);
    return ir_const_expr_cast(ir);
}

static struct ExprIr* parse_string_literal(struct Parser* parser) {
    assert(acceptable(parser, Token_String));
    struct Token* token = peek(parser);
    struct ConstExprIr* ir = ir_new_string_const_expr(token->strtable_index);
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
    if (acceptable(parser, Token_Integer))
        return parse_integer_constant(parser);
    else if (acceptable(parser, Token_String))
        return parse_string_literal(parser);
    else
        assert(false);
}

/***** expressions *****/

static struct ExprIr* parse_expression(struct Parser* parser);

static struct ExprIr* parse_primary_expression(struct Parser* parser) {
    switch (peek(parser)->tag) {
        case Token_Integer:
        case Token_String:
            return parse_constant(parser);
        case Token_Id: {
            strtable_id index = parse_identifier(parser);
            intptr_t value = (intptr_t)map_find(
                parser->enumeration_constant_map, (void*)index);
            if (value) {
                --value;
                return ir_const_expr_cast(ir_new_integer_const_expr(value));
            }
            struct VarExprIr* var = ir_new_var_expr(index);
            struct DerefExprIr* deref_var =
                ir_new_deref_expr(ir_var_expr_cast(var));
            return ir_deref_expr_cast(deref_var);
        } break;
        case Token_LeftParen: {
            advance(parser);
            struct ExprIr* expr = parse_expression(parser);
            expect(parser, Token_RightParen);
            return expr;
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
        } else if (acceptable(parser, Token_Arrow)) {
            advance(parser);
            strtable_id name_index = parse_identifier(parser);
            struct DerefExprIr* inner_deref = ir_new_deref_expr(expr);
            struct AddrofExprIr* base =
                ir_new_addrof_expr(ir_deref_expr_cast(inner_deref));
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
    } else if (acceptable(parser, Token_Minus)) {
        advance(parser);
        struct ExprIr* operand = parse_cast_expression(parser);
        return ir_unop_expr_cast(ir_new_unop_expr(UnopExprIrTag_Neg, operand));
    } else if (acceptable(parser, Token_Exclamation)) {
        advance(parser);
        struct ExprIr* operand = parse_cast_expression(parser);
        return ir_unop_expr_cast(ir_new_unop_expr(UnopExprIrTag_Not, operand));
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
    } else if (acceptable(parser, Token_PlusPlus)) {
        advance(parser);
        struct ExprIr* operand = parse_unary_expression(parser);
        struct ExprIr* dst = ir_expr_clone(operand);
        struct AddrofExprIr* dst_addr = ir_new_addrof_expr(dst);
        struct BinopExprIr* add =
            ir_new_binop_expr(BinopExprIrTag_Add, operand,
                              ir_const_expr_cast(ir_new_integer_const_expr(1)));
        struct SubstExprIr* subst = ir_new_subst_expr(
            ir_addrof_expr_cast(dst_addr), ir_binop_expr_cast(add));
        return ir_subst_expr_cast(subst);
    } else if (acceptable(parser, Token_MinusMinus)) {
        advance(parser);
        struct ExprIr* operand = parse_unary_expression(parser);
        struct ExprIr* dst = ir_expr_clone(operand);
        struct AddrofExprIr* dst_addr = ir_new_addrof_expr(dst);
        struct BinopExprIr* sub =
            ir_new_binop_expr(BinopExprIrTag_Sub, operand,
                              ir_const_expr_cast(ir_new_integer_const_expr(1)));
        struct SubstExprIr* subst = ir_new_subst_expr(
            ir_addrof_expr_cast(dst_addr), ir_binop_expr_cast(sub));
        return ir_subst_expr_cast(subst);
    } else
        return parse_postfix_expression(parser);
}

static struct ExprIr* parse_cast_expression(struct Parser* parser) {
    if (!(acceptable(parser, Token_LeftParen) &&
          acceptable_type_specifier_token(parser, peek_k(parser, 1))))
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

static struct ExprIr* parse_relational_expression(struct Parser* parser) {
    struct ExprIr* lhs = parse_additive_expression(parser);
    for (;;) {
        enum BinopExprIrTag op;
        if (acceptable(parser, Token_LeftAngle))
            op = BinopExprIrTag_Lt;
        else if (acceptable(parser, Token_LeftAngleEqual))
            op = BinopExprIrTag_Le;
        else if (acceptable(parser, Token_RightAngle))
            op = BinopExprIrTag_Gt;
        else if (acceptable(parser, Token_RightAngleEqual))
            op = BinopExprIrTag_Ge;
        else
            break;
        advance(parser);
        struct ExprIr* rhs = parse_additive_expression(parser);
        lhs = ir_binop_expr_cast(ir_new_binop_expr(op, lhs, rhs));
    }
    return lhs;
}

static struct ExprIr* parse_equality_expression(struct Parser* parser) {
    struct ExprIr* lhs = parse_relational_expression(parser);
    for (;;) {
        if (acceptable(parser, Token_EqualEqual)) {
            advance(parser);
            struct ExprIr* rhs = parse_relational_expression(parser);
            lhs = ir_binop_expr_cast(
                ir_new_binop_expr(BinopExprIrTag_Equal, lhs, rhs));
        } else if (acceptable(parser, Token_ExclamationEqual)) {
            advance(parser);
            struct ExprIr* rhs = parse_relational_expression(parser);
            lhs = ir_binop_expr_cast(
                ir_new_binop_expr(BinopExprIrTag_Equal, lhs, rhs));
            lhs = ir_unop_expr_cast(ir_new_unop_expr(UnopExprIrTag_Not, lhs));
        } else
            break;
    }
    return lhs;
}

static struct ExprIr* parse_logical_and_expression(struct Parser* parser) {
    struct ExprIr* lhs = parse_equality_expression(parser);
    while (acceptable(parser, Token_AmpersandAmpersand)) {
        advance(parser);
        struct ExprIr* rhs = parse_equality_expression(parser);
        lhs = ir_binop_expr_cast(
            ir_new_binop_expr(BinopExprIrTag_LogicalAnd, lhs, rhs));
    }
    return lhs;
}

static struct ExprIr* parse_logical_or_expression(struct Parser* parser) {
    struct ExprIr* lhs = parse_logical_and_expression(parser);
    while (acceptable(parser, Token_PipePipe)) {
        advance(parser);
        struct ExprIr* rhs = parse_logical_and_expression(parser);
        lhs = ir_binop_expr_cast(
            ir_new_binop_expr(BinopExprIrTag_LogicalOr, lhs, rhs));
    }
    return lhs;
}

static struct ExprIr* parse_conditional_expression(struct Parser* parser) {
    struct ExprIr* cond = parse_logical_or_expression(parser);
    if (!acceptable(parser, Token_Question)) return cond;
    advance(parser);
    struct ExprIr* true_expr = parse_expression(parser);
    expect(parser, Token_Colon);
    struct ExprIr* false_expr = parse_conditional_expression(parser);
    return ir_cond_expr_cast(ir_new_cond_expr(cond, true_expr, false_expr));
}

static struct ExprIr* parse_assignment_expression(struct Parser* parser) {
    struct ExprIr* lhs = parse_conditional_expression(parser);
    if (acceptable(parser, Token_Equal)) {
        advance(parser);
        struct AddrofExprIr* addrof = ir_new_addrof_expr(lhs);
        struct ExprIr* rhs = parse_assignment_expression(parser);
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
    struct List* declaration_specifiers = parse_declaration_specifiers(parser);
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

static struct List* parse_declaration_specifiers(struct Parser* parser) {
    struct List* specifiers = malloc(sizeof(struct List));
    list_initialize(specifiers);
    while (true) {
        struct DeclarationSpecifier* specifier = NULL;
        if (acceptable_type_specifier(parser)) {
            struct TypeIr* type = parse_type_specifier(parser);
            if (!type) return NULL;
            specifier = new_declaration_specifier(DeclarationSpecifierTag_Type);
            specifier->type = type;
        } else if (acceptable_storage_class_specifier(parser)) {
            enum StorageClassSpecifierTag tag =
                parse_storage_class_specifier(parser);
            specifier =
                new_declaration_specifier(DeclarationSpecifierTag_StorageClass);
            specifier->storage_class_tag = tag;
        } else
            break;
        insert_at_end_as_list_item(specifiers, specifier);
    }
    return list_size(specifiers) ? specifiers : NULL;
}

static struct List* parse_init_declarator_list(struct Parser* parser) {
    struct InitDeclarator* init_declarator = parse_init_declarator(parser);
    if (!init_declarator) return NULL;

    struct List* init_declarator_list = malloc(sizeof(struct List));
    list_initialize(init_declarator_list);
    insert_at_end_as_list_item(init_declarator_list, init_declarator);

    while (acceptable(parser, Token_Comma)) {
        advance(parser);
        init_declarator = parse_init_declarator(parser);
        if (!init_declarator) return NULL;
        insert_at_end_as_list_item(init_declarator_list, init_declarator);
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
        case Token_KeywordStatic:
            tag = StorageClassSpecifierTag_Static;
            break;
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
        case Token_KeywordEnum:
            return parse_enum_specifier(parser);
        case Token_KeywordStruct:
        case Token_KeywordUnion:
            return parse_struct_or_union_specifier(parser);
        case Token_Id:
            type = context_find_user_defined_type(parser->context,
                                                  peek(parser)->strtable_index);
            break;
        default:
            assert(false);
    }
    advance(parser);
    return type;
}

static struct TypeIr* parse_enum_specifier(struct Parser* parser) {
    expect(parser, Token_KeywordEnum);

    strtable_id name_index = parse_identifier(parser);
    struct TypeIr* type =
        context_find_user_defined_type(parser->context, name_index);
    if (type) {
        assert(!acceptable(parser, Token_LeftCurry));
        return type;
    }

    type = type_int_super(type_new_int());
    context_insert_user_defined_type(parser->context, name_index, type);

    if (!acceptable(parser, Token_LeftCurry)) return type;

    advance(parser);
    intptr_t enumeration_constant_value = 0;
    while (!acceptable(parser, Token_RightCurry)) {
        strtable_id enumeration_constant = parse_identifier(parser);
        map_insert(parser->enumeration_constant_map,
                   (void*)enumeration_constant,
                   (void*)(enumeration_constant_value + 1));
        ++enumeration_constant_value;
        expect(parser, Token_Comma);
    }
    advance(parser);

    return type;
}

static struct TypeIr* parse_struct_or_union_specifier(struct Parser* parser) {
    assert(acceptable(parser, Token_KeywordStruct) ||
           acceptable(parser, Token_KeywordUnion));
    bool is_union = acceptable(parser, Token_KeywordUnion);
    advance(parser);

    struct TypeIr* type = NULL;
    if (acceptable(parser, Token_Id)) {
        strtable_id name_index = parse_identifier(parser);
        type = context_find_user_defined_type(parser->context, name_index);
        if (type) {
            struct StructTypeIr* struct_type = type_as_struct(type);
            assert(struct_type);
            assert(is_union == type_struct_is_union(struct_type));
        } else {
            type =
                type_struct_super(type_new_struct(name_index, is_union, NULL));
            context_insert_user_defined_type(parser->context, name_index, type);
        }
    } else
        type = type_struct_super(
            type_new_struct(STRTABLE_INVALID_ID, is_union, NULL));
    assert(type);

    if (acceptable(parser, Token_LeftCurry)) {
        advance(parser);

        struct List* elem_types = malloc(sizeof(struct List));
        list_initialize(elem_types);

        while (!acceptable(parser,
                           Token_RightCurry)) {  // ToDo: fix to parse precisely
            struct StructDeclaration* declaration =
                parse_struct_declaration(parser);

            struct TypeIr* type = declaration->type_specifier;
            strtable_id member_index = STRTABLE_INVALID_ID;
            if (declaration->declarator) {  // NULL for anonymous struct
                if (declaration->declarator->has_pointer)
                    type = type_pointer_super(type_new_pointer(type));
                struct DirectDeclarator* direct_declarator =
                    declaration->declarator->direct_declarator;
                switch (direct_declarator->tag) {
                    case DirectDeclaratorTag_Identifier:
                        member_index = direct_declarator->identifier;
                        break;
                    case DirectDeclaratorTag_Parameters: {
                        struct DeclarationSpecifier* specifier =
                            new_declaration_specifier(
                                DeclarationSpecifierTag_Type);
                        specifier->type = type;
                        struct List* specifiers = malloc(sizeof(struct List));
                        list_initialize(specifiers);
                        insert_at_end_as_list_item(specifiers, specifier);

                        struct Res* res = parse_function_declaration(
                            parser->context, specifiers,
                            declaration->declarator);

                        member_index = res->id;
                        type = type_function_super(res->type);
                    } break;
                    default:
                        assert(false);
                }
            }

            struct MemberEntry* entry =
                type_new_member_entry(member_index, type);
            list_insert_at_end(elem_types,
                               type_member_entry_as_list_header(entry));
        }
        expect(parser, Token_RightCurry);

        type_struct_set_elem_types(type_as_struct(type), elem_types);
    }

    return type;
}

static struct StructDeclaration* parse_struct_declaration(
    struct Parser* parser) {
    assert(acceptable_type_specifier(parser));
    struct TypeIr* type_specifier = parse_type_specifier(parser);
    struct Declarator* declarator = NULL;
    if (!acceptable(parser, Token_Semicolon)) {
        declarator = parse_declarator2(parser);
        assert(declarator);
    }
    expect(parser, Token_Semicolon);
    return new_struct_declaration(type_specifier, declarator);
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
            bool is_variable_length;
            struct List* parameter_type_list =
                parse_parameter_type_list(parser, &is_variable_length);
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
            direct_declarator->parameters.is_variable_length =
                is_variable_length;
        } else
            break;
    }
    return direct_declarator;
}

static struct List* parse_parameter_type_list(struct Parser* parser,
                                              bool* is_variable_length) {
    struct List* parameter_list = parse_parameter_list(parser);
    *is_variable_length = false;
    if (!acceptable(parser, Token_Comma)) return parameter_list;
    advance(parser);
    expect(parser, Token_DotDotDot);
    *is_variable_length = true;
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
        if (peek_k(parser, 1)->tag == Token_DotDotDot) break;
        advance(parser);
        parameter = parse_parameter_declaration(parser);
        if (!parameter) return NULL;
        insert_at_end_as_list_item(parameter_list, parameter);
    }

    if (list_size(parameter_list) == 1) {
        struct DeclarationSpecifier* first_specifiers =
            nth_list_item(first_decl->declaration_specifiers, 0);
        assert(first_specifiers->tag == DeclarationSpecifierTag_Type);
        if (type_as_void(first_specifiers->type)) {
            struct InitDeclarator* first_init =
                nth_list_item(first_decl->init_declarator_list, 0);
            struct Declarator* first_declarator = first_init->declarator;
            if (first_init->declarator)
                assert(first_declarator->has_pointer);
            else
                list_initialize(parameter_list);
        }
    }

    return parameter_list;
}

static struct Declaration2* parse_parameter_declaration(struct Parser* parser) {
    struct List* declaration_specifiers = parse_declaration_specifiers(parser);
    if (!declaration_specifiers) return NULL;

    struct Declarator* declarator = parse_declarator2(parser);
    struct DeclarationSpecifier* specifier =
        nth_list_item(declaration_specifiers, 0);
    assert(specifier->tag == DeclarationSpecifierTag_Type);
    if (!type_as_void(specifier->type) && !declarator) return NULL;

    struct InitDeclarator* init_declarator =
        new_init_declarator(declarator, NULL);
    struct List* init_declarator_list = malloc(sizeof(struct List));
    list_initialize(init_declarator_list);
    insert_at_end_as_list_item(init_declarator_list, init_declarator);

    struct Declaration2* declaration =
        new_declaration(declaration_specifiers, init_declarator_list);
    return declaration;
}

static struct TypeIr* parse_type_name(
    struct Parser* parser) {  // ToDo: fix to handle precisely
    struct TypeIr* type = parse_type_specifier(parser);
    while (acceptable(parser, Token_Asterisk)) {
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
            handle_declaration(declaration, block, parser->context);
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

static struct SwitchStmtIr* parse_switch_statement(struct Parser* parser) {
    expect(parser, Token_KeywordSwitch);

    expect(parser, Token_LeftParen);
    struct ExprIr* cond_expr = parse_expression(parser);
    expect(parser, Token_RightParen);

    expect(parser, Token_LeftCurry);
    struct List* branches = malloc(sizeof(struct List));
    list_initialize(branches);
    struct StmtIr* default_stmt = NULL;
    while (!acceptable(parser, Token_RightCurry)) {
        bool is_default_label = false;
        intptr_t case_value;
        if (acceptable(parser, Token_KeywordCase)) {
            advance(parser);
            struct ConstExprIr* const_expr = NULL;
            if (acceptable(parser, Token_Integer)) {
                struct ExprIr* expr = parse_integer_constant(parser);
                const_expr = ir_expr_as_const(expr);
            } else if (acceptable(parser, Token_Id)) {
                strtable_id enumeration_constant = parse_identifier(parser);
                intptr_t value =
                    (intptr_t)map_find(parser->enumeration_constant_map,
                                       (void*)enumeration_constant);
                assert(value);
                --value;
                const_expr = ir_new_integer_const_expr(value);
            } else
                assert(false);
            assert(const_expr);
            case_value = ir_const_expr_integer_value(const_expr);
            expect(parser, Token_Colon);
        } else if (acceptable(parser, Token_KeywordDefault)) {
            assert(!default_stmt);
            advance(parser);
            expect(parser, Token_Colon);
            is_default_label = true;
        } else
            assert(false);

        struct BlockStmtIr* block = ir_new_block_stmt();
        while (!(acceptable(parser, Token_KeywordCase) ||
                 acceptable(parser, Token_KeywordDefault) ||
                 acceptable(parser, Token_RightCurry))) {
            struct StmtIr* stmt = parse_statement(parser);
            if (stmt) ir_block_stmt_insert_at_end(block, stmt);
        }

        if (is_default_label) {
            default_stmt = ir_block_stmt_super(block);
        } else {
            struct SwitchStmtBranch* branch =
                ir_new_switch_branch(case_value, ir_block_stmt_super(block));
            struct ListItem* item = malloc(sizeof(struct ListItem));
            item->item = branch;
            list_insert_at_end(branches, list_from(item));
        }
    }
    advance(parser);

    return ir_new_switch_stmt(cond_expr, branches, default_stmt);
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
        } else {
            false_stmt = ir_block_stmt_super(ir_new_block_stmt());
        }

        return ir_if_stmt_super(
            ir_new_if_stmt(cond_expr, true_stmt, false_stmt));
    } else if (acceptable(parser, Token_KeywordSwitch)) {
        return ir_switch_stmt_super(parse_switch_statement(parser));
    }
    assert(false);
}

static struct StmtIr* parse_iteration_statement(struct Parser* parser) {
    if (acceptable(parser, Token_KeywordWhile)) {
        advance(parser);

        expect(parser, Token_LeftParen);
        struct ExprIr* cond_expr = parse_expression(parser);
        expect(parser, Token_RightParen);

        struct StmtIr* body_stmt = parse_statement(parser);

        return ir_while_stmt_super(
            ir_new_while_stmt(cond_expr, NULL, body_stmt));
    } else if (acceptable(parser, Token_KeywordFor)) {
        advance(parser);

        expect(parser, Token_LeftParen);
        struct ListHeader* saved_current_token = parser->current_token;

        struct ExprIr* head_expr = NULL;
        struct Declaration2* head_decl = NULL;
        if (!acceptable(parser, Token_Semicolon)) {
            head_decl = parse_declaration2(parser);
            if (!head_decl) {
                parser->current_token = saved_current_token;
                head_expr = parse_expression(parser);
                assert(head_expr);
            }
        }
        assert(!(head_expr && head_decl));
        if (!head_decl) expect(parser, Token_Semicolon);

        struct BlockStmtIr* block = NULL;
        if (head_decl) {
            block = ir_new_block_stmt();
            handle_declaration(head_decl, block, parser->context);
        } else if (head_expr) {
            block = ir_new_block_stmt();
            struct ExprStmtIr* expr_stmt = ir_new_expr_stmt(head_expr);
            ir_block_stmt_insert_at_end(block, ir_expr_stmt_super(expr_stmt));
        }

        struct ExprIr* cond_expr = NULL;
        if (!acceptable(parser, Token_Semicolon))
            cond_expr = parse_expression(parser);
        expect(parser, Token_Semicolon);
        if (!cond_expr)
            cond_expr = ir_const_expr_cast(ir_new_integer_const_expr(1));

        struct ExprIr* tail_expr = NULL;
        if (!acceptable(parser, Token_RightParen))
            tail_expr = parse_expression(parser);
        expect(parser, Token_RightParen);

        struct StmtIr* body_stmt = parse_statement(parser);

        struct WhileStmtIr* stmt =
            ir_new_while_stmt(cond_expr, tail_expr, body_stmt);
        if (block) {
            ir_block_stmt_insert_at_end(block, ir_while_stmt_super(stmt));
            return ir_block_stmt_super(block);
        } else
            return ir_while_stmt_super(stmt);
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
    } else if (acceptable(parser, Token_KeywordBreak)) {
        advance(parser);
        expect(parser, Token_Semicolon);
        return ir_break_stmt_super(ir_new_break_stmt());
    } else if (acceptable(parser, Token_KeywordContinue)) {
        advance(parser);
        expect(parser, Token_Semicolon);
        return ir_continue_stmt_super(ir_new_continue_stmt());
    }
    assert(false);
}

static struct StmtIr* parse_statement(struct Parser* parser) {
    if (acceptable(parser, Token_LeftCurry))
        return ir_block_stmt_super(parse_compound_statement(parser, NULL));
    else if (acceptable(parser, Token_KeywordIf) ||
             acceptable(parser, Token_KeywordSwitch))
        return parse_selection_statement(parser);
    else if (acceptable(parser, Token_KeywordWhile) ||
             acceptable(parser, Token_KeywordFor))
        return parse_iteration_statement(parser);
    else if (acceptable(parser, Token_KeywordReturn) ||
             acceptable(parser, Token_KeywordBreak) ||
             acceptable(parser, Token_KeywordContinue))
        return parse_jump_statement(parser);
    else if (acceptable(parser, Token_Semicolon)) {
        advance(parser);
        return NULL;
    } else
        return ir_expr_stmt_super(
            ir_new_expr_stmt(parse_expression_statement(parser)));
}

/***** external definitions *****/

static struct Res* parse_function_declaration(
    struct Context* context, struct List* declaration_specifier_list,
    struct Declarator* declarator) {
    struct Res* res = malloc(sizeof(struct Res));

    struct DeclarationSpecifier* specifier =
        nth_list_item(declaration_specifier_list, 0);
    if (specifier->tag == DeclarationSpecifierTag_StorageClass) {
        res->is_static =
            specifier->storage_class_tag == StorageClassSpecifierTag_Static;
        specifier = nth_list_item(declaration_specifier_list, 1);
    }
    struct TypeIr* return_type = specifier->type;
    if (declarator->has_pointer)
        return_type = type_pointer_super(type_new_pointer(return_type));

    struct DirectDeclarator* direct_declarator = declarator->direct_declarator;
    assert(direct_declarator->tag == DirectDeclaratorTag_Parameters);

    struct DirectDeclarator* inner_direct_declarator =
        direct_declarator->parameters.direct_declarator;
    strtable_id name_index;
    switch (inner_direct_declarator->tag) {
        case DirectDeclaratorTag_Identifier:
            name_index = inner_direct_declarator->identifier;
            break;
        case DirectDeclaratorTag_Declarator: {
            struct Declarator* inner_declarator =
                inner_direct_declarator->declarator;
            assert(inner_declarator->has_pointer);
            assert(inner_declarator->direct_declarator->tag ==
                   DirectDeclaratorTag_Identifier);
            name_index = inner_declarator->direct_declarator->identifier;
        } break;
        default:
            assert(false);
    }

    construct_function_type(context, return_type, direct_declarator, res);
    res->id = name_index;
    return res;
}

static struct FunctionIr* parse_function_definition(struct Parser* parser) {
    struct List* specifiers = parse_declaration_specifiers(parser);
    if (!specifiers) return NULL;
    struct Declarator* declarator = parse_declarator2(parser);
    if (!declarator) return NULL;

    struct Res* result =
        parse_function_declaration(parser->context, specifiers, declarator);
    strtable_id name_index = result->id;
    struct FunctionTypeIr* func_type = result->type;
    struct BlockStmtIr* body = result->block;
    struct List* param_decl_stmt_list = result->decl_stmt_list;

    struct FunctionIr* function =
        context_find_function_definition(parser->context, name_index);
    if (function) {
        // ToDo: check function type

        // prevent to insert dupulicate definitions
        assert(!ir_function_has_defined(function));
    } else {
        function = ir_new_function(name_index, func_type);
        context_insert_function_definition(parser->context, name_index,
                                           function);
    }

    struct BlockStmtIr* block = parse_compound_statement(parser, NULL);
    ir_block_stmt_insert_at_end(body, ir_block_stmt_super(block));
    assert(!ir_function_has_defined(function));
    ir_function_define(function, param_decl_stmt_list, body);

    struct GlobalIr* global =
        ir_new_global_from_function(function, true, !result->is_static);
    context_append_global_declaration(parser->context, global);

    return function;
}

static void parse_external_declaration(struct Parser* parser) {
    struct ListHeader* saved_current_token = parser->current_token;

    struct Declaration2* declaration = parse_declaration2(parser);
    if (declaration) {
        if (!declaration->init_declarator_list) return;

        struct InitDeclarator* init_decl =
            nth_list_item(declaration->init_declarator_list, 0);
        struct Declarator* declarator = init_decl->declarator;
        struct DirectDeclarator* direct_declarator =
            declarator->direct_declarator;

        struct DeclarationSpecifier* first_specifier =
            nth_list_item(declaration->declaration_specifiers, 0);
        if (first_specifier->tag == DeclarationSpecifierTag_StorageClass) {
            if (first_specifier->storage_class_tag ==
                StorageClassSpecifierTag_Typedef) {
                assert(direct_declarator->tag ==
                       DirectDeclaratorTag_Identifier);
                strtable_id id = direct_declarator->identifier;

                struct DeclarationSpecifier* second_specifier =
                    nth_list_item(declaration->declaration_specifiers, 1);
                assert(second_specifier->tag == DeclarationSpecifierTag_Type);
                struct TypeIr* type = second_specifier->type;
                context_insert_user_defined_type(parser->context, id, type);
                return;
            } else {
                assert(first_specifier->storage_class_tag ==
                       StorageClassSpecifierTag_Static);
                first_specifier =
                    nth_list_item(declaration->declaration_specifiers, 1);
            }
        }

        if (direct_declarator->tag != DirectDeclaratorTag_Parameters) return;

        struct List* declaration_specifiers =
            declaration->declaration_specifiers;
        struct Res* result = parse_function_declaration(
            parser->context, declaration_specifiers, declarator);
        struct FunctionIr* function = ir_new_function(result->id, result->type);
        struct GlobalIr* global =
            ir_new_global_from_function(function, false, !result->is_static);
        context_append_global_declaration(parser->context, global);
        return;
    }

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
    parser->enumeration_constant_map = malloc(sizeof(struct Map));
    map_initialize(parser->enumeration_constant_map);
    return parser;
}
