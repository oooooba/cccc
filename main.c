#include "ast.h"
#include "context.h"
#include "lexer.h"
#include "list.h"
#include "parser.h"
#include "token.h"

#include <stdbool.h>
#include <stdio.h>

void dump_apply(struct FundefNode* node, struct Context* context,
                FILE* output_stream);

struct RegallocVisitor;
struct RegallocVisitor* new_regalloc_visitor(struct Context* context);
void regalloc_apply(struct RegallocVisitor* visitor, struct FundefNode* node);

struct CodegenVisitor;
struct CodegenVisitor* new_codegen_visitor(struct Context* context,
                                           FILE* output_stream);
void codegen_apply(struct CodegenVisitor* visitor, struct FundefNode* node);

int main(void) {
    struct Context context;
    context_initialize(&context);
    context_register_registers(&context);

    struct List tokens;
    list_initialize(&tokens);

    struct Lexer lexer;
    lexer_initialize(&lexer, &context, &tokens, stdin);
    lexer_read_and_tokenize(&lexer);

    struct Parser parser;
    parser_initialize(&parser, &context, lexer.tokens);
    struct List* list = parser_parse(&parser);

    fprintf(stderr, "--------------------------------------------------\n");
    for (struct ListHeader *it = list_begin(list), *eit = list_end(list);
         it != eit; it = list_next(it)) {
        struct FundefNode* fundef = ((struct ListItem*)it)->item;
        fprintf(stderr, "[%p]\n", fundef);
        dump_apply(fundef, &context, stderr);
    }

    fprintf(stderr, "--------------------------------------------------\n");
    struct RegallocVisitor* regalloc_visitor = new_regalloc_visitor(&context);
    for (struct ListHeader *it = list_begin(list), *eit = list_end(list);
         it != eit; it = list_next(it)) {
        struct FundefNode* fundef = ((struct ListItem*)it)->item;
        fprintf(stderr, "[%p]\n", fundef);
        regalloc_apply(regalloc_visitor, fundef);
    }

    fprintf(stderr, "--------------------------------------------------\n");
    for (struct ListHeader *it = list_begin(list), *eit = list_end(list);
         it != eit; it = list_next(it)) {
        struct FundefNode* fundef = ((struct ListItem*)it)->item;
        fprintf(stderr, "[%p]\n", fundef);
        dump_apply(fundef, &context, stderr);
    }

    fprintf(stderr, "--------------------------------------------------\n");
    struct CodegenVisitor* codegen_visitor =
        new_codegen_visitor(&context, stdout);
    for (struct ListHeader *it = list_begin(list), *eit = list_end(list);
         it != eit; it = list_next(it)) {
        struct FundefNode* fundef = ((struct ListItem*)it)->item;
        fprintf(stderr, "[%p]\n", fundef);
        codegen_apply(codegen_visitor, fundef);
    }

    fprintf(stderr, "--------------------------------------------------\n");

    return 0;
}
