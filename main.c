#include "context.h"
#include "ir.h"
#include "lexer.h"
#include "list.h"
#include "parser.h"
#include "token.h"

#include <stdio.h>

struct DumpVisitor2;
struct DumpVisitor2* new_dump_visitor2(struct Context* context, FILE* stream);
void dump2_apply(struct DumpVisitor2* visitor, struct BlockIr* ir);

struct SimplifyVisitor2;
struct SimplifyVisitor2* new_simplify_visitor2(struct Context* context);
void simplify2_apply(struct SimplifyVisitor2* visitor, struct BlockIr* ir);

struct RegallocVisitor2;
void regalloc2_apply(struct RegallocVisitor2* visitor, struct BlockIr* ir);
struct RegallocVisitor2* new_regalloc_visitor2(struct Context* context);

struct PostRegallocVisitor2;
struct PostRegallocVisitor2* new_post_regalloc_visitor2(
    struct Context* context);
void regalloc2_apply_post_process(struct PostRegallocVisitor2* visitor,
                                  struct BlockIr* ir);

struct CodegenVisitor2;
struct CodegenVisitor2* new_codegen_visitor2(struct Context* context,
                                             FILE* stream);
void codegen2_apply(struct CodegenVisitor2* visitor, struct BlockIr* ir);

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

    struct BlockIr* translation_unit = parser_run(&parser);

    fprintf(stderr, "[apply dump (1)]\n");
    struct DumpVisitor2* dump_visitor = new_dump_visitor2(&context, stderr);
    dump2_apply(dump_visitor, translation_unit);

    fprintf(stderr, "[apply simplify]\n");
    struct SimplifyVisitor2* simplify_visitor = new_simplify_visitor2(&context);
    simplify2_apply(simplify_visitor, translation_unit);

    fprintf(stderr, "[apply dump (2)]\n");
    dump2_apply(dump_visitor, translation_unit);

    fprintf(stderr, "[apply regalloc]\n");
    struct RegallocVisitor2* regalloc_visitor = new_regalloc_visitor2(&context);
    regalloc2_apply(regalloc_visitor, translation_unit);

    fprintf(stderr, "[apply post regalloc]\n");
    struct PostRegallocVisitor2* post_regalloc_visitor =
        new_post_regalloc_visitor2(&context);
    regalloc2_apply_post_process(post_regalloc_visitor, translation_unit);

    fprintf(stderr, "[apply codegen]\n");
    struct CodegenVisitor2* codegen_visitor =
        new_codegen_visitor2(&context, stdout);
    codegen2_apply(codegen_visitor, translation_unit);

    return 0;
}
