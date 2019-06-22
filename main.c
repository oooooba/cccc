#include "context.h"
#include "ir.h"
#include "lexer.h"
#include "list.h"
#include "parser.h"
#include "token.h"
#include "visitor.h"

#include <stdio.h>

struct DumpVisitor;
struct DumpVisitor* new_dump_visitor(struct Context* context, FILE* stream);

struct TypingVisitor;
struct TypingVisitor* new_typing_visitor(struct Context* context);

struct SimplifyVisitor;
struct SimplifyVisitor* new_simplify_visitor(struct Context* context);

struct RegallocVisitor;
struct RegallocVisitor* new_regalloc_visitor(struct Context* context);

struct FixupVisitor;
struct FixupVisitor* new_fixup_visitor(struct Context* context);

struct CodegenVisitor;
struct CodegenVisitor* new_codegen_visitor(struct Context* context,
                                           FILE* stream);

int main(void) {
    struct Context context;
    context_initialize(&context);
    context_register_registers(&context);

    struct List tokens;
    list_initialize(&tokens);

    struct Lexer lexer;
    lexer_initialize(&lexer, &context, &tokens, stdin);
    lexer_read_and_tokenize(&lexer);

    struct Parser* parser = parser_new(&context, lexer.tokens);
    parser_run(parser);

    fprintf(stderr, "[apply dump (1)]\n");
    struct DumpVisitor* dump_visitor = new_dump_visitor(&context, stderr);
    visitor_apply((struct Visitor*)dump_visitor);

    fprintf(stderr, "[apply typing]\n");
    struct TypingVisitor* typing_visitor = new_typing_visitor(&context);
    visitor_apply((struct Visitor*)typing_visitor);

    fprintf(stderr, "[apply simplify]\n");
    struct SimplifyVisitor* simplify_visitor = new_simplify_visitor(&context);
    visitor_apply((struct Visitor*)simplify_visitor);

    fprintf(stderr, "[apply dump (2)]\n");
    visitor_apply((struct Visitor*)dump_visitor);

    fprintf(stderr, "[apply regalloc]\n");
    struct RegallocVisitor* regalloc_visitor = new_regalloc_visitor(&context);
    visitor_apply((struct Visitor*)regalloc_visitor);

    fprintf(stderr, "[apply fixup]\n");
    struct FixupVisitor* fixup_visitor = new_fixup_visitor(&context);
    visitor_apply((struct Visitor*)fixup_visitor);

    fprintf(stderr, "[apply codegen]\n");
    struct CodegenVisitor* codegen_visitor =
        new_codegen_visitor(&context, stdout);
    visitor_apply((struct Visitor*)codegen_visitor);

    return 0;
}
