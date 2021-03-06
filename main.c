#include "context.h"
#include "ir.h"
#include "lexer.h"
#include "list.h"
#include "parser.h"
#include "token.h"
#include "visitor.h"

#include <assert.h>
#include <stdio.h>

struct DumpVisitor;
struct DumpVisitor* new_dump_visitor(struct Context* context);

struct NameresolveVisitor;
struct NameresolveVisitor* new_nameresolve_visitor(struct Context* context);

struct TypingVisitor;
struct TypingVisitor* new_typing_visitor(struct Context* context);

struct SimplifyVisitor;
struct SimplifyVisitor* new_simplify_visitor(struct Context* context);

struct RegallocVisitor;
struct RegallocVisitor* new_regalloc_visitor(struct Context* context);

struct FixupVisitor;
struct FixupVisitor* new_fixup_visitor(struct Context* context);

struct CodegenVisitor;
struct CodegenVisitor* new_codegen_visitor(struct Context* context);

int main(void) {
    FILE* stdin = fopen("/dev/stdin", "r");
    assert(stdin);
    FILE* stdout = fopen("/dev/stdout", "w");
    assert(stdout);
    FILE* stderr = fopen("/dev/stderr", "w");
    assert(stderr);

    struct Context context;
    context_initialize(&context, stdin, stdout, stderr);
    context_register_registers(&context);

    struct List tokens;
    list_initialize(&tokens);

    fprintf(stderr, "[lexer]\n");
    struct Lexer lexer;
    lexer_initialize(&lexer, &context, &tokens);
    lexer_read_and_tokenize(&lexer);

    fprintf(stderr, "[parser]\n");
    struct Parser* parser = parser_new(&context, lexer.tokens);
    parser_run(parser);

    fprintf(stderr, "[apply dump (1)]\n");
    struct DumpVisitor* dump_visitor = new_dump_visitor(&context);
    visitor_apply((struct Visitor*)dump_visitor);

    fprintf(stderr, "[apply nameresolve]\n");
    struct NameresolveVisitor* nameresolve_visitor =
        new_nameresolve_visitor(&context);
    visitor_apply((struct Visitor*)nameresolve_visitor);

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
    struct CodegenVisitor* codegen_visitor = new_codegen_visitor(&context);
    visitor_apply((struct Visitor*)codegen_visitor);

    fclose(stdin);
    fclose(stdout);
    fclose(stderr);

    return 0;
}
