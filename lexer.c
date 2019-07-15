#include "lexer.h"
#include "context.h"
#include "list.h"
#include "token.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LEXER_INPUT_STREAM_BUFFER_SIZE 128

struct ReservedKeywordsEntry {
    const char* keyword;
    enum TokenTag token_tag;
};

static const struct ReservedKeywordsEntry reserved_keywords[] = {
#define register_keyword(symbol, tag) \
    { .keyword = #symbol, .token_tag = Token_Keyword##tag }

    register_keyword(const, Const),     register_keyword(char, Char),
    register_keyword(else, Else),       register_keyword(if, If),
    register_keyword(int, Int),         register_keyword(long, Long),
    register_keyword(return, Return),   register_keyword(sizeof, Sizeof),
    register_keyword(static, Static),   register_keyword(struct, Struct),
    register_keyword(typedef, Typedef), register_keyword(void, Void),

#undef register_keyword
};

static enum TokenTag find_token_tag(const char* str) {
    for (size_t i = 0;
         i < sizeof(reserved_keywords) / sizeof(struct ReservedKeywordsEntry);
         ++i) {
        if (strcmp(str, reserved_keywords[i].keyword) == 0)
            return reserved_keywords[i].token_tag;
    }
    return Token_Id;
}

static struct Token* new_token(enum TokenTag tag, size_t line, size_t pos) {
    struct Token* token = malloc(sizeof(struct Token));
    token->tag = tag;
    token->line = line;
    token->position = pos;
    return token;
}

static char peek_k(struct Lexer* lexer, size_t k) {
    return lexer->buf[lexer->pos + k];
}

static char peek(struct Lexer* lexer) { return peek_k(lexer, 0); }

static void advance_k(struct Lexer* lexer, size_t k) { lexer->pos += k; }

static void advance(struct Lexer* lexer) { advance_k(lexer, 1); }

static bool is_digit(char c) { return ('0' <= c) && (c <= '9'); }

static bool is_alpha(char c) {
    return (('A' <= c) && (c <= 'Z')) || (('a' <= c) && (c <= 'z'));
}

static bool is_simple_lexeme(char c) {
    return (c == '(') || (c == ')') || (c == '{') || (c == '}') || (c == ';') ||
           (c == '&') || (c == ',') || (c == '.') || (c == '[') || (c == ']');
}

static bool is_complex_lexeme(char c) {
    return (c == '+') || (c == '-') || (c == '*') || (c == '/') || (c == '=') ||
           (c == '!');
}

static enum TokenTag tokenize_number(struct Lexer* lexer) {
    assert(is_digit(peek(lexer)));
    size_t pos = lexer->pos;
    intptr_t n = 0;
    for (;;) {
        char c = peek(lexer);
        if (!is_digit(c)) break;
        n = n * 10 + (c - '0');
        advance(lexer);
    }
    struct Token* token = new_token(Token_Integer, lexer->line, pos);
    token->integer = n;
    list_insert_at_end(lexer->tokens, list_from(token));
    return Token_Integer;
}

static enum TokenTag tokenize_lexeme(struct Lexer* lexer) {
    size_t begin_pos = lexer->pos;
    for (;;) {
        char c = peek(lexer);
        if (!(is_alpha(c) || is_digit(c) || (c == '_'))) break;
        advance(lexer);
    }
    size_t len = lexer->pos - begin_pos;

    char* str = malloc(len + 1);
    memcpy(str, lexer->buf + begin_pos, len);
    str[len] = 0;

    enum TokenTag tag = find_token_tag(str);
    struct Token* token = new_token(tag, lexer->line, begin_pos);
    if (tag == Token_Id) {
        strtable_id index = strtable_find(&lexer->context->strtable, str);
        if (!index) index = strtable_register(&lexer->context->strtable, str);
        token->strtable_index = index;
    }
    if (tag == Token_KeywordConst) {
        // ignore const keyword, ToDo: fix
    } else
        list_insert_at_end(lexer->tokens, list_from(token));
    return tag;
}

static enum TokenTag tokenize_simple_lexeme(struct Lexer* lexer) {
    char c = peek(lexer);
    assert(is_simple_lexeme(c));
    enum TokenTag tag;
    switch (c) {
        case '(':
            tag = Token_LeftParen;
            break;
        case ')':
            tag = Token_RightParen;
            break;
        case '{':
            tag = Token_LeftCurry;
            break;
        case '}':
            tag = Token_RightCurry;
            break;
        case ';':
            tag = Token_Semicolon;
            break;
        case '&':
            tag = Token_Ampersand;
            break;
        case ',':
            tag = Token_Comma;
            break;
        case '.':
            tag = Token_Dot;
            break;
        case '[':
            tag = Token_LeftBracket;
            break;
        case ']':
            tag = Token_RightBracket;
            break;
        default:
            assert(false);
    };

    struct Token* token = new_token(tag, lexer->line, lexer->pos);
    list_insert_at_end(lexer->tokens, list_from(token));
    advance(lexer);
    return tag;
}

static enum TokenTag tokenize_complex_lexeme(struct Lexer* lexer) {
    char c = peek(lexer);
    assert(is_complex_lexeme(c));

    enum TokenTag tag;
    size_t num_advance;
    switch (c) {
        case '=':
            switch (peek_k(lexer, 1)) {
                case '=':
                    tag = Token_EqualEqual;
                    num_advance = 2;
                    break;
                default:
                    tag = Token_Equal;
                    num_advance = 1;
            }
            break;
        case '+':
            switch (peek_k(lexer, 1)) {
                case '+':
                    tag = Token_PlusPlus;
                    num_advance = 2;
                    break;
                case '=':
                    tag = Token_PlusEqual;
                    num_advance = 2;
                    break;
                default:
                    tag = Token_Plus;
                    num_advance = 1;
            }
            break;
        case '-':
            switch (peek_k(lexer, 1)) {
                case '-':
                    tag = Token_MinusMinus;
                    num_advance = 2;
                    break;
                case '=':
                    tag = Token_MinusEqual;
                    num_advance = 2;
                    break;
                case '>':
                    tag = Token_Arrow;
                    num_advance = 2;
                    break;
                default:
                    tag = Token_Minus;
                    num_advance = 1;
            }
            break;
        case '*':
            switch (peek_k(lexer, 1)) {
                case '=':
                    tag = Token_AsteriskEqual;
                    num_advance = 2;
                    break;
                default:
                    tag = Token_Asterisk;
                    num_advance = 1;
            }
            break;
        case '/':
            switch (peek_k(lexer, 1)) {
                case '/':
                    while (peek(lexer)) advance(lexer);
                    return Token_Invalid;
                default:
                    assert("unimplemented" && false);
            }
            break;
        case '!':
            switch (peek_k(lexer, 1)) {
                case '=':
                    tag = Token_ExclamationEqual;
                    num_advance = 2;
                    break;
                default:
                    tag = Token_Exclamation;
                    num_advance = 1;
            }
            break;
        default:
            assert(false);
    }

    struct Token* token = new_token(tag, lexer->line, lexer->pos);
    list_insert_at_end(lexer->tokens, list_from(token));
    advance_k(lexer, num_advance);
    return tag;
}

static void tokenize(struct Lexer* lexer) {
    for (;;) {
        char c = peek(lexer);
        if (!c) break;
        if (is_digit(c))
            tokenize_number(lexer);
        else if (is_alpha(c))
            tokenize_lexeme(lexer);
        else if (is_simple_lexeme(c))
            tokenize_simple_lexeme(lexer);
        else if (is_complex_lexeme(c))
            tokenize_complex_lexeme(lexer);
        else if (c == '#')
            break;
        else {
            if (c == '\n' || c == ' ')
                advance(lexer);
            else {
                fprintf(stderr, "line = %zu, pos = %zu\n", lexer->line,
                        lexer->pos);
                assert(false);
            }
        }
    }
}

static bool read(struct Lexer* lexer) {
    char* ret =
        fgets(lexer->buf, LEXER_INPUT_STREAM_BUFFER_SIZE, lexer->input_stream);
    return ret != NULL;
}

void lexer_read_and_tokenize(struct Lexer* lexer) {
    list_insert_at_end(
        lexer->tokens,
        list_from(new_token(Token_PseudoFileBegin, lexer->line, lexer->pos)));
    while (read(lexer)) {
        ++lexer->line;
        lexer->pos = 0;
        tokenize(lexer);
    }
    list_insert_at_end(
        lexer->tokens,
        list_from(new_token(Token_PseudoFileEnd, lexer->line, lexer->pos)));
}

void lexer_initialize(struct Lexer* lexer, struct Context* context,
                      struct List* tokens, FILE* input_stream) {
    lexer->context = context;
    lexer->buf = malloc(LEXER_INPUT_STREAM_BUFFER_SIZE);
    lexer->line = 0;
    lexer->pos = 0;
    lexer->input_stream = input_stream;
    lexer->tokens = tokens;
}
