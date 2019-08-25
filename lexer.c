#include "lexer.h"
#include "context.h"
#include "list.h"
#include "token.h"
#include "vector.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LEXER_INPUT_STREAM_BUFFER_SIZE 256

struct ReservedKeywordsEntry {
    const char* keyword;
    enum TokenTag token_tag;
};

static void register_reserved_keywords(struct Lexer* lexer) {
#define register_keyword(symbol, tag)                        \
    {                                                        \
        struct ReservedKeywordsEntry* entry =                \
            vector_allocate_back(&lexer->reserved_keywords); \
        entry->keyword = #symbol;                            \
        entry->token_tag = Token_Keyword##tag;               \
    }

    register_keyword(_Bool, Char);
    register_keyword(break, Break);
    register_keyword(case, Case);
    register_keyword(char, Char);
    register_keyword(const, Const);
    register_keyword(continue, Continue);
    register_keyword(default, Default);
    register_keyword(else, Else);
    register_keyword(enum, Enum);
    register_keyword(for, For);
    register_keyword(if, If);
    register_keyword(int, Int);
    register_keyword(long, Long);
    register_keyword(return, Return);
    register_keyword(sizeof, Sizeof);
    register_keyword(static, Static);
    register_keyword(struct, Struct);
    register_keyword(switch, Switch);
    register_keyword(typedef, Typedef);
    register_keyword(union, Union);
    register_keyword(void, Void);
    register_keyword(while, While);

#undef register_keyword
}

static enum TokenTag find_token_tag(struct Lexer* lexer, const char* str) {
    for (size_t i = 0, len = vector_size(&lexer->reserved_keywords); i < len;
         ++i) {
        struct ReservedKeywordsEntry* entry =
            vector_at(&lexer->reserved_keywords, i);
        if (strcmp(str, entry->keyword) == 0) return entry->token_tag;
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

static void advance_k(struct Lexer* lexer, size_t k) {
    lexer->pos = lexer->pos + k;
}

static void advance(struct Lexer* lexer) { advance_k(lexer, 1); }

static bool is_digit(char c) { return ('0' <= c) && (c <= '9'); }

static bool is_alpha(char c) {
    return (('A' <= c) && (c <= 'Z')) || (('a' <= c) && (c <= 'z'));
}

static bool is_underscore(char c) { return c == '_'; }

static bool is_simple_lexeme(char c) {
    return (c == '(') || (c == ')') || (c == '{') || (c == '}') || (c == ';') ||
           (c == ',') || (c == '[') || (c == ']') || (c == ':') || (c == '?');
}

static bool is_complex_lexeme(char c) {
    return (c == '+') || (c == '-') || (c == '*') || (c == '/') || (c == '=') ||
           (c == '!') || (c == '<') || (c == '>') || (c == '.') || (c == '&') ||
           (c == '|');
}

static bool is_character_quote(char c) { return c == '\''; }

static bool is_string_quote(char c) { return c == '"'; }

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
        if (!(is_alpha(c) || is_digit(c) || is_underscore(c))) break;
        advance(lexer);
    }
    size_t len = lexer->pos - begin_pos;

    char* str = malloc(len + 1);
    memcpy(str, lexer->buf + begin_pos, len);
    str[len] = 0;

    enum TokenTag tag = find_token_tag(lexer, str);

    if (tag == Token_KeywordConst) {
        // ignore const keyword, ToDo: fix
        return tag;
    }

    struct Token* token = new_token(tag, lexer->line, begin_pos);
    if (tag == Token_Id) {
        strtable_id index = strtable_find(&lexer->context->strtable, str);
        if (!index) index = strtable_register(&lexer->context->strtable, str);
        token->strtable_index = index;
    }

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
        case ',':
            tag = Token_Comma;
            break;
        case '[':
            tag = Token_LeftBracket;
            break;
        case ']':
            tag = Token_RightBracket;
            break;
        case ':':
            tag = Token_Colon;
            break;
        case '?':
            tag = Token_Question;
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
        case '<':
            switch (peek_k(lexer, 1)) {
                case '=':
                    tag = Token_LeftAngleEqual;
                    num_advance = 2;
                    break;
                default:
                    tag = Token_LeftAngle;
                    num_advance = 1;
            }
            break;
        case '>':
            switch (peek_k(lexer, 1)) {
                case '=':
                    tag = Token_RightAngleEqual;
                    num_advance = 2;
                    break;
                default:
                    tag = Token_RightAngle;
                    num_advance = 1;
            }
            break;
        case '&':
            switch (peek_k(lexer, 1)) {
                case '&':
                    tag = Token_AmpersandAmpersand;
                    num_advance = 2;
                    break;
                default:
                    tag = Token_Ampersand;
                    num_advance = 1;
            }
            break;
        case '|':
            switch (peek_k(lexer, 1)) {
                case '|':
                    tag = Token_PipePipe;
                    num_advance = 2;
                    break;
                default:
                    tag = Token_Pipe;
                    num_advance = 1;
            }
            break;
        case '.':
            if (peek_k(lexer, 1) == '.' && peek_k(lexer, 2) == '.') {
                tag = Token_DotDotDot;
                num_advance = 3;
            } else {
                tag = Token_Dot;
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

static void tokenize_character(struct Lexer* lexer) {
    size_t begin_pos = lexer->pos;

    char c = peek(lexer);
    assert(is_character_quote(c));
    advance(lexer);

    char value = peek(lexer);
    advance(lexer);
    if (value == '\\') {
        c = peek(lexer);
        switch (c) {
            case 'n':
                value = '\n';
                break;
            case '\'':
                value = '\'';
                break;
            case '\\':
                value = '\\';
                break;
            default:
                assert(false);
        }
        advance(lexer);
    }

    c = peek(lexer);
    assert(is_character_quote(c));
    advance(lexer);

    struct Token* token = new_token(Token_Integer, lexer->line, begin_pos);
    token->integer = (intptr_t)value;
    list_insert_at_end(lexer->tokens, list_from(token));
}

static void tokenize_string(struct Lexer* lexer) {
    char c = peek(lexer);
    assert(is_string_quote(c));

    size_t begin_pos = lexer->pos;
    advance(lexer);
    for (;;) {
        char c = peek(lexer);
        if (is_string_quote(c)) break;
        advance(lexer);
    }
    advance(lexer);
    size_t len = lexer->pos - begin_pos;

    char* str = malloc(len + 1);
    memcpy(str, lexer->buf + begin_pos, len);
    str[len] = 0;

    strtable_id index = strtable_register(&lexer->context->strtable, str);
    struct Token* token = new_token(Token_String, lexer->line, begin_pos);
    token->strtable_index = index;
    list_insert_at_end(lexer->tokens, list_from(token));
}

static void tokenize(struct Lexer* lexer) {
    for (;;) {
        char c = peek(lexer);
        if (!c) break;
        if (is_digit(c))
            tokenize_number(lexer);
        else if (is_alpha(c) || is_underscore(c))
            tokenize_lexeme(lexer);
        else if (is_simple_lexeme(c))
            tokenize_simple_lexeme(lexer);
        else if (is_complex_lexeme(c))
            tokenize_complex_lexeme(lexer);
        else if (is_character_quote(c))
            tokenize_character(lexer);
        else if (is_string_quote(c))
            tokenize_string(lexer);
        else if (c == '#')
            break;
        else {
            if (c == '\n' || c == ' ')
                advance(lexer);
            else {
                fprintf(lexer->error_stream, "line = %zu, pos = %zu\n",
                        lexer->line, lexer->pos);
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

#if 0
    struct ListHeader* it = list_begin(lexer->tokens);
    while (true) {
        struct Token* token = (struct Token*)it;
        if (token->tag == Token_PseudoFileEnd) break;
        fprintf(lexer->error_stream, "line=%zu, pos=%zu, %d %zu\n", token->line,
                token->position, token->tag, token->strtable_index);
        it = list_next(it);
    }
#endif
}

void lexer_initialize(struct Lexer* lexer, struct Context* context,
                      struct List* tokens) {
    lexer->context = context;
    lexer->buf = malloc(LEXER_INPUT_STREAM_BUFFER_SIZE);
    lexer->line = 0;
    lexer->pos = 0;
    lexer->input_stream = context->input_stream;
    lexer->error_stream = context->error_stream;
    lexer->tokens = tokens;
    vector_initialize(&lexer->reserved_keywords,
                      sizeof(struct ReservedKeywordsEntry));
    register_reserved_keywords(lexer);
}
