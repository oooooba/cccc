#ifndef TOKEN_H
#define TOKEN_H

#include "list.h"
#include "strtable.h"

#include <stdint.h>

enum TokenTag {
    Token_Invalid,

    Token_KeywordElse,
    Token_KeywordIf,
    Token_KeywordInt,
    Token_KeywordReturn,
    Token_KeywordStruct,

    Token_LeftParen,
    Token_RightParen,
    Token_LeftCurry,
    Token_RightCurry,

    Token_Comma,
    Token_Semicolon,

    Token_Ampersand,

    Token_Equal,
    Token_EqualEqual,
    Token_Plus,
    Token_PlusPlus,
    Token_PlusEqual,
    Token_Minus,
    Token_MinusMinus,
    Token_MinusEqual,
    Token_Asterisk,
    Token_AsteriskEqual,

    Token_Integer,
    Token_Id,

    Token_PseudoFileBegin,
    Token_PseudoFileEnd,
};

struct Token {
    struct ListHeader as_list;
    enum TokenTag tag;
    union {
        intptr_t integer;
        strtable_id strtable_index;
    };
};

#endif
