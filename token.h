#ifndef TOKEN_H
#define TOKEN_H

#include "list.h"
#include "strtable.h"

#include <stdint.h>

enum TokenTag {
    Token_Invalid,

    Token_KeywordBool,
    Token_KeywordBreak,
    Token_KeywordCase,
    Token_KeywordChar,
    Token_KeywordConst,
    Token_KeywordContinue,
    Token_KeywordDefault,
    Token_KeywordElse,
    Token_KeywordEnum,
    Token_KeywordFor,
    Token_KeywordIf,
    Token_KeywordInt,
    Token_KeywordLong,
    Token_KeywordReturn,
    Token_KeywordSizeof,
    Token_KeywordStatic,
    Token_KeywordStruct,
    Token_KeywordSwitch,
    Token_KeywordTypedef,
    Token_KeywordUnion,
    Token_KeywordVoid,
    Token_KeywordWhile,

    Token_LeftParen,
    Token_RightParen,
    Token_LeftCurry,
    Token_RightCurry,
    Token_LeftBracket,
    Token_RightBracket,
    Token_LeftAngle,
    Token_LeftAngleEqual,
    Token_RightAngle,
    Token_RightAngleEqual,

    Token_Comma,
    Token_Colon,
    Token_Semicolon,
    Token_Dot,
    Token_DotDotDot,
    Token_Question,

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
    Token_Arrow,
    Token_Exclamation,
    Token_ExclamationEqual,
    Token_Ampersand,
    Token_AmpersandAmpersand,
    Token_Pipe,
    Token_PipePipe,

    Token_Integer,
    Token_String,
    Token_Id,

    Token_PseudoFileBegin,
    Token_PseudoFileEnd,
};

struct Token {
    struct ListHeader as_list;
    size_t line;
    size_t position;
    enum TokenTag tag;
    union {
        intptr_t integer;
        strtable_id strtable_index;
    };
};

#endif
