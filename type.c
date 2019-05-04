#include "type.h"
#include "list.h"

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

struct PointerTypeNode {
    struct TypeNode* elem_type;
};

struct StructTypeNode {
    struct IdNode* name;
    struct List elem_types;
};

struct ArrayTypeNode {
    struct TypeNode* elem_type;
    size_t length;
};

struct TypeNode {
    enum TypeTag tag;
    union {
        struct PointerTypeNode* pointer;
        struct StructTypeNode* structure;
        struct ArrayTypeNode* array;
    };
};

enum TypeTag type_tag(struct TypeNode* node) { return node->tag; }

struct TypeNode* type_new_int(void) {
    struct TypeNode* type = malloc(sizeof(struct TypeNode));
    type->tag = Type_Int;
    return type;
}

const char* type_size_str(struct TypeNode* node) {
    switch (node->tag) {
        case Type_Int:
            return "dword";
        case Type_Pointer:
            return "qword";
        default:
            assert(false);
    }
    return NULL;
}

size_t type_size(struct TypeNode* node) {
    switch (node->tag) {
        case Type_Int:
            return sizeof(void*);
        case Type_Pointer:
            return sizeof(void*);
        default:
            assert(false);
    }
    return (size_t)-1;
}

////////////////////////////////////////////////////////

struct TypeIr {
    enum TypeTag tag;
#if 0
    union {
        struct PointerTypeIr* pointer;
        struct StructTypeIr* structure;
        struct ArrayTypeIr* array;
    };
#endif
};

struct TypeIr* type_new_int2(void) {
    struct TypeIr* type = malloc(sizeof(struct TypeIr));
    type->tag = Type_Int;
    return type;
}
