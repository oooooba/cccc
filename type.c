#include "type.h"
#include "list.h"
#include "strtable.h"

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

struct PointerTypeIr {
    struct TypeIr* elem_type;
};

struct StructTypeIr {
    strtable_id name_index;
    struct List* elem_types;  // MemberEntry* list
};

struct TypeIr {
    enum TypeTag tag;
    union {
        struct PointerTypeIr* pointer;
        struct StructTypeIr* structure;
    };
};

struct MemberEntry {
    struct ListHeader as_list;
    strtable_id name_index;
    struct TypeIr* type;
};

enum TypeTag type_tag2(struct TypeIr* type) { return type->tag; }

struct PointerTypeIr* type_as_pointer(struct TypeIr* type) {
    return type->tag == Type_Pointer ? type->pointer : NULL;
}

struct StructTypeIr* type_as_struct(struct TypeIr* type) {
    return type->tag == Type_Struct ? type->structure : NULL;
}

struct TypeIr* type_new_int2(void) {
    struct TypeIr* type = malloc(sizeof(struct TypeIr));
    type->tag = Type_Int;
    return type;
}

struct TypeIr* type_new_pointer2(struct TypeIr* elem_type) {
    struct TypeIr* type = malloc(sizeof(struct TypeIr));
    type->tag = Type_Pointer;
    type->pointer = malloc(sizeof(struct PointerTypeIr));
    type->pointer->elem_type = elem_type;
    return type;
}

struct TypeIr* type_pointer_elem_type(struct PointerTypeIr* type) {
    return type->elem_type;
}

struct TypeIr* type_new_struct(strtable_id name_index,
                               struct List* elem_types) {
    struct TypeIr* type = malloc(sizeof(struct TypeIr));
    type->tag = Type_Struct;
    type->structure = malloc(sizeof(struct StructTypeIr));
    type->structure->name_index = name_index;
    type->structure->elem_types = elem_types;
    return type;
}

strtable_id type_struct_name_index(struct StructTypeIr* type) {
    return type->name_index;
}

struct List* type_struct_elem_types(struct StructTypeIr* type) {
    return type->elem_types;
}

void type_struct_set_elem_types(struct StructTypeIr* type,
                                struct List* elem_types) {
    type->elem_types = elem_types;
}

struct MemberEntry* type_new_member_entry(strtable_id name_index,
                                          struct TypeIr* type) {
    struct MemberEntry* entry = malloc(sizeof(struct MemberEntry));
    entry->name_index = name_index;
    entry->type = type;
    return entry;
}

struct ListHeader* type_member_entry_as_list_header(struct MemberEntry* entry) {
    return &entry->as_list;
}

strtable_id type_member_entry_name_index(struct MemberEntry* entry) {
    return entry->name_index;
}

struct TypeIr* type_member_entry_type(struct MemberEntry* entry) {
    return entry->type;
}
