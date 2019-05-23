#include "type.h"
#include "list.h"
#include "strtable.h"

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

struct PointerTypeIr {
    struct TypeIr* elem_type;
};

struct StructTypeIr {
    strtable_id name_index;
    struct List* elem_types;  // MemberEntry* list
};

// ToDo: treat above types as subtype of TypeIr

struct TypeIr {
    enum TypeTag tag;
    size_t size;
    union {
        struct PointerTypeIr* pointer;
        struct StructTypeIr* structure;
    };
};

struct MemberEntry {
    struct ListHeader as_list;
    strtable_id name_index;
    struct TypeIr* type;
    size_t offset;
};

enum TypeTag type_tag2(struct TypeIr* type) { return type->tag; }

size_t type_size(struct TypeIr* type) { return type->size; }

struct PointerTypeIr* type_as_pointer(struct TypeIr* type) {
    return type->tag == Type_Pointer ? type->pointer : NULL;
}

struct StructTypeIr* type_as_struct(struct TypeIr* type) {
    return type->tag == Type_Struct ? type->structure : NULL;
}

struct TypeIr* type_new_int2(void) {
    static struct TypeIr* type = NULL;
    if (!type) {
        type = malloc(sizeof(struct TypeIr));
        type->tag = Type_Int;
        type->size = sizeof(void*);  // ToDo: fix
    }
    return type;
}

struct TypeIr* type_new_pointer2(struct TypeIr* elem_type) {
    struct TypeIr* type = malloc(sizeof(struct TypeIr));
    type->tag = Type_Pointer;
    type->pointer = malloc(sizeof(struct PointerTypeIr));
    type->pointer->elem_type = elem_type;
    type->size = sizeof(void*);
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
    type->size = (size_t)-1;
    return type;
}

strtable_id type_struct_name_index(struct StructTypeIr* type) {
    return type->name_index;
}

struct List* type_struct_elem_types(struct StructTypeIr* type) {
    return type->elem_types;
}

void type_set_elem_types_as_struct(struct TypeIr* type,
                                   struct List* elem_types) {
    assert(type->tag == Type_Struct);
    type->structure->elem_types = elem_types;

    size_t size = 0;
    for (struct ListHeader *it = list_begin(elem_types),
                           *eit = list_end(elem_types);
         it != eit; it = list_next(it)) {
        struct MemberEntry* entry = (struct MemberEntry*)it;
        struct TypeIr* member_type = entry->type;
        entry->offset = size;
        size_t s = member_type->size;
        size = (size + s - 1) / s * s;
        size += s;
    }
    size_t alignment = sizeof(void*);
    size = (size + alignment - 1) / alignment * alignment;

    type->size = size;
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
