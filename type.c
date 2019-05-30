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

struct FunctionTypeIr {
    struct TypeIr* result_type;
    struct List* param_types;  // TypeIr* list
};

// ToDo: treat above types as subtype of TypeIr

struct TypeIr {
    enum TypeTag tag;
    size_t size;
    union {
        struct PointerTypeIr* pointer;
        struct StructTypeIr* structure;
        struct FunctionTypeIr* function;
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

bool type_equal(struct TypeIr* type1, struct TypeIr* type2) {
    if (type1 == type2)
        return true;
    else if (type1->tag == Type_Pointer && type2->tag == Type_Pointer)
        return type_equal(type1->pointer->elem_type, type2->pointer->elem_type);
    else if (type1->tag == Type_Function && type2->tag == Type_Function) {
        struct FunctionTypeIr* ft1 = type1->function;
        struct FunctionTypeIr* ft2 = type2->function;
        if (!type_equal(ft1->result_type, ft2->result_type)) return false;
        struct ListHeader *it1 = list_begin(ft1->param_types),
                          *it2 = list_begin(ft2->param_types),
                          *eit1 = list_end(ft1->param_types),
                          *eit2 = list_end(ft2->param_types);
        while (!(it1 == eit1 || it2 == eit2)) {
            struct TypeIr* t1 = ((struct ListItem*)it1)->item;
            struct TypeIr* t2 = ((struct ListItem*)it2)->item;
            if (!type_equal(t1, t2)) return false;
            it1 = list_next(it1);
            it2 = list_next(it2);
        }
        return (it1 == eit1 && it2 == eit2);
    } else
        return type1->tag == type2->tag;
}

struct PointerTypeIr* type_as_pointer(struct TypeIr* type) {
    return type->tag == Type_Pointer ? type->pointer : NULL;
}

struct StructTypeIr* type_as_struct(struct TypeIr* type) {
    return type->tag == Type_Struct ? type->structure : NULL;
}

struct FunctionTypeIr* type_as_function(struct TypeIr* type) {
    return type->tag == Type_Function ? type->function : NULL;
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

struct TypeIr* type_new_function(struct TypeIr* result_type,
                                 struct List* param_types) {
    struct TypeIr* type = malloc(sizeof(struct TypeIr));
    type->tag = Type_Function;
    type->function = malloc(sizeof(struct FunctionTypeIr));
    type->function->result_type = result_type;
    type->function->param_types = param_types;
    return type;
}

struct TypeIr* type_function_result_type(struct FunctionTypeIr* type) {
    return type->result_type;
}

struct List* type_function_param_types(struct FunctionTypeIr* type) {
    return type->param_types;
}
