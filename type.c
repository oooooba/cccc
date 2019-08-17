#include "type.h"
#include "list.h"
#include "strtable.h"

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

struct MemberEntry {
    struct ListHeader as_list;
    strtable_id name_index;
    struct TypeIr* type;
    size_t offset;
};

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

size_t type_member_entry_offset(struct MemberEntry* entry) {
    return entry->offset;
}

struct TypeIr {
    enum TypeTag tag;
    size_t size;
};

static void initialize_type(struct TypeIr* type, enum TypeTag tag,
                            size_t size) {
    type->tag = tag;
    type->size = size;
}

struct VoidTypeIr* type_as_void(struct TypeIr* type) {
    return type->tag == Type_Void ? (struct VoidTypeIr*)type : NULL;
}

struct PointerTypeIr* type_as_pointer(struct TypeIr* type) {
    return type->tag == Type_Pointer ? (struct PointerTypeIr*)type : NULL;
}

struct StructTypeIr* type_as_struct(struct TypeIr* type) {
    return type->tag == Type_Struct ? (struct StructTypeIr*)type : NULL;
}

struct FunctionTypeIr* type_as_function(struct TypeIr* type) {
    return type->tag == Type_Function ? (struct FunctionTypeIr*)type : NULL;
}

enum TypeTag type_tag(struct TypeIr* type) { return type->tag; }

size_t type_size(struct TypeIr* type) {
    assert(type->tag != Type_Function);
    return type->size;
}

struct VoidTypeIr {
    struct TypeIr super;
};

struct VoidTypeIr* type_new_void(void) {
    struct VoidTypeIr* type = malloc(sizeof(struct VoidTypeIr));
    initialize_type(type_void_super(type), Type_Void, 1);
    return type;
}

struct TypeIr* type_void_super(struct VoidTypeIr* type) {
    return &type->super;
}

struct LongTypeIr {
    struct TypeIr super;
};

struct LongTypeIr* type_new_long(void) {
    struct LongTypeIr* type = malloc(sizeof(struct LongTypeIr));
    assert(sizeof(long) == 8);
    initialize_type(type_long_super(type), Type_Long, sizeof(long));
    return type;
}

struct TypeIr* type_long_super(struct LongTypeIr* type) {
    return &type->super;
}

struct IntTypeIr {
    struct TypeIr super;
};

struct IntTypeIr* type_new_int(void) {
    struct IntTypeIr* type = malloc(sizeof(struct IntTypeIr));
    assert(sizeof(int) == 4);
    initialize_type(type_int_super(type), Type_Int, sizeof(int));
    return type;
}

struct TypeIr* type_int_super(struct IntTypeIr* type) {
    return &type->super;
}

struct CharTypeIr {
    struct TypeIr super;
};

struct CharTypeIr* type_new_char(void) {
    struct CharTypeIr* type = malloc(sizeof(struct CharTypeIr));
    assert(sizeof(char) == 1);
    initialize_type(type_char_super(type), Type_Char, sizeof(char));
    return type;
}

struct TypeIr* type_char_super(struct CharTypeIr* type) {
    return &type->super;
}

struct PointerTypeIr {
    struct TypeIr super;
    struct TypeIr* elem_type;
};

struct PointerTypeIr* type_new_pointer(struct TypeIr* elem_type) {
    struct PointerTypeIr* type = malloc(sizeof(struct PointerTypeIr));
    initialize_type(type_pointer_super(type), Type_Pointer, sizeof(void*));
    type->elem_type = elem_type;
    return type;
}

struct TypeIr* type_pointer_super(struct PointerTypeIr* type) {
    return &type->super;
}

struct TypeIr* type_pointer_elem_type(struct PointerTypeIr* type) {
    return type->elem_type;
}

struct StructTypeIr {
    struct TypeIr super;
    strtable_id name_index;
    bool is_union;
    struct List* elem_types;  // MemberEntry* list
};

struct StructTypeIr* type_new_struct(strtable_id name_index, bool is_union,
                                     struct List* elem_types) {
    struct StructTypeIr* type = malloc(sizeof(struct StructTypeIr));
    initialize_type(type_struct_super(type), Type_Struct, (size_t)-1);
    type->name_index = name_index;
    type->is_union = is_union;
    type->elem_types = elem_types;
    return type;
}

struct TypeIr* type_struct_super(struct StructTypeIr* type) {
    return &type->super;
}

strtable_id type_struct_name_index(struct StructTypeIr* type) {
    return type->name_index;
}

bool type_struct_is_union(struct StructTypeIr* type) { return type->is_union; }

struct List* type_struct_elem_types(struct StructTypeIr* type) {
    return type->elem_types;
}

static size_t roundup(size_t x, size_t y) {
    // to avoid division operation, ToDo: fix to handle div instruction
    long ix = x;
    size_t z = 0;
    while (ix > 0) {
        ix = ix - y;
        ++z;
    }
    return z * y;
}

static size_t set_member_offset(struct StructTypeIr* type, size_t base) {
    size_t offset = 0;
    size_t size = 0;
    bool is_union = type->is_union;
    for (struct ListHeader *it = list_begin(type->elem_types),
                           *eit = list_end(type->elem_types);
         it != eit; it = list_next(it)) {
        struct MemberEntry* entry = (struct MemberEntry*)it;
        struct TypeIr* member_type = entry->type;
        size_t s = member_type->size;
        if (!is_union) offset = roundup(offset, s);
        entry->offset = base + offset;
        if (entry->name_index == STRTABLE_INVALID_ID) {
            assert(type_as_struct(member_type));
            size_t anonymous_size =
                set_member_offset(type_as_struct(member_type), offset + base);
            assert(anonymous_size == s);
        }
        if (is_union)
            size = size > s ? size : s;
        else {
            offset = offset + s;
            size = offset;
        }
    }
    size_t alignment = sizeof(void*);
    size = roundup(size, alignment);
    return size;
}

void type_struct_set_elem_types(struct StructTypeIr* type,
                                struct List* elem_types) {
    type->elem_types = elem_types;

    type_struct_super(type)->size = set_member_offset(type, 0);
}

struct MemberEntry* type_struct_find_member(struct StructTypeIr* type,
                                            strtable_id name_index) {
    for (struct ListHeader *it = list_begin(type->elem_types),
                           *eit = list_end(type->elem_types);
         it != eit; it = list_next(it)) {
        struct MemberEntry* entry = (struct MemberEntry*)it;
        if (entry->name_index == name_index) return entry;
        if (entry->name_index != STRTABLE_INVALID_ID) continue;

        // search in anonymous member
        struct StructTypeIr* elem_type =
            type_as_struct(type_member_entry_type(entry));
        assert(elem_type);
        entry = type_struct_find_member(elem_type, name_index);
        if (entry) return entry;
    }
    return NULL;
}

struct FunctionTypeIr {
    struct TypeIr super;
    struct TypeIr* result_type;
    struct List* param_types;  // TypeIr* list
    bool is_parameters_variable_length;
};

struct FunctionTypeIr* type_new_function(struct TypeIr* result_type,
                                         struct List* param_types,
                                         bool is_parameters_variable_length) {
    struct FunctionTypeIr* type = malloc(sizeof(struct FunctionTypeIr));
    initialize_type(type_function_super(type), Type_Function, sizeof(void*));
    type->result_type = result_type;
    type->param_types = param_types;
    type->is_parameters_variable_length = is_parameters_variable_length;
    return type;
}

struct TypeIr* type_function_super(struct FunctionTypeIr* type) {
    return &type->super;
}

struct TypeIr* type_function_result_type(struct FunctionTypeIr* type) {
    return type->result_type;
}

struct List* type_function_param_types(struct FunctionTypeIr* type) {
    return type->param_types;
}

bool type_function_is_parameters_variable_length(struct FunctionTypeIr* type) {
    return type->is_parameters_variable_length;
}

bool type_equal(struct TypeIr* type1, struct TypeIr* type2) {
    if (type1 == NULL || type2 == NULL)
        return false;
    else if (type1 == type2)
        return true;
    else if (type1->tag == Type_Pointer && type2->tag == Type_Pointer)
        return type_equal(type_as_pointer(type1)->elem_type,
                          type_as_pointer(type2)->elem_type);
    else if (type1->tag == Type_Struct && type2->tag == Type_Struct)
        assert(false);
    else if (type1->tag == Type_Function && type2->tag == Type_Function) {
        struct FunctionTypeIr* ft1 = type_as_function(type1);
        struct FunctionTypeIr* ft2 = type_as_function(type2);
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
