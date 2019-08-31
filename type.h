#ifndef TYPE_H
#define TYPE_H

#include "list.h"
#include "strtable.h"

#include <stdbool.h>
#include <stddef.h>

struct TypeIr;
struct VoidTypeIr;
struct LongTypeIr;
struct IntTypeIr;
struct CharTypeIr;
struct BoolTypeIr;
struct PointerTypeIr;
struct StructTypeIr;
struct FunctionTypeIr;

struct MemberEntry;

enum TypeTag {
    Type_Void,
    Type_Long,
    Type_Int,
    Type_Char,
    Type_Bool,
    Type_Pointer,
    Type_Struct,
    Type_Function,
};

struct VoidTypeIr* type_as_void(struct TypeIr* type);
struct PointerTypeIr* type_as_pointer(struct TypeIr* type);
struct StructTypeIr* type_as_struct(struct TypeIr* type);
struct FunctionTypeIr* type_as_function(struct TypeIr* type);

enum TypeTag type_tag(struct TypeIr* type);
size_t type_size(struct TypeIr* type);

struct VoidTypeIr* type_new_void(void);
struct TypeIr* type_void_super(struct VoidTypeIr* type);

struct LongTypeIr* type_new_long(void);
struct TypeIr* type_long_super(struct LongTypeIr* type);

struct IntTypeIr* type_new_int(void);
struct TypeIr* type_int_super(struct IntTypeIr* type);

struct CharTypeIr* type_new_char(void);
struct TypeIr* type_char_super(struct CharTypeIr* type);

struct BoolTypeIr* type_new_bool(void);
struct TypeIr* type_bool_super(struct BoolTypeIr* type);

struct PointerTypeIr* type_new_pointer(struct TypeIr* elem_type);
struct TypeIr* type_pointer_super(struct PointerTypeIr* type);
struct TypeIr* type_pointer_elem_type(struct PointerTypeIr* type);

struct StructTypeIr* type_new_struct(strtable_id name_index, bool is_union,
                                     struct List* elem_types);
struct TypeIr* type_struct_super(struct StructTypeIr* type);
strtable_id type_struct_name_index(struct StructTypeIr* type);
bool type_struct_is_union(struct StructTypeIr* type);
struct List* type_struct_elem_types(struct StructTypeIr* type);
void type_struct_set_elem_types(struct StructTypeIr* type,
                                struct List* elem_types);
struct MemberEntry* type_struct_find_member(struct StructTypeIr* type,
                                            strtable_id name_index);

struct FunctionTypeIr* type_new_function(struct TypeIr* result_type,
                                         struct List* param_types,
                                         bool is_parameters_variable_length);
struct TypeIr* type_function_super(struct FunctionTypeIr* type);
struct TypeIr* type_function_result_type(struct FunctionTypeIr* type);
struct List* type_function_param_types(struct FunctionTypeIr* type);
bool type_function_is_parameters_variable_length(struct FunctionTypeIr* type);

struct MemberEntry* type_new_member_entry(strtable_id name_index,
                                          struct TypeIr* type);
struct ListHeader* type_member_entry_as_list_header(struct MemberEntry* entry);
strtable_id type_member_entry_name_index(struct MemberEntry* entry);
struct TypeIr* type_member_entry_type(struct MemberEntry* entry);
size_t type_member_entry_offset(struct MemberEntry* entry);

bool type_equal(struct TypeIr* type1, struct TypeIr* type2);

#endif
