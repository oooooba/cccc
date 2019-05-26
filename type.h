#ifndef TYPE_H
#define TYPE_H

#include "list.h"
#include "strtable.h"

#include <stdbool.h>
#include <stddef.h>

struct TypeIr;

struct PointerTypeIr;
struct StructTypeIr;
struct FunctionTypeIr;

struct MemberEntry;

enum TypeTag {
    Type_Int,
    Type_Pointer,
    Type_Array,
    Type_Struct,
    Type_Function,
};

enum TypeTag type_tag2(struct TypeIr* type);
size_t type_size(struct TypeIr* type);
bool type_equal(struct TypeIr* type1, struct TypeIr* type2);
struct PointerTypeIr* type_as_pointer(struct TypeIr* type);
struct StructTypeIr* type_as_struct(struct TypeIr* type);
struct FunctionTypeIr* type_as_function(struct TypeIr* type);

struct TypeIr* type_new_int2(void);

struct TypeIr* type_new_pointer2(struct TypeIr* elem_type);
struct TypeIr* type_pointer_elem_type(struct PointerTypeIr* type);

struct TypeIr* type_new_struct(strtable_id name_index, struct List* elem_types);
strtable_id type_struct_name_index(struct StructTypeIr* type);
struct List* type_struct_elem_types(struct StructTypeIr* type);
void type_set_elem_types_as_struct(struct TypeIr* type,
                                   struct List* elem_types);

struct MemberEntry* type_new_member_entry(strtable_id name_index,
                                          struct TypeIr* type);
struct ListHeader* type_member_entry_as_list_header(struct MemberEntry* entry);
strtable_id type_member_entry_name_index(struct MemberEntry* entry);
struct TypeIr* type_member_entry_type(struct MemberEntry* entry);

struct TypeIr* type_new_function(struct TypeIr* result_type,
                                 struct List* param_types);
struct TypeIr* type_function_result_type(struct FunctionTypeIr* type);
struct List* type_function_param_types(struct FunctionTypeIr* type);

#endif
