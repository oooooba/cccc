#ifndef TYPE_H
#define TYPE_H

#include <stddef.h>

struct TypeNode;
struct PointerTypeNode;
struct ArrayTypeNode;
struct StructTypeNode;

enum TypeTag {
    Type_Int,
    Type_Pointer,
    Type_Array,
    Type_Struct,
};

enum TypeTag type_tag(struct TypeNode* node);
struct TypeNode* type_new_int(void);
size_t type_size(struct TypeNode* node);

struct TypeIr;

struct TypeIr* type_new_int2(void);

#endif
