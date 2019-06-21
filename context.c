#include "context.h"
#include "ir.h"
#include "list.h"
#include "map.h"
#include "strtable.h"
#include "type.h"
#include "vector.h"

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

void context_initialize(struct Context* context) {
    strtable_initialize(&context->strtable);
    map_initialize(&context->function_declaration_map);
    map_initialize(&context->user_defined_type_map);
    vector_initialize(&context->register_ids, sizeof(strtable_id));
}

void context_insert_function_declaration(struct Context* context,
                                         strtable_id index,
                                         struct FunctionIr* func) {
    map_insert(&context->function_declaration_map, (void*)index, func);
}

struct FunctionIr* context_find_function_declaration(struct Context* context,
                                                     strtable_id index) {
    return map_find(&context->function_declaration_map, (void*)index);
}

struct ListHeader* context_function_declaration_begin(struct Context* context) {
    return map_begin(&context->function_declaration_map);
}

struct ListHeader* context_function_declaration_end(struct Context* context) {
    return map_end(&context->function_declaration_map);
}

void context_insert_user_defined_type(struct Context* context,
                                      strtable_id index, struct TypeIr* type) {
    map_insert(&context->user_defined_type_map, (void*)index, type);
}

struct ListHeader* context_user_defined_type_begin(struct Context* context) {
    return map_begin(&context->user_defined_type_map);
}

struct ListHeader* context_user_defined_type_end(struct Context* context) {
    return map_end(&context->user_defined_type_map);
}

struct TypeIr* context_find_user_defined_type(struct Context* context,
                                              strtable_id index) {
    return map_find(&context->user_defined_type_map, (void*)index);
}

void context_register_registers(struct Context* context) {
    strtable_id id;

#define reg(name)                                                           \
    do {                                                                    \
        id = strtable_register(&context->strtable, "%" #name);              \
        *((strtable_id*)vector_allocate_back(&context->register_ids)) = id; \
    } while (0)

    reg(al);
    reg(eax);
    reg(rax);
    reg(bl);
    reg(ebx);
    reg(rbx);
    reg(r10l);
    reg(r10d);
    reg(r10);

    reg(dil);
    reg(edi);
    reg(rdi);
    reg(sil);
    reg(esi);
    reg(rsi);
    reg(dl);
    reg(edx);
    reg(rdx);
    reg(cl);
    reg(ecx);
    reg(rcx);
    reg(r8l);
    reg(r8d);
    reg(r8);
    reg(r9l);
    reg(r9d);
    reg(r9);

    reg(spl);
    reg(esp);
    reg(rsp);
    reg(bpl);
    reg(ebp);
    reg(rbp);

#undef reg
}

#define OFFSET_INDEX_FUNC_CALL_ARG_REG 3
#define OFFSET_INDEX_SPECIAL_PURPOSE_REG 9

strtable_id context_nth_reg(struct Context* context, size_t n,
                            enum RegisterSizeKind kind) {
    assert(n < OFFSET_INDEX_SPECIAL_PURPOSE_REG);
    size_t i = n * RegisterSizeKind_Num + kind;
    return *((strtable_id*)vector_at(&context->register_ids, i));
}

strtable_id context_nth_func_call_arg_reg(struct Context* context, size_t n,
                                          enum RegisterSizeKind kind) {
    return context_nth_reg(context, OFFSET_INDEX_FUNC_CALL_ARG_REG + n, kind);
}

strtable_id context_func_call_result_reg(struct Context* context,
                                         enum RegisterSizeKind kind) {
    return context_nth_reg(context, 0, kind);
}

strtable_id context_stack_pointer_reg(struct Context* context,
                                      enum RegisterSizeKind kind) {
    size_t i =
        (OFFSET_INDEX_SPECIAL_PURPOSE_REG + 0) * RegisterSizeKind_Num + kind;
    return *((strtable_id*)vector_at(&context->register_ids, i));
}

strtable_id context_base_pointer_reg(struct Context* context,
                                     enum RegisterSizeKind kind) {
    size_t i =
        (OFFSET_INDEX_SPECIAL_PURPOSE_REG + 1) * RegisterSizeKind_Num + kind;
    return *((strtable_id*)vector_at(&context->register_ids, i));
}

void context_dump_type(struct Context* context, FILE* stream,
                       struct TypeIr* type) {
    if (!type) {
        fprintf(stream, "<untyped>");
        return;
    }

    switch (type_tag(type)) {
        case Type_Long:
            fprintf(stream, "long");
            break;
        case Type_Int:
            fprintf(stream, "int");
            break;
        case Type_Char:
            fprintf(stream, "char");
            break;
        case Type_Pointer: {
            struct PointerTypeIr* p = type_as_pointer(type);
            context_dump_type(context, stream, type_pointer_elem_type(p));
            fprintf(stream, "*");
        } break;
        case Type_Struct: {
            struct StructTypeIr* s = type_as_struct(type);
            fprintf(stream, "struct");
            strtable_id name_index = type_struct_name_index(s);
            if (name_index != STRTABLE_INVALID_ID)
                fprintf(stream, " %s",
                        strtable_at(&context->strtable, name_index));
            struct List* elem_types = type_struct_elem_types(s);
            if (!elem_types) break;
            fprintf(stream, " { ");

            for (struct ListHeader *it = list_begin(type_struct_elem_types(s)),
                                   *eit = list_end(type_struct_elem_types(s));
                 it != eit; it = list_next(it)) {
                struct MemberEntry* entry = (struct MemberEntry*)it;
                context_dump_type(context, stream,
                                  type_member_entry_type(entry));
                const char* name = strtable_at(
                    &context->strtable, type_member_entry_name_index(entry));
                fprintf(stream, " %s; ", name);
            }

            fprintf(stream, "}");
        } break;
        default:
            assert(false);
    }
}

enum RegisterSizeKind context_type_to_register_size_kind(struct TypeIr* type) {
    switch (type_size(type)) {
        case 1:
            return RegisterSizeKind_8;
        case 4:
            return RegisterSizeKind_32;
        case 8:
            return RegisterSizeKind_64;
        default:
            assert(false);
            return -1;
    }
}
