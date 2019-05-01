#include "strtable.h"

#include "vector.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

void strtable_initialize(struct Strtable* strtable) {
    vector_initialize(&strtable->table, sizeof(const char*));
    strtable_register(strtable, NULL);  // register dummy entry
}

strtable_id strtable_register(struct Strtable* strtable, const char* str) {
    strtable_id id = vector_size(&strtable->table);
    *((const char**)vector_allocate_back(&strtable->table)) = str;
    return id;
}

// ToDo: fix to use reverse table
strtable_id strtable_find(struct Strtable* strtable, const char* str) {
    for (strtable_id i = 1, size = vector_size(&strtable->table); i < size;
         ++i) {
        if (strcmp(*((const char**)vector_at(&strtable->table, i)), str) == 0)
            return i;
    }
    return 0;
}

const char* strtable_at(struct Strtable* strtable, strtable_id id) {
    assert(id != STRTABLE_INVALID_ID);
    assert(id < vector_size(&strtable->table));
    return *((const char**)vector_at(&strtable->table, id));
}
