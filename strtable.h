#ifndef STRTABLE_H
#define STRTABLE_H

#include "vector.h"

#include <stddef.h>

typedef size_t strtable_id;

#define STRTABLE_INVALID_ID 0

struct Strtable {
    struct Vector table;
    // ToDo: implement reverse table (strtable_id from string)
};

void strtable_initialize(struct Strtable* strtable);
strtable_id strtable_register(struct Strtable* strtable, const char* str);
strtable_id strtable_find(struct Strtable* strtable, const char* str);
const char* strtable_at(struct Strtable* strtable, strtable_id id);

#endif
