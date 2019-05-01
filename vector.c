#include "vector.h"

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define INITIAL_NUM_ENTRIES 1024

void vector_initialize(struct Vector* vector, size_t elem_size) {
    void* buf = malloc(elem_size * INITIAL_NUM_ENTRIES);
    assert(buf != NULL);
    *vector = (struct Vector){
        .size = 0,
        .capacity = INITIAL_NUM_ENTRIES,
        .elem_size = elem_size,
        .buf = buf,
    };
}

#undef INITIAL_NUM_ENTRIES

size_t vector_size(struct Vector* vector) { return vector->size; }

void* vector_at(struct Vector* vector, size_t i) {
    assert(i < vector->size);
    uint8_t* buf = vector->buf;
    return &buf[i * vector->elem_size];
}

void* vector_back(struct Vector* vector) {
    assert(vector->size > 0);
    return vector_at(vector, vector->size - 1);
}

static void grow(struct Vector* vector) {
    size_t old_capacity = vector->capacity;
    void* old_buf = vector->buf;
    size_t new_capacity = old_capacity * 2;
    void* new_buf = realloc(old_buf, vector->elem_size * new_capacity);
    assert(new_buf != NULL);
    if (old_buf != new_buf) {
        vector->buf = new_buf;
        memcpy(new_buf, old_buf, vector->elem_size * old_capacity);
        free(old_buf);
    }
    vector->capacity = new_capacity;
}

void* vector_allocate_back(struct Vector* vector) {
    if (vector->size < vector->capacity) {
        ++vector->size;
        return vector_back(vector);
    }
    grow(vector);
    return vector_allocate_back(vector);
}
