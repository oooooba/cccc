#ifndef VECTOR_H
#define VECTOR_H

#include <stddef.h>

struct Vector {
    size_t size;
    size_t capacity;
    size_t elem_size;
    void* buf;
};

void vector_initialize(struct Vector* vector, size_t elem_size);
size_t vector_size(struct Vector* vector);
void* vector_at(struct Vector* vector, size_t i);
void* vector_back(struct Vector* vector);
void* vector_allocate_back(struct Vector* vector);

#endif
