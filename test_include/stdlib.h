#ifndef CCCC_STDLIB_H
#define CCCC_STDLIB_H

#include <stddef.h>

void abort(void);
void* malloc(size_t size);
void free(void* ptr);
void* realloc(void* ptr, size_t size);

#endif  // CCCC_STDLIB_H

