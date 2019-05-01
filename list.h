#ifndef LIST_H
#define LIST_H

#include <stddef.h>

struct ListHeader {
    struct ListHeader* prev;
    struct ListHeader* next;
};

struct ListItem {
    struct ListHeader header;
    void* item;
};

struct List {
    struct ListHeader head;
    size_t size;
};

void list_initialize(struct List* list);

struct ListHeader* list_begin(struct List* list);
struct ListHeader* list_end(struct List* list);
size_t list_size(struct List* list);

void list_insert_at(struct List* list, struct ListHeader* point,
                    struct ListHeader* item);
void list_insert_at_begin(struct List* list, struct ListHeader* item);
void list_insert_at_end(struct List* list, struct ListHeader* item);

struct ListHeader* list_erase_at(struct List* list, struct ListHeader* point);
struct ListHeader* list_erase_at_begin(struct List* list);
struct ListHeader* list_erase_at_end(struct List* list);

struct ListHeader* list_prev(struct ListHeader* l);
struct ListHeader* list_next(struct ListHeader* l);

struct ListHeader* list_from(void* p);
void* list_cast(struct ListHeader* l);

void list_validate(struct List* list, const char* msg);  // for debug

#endif
