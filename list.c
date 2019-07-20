#include "list.h"

#include <assert.h>

void list_initialize(struct List* list) {
    list->size = 0;
    list->head.prev = &list->head;
    list->head.next = &list->head;
}

struct ListHeader* list_begin(struct List* list) {
    return list->head.next;
}

struct ListHeader* list_end(struct List* list) {
    return &list->head;
}

static void insert(struct ListHeader* point, struct ListHeader* item) {
    struct ListHeader* prev = point->prev;
    struct ListHeader* next = point;
    item->prev = prev;
    item->next = point;
    prev->next = item;
    next->prev = item;
}

static struct ListHeader* erase(struct ListHeader* point) {
    struct ListHeader* prev = point->prev;
    struct ListHeader* next = point->next;

    // cannot apply to dummy header
    assert(next != point);
    assert(prev != point);

    prev->next = next;
    next->prev = prev;

    return next;
}

void list_insert_at(struct List* list, struct ListHeader* point,
                    struct ListHeader* item) {
    insert(point, item);
    ++list->size;
}

void list_insert_at_begin(struct List* list, struct ListHeader* item) {
    list_insert_at(list, list_begin(list), item);
}

void list_insert_at_end(struct List* list, struct ListHeader* item) {
    list_insert_at(list, list_end(list), item);
}

size_t list_size(struct List* list) { return list->size; }

struct ListHeader* list_erase_at(struct List* list, struct ListHeader* point) {
    --list->size;
    return erase(point);
}

struct ListHeader* list_erase_at_begin(struct List* list) {
    return list_erase_at(list, list_begin(list));
}

struct ListHeader* list_erase_at_end(struct List* list) {
    return list_erase_at(list, list_end(list));
}

struct ListHeader* list_prev(struct ListHeader* l) {
    return l->prev;
}

struct ListHeader* list_next(struct ListHeader* l) {
    return l->next;
}

struct ListHeader* list_from(void* p) {
    return p;
}

void* list_cast(struct ListHeader* l) { return l; }
