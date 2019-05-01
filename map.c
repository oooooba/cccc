#include "map.h"

#include "list.h"

#include <stdint.h>
#include <stdlib.h>

// map-like structure
struct MapEntry {
    struct ListHeader as_list;
    void* key;
    void* value;
};

void map_initialize(struct Map* map) { list_initialize(&map->entries); }

void map_insert(struct Map* map, void* key, void* value) {
    struct MapEntry* entry = malloc(sizeof(struct MapEntry));
    entry->key = key;
    entry->value = value;
    list_insert_at_end(&map->entries, list_from(entry));
}

void* map_find(struct Map* map, void* key) {
    struct ListHeader* it = list_begin(&map->entries);
    struct ListHeader* eit = list_end(&map->entries);
    for (; it != eit; it = list_next(it)) {
        struct MapEntry* entry = (struct MapEntry*)it;
        if (entry->key == key) return entry->value;
    }
    return NULL;
}

