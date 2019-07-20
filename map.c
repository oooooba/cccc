#include "map.h"

#include "list.h"

#include <stdlib.h>

// map-like structure
struct MapEntry {
    struct ListHeader as_list;
    void* key;
    void* value;
};

void map_initialize(struct Map* map) { list_initialize(&map->entries); }

void map_insert(struct Map* map, void* key, void* value) {
    struct ListHeader* it = list_begin(&map->entries);
    struct ListHeader* eit = list_end(&map->entries);
    for (; it != eit; it = list_next(it)) {
        struct MapEntry* entry = (struct MapEntry*)it;
        if (entry->key == key) {
            entry->value = value;
            return;
        }
    }

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

struct ListHeader* map_begin(struct Map* map) {
    return list_begin(&map->entries);
}

struct ListHeader* map_end(struct Map* map) {
    return list_end(&map->entries);
}

void* map_entry_key(struct MapEntry* map_entry) { return map_entry->key; }

void* map_entry_value(struct MapEntry* map_entry) { return map_entry->value; }

void map_entry_set_value(struct MapEntry* map_entry, void* value) {
    map_entry->value = value;
}
