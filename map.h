#ifndef MAP_H
#define MAP_H

#include "list.h"

struct Map {
    struct List entries;
};

struct MapEntry;

void map_initialize(struct Map* map);
void map_insert(struct Map* map, void* key, void* value);
void* map_find(struct Map* map, void* key);
struct ListHeader* map_begin(struct Map* map);
struct ListHeader* map_end(struct Map* map);
void* map_entry_key(struct MapEntry* map_entry);
void* map_entry_value(struct MapEntry* map_entry);
void map_entry_set_value(struct MapEntry* map_entry, void* value);

#endif
