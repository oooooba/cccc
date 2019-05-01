#ifndef MAP_H
#define MAP_H

#include "list.h"

struct Map {
    struct List entries;
};

void map_initialize(struct Map* map);
void map_insert(struct Map* map, void* key, void* value);
void* map_find(struct Map* map, void* key);

#endif
