#ifndef ARENA_ALLOCATOR_H
#define ARENA_ALLOCATOR_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
    void* memory;
    size_t size;
    size_t used;
} ArenaAllocator;

ArenaAllocator createArenaAllocator(size_t size);

void* allocate(ArenaAllocator* allocator, size_t size);

void resetArena(ArenaAllocator* allocator);

void freeArena(ArenaAllocator* allocator);

#endif // ARENA_ALLOCATOR_H
