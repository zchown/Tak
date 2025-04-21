#ifndef ARENA_ALLOCATOR_H
#define ARENA_ALLOCATOR_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
    void* memory;
    uint_fast64_t size;
    uint_fast64_t used;
} ArenaAllocator;

ArenaAllocator createArenaAllocator(uint_fast64_t size);

void* allocate(ArenaAllocator* allocator, size_t size);

void resetArena(ArenaAllocator* allocator);

void freeArena(ArenaAllocator* allocator);

#endif // ARENA_ALLOCATOR_H
