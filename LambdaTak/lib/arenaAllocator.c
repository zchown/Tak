#include "arenaAllocator.h"

ArenaAllocator createArenaAllocator(size_t size) {
    ArenaAllocator allocator;
    allocator.memory = malloc(size);
    if (!allocator.memory) {
        printf("Failed to allocate memory for arena allocator\n");
        exit(EXIT_FAILURE);
    }
    allocator.size = size;
    allocator.used = 0;
    return allocator;
}

void* allocate(ArenaAllocator* allocator, size_t size) {
    if (allocator->used + size > allocator->size) {
        printf("Arena allocator out of memory\n");
        return NULL;
    }
    void* ptr = (char*)allocator->memory + allocator->used;
    allocator->used += size;

    // zero out the allocated memory
    memset(ptr, 0, size);
    return ptr;
}

void resetArena(ArenaAllocator* allocator) {
    allocator->used = 0;
}

void freeArena(ArenaAllocator* allocator) {
    free(allocator->memory);
    allocator->memory = NULL;
    allocator->size = 0;
    allocator->used = 0;
}
