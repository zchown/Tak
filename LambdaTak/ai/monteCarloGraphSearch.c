#include "monteCarloGraphSearch.h"

Move monteCarloGraphSearch(GameState* state, DenseNeuralNet* net) {
    if (!monteCarloTable) {
        monteCarloTable = createMonteCarloTable();
    }
}

SelectExpandResult selectExpand(MonteCarloTable* table, GameState* state, DenseNeuralNet* net, MCGSNode* root, int depth) {
    SelectExpandResult result;
    /* result.trajectory =  */
    result.value = 0.0;

    MoveBuffer* moveBuffer = createMoveBuffer(64);

    MCGSNode* currentNode = root;

    return result;
}

Trajectory createTrajectory(int capacity) {
    Trajectory trajectory;
    trajectory.entry = malloc(capacity * sizeof(MonteCarloTableEntry*));
    trajectory.size = 0;
    trajectory.capacity = capacity;
    return trajectory;
}

void freeTrajectory(Trajectory* trajectory) {
    if (trajectory) {
        for (int i = 0; i < trajectory->size; i++) {
            freeMonteCarloTableEntry(trajectory->entry[i]);
        }
        free(trajectory->entry);
    }
}

void addToTrajectory(Trajectory* trajectory, MonteCarloTableEntry* entry) {
    if (trajectory->size < trajectory->capacity) {
        trajectory->entry[trajectory->size++] = entry;
    } else {
        trajectory->capacity *= 2;
        trajectory->entry = realloc(trajectory->entry, trajectory->capacity * sizeof(MonteCarloTableEntry*));
        if (trajectory->entry) {
            trajectory->entry[trajectory->size++] = entry;
        } else {
            printf("Failed to resize trajectory\n");
        }
    }
}

void clearTrajectory(Trajectory* trajectory) {
    for (int i = 0; i < trajectory->size; i++) {
        freeMonteCarloTableEntry(trajectory->entry[i]);
    }
    trajectory->size = 0;
}

MCGSNode* createMCGSNode(void) {
    MCGSNode* node = malloc(sizeof(MCGSNode));
    node->parent = NULL;
    node->children = NULL;
    node->numChildren = 0;
    node->numVisits = 0;
    node->valueSum = 0.0;
    node->expand = false;
    return node;
}

void freeMCGSNode(MCGSNode* node) {
    if (node) {
        for (int i = 0; i < node->numChildren; i++) {
            freeMCGSNode(node->children[i]);
        }
        free(node->children);
        free(node);
    }
}

MonteCarloTable* createMonteCarloTable(void) {
    MonteCarloTable* table = malloc(sizeof(MonteCarloTable));
    table->entries = calloc(MONTECARLO_TABLE_SIZE, sizeof(MonteCarloTableEntry));
    table->size = MONTECARLO_TABLE_SIZE;
    return table;
}

void freeMonteCarloTable(MonteCarloTable* table) {
    for (int i = 0; i < table->size; i++) {
        MonteCarloTableEntry* entry = &table->entries[i];
        if (entry->node) {
            free(entry->node);
        }
    }
    free(table->entries);
    free(table);
}

MonteCarloTableEntry* createMonteCarloTableEntry(ZobristKey hash, MCGSNode* node) {
    MonteCarloTableEntry* entry = malloc(sizeof(MonteCarloTableEntry));
    entry->isUsed = false;
    entry->hash = hash;
    entry->node = node;
    entry->next = NULL;
    return entry;
}

void freeMonteCarloTableEntry(MonteCarloTableEntry* entry) {
    if (entry) {
        free(entry->node);
        free(entry);
    }
}

MonteCarloTableEntry* lookupMonteCarloTable(MonteCarloTable* table, ZobristKey hash) {
    u32 index = zobristToIndex(hash);
    MonteCarloTableEntry* entry = &table->entries[index];

    while (entry) {
        if (entry->hash == hash && !entry->isUsed) {
            return entry;
        }
        entry = entry->next;
    }
    return NULL;
}

MonteCarloTableEntry* lookupAndCreate(MonteCarloTable* table, ZobristKey hash, MCGSNode* node) {
    u32 index = zobristToIndex(hash);
    MonteCarloTableEntry* entry = &table->entries[index];

    if (entry->hash == 0) {
        entry->hash = hash;
        entry->node = node;
        return entry;
    }

    while (entry->next) {
        if (entry->hash == hash && !entry->isUsed) {
            return entry;
        }
        entry = entry->next;
    }

    if (entry->hash == hash && !entry->isUsed) {
        return entry;
    }

    MonteCarloTableEntry* newEntry = createMonteCarloTableEntry(hash, node);
    entry->next = newEntry;
    return newEntry;
}

void updateMonteCarloTable(MonteCarloTable* table, ZobristKey hash, MCGSNode* node) {
    u32 index = zobristToIndex(hash);
    MonteCarloTableEntry* entry = &table->entries[index];

    if (entry->hash == 0) {
        entry->hash = hash;
        entry->node = node;
    } else {
        while (entry->next) {
            entry = entry->next;
        }
        entry->next = createMonteCarloTableEntry(hash, node);
    }
}

u32 zobristToIndex(ZobristKey hash) {
    return (u32)(hash & (MONTECARLO_TABLE_SIZE - 1));
}

MoveBuffer* createMoveBuffer(int capacity) {
    MoveBuffer* buffer = malloc(sizeof(MoveBuffer));
    buffer->moves = malloc(capacity * sizeof(Move));
    buffer->size = 0;
    buffer->capacity = capacity;
    return buffer;
}

void freeMoveBuffer(MoveBuffer* buffer) {
    if (buffer) {
        free(buffer->moves);
        free(buffer);
    }
}

#pragma inline
void addMoveToBuffer(MoveBuffer* buffer, Move move) {
    if (buffer->size < buffer->capacity) {
        buffer->moves[buffer->size++] = move;
    } else {
        buffer->capacity *= 2;
        buffer->moves = realloc(buffer->moves, buffer->capacity * sizeof(Move));
        if (buffer->moves) {
            buffer->moves[buffer->size++] = move;
        } else {
            printf("Failed to resize move buffer\n");
        }
    }
}

void clearMoveBuffer(MoveBuffer* buffer) {
    buffer->size = 0;
}

