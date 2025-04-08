#include "monteCarloGraphSearch.h"

Move monteCarloGraphSearch(GameState* state, DenseNeuralNet* net) {
    if (!monteCarloTable) {
        monteCarloTable = createMonteCarloTable();
    }
}




TrajectoryNode* appendTrajectoryNode(TrajectoryNode* parent, MonteCarloTableEntry* node) {
    TrajectoryNode* newNode = malloc(sizeof(TrajectoryNode));
    newNode->entry = node;
    newNode->parent = parent;
    newNode->child = NULL;
    newNode->depth = parent ? parent->depth + 1 : 0;
    if (parent) {
        parent->child = newNode;
    }
    return newNode;
}

void freeTrajectoryNode(TrajectoryNode* node) {
    if (node) {
        freeTrajectoryNode(node->child);
        free(node);
    }
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
