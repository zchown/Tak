#include "transposition.h"

TranspositionTable* createTranspositionTable(void) {
    TranspositionTable* table = calloc(1, sizeof(TranspositionTable));
    table->entries = calloc(TRANSPOSITION_TABLE_SIZE, sizeof(TranspositionEntry));
    return table;
}

void freeTranspositionTable(TranspositionTable* table) {
    free(table->entries);
    free(table);
}

#pragma inline
TranspositionEntry* lookupTranspositionTable(TranspositionTable* table, ZobristKey hash) {
    table->stats.lookups++;
    u32 index = zobristToIndex(hash);

    for (int i = 0; i < 8; i++) {
        u32 probe = (index + i) & (table->size - 1);
        TranspositionEntry* te = &table->entries[probe];

        if (te->hash == hash) {  
            table->stats.hits++;
            return te;
        } else if (te->hash == 0) {
            break;
        }
    }
    table->stats.misses++;
    return NULL;

}

#pragma inline
void updateTranspositionTable(TranspositionTable* table, ZobristKey hash, int score, EstimationType type, Move move, int depth) {
    u32 index = zobristToIndex(hash);
    table->stats.updates++;

    u32 lowest_depth_index = index;
    int lowest_depth = table->entries[index].depth;

    for (int i = 0; i < 8; i++) {
        u32 probe = (index + i) & (TRANSPOSITION_TABLE_SIZE - 1);
        TranspositionEntry* te = &table->entries[probe];

        if (te->hash == 0) {
            table->stats.fill++;
            te->hash = hash;
            te->score = score;
            te->type = type;
            te->move = move;
            te->depth = depth;
            return;
        } else if ((te->hash == hash) && (te->depth < depth)) {
            table->stats.depthRewrites++;
            te->score = score;
            te->type = type;
            te->move = move;
            te->depth = depth;
            return;
        } else if (te->depth < lowest_depth) {
            lowest_depth = te->depth;
            lowest_depth_index = probe;
        }
    }
    table->stats.collisions++;

    if (depth > lowest_depth) {
        table->stats.depthRewrites++;
        TranspositionEntry* te = &table->entries[lowest_depth_index];
        te->hash = hash;
        te->score = score;
        te->type = type;
        te->move = move;
        te->depth = depth;
    } 
}

void clearTranspositionTable(TranspositionTable* table) {
    printf("Clearing transposition table\n");
    for (int i = 0; i < table->size; i++) {
        TranspositionEntry* te = &table->entries[i];
        te->hash = 0;
        te->depth = 0;
    }
    table->stats.hits = 0;
    table->stats.misses = 0;
    table->stats.depthRewrites = 0;
    table->stats.collisions = 0;
    table->stats.updates = 0;
    table->stats.lookups = 0;
}

#pragma inline 
u32 zobristToIndex(ZobristKey hash) {
    return (u32)(hash & (TRANSPOSITION_TABLE_SIZE - 1));
}

