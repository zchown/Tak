#ifndef MONTECARLOTABLE_H
#define MONTECARLOTABLE_H

#include "../lib/board.h"
#include "../lib/moves.h"
#include "monteCarloGraphSearch.h"

typedef struct MonteCarloTableEntry {
    ZobristKey hash;
    MCGSNode* node;
    struct MonteCarloTableEntry* next;
} MonteCarloTableEntry;

typedef struct MonteCarloTable {
    MonteCarloTableEntry* entries;
    int size;
} MonteCarloTable;

MonteCarloTable* createMonteCarloTable(void);
void freeMonteCarloTable(MonteCarloTable* table);

MonteCarloTableEntry* createMonteCarloTableEntry(ZobristKey hash, MCGSNode* node);
void freeMonteCarloTableEntry(MonteCarloTableEntry* entry);

MonteCarloTableEntry* lookupMonteCarloTable(MonteCarloTable* table, ZobristKey hash);
void updateMonteCarloTable(MonteCarloTable* table, ZobristKey hash, MCGSNode* node);

u32 zobristToIndex(ZobristKey hash);

#endif // MONTECARLOTABLE_H
