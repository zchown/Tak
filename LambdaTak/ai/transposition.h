# ifndef TRANSPOSITION_H
# define TRANSPOSITION_H

#include "../lib/board.h"
#include "../lib/moves.h"

typedef enum {UNDER, OVER, EXACT} EstimationType;
static const u32 TRANSPOSITION_TABLE_SIZE = (1 << 24);

typedef struct {
    Move move;
    int score;
    int depth;
    EstimationType type;
    ZobristKey hash;
} TranspositionEntry;

typedef struct {
    int hits;
    int misses;
    int depthRewrites;
    int collisions;
    int updates;
    int lookups;
    float fill;
} TranspositionStatistics;

typedef struct {
    TranspositionEntry* entries;
    TranspositionStatistics stats;
    int size;
} TranspositionTable;

TranspositionTable* createTranspositionTable(void);
void freeTranspositionTable(TranspositionTable* table);

TranspositionEntry* lookupTranspositionTable(TranspositionTable* table, ZobristKey hash);

void updateTranspositionTable(TranspositionTable* table, ZobristKey hash, int score, EstimationType type, Move move, int depth);

u32 zobristToIndex(ZobristKey hash);

# endif // TRANSPOSITION_H
