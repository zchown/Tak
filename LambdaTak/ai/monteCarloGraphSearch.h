#ifndef MONTECARLOGRAPHSEARCH_H
#define MONTECARLOGRAPHSEARCH_H

// Monte Carlo Graph Search
// https://arxiv.org/pdf/2012.11045
// https://proceedings.mlr.press/v129/leurent20a/leurent20a.pdf
// https://github.com/lightvector/KataGo/blob/master/docs/GraphSearch.md

#include "../lib/board.h"
#include "../lib/moves.h"
#include "neuralNetworks.h"

#define PUCT_CONSTANT 1.0
#define MONTECARLO_TABLE_SIZE (1 << 26)

typedef struct MCGSNode {
    struct MCGSNode* parent;
    struct MCGSNode** children;
    int numChildren;
    int numVisits;
    double valueSum;
    bool expand;
} MCGSNode;

typedef struct MonteCarloTableEntry {
    bool isUsed; //used to keep graph acyclic
    ZobristKey hash;
    MCGSNode* node;
    struct MonteCarloTableEntry* next;
} MonteCarloTableEntry;

typedef struct MonteCarloTable {
    MonteCarloTableEntry* entries;
    int size;
} MonteCarloTable;

static MonteCarloTable* monteCarloTable = NULL;

typedef struct TrajectoryNode {
    MonteCarloTableEntry* entry;
    struct TrajectoryNode* parent;
    struct TrajectoryNode* child;
    int depth;
} TrajectoryNode;

typedef struct SelectExpandResult {
    TrajectoryNode* trajectory;
    double value;
} SelectExpandResult;

Move monteCarloGraphSearch(GameState* state, DenseNeuralNet* net);

SelectExpandResult selectExpand(MonteCarloTable* table, GameState* state, DenseNeuralNet* net, MCGSNode* root, int depth);

TrajectoryNode* appendTrajectoryNode(TrajectoryNode* parent, MonteCarloTableEntry* node);
void freeTrajectoryNode(TrajectoryNode* node);

MCGSNode* createMCGSNode(void);
void freeMCGSNode(MCGSNode* node);

MonteCarloTable* createMonteCarloTable(void);
void freeMonteCarloTable(MonteCarloTable* table);

MonteCarloTableEntry* createMonteCarloTableEntry(ZobristKey hash, MCGSNode* node);
void freeMonteCarloTableEntry(MonteCarloTableEntry* entry);

MonteCarloTableEntry* lookupMonteCarloTable(MonteCarloTable* table, ZobristKey hash);
MonteCarloTableEntry* lookupAndCreate(MonteCarloTable* table, ZobristKey hash, MCGSNode* node);
void updateMonteCarloTable(MonteCarloTable* table, ZobristKey hash, MCGSNode* node);

u32 zobristToIndex(ZobristKey hash);

#endif // MONTECARLOGRAPHSEARCH_H
