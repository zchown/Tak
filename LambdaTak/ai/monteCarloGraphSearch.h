#ifndef MONTECARLOGRAPHSEARCH_H
#define MONTECARLOGRAPHSEARCH_H

// Monte Carlo Graph Search
// https://arxiv.org/pdf/2012.11045
// https://proceedings.mlr.press/v129/leurent20a/leurent20a.pdf
// https://github.com/lightvector/KataGo/blob/master/docs/GraphSearch.md

#include "../lib/board.h"
#include "../lib/moves.h"
#include "neuralNetworks.h"

#define CPUCT = 1.0 // standard value from alphazero paper

#define MONTECARLO_TABLE_SIZE (1 << 26)

typedef struct MCGSNode {
    struct MCGSNode* parent;
    struct MCGSNode** children;
    int numChildren;
    int numVisits; // N
    double valueSum; // W
    // Q == N / W
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

typedef struct Trajectory {
    MonteCarloTableEntry** entry;
    int size;
    int capacity;
} Trajectory;

typedef struct SelectExpandResult {
    Trajectory trajectory;
    double value;
} SelectExpandResult;

typedef struct MoveBuffer {
    Move* moves;
    int size;
    int capacity;
} MoveBuffer;

Move monteCarloGraphSearch(GameState* state, DenseNeuralNet* net);

SelectExpandResult selectExpand(MonteCarloTable* table, GameState* state, DenseNeuralNet* net, MCGSNode* root, int depth);

Trajectory createTrajectory(int capacity);
void freeTrajectory(Trajectory* trajectory);
void addToTrajectory(Trajectory* trajectory, MonteCarloTableEntry* entry);
void clearTrajectory(Trajectory* trajectory);

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

MoveBuffer* createMoveBuffer(int capacity);
void freeMoveBuffer(MoveBuffer* buffer);
void addMoveToBuffer(MoveBuffer* buffer, Move move);
void clearMoveBuffer(MoveBuffer* buffer);

#endif // MONTECARLOGRAPHSEARCH_H
