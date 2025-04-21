#ifndef MONTECARLOGRAPHSEARCH_H
#define MONTECARLOGRAPHSEARCH_H

// Monte Carlo Graph Search
// https://arxiv.org/pdf/2012.11045
// https://proceedings.mlr.press/v129/leurent20a/leurent20a.pdf
// https://github.com/lightvector/KataGo/blob/master/docs/GraphSearch.md
// https://medium.com/applied-data-science/alphago-zero-explained-in-one-diagram-365f5abf67e0

#include "../lib/board.h"
#include "../lib/moves.h"
#include "neuralNetworks.h"
#include <float.h>
#include <math.h>
#include "pythonTrainer.h"
#include "policyNetwork.h"
#include  "accelerateNeuralNet.h"

#define CPUCT (1.0) // exploration constant

#define MONTECARLO_TABLE_SIZE (1 << 25)

#define V_MIN (-1.0)
#define V_MAX (1.0)
#define Q_EPSILON (0.01)

extern GraphNN* graphN;

extern GraphNN* graphNN;

typedef enum {
    MC_WIN,
    MC_LOSS,
    MC_DRAW,
    MC_UNKNOWN
} NodeState;

typedef struct MCGSNode {
    struct MCGSNode* parent;
    struct MCGSEdge** edges;
    ZobristKey hash;
    NodeState state;
    int numEdges;
    int unknownChildren;
    int endInPly;
    int numVisits; // N
    double value; 
    bool isExpanded;
    bool isTerminal;
    bool isTransposition;
} MCGSNode;

typedef struct MCGSEdge {
    Move move;
    double q; // Q
    int n; // N
    MCGSNode* child;
    struct MCGSNode* target;
} MCGSEdge;

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
    MCGSNode** nodes;
    MCGSEdge** edges;
    int size;
    int capacity;
} Trajectory;

typedef struct SelectExpandResult {
    Trajectory trajectory;
    double value;
} SelectExpandResult;

typedef struct {
    int totalNodes;
    int totalEdges;
    int maxDepth;
    int totalVisits;
    int terminalNodesHit;
    int transpositionsFound;
    double averageValueEstimate;
    double executionTimeMs;
    int iterations;
} MCGSStats;

Move monteCarloGraphSearch(GameState* state, DenseNeuralNet* net, bool trainingMode, int sock, double* probs);

SelectExpandResult selectExpand(MonteCarloTable* table, GameState* state, DenseNeuralNet* net, MCGSNode* root, MCGSStats* stats, int sock, bool trainingMode);

void backPropagate(Trajectory* trajectory, double value, MCGSStats* stats);

MCGSEdge* selectBestEdge(MCGSNode* node);

Trajectory createTrajectory(int capacity);
void freeTrajectory(Trajectory* trajectory);
void addToTrajectory(Trajectory* trajectory, MCGSNode* node, MCGSEdge* edge);

MCGSNode* createMCGSNode(ZobristKey hash, MCGSNode* parent);
void freeMCGSNode(MCGSNode* node);

MonteCarloTable* createMonteCarloTable(void);
void freeMonteCarloTable(MonteCarloTable* table);

MonteCarloTableEntry* createMonteCarloTableEntry(ZobristKey hash, MCGSNode* node);
void freeMonteCarloTableEntry(MonteCarloTableEntry* entry);

MonteCarloTableEntry* lookupMonteCarloTable(MonteCarloTable* table, ZobristKey hash);
MonteCarloTableEntry* lookupAndCreate(MonteCarloTable* table, ZobristKey hash, MCGSNode* node);
void updateMonteCarloTable(MonteCarloTable* table, ZobristKey hash, MCGSNode* node);
void markAllAsUnused(MonteCarloTable* table, ZobristKey hash);

u32 mcZobristToIndex(ZobristKey hash);

MCGSStats createMonteCarloStats(void);
void printMCGSStats(MCGSStats* stats);
void printTopMoves(MCGSNode* root, int numMoves);

#endif // MONTECARLOGRAPHSEARCH_H
