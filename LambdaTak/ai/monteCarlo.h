#ifndef MONTECARLO_H
#define MONTECARLO_H
#include "../lib/board.h"
#include "../lib/moves.h"
#include "searches.h"
#include "neuralNetworks.h"
#include "neuralNetTrainer.h"

typedef struct MCTSNode {
    int numVisits;
    Color toPlay;
    double prior; // Prior probability of selecting this node
    struct MCTSNode* parent;
    struct MCTSNode** children;
    u32 numChildren;
    double valueSum;
    Move move;
} MCTSNode;

#define DEFAULT_UCT_CONSTANT 1.41421356237  // sqrt(2)
#define MAX_MCTS_ITERATIONS 1000
#define MIN_PLAYOUTS_PER_NODE 5
#define MAX_TURNS 5

#define MCTSNODE_VALUE(node) ((node)->valueSum / (double)(node)->numVisits)
#define MCTSNODE_EXPANDED(node) ((node)->numChildren > 0)

Move monteCarloTreeSearch(GameState* state, int timeLimit, DenseNeuralNet* net);

// Functions that use make/unmake for efficiency
MCTSNode* selectNodeWithMakeUnmake(MCTSNode* node, GameState* state, DenseNeuralNet* net);
void restoreToRoot(MCTSNode* node, GameState* state);
double simulateWithMakeUnmake(GameState* state, DenseNeuralNet* net);

// Expanded version for efficiency
MCTSNode* expand(MCTSNode* node, GameState* state, double prior, DenseNeuralNet* net);
void backup(MCTSNode* node, double value);

// Helper function for batch evaluations
double* batchEvaluateWithNN(double** inputs, int numInputs, DenseNeuralNet* net);

double ucbScore(MCTSNode* parent, MCTSNode* child);

MCTSNode* createMCTSNode(Color toPlay, MCTSNode* parent, double prior, Move move);
void freeMCTSNode(MCTSNode* node);

#pragma inline
static double getTimeMs();

#endif // MONTECARLO_H
