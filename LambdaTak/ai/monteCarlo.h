#ifndef MONTECARLO_H
#define MONTECARLO_H
#include "../lib/board.h"
#include "../lib/moves.h"
#include "searches.h"
#include "neuralNetworks.h"

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

#define DEFAULT_UCT_CONSTANT (2 * 1.41421356237) // sqrt(2) * 21.41421356237  // sqrt(2)
#define MAX_MCTS_ITERATIONS 10000
#define MIN_PLAYOUTS_PER_NODE 1
#define MAX_MCTS_DEPTH 20
#define MAX_TURNS 0

#define MCTSNODE_VALUE(node) ((node)->valueSum / (double)(node)->numVisits)
#define MCTSNODE_EXPANDED(node) ((node)->numChildren > 0)

Move monteCarloTreeSearch(GameState* state, int timeLimit, DenseNeuralNet* net);

MCTSNode* selectNode(MCTSNode* node, GameState* state, DenseNeuralNet* net, Move* moveStack, int* moveCount);

MCTSNode* expand(MCTSNode* node, GameState* state, double prior, DenseNeuralNet* net);

double simulate(GameState* state, DenseNeuralNet* net);

void backup(MCTSNode* node, double value);

double ucbScore(MCTSNode* parent, MCTSNode* child);

MCTSNode* createMCTSNode(Color toPlay, MCTSNode* parent, double prior, Move move);

void freeMCTSNode(MCTSNode* node);

#pragma inline
static double getTimeMs();

int moveHeuristic(const GameState* state, const Move* move);

#endif // MONTECARLO_H
