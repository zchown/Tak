#ifndef MONTECARLO_H
#define MONTECARLO_H
#include "../lib/board.h"
#include "../lib/moves.h"
#include "qlearner.h"
#include "searches.h"
#include "utils.h"

typedef struct MCTSNode {
    int numVisits;
    Color toPlay;
    Color originalToPlay;
    double prior; // Prior probability of selecting this node
    struct MCTSNode* parent;
    struct MCTSNode** children;
    u32 numChildren;
    double valueSum;
    Move move;
} MCTSNode;

/* #define DEFAULT_UCT_CONSTANT 1.41421356237  // sqrt(2) */
#define DEFAULT_UCT_CONSTANT 5.0
#define MAX_MCTS_ITERATIONS 5000
#define MIN_PLAYOUTS_PER_NODE 5
#define MAX_TURNS 100

#define MCTSNODE_VALUE(node) ((node)->valueSum / (double)(node)->numVisits)
#define MCTSNODE_EXPANDED(node) ((node)->numChildren > 0)

Move monteCarloTreeSearch(GameState* state, int timeLimit, QLearningAgent* agent);

MCTSNode* selectNode(MCTSNode* node, GameState* state, QLearningAgent* agent);
MCTSNode* expand(MCTSNode* node, GameState* state, double prior, QLearningAgent* agent);
void backup(MCTSNode* node, double value);
double simulate(GameState* state, QLearningAgent* agent);

double ucbScore(MCTSNode* parent, MCTSNode* child);

MCTSNode* createMCTSNode(Color toPlay, MCTSNode* parent, double prior, Move move);
void freeMCTSNode(MCTSNode* node);

#endif // MONTECARLO_H
