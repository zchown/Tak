#ifndef MONTECARLO_H
#define MONTECARLO_H
#include "../lib/board.h"
#include "../lib/moves.h"
#include "searches.h"
#include "neuralNetworks.h"
#include <stdbool.h>

// Evaluation type to represent different node states
typedef enum {
    EVAL_VALUE,
    EVAL_WIN,
    EVAL_LOSS,
    EVAL_DRAW
} EvalType;

typedef struct {
    EvalType type;
    double value;
} Eval;

// Forward result enum for the forward pass
typedef enum {
    FORWARD_KNOWN_EVAL,
    FORWARD_NEEDS_NETWORK
} ForwardResult;

typedef struct {
    Eval eval;
    double variance;
} Propagated;

typedef struct MCTSNode {
    int numVisits;
    Color toPlay;
    double prior;
    struct MCTSNode* parent;
    struct MCTSNode** children;
    u32 numChildren;
    double valueSum;
    double variance;
    bool isSolved;
    Eval evaluation;
    Move move;
} MCTSNode;

#define DEFAULT_UCT_CONSTANT (1.41421356237) // sqrt(2)
#define MAX_MCTS_ITERATIONS 10000
#define MIN_PLAYOUTS_PER_NODE 1
#define MAX_MCTS_DEPTH 15
#define DISCOUNT_FACTOR 0.99

#define MCTSNODE_VALUE(node) ((node)->numVisits > 0 ? (node)->valueSum / (double)(node)->numVisits : 0.0)
#define MCTSNODE_EXPANDED(node) ((node)->numChildren > 0)

Move monteCarloTreeSearch(GameState* state, int timeLimit, DenseNeuralNet* net);

ForwardResult forward(MCTSNode* root, GameState* state, Move* moveStack, int* moveCount);
Propagated backwardKnownEval(MCTSNode* node, Move* moveStack, int moveCount, Eval eval);
Propagated backwardNetworkEval(MCTSNode* node, Move* moveStack, int moveCount, MCTSNode** expandedNodes, double* values, double* variances);

MCTSNode* selectNode(MCTSNode* node, GameState* state, DenseNeuralNet* net, Move* moveStack, int* moveCount);
MCTSNode* expand(MCTSNode* node, GameState* state, double prior, DenseNeuralNet* net);

double simulate(GameState* state, DenseNeuralNet* net);
Propagated backup(MCTSNode* node, double value, double variance);

void nodeSolver(MCTSNode* node);
bool isTerminal(MCTSNode* node);
bool needsInitialization(MCTSNode* node);

double ucbScore(MCTSNode* parent, MCTSNode* child);
double puctScore(MCTSNode* parent, MCTSNode* child, double beta);
double getAdaptiveCPuct(const GameState* state);

Eval negateEval(Eval eval);
Eval resultToEval(Result result);
double evalToValue(Eval eval);

MCTSNode* createMCTSNode(Color toPlay, MCTSNode* parent, double prior, Move move);
void freeMCTSNode(MCTSNode* node);
MCTSNode* selectBestUCB(MCTSNode* node);
int getAdaptiveDepth(double timeRemaining);

Move findDefensiveMove(GameState* originalState, Move threatMove);
bool detectImmediateThreats(GameState* state, Move* threatMove);

#pragma inline
static double getTimeMs();
int moveHeuristic(const GameState* state, const Move* move);
bool movesEqualMCTS(Move a, Move b);

#endif // MONTECARLO_H
