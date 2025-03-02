# ifndef SEARCHES_H
# define SEARCHES_H
#include "board.h"
#include "moves.h"
#include "eval.h"
#include <time.h>
#include <math.h>

#define MAX_DEPTH 64
#define KILLER_MOVES 2
#include <math.h>

#define DEFAULT_UCT_CONSTANT 1.41421356237  // sqrt(2)
#define MAX_MCTS_ITERATIONS 1000000
#define MIN_PLAYOUTS_PER_NODE 10

extern Move killerMoves[MAX_DEPTH][KILLER_MOVES];
extern int historyHeuristic[NUM_COLORS][TOTAL_SQUARES][TOTAL_SQUARES];

typedef struct SearchStatistics {
    int maxDepth;
    int totalNodes;
    u64 generatedMoves;
    int timeLimit;
    int transpositionHits;
    int transpositionMisses;
    int transpositionDepthRewrites;
    int transpositionCollisions;
    int transpositionCutOffs;
    int transpositionFill;
    int alphaBetaCutoffs;
    int failHighResearches;
    int transpositionTableUpdates;
    int transpositionLookups;
    int reducedDepthSearches;
} SearchStatistics;

typedef struct MCTSNode {
    ZobristKey hash;
    Move move;
    int visits;
    double score;
    int numChildren;
    struct MCTSNode** children;
    struct MCTSNode* parent;
    bool fullyExpanded;
    Color originalPlayer;
} MCTSNode;

Move iterativeDeepeningSearch(GameState* state, int timeLimit);

Move negaMaxRoot(GameState* state, int depth, bool* timeUp, double startTime, int timeLimit, SearchStatistics* stats);

int negaMax(GameState* state, int depth, int alpha, int beta, int color, bool* timeUp, double startTime, int timeLimit, u32 prevMoves, bool doReducedDepth, SearchStatistics* stats);

Move monteCarloTreeSearch(GameState* state, int timeLimit);

static MCTSNode* getBestChildVisits(MCTSNode* node);

static double getTimeMs();

u32 zobristToIndex(ZobristKey hash);
const TranspositionEntry* lookupTranspositionTable(ZobristKey hash, int depth, int alpha, int beta, SearchStatistics* stats);
void updateTranspositionTable(ZobristKey hash, int score, EstimationType type, Move move, int depth, SearchStatistics* stats);

int scoreMove(const GameState* state, const Move* move, const Move* bestMove);
int compareMoves(const GameState* state, const Move* a, const Move* b, const Move* bestMove);

void sortMoves(GameState* state, Move* moves, int numMoves);
void clearKillerMoves(void);
void clearHistoryHeuristic(void);
void clearTranspositionTable(void);

void printSearchStats(const SearchStatistics* stats);

#endif // SEARCHES_H
