# ifndef SEARCHES_H
# define SEARCHES_H
#include "../lib/board.h"
#include "../lib/moves.h"
#include "transposition.h"
#include "eval.h"
#include <time.h>
#include <math.h>

#define MAX_DEPTH 100
#define KILLER_MOVES 2
#include <math.h>

extern Move killerMoves[MAX_DEPTH][KILLER_MOVES];
extern int historyHeuristic[NUM_COLORS][TOTAL_SQUARES][TOTAL_SQUARES];
extern MoveList** moveLists;

extern TranspositionTable* transpositionTable;

typedef struct SearchStatistics {
    int maxDepth;
    int totalNodes;
    u64 generatedMoves;
    int timeLimit;
    int alphaBetaCutoffs;
    int transpositionCutOffs;
    int failHighResearches;
    int reducedDepthSearches;
    TranspositionStatistics ttStats;
} SearchStatistics;

Move iterativeDeepeningSearch(GameState* state, int timeLimit);

Move negaMaxRoot(GameState* state, int depth, bool* timeUp, double startTime, int timeLimit, SearchStatistics* stats);

int negaMax(GameState* state, int depth, int alpha, int beta, int color, bool* timeUp, double startTime, int timeLimit, u32 prevMoves, bool doReducedDepth, SearchStatistics* stats);

static double getTimeMs();

int scoreMove(const GameState* state, const Move* move, const Move* bestMove);
int compareMoves(const GameState* state, const Move* a, const Move* b, const Move* bestMove);

void sortMoves(GameState* state, Move* moves, int numMoves);
void clearKillerMoves(void);
void clearHistoryHeuristic(void);

void printSearchStats(const SearchStatistics* stats);

#endif // SEARCHES_H
