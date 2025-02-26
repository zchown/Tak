# ifndef SEARCHES_H
# define SEARCHES_H
#include "board.h"
#include "moves.h"
#include "eval.h"
#include <time.h>

#define MAX_DEPTH 64
#define KILLER_MOVES 2

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
    int alphaBetaCutoffs;
} SearchStatistics;

Move iterativeDeepeningSearch(GameState* state, int timeLimit);

Move negaMaxRoot(GameState* state, int depth, bool* timeUp, double startTime, int timeLimit, SearchStatistics* stats);

int negaMax(GameState* state, int depth, int alpha, int beta, int color, bool* timeUp, double startTime, int timeLimit, u32 prevMoves, SearchStatistics* stats);

static double getTimeMs();

u32 zobristToIndex(ZobristKey hash);
void updateTranspositionTable(ZobristKey hash, int score, EstimationType type, Move move, int depth, SearchStatistics* stats);

int scoreMove(const GameState* state, const Move* move, const Move* bestMove);
int compareMoves(const GameState* state, const Move* a, const Move* b, const Move* bestMove);

void sortMoves(GameState* state, Move* moves, int numMoves);
void clearKillerMoves(void);
void clearHistoryHeuristic(void);
void clearTranspositionTable(void);

void printSearchStats(const SearchStatistics* stats);

#endif // SEARCHES_H
