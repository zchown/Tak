# ifndef SEARCHES_H
# define SEARCHES_H
#include "../lib/board.h"
#include "../lib/moves.h"
#include "eval.h"
#include <time.h>
#include <math.h>

#define MAX_DEPTH 64
#define KILLER_MOVES 2
#include <math.h>

extern Move killerMoves[MAX_DEPTH][KILLER_MOVES];
extern int historyHeuristic[NUM_COLORS][TOTAL_SQUARES][TOTAL_SQUARES];
static const u32 TRANSPOSITION_TABLE_SIZE = (1 << 29);

typedef enum {UNDER, OVER, EXACT} EstimationType;

typedef struct {
    Move move;
    int score;
    int depth;
    EstimationType type;
    ZobristKey hash;
} TranspositionEntry;

extern TranspositionEntry* transpositionTable;

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

Move iterativeDeepeningSearch(GameState* state, int timeLimit);

Move negaMaxRoot(GameState* state, int depth, bool* timeUp, double startTime, int timeLimit, SearchStatistics* stats);

int negaMax(GameState* state, int depth, int alpha, int beta, int color, bool* timeUp, double startTime, int timeLimit, u32 prevMoves, bool doReducedDepth, SearchStatistics* stats);

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
