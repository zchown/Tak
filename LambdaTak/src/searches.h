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

u32 zobristToIndex(ZobristKey hash);
static double getTimeMs();
void updateTranspositionTable(ZobristKey hash, int score, EstimationType type, Move move, int depth);
int scoreMove(const GameState* state, const Move* move, const Move* bestMove);
int compareMoves(const GameState* state, const Move* a, const Move* b, const Move* bestMove);
void sortMoves(GameState* state, Move* moves, int numMoves);

Move iterativeDeepeningSearch(GameState* state, u64* nodes, int timeLimit);

Move negaMaxRoot(GameState* state, int depth, u64* nodes, bool* timeUp, double startTime, int timeLimit);

int negaMax(GameState* state, int depth, int alpha, int beta, int color, u64* nodes, bool* timeUp, double startTime, int timeLimit, u32 prevMoves);


#endif // SEARCHES_H
