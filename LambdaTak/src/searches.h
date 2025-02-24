# ifndef SEARCHES_H
# define SEARCHES_H
#include "board.h"
#include "moves.h"
#include "eval.h"
#include <time.h>

Move negaMaxRoot(GameState* state, u8 depth, u64* nodes);

int negaMax(GameState* state, u8 depth, int alpha, int beta, int color, u64* nodes);

Move iterativeDeepeningSearch(GameState* state, u8 maxDepth, u64* nodes, int timeLimit);


Move negaMaxRootID(GameState* state, u8 depth, u64* nodes, bool* timeUp, double startTime, int timeLimit);

int negaMaxID(GameState* state, u8 depth, int alpha, int beta, int color, u64* nodes, bool* timeUp, double startTime, int timeLimit);

void sortMovesByHistory(Move* moves, u32 count, int* moveHistory);

int getMoveIndex(Move* move);

#endif // SEARCHES_H
