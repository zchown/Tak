# ifndef SEARCHES_H
# define SEARCHES_H
#include "board.h"
#include "moves.h"
#include "eval.h"
#include <time.h>

Move iterativeDeepeningSearch(GameState* state, u8 maxDepth, u64* nodes, int timeLimit);


Move negaMaxRoot(GameState* state, u8 depth, u64* nodes, bool* timeUp, double startTime, int timeLimit);

int negaMax(GameState* state, u8 depth, int alpha, int beta, int color, u64* nodes, bool* timeUp, double startTime, int timeLimit);


#endif // SEARCHES_H
