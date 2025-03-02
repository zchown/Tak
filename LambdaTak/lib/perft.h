#include "moves.h"
#include "board.h"
#include <time.h>
#include "tps.h"

u64 perft(GameState* state, int depth, int currentDepth, u64 nodes, u32 prevMoves);

void runPerft(GameState* state, int maxDepth);

