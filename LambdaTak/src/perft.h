#include "moves.h"
#include "board.h"
#include <time.h>

u64 perft(GameState* state, int depth, int currentDepth, u64 nodes);

void runPerft(GameState* state, int maxDepth);

