#include "moves.h"
#include "board.h"
#include "searches.h"
#include <time.h>
#include "tps.h"

u64 perft(GameState* state, int depth, int currentDepth, u64 nodes);

void runPerft(GameState* state, int maxDepth);

