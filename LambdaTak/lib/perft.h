#include "moves.h"
#include "board.h"
#include <time.h>
#include "tps.h"
#include "../ai/neuralNetworks.h"
#include "../ai/eval.h"

u64 perft(GameState* state, int depth, int currentDepth, u64 nodes, u32 prevMoves);

void runPerft(GameState* state, int maxDepth);

