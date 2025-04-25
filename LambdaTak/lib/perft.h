#include "moves.h"
#include "board.h"
#include <time.h>
#include "tps.h"
#include "../ai/neuralNetworks.h"
#include "../ai/eval.h"

typedef struct {
    MoveList** moves;
    int numLists;
} MoveListList;

u64 perft(GameState* state, int depth, int currentDepth, u64 nodes, MoveListList* moves);

void runPerft(GameState* state, int maxDepth);

