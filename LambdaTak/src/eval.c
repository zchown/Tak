#include "eval.h"

int evaluate(GameState* state) {
    int whiteCount = __builtin_popcountll(state->whiteControlled);
    int blackCount = __builtin_popcountll(state->blackControlled);
    return whiteCount - blackCount;
}
