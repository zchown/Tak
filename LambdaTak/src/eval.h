#include "board.h"
#include "moves.h"

#define WHITE_ROAD_WIN 10000000 // 100_000_000
#define WHITE_FLAT_WIN 9000000 // 90_000_000
#define BLACK_ROAD_WIN -100000000 //-100_000_000
#define BLACK_FLAT_WIN -90000000 //-90_000_000
#define DRAW_SCORE 0

static const int centrality[TOTAL_SQUARES] = {
    0, 3, 1, 1, 3, 0,
    3, 12, 8, 8, 12, 3,
    1, 8, 18, 18, 8, 1,
    1, 8, 18, 18, 8, 1,
    3, 12, 8, 8, 12, 3,
    0, 3, 1, 1, 3, 0
};

#define ROW_COL_BONUS 225
#define WALL_BONUS 250
#define PRISONER_BONUS 1
#define RESERVE_BONUS 15
#define CENTRALITY_BONUS 125
#define FLAT_SCORE 2000
#define CONTROL_BONUS 55
#define STACK_AT_RISK 125
#define BUDDY_BONUS 112
#define THREAT_BONUS 30
#define PROTECTION_BONUS 20
#define SQUARE_AT_RISK 40
#define IMMOBILITY_PENALTY 50


#define WHITE_FLATS(state) (state->whiteControlled & ~state->standingStones)
#define BLACK_FLATS(state) (state->blackControlled & ~state->standingStones)

#define ROW_COL_WALLS(state, rowcol) (__builtin_popcountll(state->standingStones & rowcol))

#define FLAT_DIFF(state) (__builtin_popcountll(WHITE_FLATS(state)) - __builtin_popcountll(BLACK_FLATS(state)))

#define CONTROL_CALCULATION(state) (__builtin_popcountll(state->whiteControlled) * CONTROL_BONUS - __builtin_popcountll(state->blackControlled) * CONTROL_BONUS)

#define SCORE_STABILIZATION(score, turn) if (turn == WHITE) { \
    if (score > 0) { \
        score = score * 0.8; \
    } \
} else { \
    if (score < 0) { \
        score = score * 0.8; \
    } \
}

#define GET_NEIGHBORS(pos) (1ULL << (pos - 1) | 1ULL << (pos + 1) | 1ULL << (pos - 6) | 1ULL << (pos + 6))

int evaluate(GameState* state);

int calculateFlatDiff(GameState* state);
int calculateLongRowCol(GameState* state);
int squareLoop(GameState* state);
