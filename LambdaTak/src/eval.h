#include "board.h"
#include "moves.h"
#include <limits.h>

#define WHITE_ROAD_WIN INT_MAX
#define WHITE_FLAT_WIN INT_MAX - 100
#define BLACK_ROAD_WIN -INT_MAX
#define BLACK_FLAT_WIN -INT_MAX + 100
#define DRAW_SCORE 0

static const int centrality[TOTAL_SQUARES] = {
    0, 3, 3, 3, 3, 0,
    3, 12, 8, 8, 12, 3,
    3, 8, 18, 18, 8, 3,
    3, 8, 18, 18, 8, 3,
    3, 12, 8, 8, 12, 3,
    0, 3, 3, 3, 3, 0
};

#define ROW_COL_BONUS 5
#define ENCOURAGE_PLACEMENT 10
#define PATH_BONUS 50
#define WALL_BONUS 40
#define PRISONER_BONUS 0
#define RESERVE_BONUS 8
#define CENTRALITY_BONUS 15
#define FLAT_SCORE 1000
#define CONTROL_BONUS 22
#define STACK_AT_RISK 18
#define BUDDY_BONUS 30
#define THREAT_BONUS 16
#define PROTECTION_BONUS 16
#define SQUARE_AT_RISK 25
#define IMMOBILITY_PENALTY 25


#define WHITE_FLATS(state) (state->whiteControlled & ~state->standingStones)
#define BLACK_FLATS(state) (state->blackControlled & ~state->standingStones)

#define ROW_COL_WALLS(state, rowcol) (__builtin_popcountll(state->standingStones & rowcol))

#define FLAT_DIFF(state) (__builtin_popcountll(WHITE_FLATS(state)) - __builtin_popcountll(BLACK_FLATS(state)))

#define CONTROL_CALCULATION(state) (__builtin_popcountll(state->whiteControlled) * CONTROL_BONUS - __builtin_popcountll(state->blackControlled) * CONTROL_BONUS)

#define GET_NEIGHBORS(pos) (1ULL << (pos - 1) | 1ULL << (pos + 1) | 1ULL << (pos - 6) | 1ULL << (pos + 6))

int evaluate(GameState* state);

int calculateFlatDiff(GameState* state);
int calculateLongRowCol(GameState* state);
int squareLoop(GameState* state);
int calculateLongestDFS(GameState* state, Color player, Bitboard visited, int pos, int depth);
int calculatePathScore(GameState* state);
int connectivityIndex(GameState* state);
