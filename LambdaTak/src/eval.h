#include "board.h"
#include "moves.h"

#define WHITE_ROAD_WIN 1000000 //1_000_000
#define WHITE_FLAT_WIN 999999 //999_999
#define BLACK_ROAD_WIN -1000000 //-1_000_000
#define BLACK_FLAT_WIN -999999 //-999_999
#define DRAW_SCORE 0

static const int centrality[TOTAL_SQUARES] = {
    1, 2, 2, 2, 2, 1,
    2, 4, 3, 3, 4, 2,
    2, 3, 6, 6, 3, 2,
    2, 3, 6, 6, 3, 2,
    2, 4, 3, 3, 4, 2,
    1, 2, 2, 2, 2, 1
};

#define ROW_COL_BONUS 500
#define WALL_BONUS 600
#define STACK_BONUS 1
#define CENTRALITY_BONUS 15
#define FLAT_SCORE 1000


#define WHITE_FLATS(state) (state->whiteControlled & ~state->standingStones)
#define BLACK_FLATS(state) (state->blackControlled & ~state->standingStones)

#define FLAT_DIFF(state) (__builtin_popcountll(WHITE_FLATS(state)) - __builtin_popcountll(BLACK_FLATS(state)))



int evaluate(GameState* state);
