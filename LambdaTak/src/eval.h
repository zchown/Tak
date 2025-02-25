#include "board.h"
#include "moves.h"

#define WHITE_ROAD_WIN 1000000 //1_000_000
#define WHITE_FLAT_WIN 999999 //999_999
#define BLACK_ROAD_WIN -1000000 //-1_000_000
#define BLACK_FLAT_WIN -999999 //-999_999
#define DRAW_SCORE 0

static const int centrality[TOTAL_SQUARES] = {
    0, 1, 2, 2, 1, 0,
    1, 6, 4, 4, 6, 1,
    2, 4, 8, 8, 4, 2,
    2, 4, 8, 8, 4, 2,
    1, 6, 4, 3, 6, 1,
    0, 1, 2, 2, 1, 0
};

#define ROW_COL_BONUS 750
#define WALL_BONUS 600
#define PRISONER_BONUS 2
#define RESERVE_BONUS 20
#define CENTRALITY_BONUS 15
#define FLAT_SCORE 1000
#define CONTROL_BONUS 10


#define WHITE_FLATS(state) (state->whiteControlled & ~state->standingStones)
#define BLACK_FLATS(state) (state->blackControlled & ~state->standingStones)

#define FLAT_DIFF(state) (__builtin_popcountll(WHITE_FLATS(state)) - __builtin_popcountll(BLACK_FLATS(state)))



int evaluate(GameState* state);
