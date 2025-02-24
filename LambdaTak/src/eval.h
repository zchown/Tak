#include "board.h"
#include "moves.h"

#define WHITE_ROAD_WIN 1000000
#define WHITE_FLAT_WIN 999999
#define BLACK_ROAD_WIN -1000000
#define BLACK_FLAT_WIN -999999
#define DRAW_SCORE 0


int evaluate(GameState* state);
