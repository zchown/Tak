#include "eval.h"

int evaluate(GameState* state) {
    if (state->result != CONTINUE) {
        switch (state->result) {
            case ROAD_WHITE: return WHITE_ROAD_WIN;
            case ROAD_BLACK: return BLACK_ROAD_WIN;
            case FLAT_WHITE: return WHITE_FLAT_WIN;
            case FLAT_BLACK: return BLACK_FLAT_WIN;
            case DRAW: return DRAW_SCORE;
            default: return 0;
        }
    }

    int score = FLAT_DIFF(state) * FLAT_SCORE;

    u64 rowMasks[] = {ROW1, ROW2, ROW3, ROW4, ROW5, ROW6};
    u64 colMasks[] = {COLA, COLB, COLC, COLD, COLE, COLF};
    for (int i = 0; i < BOARD_SIZE; i++) {
        u64 whiteRow = WHITE_FLATS(state) & rowMasks[i];
        u64 blackRow = BLACK_FLATS(state) & rowMasks[i];
        int diff = __builtin_popcountll(whiteRow) - __builtin_popcountll(blackRow);
        score += diff * ROW_COL_BONUS;

        u64 whiteCol = WHITE_FLATS(state) & colMasks[i];
        u64 blackCol = BLACK_FLATS(state) & colMasks[i];
        diff = __builtin_popcountll(whiteCol) - __builtin_popcountll(blackCol);
        score += diff * ROW_COL_BONUS;
    }

    int wallDiff = (__builtin_popcountll(state->whiteControlled & state->standingStones) -
                   __builtin_popcountll(state->blackControlled & state->standingStones));
    score += wallDiff * WALL_BONUS;

    for (int pos = 0; pos < TOTAL_SQUARES; pos++) {
        Square* square = &state->board->squares[pos];
        if (state->whiteControlled & (1ULL << pos)) {
            score += square->numPieces * STACK_BONUS;
        } else if (state->blackControlled & (1ULL << pos)) {
            score -= square->numPieces * STACK_BONUS;
        }
    }

    for (int pos = 0; pos < TOTAL_SQUARES; pos++) {
        if (state->whiteControlled & (1ULL << pos)) {
            score += centrality[pos] * CENTRALITY_BONUS;
        } else if (state->blackControlled & (1ULL << pos)) {
            score -= centrality[pos] * CENTRALITY_BONUS;
        }
    }

    return score;
}
