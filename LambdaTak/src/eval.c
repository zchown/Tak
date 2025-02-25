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

    int score = 0;
    if (state->player1.stones < 20 || state->player2.stones < 20) {
        score += FLAT_DIFF(state) * FLAT_SCORE * 3;
    } else if (state->player1.stones < 15 || state->player2.stones < 15) {
        score += FLAT_DIFF(state) * FLAT_SCORE * 5;
    } else if (state->player1.stones < 10 || state->player2.stones < 10) {
        score += FLAT_DIFF(state) * FLAT_SCORE * 8;
    } else if (state->player1.stones < 5 || state->player2.stones < 5) {
        score += FLAT_DIFF(state) * FLAT_SCORE * 10;
    } else {
        score += FLAT_DIFF(state) * FLAT_SCORE;
    }

    u64 rowMasks[] = {ROW1, ROW2, ROW3, ROW4, ROW5, ROW6};
    u64 colMasks[] = {COLA, COLB, COLC, COLD, COLE, COLF};
    for (int i = 0; i < BOARD_SIZE; i++) {
        u64 whiteRow = WHITE_FLATS(state) & rowMasks[i];
        u64 blackRow = BLACK_FLATS(state) & rowMasks[i];
        int diff = __builtin_popcountll(whiteRow) - __builtin_popcountll(blackRow);
        if (diff > 0) {
            score += (diff * diff) * ROW_COL_BONUS;
        } else {
            score -= (diff * diff) * ROW_COL_BONUS;
        }

        u64 whiteCol = WHITE_FLATS(state) & colMasks[i];
        u64 blackCol = BLACK_FLATS(state) & colMasks[i];
        diff = __builtin_popcountll(whiteCol) - __builtin_popcountll(blackCol);
        if (diff > 0) {
            score += (diff * diff) * ROW_COL_BONUS;
        } else {
            score -= (diff * diff) * ROW_COL_BONUS;
        }
    }

    int wallDiff = (__builtin_popcountll(state->whiteControlled & state->standingStones) -
                   __builtin_popcountll(state->blackControlled & state->standingStones));
    score += wallDiff * WALL_BONUS;

    for (int pos = 0; pos < TOTAL_SQUARES; pos++) {
        Square* square = &state->board->squares[pos];
        if (state->whiteControlled & (1ULL << pos)) {
            if (SQ_HEAD(square).stone == FLAT) {
                score += square->whiteStones * RESERVE_BONUS;
                score += square->blackStones * PRISONER_BONUS;
            } else {
                score += square->whiteStones * (RESERVE_BONUS + 5);
                score += square->blackStones * (PRISONER_BONUS + 5);
            }
            // check neighbours
            u8 neighbours = 0;
            if (VALID_POSITION(RIGHT_POSITION(GET_X(pos))) && state->whiteControlled & (1ULL << RIGHT_POSITION(pos))) {
                neighbours++;
            }
            if (VALID_POSITION(LEFT_POSITION(GET_X(pos))) && state->whiteControlled & (1ULL << LEFT_POSITION(pos))) {
                neighbours++;
            }
            if (VALID_POSITION(UP_POSITION(GET_Y(pos))) && state->whiteControlled & (1ULL << UP_POSITION(pos))) {
                neighbours++;
            }
            if (VALID_POSITION(DOWN_POSITION(GET_Y(pos))) && state->whiteControlled & (1ULL << DOWN_POSITION(pos))) {
                neighbours++;
            }
            score += neighbours * 50;
        } else if (state->blackControlled & (1ULL << pos)) {
            if (SQ_HEAD(square).stone == FLAT) {
                score -= square->blackStones * RESERVE_BONUS;
                score -= square->whiteStones * PRISONER_BONUS;
            } else {
                score -= square->blackStones * (RESERVE_BONUS + 5);
                score -= square->whiteStones * (PRISONER_BONUS + 5);
            }
            // check neighbours
            u8 neighbours = 0;
            if (VALID_POSITION(RIGHT_POSITION(GET_X(pos))) && state->blackControlled & (1ULL << RIGHT_POSITION(pos))) {
                neighbours++;
            }
            if (VALID_POSITION(LEFT_POSITION(GET_X(pos))) && state->blackControlled & (1ULL << LEFT_POSITION(pos))) {
                neighbours++;
            }
            if (VALID_POSITION(UP_POSITION(GET_Y(pos))) && state->blackControlled & (1ULL << UP_POSITION(pos))) {
                neighbours++;
            }
            if (VALID_POSITION(DOWN_POSITION(GET_Y(pos))) && state->blackControlled & (1ULL << DOWN_POSITION(pos))) {
                neighbours++;
            }
            score -= neighbours * 50;
        }
    }

    for (int pos = 0; pos < TOTAL_SQUARES; pos++) {
        if (state->whiteControlled & (1ULL << pos)) {
            if (state->capstones & (1ULL << pos)) {
                score += (centrality[pos] - 3) * CENTRALITY_BONUS * 5;
            } else {
                score += centrality[pos] * CENTRALITY_BONUS;
            }
        } else if (state->blackControlled & (1ULL << pos)) {
            if (state->capstones & (1ULL << pos)) {
                score -= (centrality[pos] - 3) * CENTRALITY_BONUS * 5;
            } else {
                score -= centrality[pos] * CENTRALITY_BONUS;
            }
        }
    }

    score -= state->player1.stones * 151;
    score += state->player2.stones * 151;

    score += __builtin_popcountll(state->whiteControlled) * CONTROL_BONUS;
    score -= __builtin_popcountll(state->blackControlled) * CONTROL_BONUS;

    if (state->turn == WHITE) {
        if (score > 0) {
            score = score * 0.8;
        } else {
            score = score * 1.2;
        }
    } else {
        if (score > 0) {
            score = score * 1.2;
        } else {
            score = score * 0.8;
        }
    }

    return score;
}
