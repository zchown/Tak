#include "eval.h"
#include "board.h"

int evaluate(GameState* state) {
    int score = 0;

    score += calculateFlatDiff(state);
    score += calculateLongRowCol(state);
    
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

    int minStones = (state->player1.stones < state->player2.stones) ? state->player1.stones : state->player2.stones;

    minStones = minStones / 8;

    int stabalize = 0;

    switch (minStones) {
        case 0: stabalize += FLAT_SCORE * 10;
        case 1: stabalize += FLAT_SCORE * 8;
        case 2: stabalize += FLAT_SCORE * 5;
        case 3: stabalize += FLAT_SCORE * 3;
        default: stabalize += FLAT_SCORE;
    }

    stabalize += (abs(score) / 20);

    if (state->turn == WHITE) { 
        score += stabalize;
    } else {
        score -= stabalize;
    }

    return score;
}

#pragma inline
int calculateFlatDiff(GameState* state) {

    int minStones = (state->player1.stones < state->player2.stones) ? state->player1.stones : state->player2.stones;

    minStones = minStones / 8;

    switch (minStones) {
        case 0: return FLAT_DIFF(state) * FLAT_SCORE * 10;
        case 1: return FLAT_DIFF(state) * FLAT_SCORE * 8;
        case 2: return FLAT_DIFF(state) * FLAT_SCORE * 5;
        case 3: return FLAT_DIFF(state) * FLAT_SCORE * 3;
        default: return FLAT_DIFF(state) * FLAT_SCORE;
    }
}

#pragma inline
int calculateLongRowCol(GameState* state) {
    int score = 0;

    Bitboard rowMasks[] = {ROW1, ROW2, ROW3, ROW4, ROW5, ROW6};
    Bitboard colMasks[] = {COLA, COLB, COLC, COLD, COLE, COLF};

    for (int i = 0; i < BOARD_SIZE; i++) {
        Bitboard whiteRow = WHITE_FLATS(state) & rowMasks[i];
        Bitboard blackRow = BLACK_FLATS(state) & rowMasks[i];

        score += (ROW_COL_BONUS * (whiteRow - ROW_COL_WALLS(state, rowMasks[i])) / 1000000);
        score -= (ROW_COL_BONUS * (blackRow - ROW_COL_WALLS(state, rowMasks[i])) / 1000000);

        Bitboard whiteCol = WHITE_FLATS(state) & colMasks[i];
        Bitboard blackCol = BLACK_FLATS(state) & colMasks[i];

        score += (ROW_COL_BONUS * (whiteCol - ROW_COL_WALLS(state, colMasks[i])) / 1000000);
        score -= (ROW_COL_BONUS * (blackCol - ROW_COL_WALLS(state, colMasks[i])) / 1000000);
    }

    score = score / 500000;
    return score;
}
