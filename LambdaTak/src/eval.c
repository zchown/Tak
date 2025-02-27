#include "eval.h"

#pragma inline
int evaluate(GameState* state) {
    int score = 0;
    score += calculateFlatDiff(state);
    score += calculateLongRowCol(state);
    score += CONTROL_CALCULATION(state);
    score += squareLoop(state);

    SCORE_STABILIZATION(score, state->turn);
    return score;
}

#pragma inline
int calculateFlatDiff(GameState* state) {
    int min = (state->player1.stones < state->player2.stones) ? state->player1.stones : state->player2.stones;
    min = min / 8;
    switch (min) {
        case 0: return FLAT_DIFF(state) * FLAT_SCORE * 10;
        case 1: return FLAT_DIFF(state) * FLAT_SCORE * 8;
        case 2: return FLAT_DIFF(state) * FLAT_SCORE * 5;
        case 3: return FLAT_DIFF(state) * FLAT_SCORE * 3;
        default: return FLAT_DIFF(state) * FLAT_SCORE;
    }
}

#pragma inline
int calculateLongRowCol(GameState* state) {
    int whiteScore = 0;
    int blackScore = 0;

    Bitboard whiteControlled = state->whiteControlled & ~state->standingStones;
    Bitboard blackControlled = state->blackControlled & ~state->standingStones;

    Bitboard rowcolMasks[] = {ROW1, ROW2, ROW3, ROW4, ROW5, ROW6, COLA, COLB, COLC, COLD, COLE, COLF};

    for (int i = 1; i < 11; i++) {
        whiteScore += ROW_COL_BONUS * __builtin_popcountll(whiteControlled & rowcolMasks[i]) 
            * __builtin_popcountll(whiteControlled & rowcolMasks[i-1]) 
            * __builtin_popcountll(whiteControlled & rowcolMasks[i+1]);
        blackScore += ROW_COL_BONUS * __builtin_popcountll(blackControlled & rowcolMasks[i]) 
            * __builtin_popcountll(blackControlled & rowcolMasks[i-1]) 
            *  __builtin_popcountll(blackControlled & rowcolMasks[i+1]);
    }

    blackScore += ROW_COL_BONUS * __builtin_popcountll(blackControlled & rowcolMasks[0]) 
        * __builtin_popcountll(blackControlled & rowcolMasks[1]);
    blackScore += ROW_COL_BONUS * __builtin_popcountll(blackControlled & rowcolMasks[11] * 
        __builtin_popcountll(blackControlled & rowcolMasks[10]));

    whiteScore += ROW_COL_BONUS * __builtin_popcountll(whiteControlled & rowcolMasks[0])
        * __builtin_popcountll(whiteControlled & rowcolMasks[1]);
    whiteScore += ROW_COL_BONUS * __builtin_popcountll(whiteControlled & rowcolMasks[11])
        * __builtin_popcountll(whiteControlled & rowcolMasks[10]);

    return whiteScore - blackScore;
}

int squareLoop(GameState* state) {
    int score = 0;

    Bitboard whiteControlled = state->whiteControlled;
    Bitboard blackControlled = state->blackControlled;
    Bitboard combined = whiteControlled | blackControlled;

    Bitboard blackWallCaps = (state->standingStones | state->capstones) & blackControlled;
    Bitboard whiteWallCaps = (state->standingStones | state->capstones) & whiteControlled;

    Bitboard whiteWalls = state->standingStones & whiteControlled;
    Bitboard blackWalls = state->standingStones & blackControlled;

    Bitboard whiteCapstones = state->capstones & whiteControlled;
    Bitboard blackCapstones = state->capstones & blackControlled;

    Bitboard whiteSurround = (whiteControlled >> 6) | (whiteControlled << 6) | (whiteControlled >> 1) | (whiteControlled << 1);
    whiteSurround &= ~whiteControlled;
    Bitboard blackSurround = (blackControlled >> 6) | (blackControlled << 6) | (blackControlled >> 1) | (blackControlled << 1);
    blackSurround &= ~blackControlled;

    for (int i = 0; i < TOTAL_SQUARES; i++) {
        if (!(combined & (1ULL << i))) {
            break;
        }
        Square sq = state->board->squares[i];
        Bitboard neighbors = GET_NEIGHBORS(i);
        if (whiteControlled & (1ULL << i)) {
            if (sq.numPieces > 1) {
                if (!(whiteWallCaps & (1ULL << i))) {
                    if (neighbors & blackWallCaps) {
                        score -= (STACK_AT_RISK * (sq.whiteStones + sq.blackStones));
                    }
                } else if (whiteWalls & (1ULL << i)) {
                    score += sq.whiteStones * RESERVE_BONUS;
                    score += sq.blackStones * PRISONER_BONUS;
                    if (neighbors & blackWallCaps) {
                        score -= 2 * (STACK_AT_RISK * (sq.whiteStones + sq.blackStones));
                    }
                }
                else {
                    score += sq.whiteStones * RESERVE_BONUS;
                    score += sq.blackStones * PRISONER_BONUS;
                }
                score -= IMMOBILITY_PENALTY * (neighbors & state->standingStones);
            } else {
                score -= THREAT_BONUS * __builtin_popcountll(neighbors & blackWallCaps);
                score += PROTECTION_BONUS * __builtin_popcountll(neighbors & whiteWallCaps);
                score -= SQUARE_AT_RISK * 
                    (__builtin_popcountll(neighbors & blackControlled) - 
                     __builtin_popcountll(neighbors & whiteControlled));
            }
            score += CENTRALITY_BONUS * centrality[i];
            if (state->capstones & (1ULL << i)) {
                score += 100 * CENTRALITY_BONUS * (centrality[i]);
            }
        } else {
            if (sq.numPieces > 1) {
                if (!(blackWallCaps & (1ULL << i))) {
                    if (neighbors & whiteWallCaps) {
                        score += (STACK_AT_RISK * (sq.whiteStones + sq.blackStones));
                    }
                } else if (blackWalls & (1ULL << i)) {
                    score -= sq.blackStones * RESERVE_BONUS;
                    score -= sq.whiteStones * PRISONER_BONUS;
                    if (neighbors & whiteWallCaps) {
                        score += 2 * (STACK_AT_RISK * (sq.whiteStones + sq.blackStones));
                    }
                }
                else {
                    score -= sq.blackStones * RESERVE_BONUS;
                    score -= sq.whiteStones * PRISONER_BONUS;
                }
                score -= IMMOBILITY_PENALTY * (neighbors & state->standingStones);
            } else {
                score += THREAT_BONUS * __builtin_popcountll(neighbors & whiteWallCaps);
                score -= PROTECTION_BONUS * __builtin_popcountll(neighbors & blackWallCaps);
                score += SQUARE_AT_RISK * 
                    (__builtin_popcountll(neighbors & whiteControlled) - 
                     __builtin_popcountll(neighbors & blackControlled));
            }
            score -= CENTRALITY_BONUS * centrality[i];
            if (state->capstones & (1ULL << i)) {
                score -= 100 * CENTRALITY_BONUS * (centrality[i]);
            }
        }
    }

    // buddies
    score += BUDDY_BONUS * __builtin_popcountll(whiteSurround & whiteControlled);
    score -= BUDDY_BONUS * __builtin_popcountll(blackSurround & blackControlled);

    score += WALL_BONUS * (__builtin_popcountll(whiteControlled & state->standingStones) - __builtin_popcountll(blackControlled & state->standingStones));

    return score;
}

