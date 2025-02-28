#include "eval.h"
#include "board.h"

int evaluate(GameState* state) {
    int score = calculateFlatDiff(state);
    score += PATH_BONUS * connectivityIndex(state);
    score += calculateLongRowCol(state);
    score += squareLoop(state);

    score += CONTROL_CALCULATION(state);

    score += WALL_BONUS * (__builtin_popcountll(state->standingStones & state->whiteControlled) - __builtin_popcountll(state->standingStones & state->blackControlled));


    int whiteEncoragement = state->player1.stones - 18;
    if (whiteEncoragement > 0) {
        score -= ENCOURAGE_PLACEMENT * whiteEncoragement * whiteEncoragement;
    }

    int blackEncoragement = state->player2.stones - 18;
    if (blackEncoragement > 0) {
        score += ENCOURAGE_PLACEMENT * blackEncoragement * blackEncoragement;
    }

    whiteEncoragement = __builtin_popcountll(state->whiteControlled) - 12;
    if (whiteEncoragement < 0) {
        score += CONTROL_BONUS * whiteEncoragement * whiteEncoragement;
    }

    blackEncoragement = __builtin_popcountll(state->blackControlled) - 12;
    if (blackEncoragement < 0) {
        score -= CONTROL_BONUS * blackEncoragement * blackEncoragement;
    }

    if (state->turn == WHITE) {
        score += FLAT_SCORE;
    } else {
        score -= FLAT_SCORE;
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
        Bitboard whiteRow = state->whiteControlled & rowMasks[i];
        Bitboard blackRow = state->blackControlled & rowMasks[i];

        Bitboard whiteCol = state->whiteControlled & colMasks[i];
        Bitboard blackCol = state->blackControlled & colMasks[i];

        score += __builtin_popcountll(whiteRow) - __builtin_popcountll(blackRow);
        score += __builtin_popcountll(whiteCol) - __builtin_popcountll(blackCol);
    }

    return score * ROW_COL_BONUS;
}

// do not use, way too slow
#pragma inline
int calculateLongestDFS(GameState* state, Color player, Bitboard visited, int pos, int depth) {
    if (depth == 0) {
        return 0;
    }

    Bitboard neighbours = GET_NEIGHBORS(pos);

    Bitboard playerStones = (player == WHITE) ? state->whiteControlled : state->blackControlled;

    int max = 0;

    while (neighbours) {
        int lsb = __builtin_ctzll(neighbours);
        if (!(visited & (1ULL << lsb))) {
            Bitboard newVisited = visited | (1ULL << lsb);

            int score = calculateLongestDFS(state, player, newVisited, lsb, depth - 1);

            if (score > max) {
                max = score;
            }
        }
        neighbours &= neighbours - 1;
    }

    return max + 1;
}

#pragma inline
int calculatePathScore(GameState* state) {
    int whiteMaxPath = 0;
    for (int i = 0; i < TOTAL_SQUARES; i++) {
        if (state->whiteControlled & (1ULL << i)) {
            Bitboard visited = 1ULL << i;
            int pathLength = calculateLongestDFS(state, WHITE, visited, i, 6);
            if (pathLength > whiteMaxPath) {
                whiteMaxPath = pathLength;
            }
        }
    }
    int blackMaxPath = 0;
    for (int i = 0; i < TOTAL_SQUARES; i++) {
        if (state->blackControlled & (1ULL << i)) {
            Bitboard visited = 1ULL << i;
            int pathLength = calculateLongestDFS(state, BLACK, visited, i, 6);
            if (pathLength > blackMaxPath) {
                blackMaxPath = pathLength;
            }
        }
    }

    return (whiteMaxPath - blackMaxPath) * ROW_COL_BONUS;
}

#pragma inline
int squareLoop(GameState* state) {
    int score = 0;
    Bitboard controlled = state->whiteControlled | state->blackControlled;
    Bitboard controlledForLoop = controlled;
    Bitboard wallCaps = state->capstones & state->standingStones;

    score += __builtin_popcountll(state->whiteControlled & wallCaps) * WALL_BONUS;
    score -= __builtin_popcountll(state->blackControlled & wallCaps) * WALL_BONUS;

    while (controlledForLoop) {
        int pos = __builtin_ctzll(controlledForLoop);
        controlledForLoop &= controlledForLoop - 1;

        Square* square = readSquare(state->board, pos);

        if (state->whiteControlled & (1ULL << pos)) {

            score += CENTRALITY_BONUS * centrality[pos];

            if (state->capstones & (1ULL << pos)) {
                score += 5 * CONTROL_BONUS * (centrality[pos] - 5);
            }

            Bitboard neighbours = GET_NEIGHBORS(pos);
            int whiteNeighbours = __builtin_popcountll(neighbours & state->whiteControlled);
            int blackNeighbours = __builtin_popcountll(neighbours & state->blackControlled);

            score += (whiteNeighbours - blackNeighbours) * BUDDY_BONUS;

            if (square->numPieces > 0) {
                int prisoners = square->blackStones;
                score += prisoners * PRISONER_BONUS;

                int reserves = square->whiteStones - 1;
                if (reserves > 0) {
                    score += reserves * RESERVE_BONUS;
                }

                score -= IMMOBILITY_PENALTY * 
                    (__builtin_popcountll(neighbours & wallCaps));

                if (!(wallCaps & (1ULL << pos))) {
                    score -= STACK_AT_RISK * 
                        (__builtin_popcountll(neighbours & (state->blackControlled & wallCaps)));
                }

                if (whiteNeighbours > 0) {
                    score += PROTECTION_BONUS;
                }
            }


        } else {
            score -= CENTRALITY_BONUS * centrality[pos];

            if (state->capstones & (1ULL << pos)) {
                score -= 5 * CENTRALITY_BONUS * (centrality[pos] - 5);
            }

            Bitboard neighbours = GET_NEIGHBORS(pos);
            int blackNeighbours = __builtin_popcountll(neighbours & state->blackControlled);
            int whiteNeighbours = __builtin_popcountll(neighbours & state->whiteControlled);

            score += (blackNeighbours - whiteNeighbours) * BUDDY_BONUS;

            if (square->numPieces > 0) {
                int prisoners = square->whiteStones;
                score -= prisoners * PRISONER_BONUS;

                int reserves = square->blackStones - 1;
                if (reserves > 0) {
                    score -= reserves * RESERVE_BONUS;
                }

                score += IMMOBILITY_PENALTY * 
                    (__builtin_popcountll(neighbours & wallCaps));

                if (!(wallCaps & (1ULL << pos))) {
                    score += STACK_AT_RISK * 
                        (__builtin_popcountll(neighbours & (state->whiteControlled & wallCaps)));
                }

                if (blackNeighbours > 0) {
                    score -= PROTECTION_BONUS;
                }
            }
        }

    }

    return score;
}

// yes this is questionable and mildly evil
// I also made it up so idk if its helpful
#define INIT_BOARD(board, control, opponentCaps, standing) \
    for (int i = 0; i < 6; i++) { \
        if (control & (1ULL << i)) { \
            board[i] = 1; \
            if (opponentCaps & (1ULL << i)) board[i] -= 3; \
            if (standing & (1ULL << i)) board[i] -= 2; \
        } else { \
            board[i] = 0; \
        } \
    }

#define UPDATE_BOARD(board, control, opponentCaps, standing, maxVal, pos, adj1, adj2, adj3) \
    if (control & (1ULL << pos)) { \
        if (adj2 == -1) { \
            board[pos] = 3 * board[adj1] + board[adj3]; \
        } else { \
            board[pos] = 3 * board[adj1] + board[adj2] + board[adj3]; \
        } \
        if (opponentCaps & (1ULL << pos)) board[pos] -= 3; \
        if (standing & (1ULL << pos)) board[pos] -= 2; \
        if (board[pos] > maxVal) { \
            maxVal = board[pos]; \
        } else if (board[pos] == 0) { \
            board[pos] = 1; \
        } \
    } else { \
        board[pos] = 0; \
    }

#pragma inline
int connectivityIndex(GameState* state) {

    Bitboard whiteCaps = state->capstones & state->whiteControlled;
    Bitboard blackCaps = state->capstones & state->blackControlled;
    Bitboard standing = state->standingStones;

    int whiteBoard[TOTAL_SQUARES];
    int blackBoard[TOTAL_SQUARES];
    int whiteMax = 0;
    int blackMax = 0;

    // Initialize first row
    INIT_BOARD(whiteBoard, state->whiteControlled, blackCaps, standing);
    INIT_BOARD(blackBoard, state->blackControlled, whiteCaps, standing);

    // Compute connectivity for remaining rows
    for (int i = 1; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
            Position pos = SET_POS(j, i);
            Position down = DOWN_POSITION(pos);
            Position right = RIGHT_POSITION(down);
            Position left = LEFT_POSITION(down);

            UPDATE_BOARD(whiteBoard, state->whiteControlled, blackCaps, standing, whiteMax, pos, down, (j == 0 ? -1 : left), right);
            UPDATE_BOARD(blackBoard, state->blackControlled, whiteCaps, standing, blackMax, pos, down, (j == 0 ? -1 : left), right);
        }
    }

    int whiteBoard2[TOTAL_SQUARES];
    int blackBoard2[TOTAL_SQUARES];

    // Initialize first column
    INIT_BOARD(whiteBoard2, state->whiteControlled, blackCaps, standing);
    INIT_BOARD(blackBoard2, state->blackControlled, whiteCaps, standing);

    // Compute connectivity for remaining columns
    for (int i = 1; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
            Position pos = SET_POS(i, j);
            Position right = RIGHT_POSITION(pos);
            Position up = UP_POSITION(right);
            Position down = DOWN_POSITION(right);

            UPDATE_BOARD(whiteBoard2, state->whiteControlled, blackCaps, standing, whiteMax, pos, right, (i == 0 ? -1 : up), down);
            UPDATE_BOARD(blackBoard2, state->blackControlled, whiteCaps, standing, blackMax, pos, right, (i == 0 ? -1 : up), down);
        }
    }

    return whiteMax - blackMax;
}

