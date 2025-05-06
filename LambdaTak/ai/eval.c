#include "eval.h"

static DenseNeuralNet net;

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

int evaluateWithNN(GameState* state) {
    double* result = feedForwardDense(&net, FEATURE_SIZE, gameStateToVector(state), 0.0, false);
    return (int)(result[0] * 10000);
}

void setupNN(void) {
    int layerSizes[] = {(7 * TOTAL_SQUARES), (7 * TOTAL_SQUARES), (7 * TOTAL_SQUARES), 252, 252, 252, 64, 64, 32, 32, 16, 16, 8, 4, 1};
    int numLayers = 15;

    net = createDenseNeuralNet(layerSizes, numLayers, Relu);
    loadDenseNeuralNet(&net, "n_models/tak_model.weights_large");
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

    Bitboard neighbours = getNeighbors(pos);

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
    Bitboard controlledForLoop = controlled & ((1ULL << TOTAL_SQUARES) - 1);
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

            Bitboard neighbours = getNeighbors(pos);
            int whiteNeighbours = __builtin_popcountll(neighbours & state->whiteControlled);
            int blackNeighbours = __builtin_popcountll(neighbours & state->blackControlled);

            score += (whiteNeighbours - blackNeighbours) * BUDDY_BONUS;

            if (square->numPieces > 0) {
                score -= DISCOURAGE_BIG_STACK * square->numPieces * (square->numPieces - square->whiteStones);
                int prisoners = square->blackStones;
                score += prisoners * PRISONER_BONUS;

                int reserves = square->whiteStones - 1;
                if (reserves > 0) {
                    score += reserves * RESERVE_BONUS;
                }

                if (square->numPieces >= 2 && square->pieces[square->numPieces - 2].color == WHITE) {
                    if ((state->standingStones | state->capstones) & (1ULL << pos)) {
                        score += 25 * RESERVE_BONUS;
                    } else {
                        score += 15 * RESERVE_BONUS;
                    }
               }

                score -= IMMOBILITY_PENALTY * square->numPieces *
                    (__builtin_popcountll(neighbours & wallCaps));

                if (!(wallCaps & (1ULL << pos))) {
                    score -= STACK_AT_RISK * square->blackStones * square->whiteStones *
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

            Bitboard neighbours = getNeighbors(pos);
            int blackNeighbours = __builtin_popcountll(neighbours & state->blackControlled);
            int whiteNeighbours = __builtin_popcountll(neighbours & state->whiteControlled);

            score += (blackNeighbours - whiteNeighbours) * BUDDY_BONUS;

            if (square->numPieces > 0) {
                score += DISCOURAGE_BIG_STACK * square->numPieces * (square->numPieces - square->blackStones);
                int prisoners = square->whiteStones;
                score -= prisoners * PRISONER_BONUS;

                int reserves = square->blackStones - 1;
                if (reserves > 0) {
                    score -= reserves * RESERVE_BONUS;
                }

                if (square->numPieces >= 2 && square->pieces[square->numPieces - 2].color == BLACK) {
                    if ((state->standingStones | state->capstones) & (1ULL << pos)) {
                        score -= 25 * RESERVE_BONUS;
                    } else {
                        score -= 15 * RESERVE_BONUS;
                    }
                }

                score += IMMOBILITY_PENALTY * square->numPieces *
                    (__builtin_popcountll(neighbours & wallCaps));

                if (!(wallCaps & (1ULL << pos))) {
                    score += STACK_AT_RISK * square->blackStones * square->whiteStones *
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
// Define macros to reduce repetition
#define PROCESS_INITIAL_POSITIONS(color, opponentColor, opponentCaps, boardArray, isHorizontal) \
    for (int i = 0; i < 6; i++) { \
        Position pos = isHorizontal ? SET_POS(0, i) : SET_POS(i, 0); \
        if (state->color##Controlled & (1ULL << pos)) { \
            boardArray[pos] = 1; \
            /* Subtract 3 if opponent's capstone is in one of these squares */ \
            if (opponentCaps & (1ULL << pos)) { \
                boardArray[pos] -= 5; \
            } \
            /* Subtract 2 if there's a standing stone */ \
            if (standing & (1ULL << pos) & state->opponentColor##Controlled) { \
                boardArray[pos] -= 4; \
            } \
        } else { \
            boardArray[pos] = 0; \
        } \
    }

#define PROCESS_CONNECTIVITY(color, opponentCaps, opponentColor, boardArray, isHorizontal) \
    for (int i = 1; i < 6; i++) { \
        for (int j = 0; j < 6; j++) { \
            Position pos = isHorizontal ? SET_POS(i, j) : SET_POS(j, i); \
            Position primary = isHorizontal ? LEFT_POSITION(pos) : DOWN_POSITION(pos); \
            Position secondary1, secondary2; \
            \
            if (isHorizontal) { \
                secondary1 = UP_POSITION(primary); \
                secondary2 = DOWN_POSITION(primary); \
            } else { \
                secondary1 = RIGHT_POSITION(primary); \
                secondary2 = LEFT_POSITION(primary); \
            } \
            \
            if (state->color##Controlled & (1ULL << pos)) { \
                if ((isHorizontal && i == 0) || (!isHorizontal && j == 0)) { \
                    boardArray[pos] = 3 * boardArray[primary] + boardArray[secondary2]; \
                } else if ((isHorizontal && i == 5) || (!isHorizontal && j == 5)) { \
                    boardArray[pos] = 3 * boardArray[primary] + boardArray[secondary1]; \
                } else { \
                    boardArray[pos] = 3 * boardArray[primary] + boardArray[secondary1] + boardArray[secondary2]; \
                } \
                /* Subtract 3 if opponent's capstone is in one of these squares */ \
                if (opponentCaps & (1ULL << pos)) { \
                    boardArray[pos] -= 3; \
                } \
                /* Subtract 2 if there's a standing stone */ \
                if (standing & (1ULL << pos) & state->opponentColor##Controlled) { \
                    boardArray[pos] -= 2; \
                } \
                \
                if (boardArray[pos] > color##Max) { \
                    color##Max = boardArray[pos]; \
                } else if (boardArray[pos] == 0) { \
                    boardArray[pos] = 1; \
                } \
            } else { \
                boardArray[pos] = 0; \
            } \
        } \
    }

#pragma inline
int connectivityIndex(GameState* state) {
    Bitboard whiteCaps = state->capstones & state->whiteControlled;
    Bitboard blackCaps = state->capstones & state->blackControlled;
    Bitboard standing = state->standingStones;

    int whiteBoard[TOTAL_SQUARES] = {0};
    int blackBoard[TOTAL_SQUARES] = {0};
    int whiteBoard2[TOTAL_SQUARES] = {0};
    int blackBoard2[TOTAL_SQUARES] = {0};
    int whiteMax = 0;
    int blackMax = 0;

    // Process initial positions for vertical connectivity
    for (int i = 0; i < BOARD_SIZE; i++) {
        Position pos = SET_POS(0, i);
        if (pos < TOTAL_SQUARES) {
            if (state->whiteControlled & (1ULL << pos)) {
                whiteBoard[pos] = 1;
                // Subtract 5 if opponent's capstone is in this square
                if (blackCaps & (1ULL << pos)) {
                    whiteBoard[pos] -= 5;
                }
                // Subtract 4 if there's an opponent's standing stone
                if (standing & (1ULL << pos) & state->blackControlled) {
                    whiteBoard[pos] -= 4;
                }
            }

            if (state->blackControlled & (1ULL << pos)) {
                blackBoard[pos] = 1;
                // Subtract 5 if opponent's capstone is in this square
                if (whiteCaps & (1ULL << pos)) {
                    blackBoard[pos] -= 5;
                }
                // Subtract 4 if there's an opponent's standing stone
                if (standing & (1ULL << pos) & state->whiteControlled) {
                    blackBoard[pos] -= 4;
                }
            }
        }
    }

    // Process connectivity for vertical paths
    for (int i = 1; i < BOARD_SIZE; i++) {
        for (int j = 0; j < BOARD_SIZE; j++) {
            Position pos = SET_POS(i, j);
            if (pos >= TOTAL_SQUARES) continue;

            Position primary = DOWN_POSITION(pos);
            if (primary >= TOTAL_SQUARES) continue;

            Position secondary1 = RIGHT_POSITION(primary);
            Position secondary2 = LEFT_POSITION(primary);

            // Process white connectivity
            if (state->whiteControlled & (1ULL << pos)) {
                whiteBoard[pos] = 3 * whiteBoard[primary];

                if (j > 0 && secondary2 < TOTAL_SQUARES) {
                    whiteBoard[pos] += whiteBoard[secondary2];
                }

                if (j < BOARD_SIZE - 1 && secondary1 < TOTAL_SQUARES) {
                    whiteBoard[pos] += whiteBoard[secondary1];
                }

                // Subtract 3 if opponent's capstone is in this square
                if (blackCaps & (1ULL << pos)) {
                    whiteBoard[pos] -= 3;
                }

                // Subtract 2 if there's an opponent's standing stone
                if (standing & (1ULL << pos) & state->blackControlled) {
                    whiteBoard[pos] -= 2;
                }

                if (whiteBoard[pos] > whiteMax) {
                    whiteMax = whiteBoard[pos];
                } else if (whiteBoard[pos] <= 0) {
                    whiteBoard[pos] = 1;
                }
            }

            // Process black connectivity
            if (state->blackControlled & (1ULL << pos)) {
                blackBoard[pos] = 3 * blackBoard[primary];

                if (j > 0 && secondary2 < TOTAL_SQUARES) {
                    blackBoard[pos] += blackBoard[secondary2];
                }

                if (j < BOARD_SIZE - 1 && secondary1 < TOTAL_SQUARES) {
                    blackBoard[pos] += blackBoard[secondary1];
                }

                // Subtract 3 if opponent's capstone is in this square
                if (whiteCaps & (1ULL << pos)) {
                    blackBoard[pos] -= 3;
                }

                // Subtract 2 if there's an opponent's standing stone
                if (standing & (1ULL << pos) & state->whiteControlled) {
                    blackBoard[pos] -= 2;
                }

                if (blackBoard[pos] > blackMax) {
                    blackMax = blackBoard[pos];
                } else if (blackBoard[pos] <= 0) {
                    blackBoard[pos] = 1;
                }
            }
        }
    }

    // Process initial positions for horizontal connectivity
    for (int i = 0; i < BOARD_SIZE; i++) {
        Position pos = SET_POS(i, 0);
        if (pos < TOTAL_SQUARES) {
            if (state->whiteControlled & (1ULL << pos)) {
                whiteBoard2[pos] = 1;
                // Subtract 5 if opponent's capstone is in this square
                if (blackCaps & (1ULL << pos)) {
                    whiteBoard2[pos] -= 5;
                }
                // Subtract 4 if there's an opponent's standing stone
                if (standing & (1ULL << pos) & state->blackControlled) {
                    whiteBoard2[pos] -= 4;
                }
            }

            if (state->blackControlled & (1ULL << pos)) {
                blackBoard2[pos] = 1;
                // Subtract 5 if opponent's capstone is in this square
                if (whiteCaps & (1ULL << pos)) {
                    blackBoard2[pos] -= 5;
                }
                // Subtract 4 if there's an opponent's standing stone
                if (standing & (1ULL << pos) & state->whiteControlled) {
                    blackBoard2[pos] -= 4;
                }
            }
        }
    }

    // Process connectivity for horizontal paths
    for (int i = 0; i < BOARD_SIZE; i++) {
        for (int j = 1; j < BOARD_SIZE; j++) {
            Position pos = SET_POS(i, j);
            if (pos >= TOTAL_SQUARES) continue;

            Position primary = LEFT_POSITION(pos);
            if (primary >= TOTAL_SQUARES) continue;

            Position secondary1 = UP_POSITION(primary);
            Position secondary2 = DOWN_POSITION(primary);

            // Process white connectivity
            if (state->whiteControlled & (1ULL << pos)) {
                whiteBoard2[pos] = 3 * whiteBoard2[primary];

                if (i > 0 && secondary2 < TOTAL_SQUARES) {
                    whiteBoard2[pos] += whiteBoard2[secondary2];
                }

                if (i < BOARD_SIZE - 1 && secondary1 < TOTAL_SQUARES) {
                    whiteBoard2[pos] += whiteBoard2[secondary1];
                }

                // Subtract 3 if opponent's capstone is in this square
                if (blackCaps & (1ULL << pos)) {
                    whiteBoard2[pos] -= 3;
                }

                // Subtract 2 if there's an opponent's standing stone
                if (standing & (1ULL << pos) & state->blackControlled) {
                    whiteBoard2[pos] -= 2;
                }

                if (whiteBoard2[pos] > whiteMax) {
                    whiteMax = whiteBoard2[pos];
                } else if (whiteBoard2[pos] <= 0) {
                    whiteBoard2[pos] = 1;
                }
            }

            // Process black connectivity
            if (state->blackControlled & (1ULL << pos)) {
                blackBoard2[pos] = 3 * blackBoard2[primary];

                if (i > 0 && secondary2 < TOTAL_SQUARES) {
                    blackBoard2[pos] += blackBoard2[secondary2];
                }

                if (i < BOARD_SIZE - 1 && secondary1 < TOTAL_SQUARES) {
                    blackBoard2[pos] += blackBoard2[secondary1];
                }

                // Subtract 3 if opponent's capstone is in this square
                if (whiteCaps & (1ULL << pos)) {
                    blackBoard2[pos] -= 3;
                }

                // Subtract 2 if there's an opponent's standing stone
                if (standing & (1ULL << pos) & state->whiteControlled) {
                    blackBoard2[pos] -= 2;
                }

                if (blackBoard2[pos] > blackMax) {
                    blackMax = blackBoard2[pos];
                } else if (blackBoard2[pos] <= 0) {
                    blackBoard2[pos] = 1;
                }
            }
        }
    }

    return whiteMax - blackMax;
}

#pragma inline
Bitboard getNeighbors(int pos) {
    Bitboard neighbors = 0;
    Bitboard board = (1ULL << pos);
    if (pos >= 6) {
        neighbors |= (board >> 6);
    }
    if (pos < 30) {
        neighbors |= (board << 6);
    }
    if (pos % 6 != 0) {
        neighbors |= (board >> 1);
    }
    if (pos % 6 != 5) {
        neighbors |= (board << 1);
    }
    return neighbors;
}
