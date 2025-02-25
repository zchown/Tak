#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>
#include "../src/board.h"
#include "../src/tps.h"

void test_checkRoadWin() {
    GameState* state = createGameState();
    // Create a vertical road for Black
    for (int y = 0; y < BOARD_SIZE; y++) {
        /* Position pos = {0, y}; */
        Position pos = SET_POS(0, y);
        Square* sq = readSquare(state->board, pos);
        /* Piece* p = createPiece(FLAT, BLACK); */
        Piece p = {FLAT, BLACK};
        squareInsertPiece(state, sq, p);
    }
    Result result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, ROAD_BLACK);
    freeGameState(state);

    state = createGameState();
    // Create a horizontal road for White
    for (int x = 0; x < BOARD_SIZE; x++) {
        /* Position pos = {x, 0}; */
        Position pos = SET_POS(x, 0);
        Square* sq = readSquare(state->board, pos);
        Piece p = (Piece) {FLAT, WHITE};
        squareInsertPiece(state, sq, p);
    }
    result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, ROAD_WHITE);

    // Ensure that a road win is not detected if the road is not long enough
    /* squareRemovePiece(state, readSquare(state->board, (Position){0, 0})); */
    squareRemovePiece(state, readSquare(state->board, SET_POS(0, 0)));
    result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, CONTINUE);

    // Ensure that a road win is not detected if the road includes standing stones
    Piece p = (Piece){STANDING, WHITE};
    squareInsertPiece(state, readSquare(state->board, SET_POS(0, 0)), p);
    result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, CONTINUE);

    // Ensure that a road win is detected if the road includes a capstone
    /* squareRemovePiece(state, readSquare(state->board, (Position){0, 0})); */
    squareRemovePiece(state, readSquare(state->board, SET_POS(0, 0)));
    p = (Piece){CAP, WHITE};
    /* squareInsertPiece(state, readSquare(state->board, (Position){0, 0}), p); */
    squareInsertPiece(state, readSquare(state->board, SET_POS(0, 0)), p);
    result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, ROAD_WHITE);
    freeGameState(state);
}

void test_checkFullBoard() {
    /* GameState* state = parseTPS("[TPS 1,1,1,1,1,2/1,1,1,1,2,1/1,x2,2,1,1/x2,2,1,1,1/x,2,1,1,1,1/2,x5 2 20]"); */
    GameState* state = createGameState();
    // Fill the board with White flats
    for (int i = 0; i < BOARD_SIZE; i++) {
        for (int j = 0; j < BOARD_SIZE; j++) {
            /* Position pos = {i, j}; */
            Position pos = SET_POS(i, j);
            Color c = WHITE;
            if (i == j) {
                c = BLACK;
            }
            Piece piece = (Piece){FLAT, c};
            squareInsertPiece(state, readSquare(state->board, pos), piece);
        }
    }
    /* printBoard(state->board); */
    Result result = checkGameResult(state);
    /* printf("Result: %d\n", result); */
    CU_ASSERT_EQUAL(result, FLAT_WHITE);
    freeGameState(state);

    // Detects a draw if the board is filled with alternating flats
    state = createGameState();
    for (int i = 0; i < BOARD_SIZE; i++) {
        for (int j = 0; j < BOARD_SIZE; j++) {
            /* Position pos = {i, j}; */
            Position pos = SET_POS(i, j);
            Color c = WHITE;
            if ((i + j) % 2 == 0) {
                c = BLACK;
            }
            Piece piece = (Piece){FLAT, c};
            squareInsertPiece(state, readSquare(state->board, pos), piece);
        }
    }
    /* printBoard(state->board); */
    result = checkGameResult(state);
    /* printf("Result: %d\n", result); */
    CU_ASSERT_EQUAL(result, DRAW);
    freeGameState(state);

    // Detect win if reserves are empty
    state = createGameState();
    state->player1.stones = 0;
    state->player1.caps = 0;

    /* squareInsertPiece(state, readSquare(state->board, (Position){0, 0}), createPiece(FLAT, WHITE)); */
    squareInsertPiece(state, readSquare(state->board, SET_POS(0, 0)), (Piece){FLAT, WHITE});

    result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, FLAT_WHITE);
}

void test_checkHardRoads() {
    GameState* state = parseTPS("[TPS 1,x3,2C,x/1,x2,2,x2/1,1,1,2,2S,x/1,x,1,x3/2,x,1,x3/x,x,2,x3 2 1]");
    Result result = checkGameResult(state);
    /* printf("Result: %d\n", result); */
    CU_ASSERT_EQUAL(result, CONTINUE);

    state = parseTPS("[TPS 2,x5/2,x5/2,x5/2,x5/2,x5/2,x5 2 2]");
    result = checkGameResult(state);
    /* printf("Result: %d\n", result); */
    CU_ASSERT_EQUAL(result, ROAD_BLACK);

    state = parseTPS("[TPS x6/x6/x6/x,212121,x4/22,12,2,2,2,12/x6 1 31]");
    result = checkGameResult(state);
    /* printf("Result: %d\n", result); */
    CU_ASSERT_EQUAL(result, ROAD_BLACK);

    state = parseTPS("[TPS x6/x6/x6/x6/x6/x5,1 2 2]");
    result = checkGameResult(state);
    /* printf("Result: %d\n", result); */
    CU_ASSERT_EQUAL(result, CONTINUE);

    state = parseTPS("[TPS 2S,2S,2S,2S,2S,2S/1S,1S,1S,1S,1S,1S/2S,2S,2S,2S,2S,2S/1S,1S,1S,1S,1S,1S/2S,2S,2S,2S,2S,2S/11,2,1,1,1,1 2 6]");
    result = checkGameResult(state);
    printf("Result: %d\n", result);
    CU_ASSERT_EQUAL(result, FLAT_WHITE);
}
