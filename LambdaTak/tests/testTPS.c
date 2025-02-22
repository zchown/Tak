#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>
#include "../src/board.h"
#include "../src/tps.h"

void test_parseTPS_valid() {
    GameState* state = parseTPS("[TPS x6/x6/x6/x6/x6/x6 1 10]");
    CU_ASSERT_PTR_NOT_NULL(state);
    if (state) {
        CU_ASSERT_EQUAL(state->turn, WHITE);
        CU_ASSERT_EQUAL(state->turnNumber, 10);
        freeGameState(state);
    }
}

void test_parseTPS_invalid() {
    /* GameState* state = parseTPS("[TPS x6/x6/x6/x6/x6 1 10]");  // Invalid row count */
    /* CU_ASSERT_PTR_NULL(state); */
    /*  */
    /* GameState* state3 = parseTPS("[TPS x6/x6/x6/x6/x6/x6 1]");  // Missing number */
    /* CU_ASSERT_PTR_NULL(state3); */
    /*  */
    /* GameState* state4 = parseTPS("[TPS x6/x6/x6/x6/x6/x5 1 10]"); // Invalid column count */
    /* CU_ASSERT_PTR_NULL(state4); */
}

void test_parseTPS_with_pieces() {
    GameState* state = parseTPS("[TPS x3,1,2,2C/x6/x6/x6/x6/x6 1 10]");
    if (state) {
        /* Square* sq = readSquare(state->board, (Position){3, 5}); */
        Square* sq = readSquare(state->board, SET_POS(3, 5));
        CU_ASSERT_PTR_NOT_NULL(sq);
        CU_ASSERT_PTR_NOT_NULL(sq->head);
        CU_ASSERT_EQUAL(sq->head->color, WHITE);
        CU_ASSERT_EQUAL(sq->head->stone, FLAT);
        freeGameState(state);
    }
}

void test_gameStateToTPS() {
    GameState* state = parseTPS("[TPS x6/x6/x6/x6/x6/x,11,2,x2,2C 1 5]");
    char* tps = gameStateToTPS(state);
    CU_ASSERT_PTR_NOT_NULL(tps);
    if (tps) {
        CU_ASSERT_STRING_EQUAL(tps, "[TPS x6/x6/x6/x6/x6/x,11,2,x2,2C 1 5]");
        free(tps);
    }
    freeGameState(state);

    state = parseTPS("[TPS 1,x3,2C,x/1,x2,2,x2/1,1,1,2,2S,x/1,x,1,x3/2,x,1,x3/x2,2,x3 2 1]");
    tps = gameStateToTPS(state);
    CU_ASSERT_PTR_NOT_NULL(tps);
    if (tps) {
        CU_ASSERT_STRING_EQUAL(tps, "[TPS 1,x3,2C,x/1,x2,2,x2/1,1,1,2,2S,x/1,x,1,x3/2,x,1,x3/x2,2,x3 2 1]");
        free(tps);
    }
    freeGameState(state);

    state = parseTPS("[TPS 2,x5/2,x5/2,x5/2,x5/2,x5/2,x5 2 2]");
    tps = gameStateToTPS(state);
    CU_ASSERT_PTR_NOT_NULL(tps);
    if (tps) {
        CU_ASSERT_STRING_EQUAL(tps, "[TPS 2,x5/2,x5/2,x5/2,x5/2,x5/2,x5 2 2]");
        free(tps);
    }
    freeGameState(state);

    state = parseTPS("[TPS x6/x6/x6/x,212121,x4/22,12,2,2,2,12/x6 1 31]");
    tps = gameStateToTPS(state);
    CU_ASSERT_PTR_NOT_NULL(tps);
    if (tps) {
        CU_ASSERT_STRING_EQUAL(tps, "[TPS x6/x6/x6/x,212121,x4/22,12,2,2,2,12/x6 1 31]");
        free(tps);
    }
    freeGameState(state);

    state = parseTPS("[TPS x6/x6/x6/x6/x6/x5,1 2 2]");
    tps = gameStateToTPS(state);
    CU_ASSERT_PTR_NOT_NULL(tps);
    if (tps) {
        CU_ASSERT_STRING_EQUAL(tps, "[TPS x6/x6/x6/x6/x6/x5,1 2 2]");
        free(tps);
    }
    freeGameState(state);
}

void test_boardToTPS() {
    Board* board = createEmptyBoard();
    /* Position pos = {0, 0}; */
    Position pos = SET_POS(0, 0);
    Piece* piece = createPiece(FLAT, WHITE);
    Square* sq = readSquare(board, pos);
    squareInsertPiece(NULL, sq, piece);

    char* tps = boardToTPS(board);
    CU_ASSERT_PTR_NOT_NULL(tps);
    if (tps) {
        /* printf("%s\n", tps); */
        CU_ASSERT_STRING_EQUAL(tps, "x6/x6/x6/x6/x6/1,x5");
        free(tps);
    }
    freeBoard(board);
}

void test_boardToTPS_empty() {
    Board* board = createEmptyBoard();
    char* tps = boardToTPS(board);
    CU_ASSERT_PTR_NOT_NULL(tps);
    if (tps) {
        /* printf("%s\n", tps); */
        CU_ASSERT_STRING_EQUAL(tps, "x6/x6/x6/x6/x6/x6");
        free(tps);
    }
    freeBoard(board);
}
