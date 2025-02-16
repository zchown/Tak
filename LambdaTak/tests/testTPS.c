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
    GameState* state = parseTPS("[TPS x6/x6/x6/x6/x6 1 10]");  // Invalid row count
    CU_ASSERT_PTR_NULL(state);
}

void test_parseTPS_with_pieces() {
    GameState* state = parseTPS("[TPS 1,2,2C/x6/x6/x6/x6/x6 1 10]");
    CU_ASSERT_PTR_NOT_NULL(state);
    if (state) {
        Square* sq = readSquare(state->board, (Position){0, 0});
        CU_ASSERT_EQUAL(sq->numPieces, 1);
        CU_ASSERT_EQUAL(sq->head->color, WHITE);
        freeGameState(state);
    }
}

void test_gameStateToTPS() {
    GameState* state = createGameState();
    Position pos = {2, 2};
    Piece* piece = createPiece(CAP, BLACK);
    Square* sq = readSquare(state->board, pos);
    squareInsertPiece(sq, piece);
    state->turnNumber = 5;

    char* tps = gameStateToTPS(state);
    CU_ASSERT_PTR_NOT_NULL(tps);
    if (tps) {
        CU_ASSERT_STRING_EQUAL(tps, "[TPS x6/x6/x6/x6/x6/3,x2,2C 1 5]");
        free(tps);
    }
    freeGameState(state);
}

void test_boardToTPS() {
    Board* board = createEmptyBoard();
    Position pos = {0, 0};
    Piece* piece = createPiece(FLAT, WHITE);
    Square* sq = readSquare(board, pos);
    squareInsertPiece(sq, piece);

    char* tps = boardToTPS(board);
    CU_ASSERT_PTR_NOT_NULL(tps);
    if (tps) {
        CU_ASSERT_STRING_EQUAL(tps, "1/x5/x5/x5/x5/x5");
        free(tps);
    }
    freeBoard(board);
}

void test_boardToTPS_empty() {
    Board* board = createEmptyBoard();
    char* tps = boardToTPS(board);
    CU_ASSERT_PTR_NOT_NULL(tps);
    if (tps) {
        CU_ASSERT_STRING_EQUAL(tps, "x6/x6/x6/x6/x6/x6");
        free(tps);
    }
    freeBoard(board);
}
