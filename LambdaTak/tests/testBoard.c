#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>
#include "../src/board.h"
#include "../src/tps.h"

void test_indexPosition() {
    Position pos = indexToPosition(0);
    CU_ASSERT_EQUAL(pos.x, 0);
    CU_ASSERT_EQUAL(pos.y, 0);

    pos = indexToPosition(BOARD_SIZE - 1);
    CU_ASSERT_EQUAL(pos.x, BOARD_SIZE - 1);
    CU_ASSERT_EQUAL(pos.y, 0);

    pos = indexToPosition(TOTAL_SQUARES - 1);
    CU_ASSERT_EQUAL(pos.x, BOARD_SIZE - 1);
    CU_ASSERT_EQUAL(pos.y, BOARD_SIZE - 1);

    u32 idx = positionToIndex((Position){0, 0});
    CU_ASSERT_EQUAL(idx, 0);

    idx = positionToIndex((Position){BOARD_SIZE - 1, 0});
    CU_ASSERT_EQUAL(idx, BOARD_SIZE - 1);

    idx = positionToIndex((Position){BOARD_SIZE - 1, BOARD_SIZE - 1});
    CU_ASSERT_EQUAL(idx, TOTAL_SQUARES - 1);

    idx = positionToIndex((Position){0, BOARD_SIZE - 1});
    CU_ASSERT_EQUAL(idx, TOTAL_SQUARES - BOARD_SIZE);

}

void test_createPiece() {
    Piece* piece = createPiece(FLAT, WHITE);
    CU_ASSERT_PTR_NOT_NULL(piece);
    CU_ASSERT_EQUAL(piece->stone, FLAT);
    CU_ASSERT_EQUAL(piece->color, WHITE);
    freePieceStack(piece);
}

void test_createBoard() {
    Board* board = createEmptyBoard();
    CU_ASSERT_PTR_NOT_NULL(board);
    for (int i = 0; i < TOTAL_SQUARES; i++) {
        CU_ASSERT_PTR_NULL(board->squares[i].head);
    }
    freeBoard(board);
}

void test_squareInsertPiece() {
    Square square = {NULL, 0};
    Piece* piece = createPiece(FLAT, WHITE);
    squareInsertPiece(NULL, &square, piece);
    CU_ASSERT_PTR_EQUAL(square.head, piece);
    CU_ASSERT_EQUAL(square.numPieces, 1);
    freePieceStack(square.head);
}

void test_squareRemovePiece() {
    Square square = {NULL, 0};
    Piece* piece = createPiece(FLAT, WHITE);
    squareInsertPiece(NULL, &square, piece);
    Piece* removed = squareRemovePiece(NULL, &square);
    CU_ASSERT_PTR_EQUAL(removed, piece);
    CU_ASSERT_EQUAL(square.numPieces, 0);
    freePieceStack(removed);
}

void test_squareRemovePieces() {
    Square square = {NULL, 0};
    for (int i = 0; i < 3; i++) {
        Piece* piece = createPiece(FLAT, WHITE);
        squareInsertPiece(NULL, &square, piece);
    }
    Piece* removed = squareRemovePieces(NULL, &square, 2);
    CU_ASSERT_PTR_NOT_NULL(removed);
    CU_ASSERT_EQUAL(square.numPieces, 1);
    freePieceStack(removed);
    freePieceStack(square.head);
}

void test_copyGameState() {
    GameState* original = createGameState();
    Position pos = {0, 0};
    Piece* piece = createPiece(FLAT, WHITE);
    Square* originalSquare = readSquare(original->board, pos);
    squareInsertPiece(original, originalSquare, piece);
    original->turn = BLACK;

    GameState* copy = copyGameState(original);
    CU_ASSERT_PTR_NOT_NULL(copy);
    Square* copySquare = readSquare(copy->board, pos);
    CU_ASSERT_EQUAL(copySquare->numPieces, 1);
    CU_ASSERT_EQUAL(copy->turn, BLACK);

    squareRemovePiece(copy, copySquare);
    CU_ASSERT_EQUAL(copySquare->numPieces, 0);
    CU_ASSERT_EQUAL(originalSquare->numPieces, 1);

    freeGameState(original);
    freeGameState(copy);
}

void test_checkRoadWin() {
    GameState* state = createGameState();
    // Create a vertical road for Black
    for (int y = 0; y < BOARD_SIZE; y++) {
        Position pos = {0, y};
        Square* sq = readSquare(state->board, pos);
        Piece* p = createPiece(FLAT, BLACK);
        squareInsertPiece(state, sq, p);
    }
    Result result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, ROAD_BLACK);
    freeGameState(state);

    state = createGameState();
    // Create a horizontal road for White
    for (int x = 0; x < BOARD_SIZE; x++) {
        Position pos = {x, 0};
        Square* sq = readSquare(state->board, pos);
        Piece* p = createPiece(FLAT, WHITE);
        squareInsertPiece(state, sq, p);
    }
    result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, ROAD_WHITE);

    // Ensure that a road win is not detected if the road is not long enough
    squareRemovePiece(state, readSquare(state->board, (Position){0, 0}));
    result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, CONTINUE);

    // Ensure that a road win is not detected if the road includes standing stones
    Piece* p = createPiece(STANDING, WHITE);
    squareInsertPiece(state, readSquare(state->board, (Position){0, 0}), p);
    result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, CONTINUE);

    // Ensure that a road win is detected if the road includes a capstone
    squareRemovePiece(state, readSquare(state->board, (Position){0, 0}));
    p = createPiece(CAP, WHITE);
    squareInsertPiece(state, readSquare(state->board, (Position){0, 0}), p);
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
            Position pos = {i, j};
            Color c = WHITE;
            if (i == j) {
                c = BLACK;
            }
            Piece* piece = createPiece(FLAT, c);
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
            Position pos = {i, j};
            Color c = WHITE;
            if ((i + j) % 2 == 0) {
                c = BLACK;
            }
            Piece* piece = createPiece(FLAT, c);
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
    
    squareInsertPiece(state, readSquare(state->board, (Position){0, 0}), createPiece(FLAT, WHITE));
    
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
    
}
