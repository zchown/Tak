#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>
#include "../src/board.h"

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
    squareInsertPiece(&square, piece);
    CU_ASSERT_PTR_EQUAL(square.head, piece);
    CU_ASSERT_EQUAL(square.numPieces, 1);
    freePieceStack(square.head);
}

void test_squareRemovePiece() {
    Square square = {NULL, 0};
    Piece* piece = createPiece(FLAT, WHITE);
    squareInsertPiece(&square, piece);
    Piece* removed = squareRemovePiece(&square);
    CU_ASSERT_PTR_EQUAL(removed, piece);
    CU_ASSERT_EQUAL(square.numPieces, 0);
    freePieceStack(removed);
}

void test_squareRemovePieces() {
    Square square = {NULL, 0};
    for (int i = 0; i < 3; i++) {
        Piece* piece = createPiece(FLAT, WHITE);
        squareInsertPiece(&square, piece);
    }
    Piece* removed = squareRemovePieces(&square, 2);
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
    squareInsertPiece(originalSquare, piece);
    original->turn = BLACK;

    GameState* copy = copyGameState(original);
    CU_ASSERT_PTR_NOT_NULL(copy);
    Square* copySquare = readSquare(copy->board, pos);
    CU_ASSERT_EQUAL(copySquare->numPieces, 1);
    CU_ASSERT_EQUAL(copy->turn, BLACK);

    squareRemovePiece(copySquare);
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
        squareInsertPiece(sq, p);
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
        squareInsertPiece(sq, p);
    }
    result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, ROAD_WHITE);

    // Ensure that a road win is not detected if the road is not long enough
    squareRemovePiece(readSquare(state->board, (Position){0, 0}));
    result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, CONTINUE);

    // Ensure that a road win is not detected if the road includes standing stones
    Piece* p = createPiece(STANDING, WHITE);
    squareInsertPiece(readSquare(state->board, (Position){0, 0}), p);
    result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, CONTINUE);

    // Ensure that a road win is detected if the road includes a capstone
    squareRemovePiece(readSquare(state->board, (Position){0, 0}));
    p = createPiece(CAP, WHITE);
    squareInsertPiece(readSquare(state->board, (Position){0, 0}), p);
    result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, ROAD_WHITE);
    freeGameState(state);
}

void test_checkFullBoard() {
    GameState* state = createGameState();
    // Fill the board with White flats
    for (int i = 0; i < TOTAL_SQUARES; i++) {
        Piece* piece = createPiece(FLAT, WHITE);
        squareInsertPiece(&state->board->squares[i], piece);
    }
    Result result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, FLAT_WHITE);
    freeGameState(state);

    // Detects a draw if the board is filled with alternating flats
    state = createGameState();
    for (int i = 0; i < TOTAL_SQUARES; i++) {
        Piece* piece = createPiece(FLAT, i % 2 == 0 ? WHITE : BLACK);
        squareInsertPiece(&state->board->squares[i], piece);
    }
    result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, DRAW);
    freeGameState(state);

    // Detect win if reserves are empty
    state = createGameState();
    state->player1.stones = 0;
    state->player1.caps = 0;

    squareInsertPiece(readSquare(state->board, (Position){0, 0}), createPiece(FLAT, WHITE));

    result = checkGameResult(state);
    CU_ASSERT_EQUAL(result, FLAT_WHITE);
}

void test_parseMove_place() {
    Move* move = parseMove("a1", WHITE);
    CU_ASSERT_PTR_NOT_NULL(move);
    if (move) {
        CU_ASSERT_EQUAL(move->type, PLACE);
        CU_ASSERT_EQUAL(move->move.place.pos.x, 0);
        CU_ASSERT_EQUAL(move->move.place.pos.y, 0);
        freeMove(move);
    }

    move = parseMove("Cb2", BLACK);
    CU_ASSERT_PTR_NOT_NULL(move);
    if (move) {
        CU_ASSERT_EQUAL(move->type, PLACE);
        CU_ASSERT_EQUAL(move->move.place.stone, CAP);
        CU_ASSERT_EQUAL(move->move.place.pos.x, 1);
        freeMove(move);
    }
}

void test_parseMove_slide() {
    Move* move = parseMove("3a1>123*", WHITE);
    CU_ASSERT_PTR_NOT_NULL(move);
    if (move) {
        CU_ASSERT_EQUAL(move->type, SLIDE);
        CU_ASSERT_EQUAL(move->move.slide.count, 3);
        CU_ASSERT_EQUAL(move->move.slide.direction, RIGHT);
        CU_ASSERT_EQUAL(move->move.slide.drops[0], 1);
        CU_ASSERT_EQUAL(move->move.slide.drops[1], 2);
        CU_ASSERT_EQUAL(move->move.slide.drops[2], 3);
        CU_ASSERT_EQUAL(move->move.slide.crush, CRUSH);
        freeMove(move);
    }

    Move* move2 = parseMove("1b2<1", BLACK);
    CU_ASSERT_PTR_NOT_NULL(move2);
    if (move2) {
        CU_ASSERT_EQUAL(move2->type, SLIDE);
        CU_ASSERT_EQUAL(move2->move.slide.count, 1);
        CU_ASSERT_EQUAL(move2->move.slide.direction, LEFT);
        CU_ASSERT_EQUAL(move2->move.slide.drops[0], 1);
        CU_ASSERT_EQUAL(move2->move.slide.crush, NO_CRUSH);
        freeMove(move2);
    }
}

