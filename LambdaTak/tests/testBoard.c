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

int main() {
    CU_initialize_registry();
    
    CU_pSuite suite = CU_add_suite("BoardTests", 0, 0);
    CU_add_test(suite, "test_createPiece", test_createPiece);
    CU_add_test(suite, "test_createBoard", test_createBoard);
    CU_add_test(suite, "test_squareInsertPiece", test_squareInsertPiece);

    CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();
    
    CU_cleanup_registry();
    return 0;
}

