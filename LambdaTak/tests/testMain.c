#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>

// Declare test functions from testBoard.c
void test_createPiece();
void test_createBoard();
void test_squareInsertPiece();
void test_squareRemovePiece();
void test_squareRemovePieces();
void test_copyGameState();
void test_checkRoadWin();
void test_checkFullBoard();
void test_parseMove_place();
void test_parseMove_slide();
void test_checkHardRoads();

// Declare test functions from testTPS.c
void test_parseTPS_valid();
void test_parseTPS_invalid();
void test_parseTPS_with_pieces();
void test_gameStateToTPS();
void test_boardToTPS();
void test_boardToTPS_empty();

int main() {
    CU_initialize_registry();

    // Add Board tests
    CU_pSuite boardSuite = CU_add_suite("BoardTests", 0, 0);
    CU_add_test(boardSuite, "test_createPiece", test_createPiece);
    CU_add_test(boardSuite, "test_createBoard", test_createBoard);
    CU_add_test(boardSuite, "test_squareInsertPiece", test_squareInsertPiece);
    CU_add_test(boardSuite, "test_squareRemovePiece", test_squareRemovePiece);
    CU_add_test(boardSuite, "test_squareRemovePieces", test_squareRemovePieces);
    CU_add_test(boardSuite, "test_copyGameState", test_copyGameState);
    CU_add_test(boardSuite, "test_checkRoadWin", test_checkRoadWin);
    CU_add_test(boardSuite, "test_checkFullBoard", test_checkFullBoard);
    CU_add_test(boardSuite, "test_parseMove_place", test_parseMove_place);
    CU_add_test(boardSuite, "test_parseMove_slide", test_parseMove_slide);
    CU_add_test(boardSuite, "test_checkHardRoads", test_checkHardRoads);

    // Add TPS tests
    CU_pSuite tpsSuite = CU_add_suite("TPSTests", 0, 0);
    CU_add_test(tpsSuite, "test_parseTPS_valid", test_parseTPS_valid);
    CU_add_test(tpsSuite, "test_parseTPS_invalid", test_parseTPS_invalid);
    CU_add_test(tpsSuite, "test_parseTPS_with_pieces", test_parseTPS_with_pieces);
    CU_add_test(tpsSuite, "test_gameStateToTPS", test_gameStateToTPS);
    CU_add_test(tpsSuite, "test_boardToTPS", test_boardToTPS);
    CU_add_test(tpsSuite, "test_boardToTPS_empty", test_boardToTPS_empty);

    CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();

    CU_cleanup_registry();
    return 0;
}
