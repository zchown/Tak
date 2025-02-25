#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>
#include "testGeneral.h"

// Declare test functions from testBoard.c
void test_checkRoadWin();
void test_checkFullBoard();
void test_checkHardRoads();

// Declare test functions from testTPS.c
void test_parseTPS_valid();
void test_parseTPS_invalid();
void test_parseTPS_with_pieces();
void test_gameStateToTPS();
void test_boardToTPS();
void test_boardToTPS_empty();

void test_checkPlaceMove();
void test_checkSlideMove();
void test_makeMove();
void test_undoMove();
void test_generateAllMoves();

int main() {
    CU_initialize_registry();

    // Add Board tests
    CU_pSuite boardSuite = CU_add_suite("BoardTests", 0, 0);
    CU_add_test(boardSuite, "test_checkRoadWin", test_checkRoadWin);
    CU_add_test(boardSuite, "test_checkFullBoard", test_checkFullBoard);
    CU_add_test(boardSuite, "test_checkHardRoads", test_checkHardRoads);

    // Add TPS tests
    CU_pSuite tpsSuite = CU_add_suite("TPSTests", 0, 0);
    CU_add_test(tpsSuite, "test_parseTPS_valid", test_parseTPS_valid);
    CU_add_test(tpsSuite, "test_parseTPS_invalid", test_parseTPS_invalid);
    CU_add_test(tpsSuite, "test_parseTPS_with_pieces", test_parseTPS_with_pieces);
    CU_add_test(tpsSuite, "test_gameStateToTPS", test_gameStateToTPS);
    CU_add_test(tpsSuite, "test_boardToTPS", test_boardToTPS);
    CU_add_test(tpsSuite, "test_boardToTPS_empty", test_boardToTPS_empty);

    // Add Move tests
    CU_pSuite suite = CU_add_suite("Move Tests", NULL, NULL);
    CU_add_test(suite, "test_checkPlaceMove", test_checkPlaceMove);
    CU_add_test(suite, "test_checkSlideMove", test_checkSlideMove);
    CU_add_test(suite, "test_makeMove", test_makeMove);
    CU_add_test(suite, "test_undoMove", test_undoMove);
    CU_add_test(suite, "test_generateAllMoves", test_generateAllMoves);

    CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();

    CU_cleanup_registry();

    printf("Running general tests...\n");
    runGeneralTests();

    return 0;
}


