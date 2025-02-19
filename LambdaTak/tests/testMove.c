#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>
#include "../src/board.h"
#include "../src/tps.h"
#include "../src/moves.h"

void test_checkPlaceMove() {
    GameState* state = parseTPS("[TPS x6/x6/x6/x6/x6/x6 1 10]");
    Move* move = createPlaceMove((Position){0, 0}, WHITE, FLAT);
    MoveResult result = checkMove(state, move);
    CU_ASSERT_EQUAL(result, SUCCESS);
    freeGameState(state);

    state = parseTPS("[TPS x6/x6/x6/x6/x6/x6 1 10]");
    move = createPlaceMove((Position){0, 0}, BLACK, STANDING);
    result = checkMove(state, move);
    CU_ASSERT_EQUAL(result, INVALID_COLOR);
    freeGameState(state);
    freeMove(move);

    state = parseTPS("[TPS x6/x6/x6/x6/x6/x6 1 10]");
    move = createPlaceMove((Position){0, 0}, WHITE, CAP);
    result = checkMove(state, move);
    CU_ASSERT_EQUAL(result, SUCCESS);
    freeGameState(state);
    freeMove(move);

    state = parseTPS("[TPS x6/x6/x6/x6/x6/1,x5 1 10]");
    move = createPlaceMove((Position){0, 0}, WHITE, STANDING);
    result = checkMove(state, move);
    CU_ASSERT_EQUAL(result, INVALID_POSITION);
    freeGameState(state);
    freeMove(move);
    
    state = parseTPS("[TPS x6/x6/x6/x6/x6/1,x5 1 10]");
    move = createPlaceMove((Position){0, 7}, WHITE, FLAT);
    result = checkMove(state, move);
    CU_ASSERT_EQUAL(result, INVALID_POSITION);
    freeGameState(state);
    freeMove(move);
}

void test_checkSlideMove() {
    GameState* state = parseTPS("[TPS x6/x6/x6/x2,121,1,x2/x6/x6 1 10]");
    u8 drops[MAX_PICKUP] = {1, 0, 0, 0, 0, 0};
    Move* move = createSlideMove(WHITE, (Position){2, 2}, RIGHT, 1, drops, NO_CRUSH);
    MoveResult result = checkMove(state, move);
    CU_ASSERT_EQUAL(result, SUCCESS);
    freeMove(move);

    move = createSlideMove(WHITE, (Position){2, 2}, DOWN, 1, drops, CRUSH);
    result = checkMove(state, move);
    CU_ASSERT_EQUAL(result, INVALID_CRUSH);
    freeMove(move);

    move = createSlideMove(WHITE, (Position){2, 2}, RIGHT, 5, drops, NO_CRUSH);
    result = checkMove(state, move);
    CU_ASSERT_EQUAL(result, INVALID_DROPS);
    freeMove(move);
    
    drops[1] = 2;
    move = createSlideMove(WHITE, (Position){2, 2}, RIGHT, 2, drops, NO_CRUSH);
    result = checkMove(state, move);
    CU_ASSERT_EQUAL(result, INVALID_DROPS);
    freeMove(move);
    
    move = createSlideMove(WHITE, (Position){2, 2}, 7, 3, drops, NO_CRUSH);
    result = checkMove(state, move);
    CU_ASSERT_EQUAL(result, INVALID_DIRECTION);
    freeMove(move);
    
    move = createSlideMove(BLACK, (Position){2, 2}, RIGHT, 3, drops, NO_CRUSH);
    result = checkMove(state, move);
    CU_ASSERT_EQUAL(result, INVALID_COLOR);
    freeMove(move);
    
    move = createSlideMove(WHITE, (Position){2, 2}, RIGHT, 3, drops, NO_CRUSH);
    result = checkMove(state, move);
    CU_ASSERT_EQUAL(result, SUCCESS);
    freeGameState(state);
    freeMove(move);
    
    state = parseTPS("[TPS x6/x6/x6/x2,121C,2S,x2/x6/x6 1 10]");
    move = createSlideMove(WHITE, (Position){2, 2}, RIGHT, 3, drops, NO_CRUSH);
    result = checkMove(state, move);
    CU_ASSERT_EQUAL(result, INVALID_SLIDE);
    freeMove(move);
    
    drops[1] = 0;
    move = createSlideMove(WHITE, (Position){2, 2}, RIGHT, 1, drops, CRUSH);
    result = checkMove(state, move);
    CU_ASSERT_EQUAL(result, SUCCESS);
}
