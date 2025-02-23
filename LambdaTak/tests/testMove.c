#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>
#include "../src/board.h"
#include "../src/tps.h"
#include "../src/moves.h"

void test_checkPlaceMove() {
    GameState* state = parseTPS("[TPS x6/x6/x6/x6/x6/x6 1 10]");
    /* Move move = createPlaceMove((Position){0, 0}, WHITE, FLAT); */
    Move move = createPlaceMove(SET_POS(0, 0), WHITE, FLAT);
    MoveResult result = checkMove(state, &move);
    CU_ASSERT_EQUAL(result, SUCCESS);
    freeGameState(state);

    state = parseTPS("[TPS x6/x6/x6/x6/x6/x6 1 10]");
    /* move = createPlaceMove((Position){0, 0}, BLACK, STANDING); */
    move = createPlaceMove(SET_POS(0, 0), BLACK, STANDING);
    result = checkMove(state, &move);
    CU_ASSERT_EQUAL(result, INVALID_COLOR);
    freeGameState(state);

    state = parseTPS("[TPS x6/x6/x6/x6/x6/x6 1 10]");
    /* move = createPlaceMove((Position){0, 0}, WHITE, CAP); */
    move = createPlaceMove(SET_POS(0, 0), WHITE, CAP);
    result = checkMove(state, &move);
    CU_ASSERT_EQUAL(result, SUCCESS);
    freeGameState(state);

    state = parseTPS("[TPS x6/x6/x6/x6/x6/1,x5 1 10]");
    /* move = createPlaceMove((Position){0, 0}, WHITE, STANDING); */
    move = createPlaceMove(SET_POS(0, 0), WHITE, STANDING);
    result = checkMove(state, &move);
    CU_ASSERT_EQUAL(result, INVALID_POSITION);
    freeGameState(state);

    state = parseTPS("[TPS x6/x6/x6/x6/x6/1,x5 1 10]");
    /* move = createPlaceMove((Position){0, 7}, WHITE, FLAT); */
    move = createPlaceMove(SET_POS(0, 7), WHITE, FLAT);
    result = checkMove(state, &move);
    CU_ASSERT_EQUAL(result, INVALID_POSITION);
    freeGameState(state);
}

void test_checkSlideMove() {
    GameState* state = parseTPS("[TPS x6/x6/x6/x2,121,1,x2/x6/x6 1 10]");

    uint16_t drops = 1;  // Packed 3-bit drop values (first drop = 1)
    /* Move move = createSlideMove(WHITE, (Position){2, 2}, RIGHT, 1, drops, NO_CRUSH); */
    Move move = createSlideMove(WHITE, SET_POS(2, 2), RIGHT, 1, drops, NO_CRUSH);
    MoveResult result = checkMove(state, &move);
    CU_ASSERT_EQUAL(result, SUCCESS);

    /* move = createSlideMove(WHITE, (Position){2, 2}, DOWN, 1, drops, CRUSH); */
    move = createSlideMove(WHITE, SET_POS(2, 2), DOWN, 1, drops, CRUSH);
    result = checkMove(state, &move);
    CU_ASSERT_EQUAL(result, INVALID_CRUSH);

    /* move = createSlideMove(WHITE, (Position){2, 2}, RIGHT, 5, drops, NO_CRUSH); */
    move = createSlideMove(WHITE, SET_POS(2, 2), RIGHT, 5, drops, NO_CRUSH);
    result = checkMove(state, &move);
    CU_ASSERT_EQUAL(result, INVALID_DROPS);

    drops |= (2 << 3);  // Add second drop (2) at bits 5-3
    /* move = createSlideMove(WHITE, (Position){2, 2}, RIGHT, 2, drops, NO_CRUSH); */
    move = createSlideMove(WHITE, SET_POS(2, 2), RIGHT, 2, drops, NO_CRUSH);
    result = checkMove(state, &move);
    CU_ASSERT_EQUAL(result, INVALID_DROPS);

    /* move = createSlideMove(WHITE, (Position){2, 2}, 7, 3, drops, NO_CRUSH); */
    move = createSlideMove(WHITE, SET_POS(2, 2), 7, 3, drops, NO_CRUSH);
    result = checkMove(state, &move);
    CU_ASSERT_EQUAL(result, INVALID_DIRECTION);

    /* move = createSlideMove(BLACK, (Position){2, 2}, RIGHT, 3, drops, NO_CRUSH); */
    move = createSlideMove(BLACK, SET_POS(2, 2), RIGHT, 3, drops, NO_CRUSH);
    result = checkMove(state, &move);
    CU_ASSERT_EQUAL(result, INVALID_COLOR);

    /* move = createSlideMove(WHITE, (Position){2, 2}, RIGHT, 3, drops, NO_CRUSH); */
    move = createSlideMove(WHITE, SET_POS(2, 2), RIGHT, 3, drops, NO_CRUSH);
    result = checkMove(state, &move);
    CU_ASSERT_EQUAL(result, SUCCESS);
    freeGameState(state);

    state = parseTPS("[TPS x6/x6/x6/x2,121C,2S,x2/x6/x6 1 10]");
    /* move = createSlideMove(WHITE, (Position){2, 2}, RIGHT, 3, drops, NO_CRUSH); */
    move = createSlideMove(WHITE, SET_POS(2, 2), RIGHT, 3, drops, NO_CRUSH);
    result = checkMove(state, &move);
    CU_ASSERT_EQUAL(result, INVALID_SLIDE);

    drops &= ~(7 << 3);  // Reset second drop (bits 5-3) to 0
    /* move = createSlideMove(WHITE, (Position){2, 2}, RIGHT, 1, drops, CRUSH); */
    move = createSlideMove(WHITE, SET_POS(2, 2), RIGHT, 1, drops, CRUSH);
    result = checkMove(state, &move);
    CU_ASSERT_EQUAL(result, SUCCESS);
}

void test_makeMove() {
    GameState* state = parseTPS("[TPS x6/x6/x6/x6/x6/x6 1 10]");
    /* Move move = createPlaceMove((Position){0, 0}, WHITE, FLAT); */
    Move move = createPlaceMove(SET_POS(0, 0), WHITE, FLAT);
    MoveResult result = makeMoveChecks(state, &move);
    CU_ASSERT_EQUAL(result, SUCCESS);
    /* CU_ASSERT_EQUAL(readSquare(state->board, (Position){0, 0})->head->stone, FLAT); */
    CU_ASSERT_EQUAL(readSquare(state->board, SET_POS(0, 0))->head->stone, FLAT);
    freeGameState(state);

    state = parseTPS("[TPS x6/x6/x6/x2,1,x3/x6/x6 1 10]");
    /* move = createSlideMove(WHITE, (Position){2, 2}, RIGHT, 1, 1, NO_CRUSH);  // 1 drop packed */
    move = createSlideMove(WHITE, SET_POS(2, 2), RIGHT, 1, 1, NO_CRUSH);  // 1 drop packed
    result = makeMoveChecks(state, &move);
    CU_ASSERT_EQUAL(result, SUCCESS);
    /* CU_ASSERT_EQUAL(readSquare(state->board, (Position){3, 2})->head->stone, FLAT); */
    CU_ASSERT_EQUAL(readSquare(state->board, SET_POS(3, 2))->head->stone, FLAT);
    freeGameState(state);
}

void test_undoMove() {
    GameState* state = parseTPS("[TPS x6/x6/x6/x6/x6/x6 1 10]");
    /* Move move = createPlaceMove((Position){0, 0}, WHITE, FLAT); */
    Move move = createPlaceMove(SET_POS(0, 0), WHITE, FLAT);
    makeMoveNoChecks(state, &move, true);
    undoMoveNoChecks(state, &move, true);
    /* CU_ASSERT_EQUAL(readSquare(state->board, (Position){0, 0})->head, NULL); */
    CU_ASSERT_EQUAL(readSquare(state->board, SET_POS(0, 0))->head, NULL);
    freeGameState(state);

    state = parseTPS("[TPS x6/x6/x6/x2,1,x3/x6/x6 1 10]");
    /* move = createSlideMove(WHITE, (Position){2, 2}, RIGHT, 1, 1, NO_CRUSH);  // 1 drop packed */
    move = createSlideMove(WHITE, SET_POS(2, 2), RIGHT, 1, 1, NO_CRUSH);  // 1 drop packed
    makeMoveNoChecks(state, &move, true);
    undoMoveNoChecks(state, &move, true);
    /* CU_ASSERT_EQUAL(readSquare(state->board, (Position){2, 2})->head->stone, FLAT); */
    CU_ASSERT_EQUAL(readSquare(state->board, SET_POS(2, 2))->head->stone, FLAT);
    /* CU_ASSERT_EQUAL(readSquare(state->board, (Position){2, 3})->head, NULL); */
    CU_ASSERT_EQUAL(readSquare(state->board, SET_POS(2, 3))->head, NULL);
    freeGameState(state);
}

void test_generateAllMoves() {
    GameState* state = parseTPS("[TPS x6/x6/x6/x6/x6/x6 1 1]");
    GeneratedMoves* moves = generateAllMoves(state);
    CU_ASSERT_EQUAL(moves->numMoves, 36);
    /* printf("\n"); */
    /* for (u8 i = 0; i < moves->numMoves; i++) { */
    /*     printMove(&moves->moves[i]); */
    /* } */
    freeGeneratedMoves(moves);
    freeGameState(state);

    state = parseTPS("[TPS x6/x6/x6/x6/x6/1,x5 1 2]");
    moves = generateAllMoves(state);
    CU_ASSERT_EQUAL(moves->numMoves, 35);
    freeGeneratedMoves(moves);
    freeGameState(state);

    state = parseTPS("[TPS 2S,2S,2S,2S,2S,2S/1S,1S,1S,1S,1S,1S/2S,2S,2S,2S,2S,2S/1S,1S,1S,1S,1S,1S/2S,2S,2S,2S,2S,2S/11,x5 1 3]");
    moves = generateAllMoves(state);
    printf("Generated %d moves\n", moves->numMoves);
    /* for (u8 i = 0; i < moves->numMoves; i++) { */
    /*     printMove(&moves->moves[i]); */
    /* } */
    CU_ASSERT_EQUAL(moves->numMoves, 18);
    freeGeneratedMoves(moves);
    freeGameState(state);
    
    state = parseTPS("[TPS 2,2,21S,2,2,2/2,x,222221,2,2,x/1,1,2221C,x,111112C,2S/x,1,2S,x2,121211212/1,1,1212S,1S,2,1S/x2,2,1,21,1 1 42]");
    moves = generateAllMoves(state);
    printf("Generated %d moves\n", moves->numMoves);
    CU_ASSERT_EQUAL(moves->numMoves, 140);
    freeGeneratedMoves(moves);
    freeGameState(state);

    /* state = parseTPS("[TPS 2,2,21S,2,2,2/2,x,222221,2,2,x/1,1,2221C,x,111112C,2S/x,1,2S,x2,121211212/1,1,1212S,1S,21S,x/x2,2,1,21,1 2 42]"); */
    /* state = parseTPS("[TPS 2,2,21S,2,2,2/2,x,222221,2,2,x/1,1,2221C,x,111112C,2S/x,1,2S,x2,121211212/1,1,1212S,1S,21S,x/x2,2,1,21,1 2 42]"); */
    /*  */
    /* char* tps = gameStateToTPS(state); */
    /* printf("TPS: %s\n", tps); */
    /* printf("TPS: %s\n", tps); */
    /* CU_ASSERT_STRING_EQUAL(tps, "[TPS 2,2,21S,2,2,2/2,x,222221,2,2,x/1,1,2221C,x,111112C,2S/x,1,2S,x2,121211212/1,1,1212S,1S,21S,x/x2,2,1,21,1 2 42]"); */
    /*  */
    /* moves = generateAllMoves(state); */
    /* printf("Generated %d moves\n", moves->numMoves); */
}

