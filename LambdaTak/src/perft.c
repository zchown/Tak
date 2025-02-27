#include "perft.h"

u64 perft(GameState* state, int depth, int currentDepth, u64 nodes, u32 prevMoves) {

    GeneratedMoves* moves;
    if (currentDepth == depth) {
        /* return 1; */
        u64 numMoves = countAllMoves(state);
        return numMoves;
    } 
    /* else if (checkGameResult(state) != CONTINUE) { */
    /*     return 0; */
    /* } */
    moves = generateAllMoves(state, prevMoves);

    for (int i = 0; i < moves->numMoves; i++) {
        /* int result = makeMoveChecks(state, &moves->moves[i]); */
        /* if (result != SUCCESS) { */
        /*     printf("Result: %d\n", result); */
        /*     printf("Invalid move: "); */
        /*     printMove(&moves->moves[i]); */
        /*     printf("\n"); */
        /*     char* tps = gameStateToTPS(state); */
        /*     printf("TPS: %s\n", tps); */
        /*     freeGeneratedMoves(moves); */
        /*     exit(1); */
        /* } */
        makeMoveNoChecks(state, &moves->moves[i], false);
        nodes += perft(state, depth, currentDepth + 1, 0, moves->numMoves);
        undoMoveNoChecks(state, &moves->moves[i], false);
        /* result = undoMoveChecks(state, &moves->moves[i]); */
        /* if (result != SUCCESS) { */
        /*     printf("Result: %d\n", result); */
        /*     printf("Invalid undo: "); */
        /*     printMove(&moves->moves[i]); */
        /*     printf("\n"); */
        /*     freeGeneratedMoves(moves); */
        /*     exit(1); */
        /* } */

    }
    freeGeneratedMoves(moves);
    return nodes;
}

void runPerft(GameState* state, int maxDepth) {
    u64 nodes[maxDepth];
    clock_t times[maxDepth];
    for (int i = 0; i <= maxDepth; i++) {
        nodes[i] = 0;
        times[i] = 0;
    }


    GameState* copy;
    for (int i = 0; i < maxDepth; i++) {
        copy = copyGameState(state);
        clock_t start = clock();
        nodes[i] = perft(copy, i, 0, 0, 512);
        clock_t end = clock();
        times[i] = end - start;
        printf("Depth %d: %llu nodes, %f seconds, %f Mnps\n", i + 1, nodes[i], (double)times[i] / CLOCKS_PER_SEC, (double)nodes[i] / ((double)times[i] / CLOCKS_PER_SEC) / 1000000);
        freeGameState(copy);
    }
}

