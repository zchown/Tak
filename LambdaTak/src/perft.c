#include "perft.h"

u64 perft(GameState* state, int depth, int currentDepth, u64 nodes) {
    /* if (checkGameResult(state) != CONTINUE) { */
    /*     return 1; */
    /* } */

    char* tps = gameStateToTPS(state);
    printf("TPS: %s\n", tps);
    GeneratedMoves* moves = generateAllMoves(state);
    printf("Depth: %d, Nodes: %llu\n", currentDepth, nodes);
    printf("Moves: %d\n", moves->numMoves);
    for (int i = 0; i < moves->numMoves; i++) {
        printMove(&moves->moves[i]);
    }
    if (currentDepth == depth) {
        u64 numMoves = moves->numMoves;
        freeGeneratedMoves(moves);
        return numMoves;
    }

    for (int i = 0; i < moves->numMoves; i++) {
        /* int r = makeMoveChecks(state, &moves->moves[i]); */
        /* if (r != SUCCESS) { */
        /*     printf("Invalid move: %d\n", r); */
        /*     printf("Depth: %d\n", currentDepth); */
        /*     printf("movetype: %d\n", moves->moves[i].type); */
        /*     printMove(&moves->moves[i]); */
        /*     printf("tps: %s\n", gameStateToTPS(state)); */
        /*     printf("color: %d\n", state->turn); */
        /*     printf("bitboard white: %llu\n", state->whiteControlled); */
        /*     printf("bitboard black: %llu\n", state->blackControlled); */
        /*     Reserves res = (state->turn== WHITE) ? state->player1 : state->player2; */
        /*     printf("reserves, stones: %d, caps: %d\n", res.stones, res.caps); */
        /*     exit(1); */
        /* } */
        makeMoveNoChecks(state, &moves->moves[i], false);
        nodes += perft(state, depth, currentDepth + 1, 0);
        undoMoveNoChecks(state, &moves->moves[i], false);
        /* r = undoMoveChecks(state, &moves->moves[i]); */
        /* if (r != SUCCESS) { */
        /*     printf("Invalid undo: %d\n", r); */
        /*     printMove(&moves->moves[i]); */
        /*     printf("tps: %s\n", gameStateToTPS(state)); */
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
        nodes[i] = perft(copy, i, 0, 0);
        clock_t end = clock();
        times[i] = end - start;
        printf("Depth %d: %llu nodes, %f seconds, %f Mnps\n", i + 1, nodes[i], (double)times[i] / CLOCKS_PER_SEC, (double)nodes[i] / ((double)times[i] / CLOCKS_PER_SEC) / 1000000);
        freeGameState(copy);
    }
}

