#include "perft.h"

u64 perft(GameState* state, int depth, int currentDepth, u64 nodes) {

    GeneratedMoves* moves;
    if (currentDepth == depth) {
        moves = generateAllMoves(state);
        u64 numMoves = moves->numMoves;
        freeGeneratedMoves(moves);
        return numMoves;
    } else if (checkGameResult(state) != CONTINUE) {
        return 0;
    }
    moves = generateAllMoves(state);

    for (int i = 0; i < moves->numMoves; i++) {
        makeMoveNoChecks(state, &moves->moves[i], false);
        nodes += perft(state, depth, currentDepth + 1, 0);
        undoMoveNoChecks(state, &moves->moves[i], false);

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

u64 perftAlphaBeta(GameState* state, int depth) {
    u64 nodes = 0;
    Move m = negaMaxRoot(state, depth, &nodes);
    printMove(&m);
    return nodes;
}

void runPerftAlphaBeta(GameState *state, int maxDepth) {
    u64 nodes[maxDepth];
    clock_t times[maxDepth];
    for (int i = 0; i < maxDepth; i++) {
        nodes[i] = 0;
        times[i] = 0;
    }

    GameState* copy;
    for (int i = 1; i < maxDepth; i++) {
        copy = copyGameState(state);
        clock_t start = clock();
        nodes[i] = perftAlphaBeta(copy, i);
        clock_t end = clock();
        times[i] = end - start;
        printf("Depth %d: %llu nodes, %f seconds, %f Mnps\n", i + 1, nodes[i], (double)times[i] / CLOCKS_PER_SEC, (double)nodes[i] / ((double)times[i] / CLOCKS_PER_SEC) / 1000000);
        freeGameState(copy);
    }
}
