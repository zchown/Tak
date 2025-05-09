#include "perft.h"

DenseNeuralNet net;

u64 perft(GameState* state, int depth, int currentDepth, u64 nodes, MoveListList* moves) {

    MoveList* moveList = moves->moves[currentDepth];

    if (currentDepth == depth) {
        /* generateAllMoves(state, moveList); */
        return countAllMoves(state);
        /* return moveList->numMoves; */
    } 
    else if (checkGameResult(state) != CONTINUE) {
        return 0;
    }

    generateAllMoves(state, moveList);
    Move* ms = moveList->moves;
    int n = moveList->numMoves;
    for (int i = 0; i < n; i++) {
        makeMoveNoChecks(state, &ms[i], false);
        nodes += perft(state, depth, currentDepth + 1, 0, moves);
        undoMoveNoChecks(state, &ms[i], false);
    }
    return nodes;
}

void runPerft(GameState* state, int maxDepth) {
    u64 nodes[maxDepth];
    clock_t times[maxDepth];
    for (int i = 0; i <= maxDepth; i++) {
        nodes[i] = 0;
        times[i] = 0;
    }

    /* int layerSizes[] = {(7 * TOTAL_SQUARES), (7 * TOTAL_SQUARES), (7 * TOTAL_SQUARES), 252, 252, 252, 64, 64, 32, 32, 16, 16, 8, 4, 1}; */
    /* int numLayers = 15; */
    /* net = createDenseNeuralNet(layerSizes, numLayers, Relu); */

    /* loadDenseNeuralNet(&net, "n_models/tak_model.weights_large"); */

    MoveListList* moveList = malloc(sizeof(MoveListList));
    moveList->moves = malloc(maxDepth * sizeof(MoveList*));
    for (int i = 0; i < maxDepth; i++) {
        moveList->moves[i] = createMoveList(2048);
        moveList->numLists = maxDepth;
    }


    GameState* copy;
    for (int i = 0; i < maxDepth; i++) {
        copy = copyGameState(state);
        clock_t start = clock();
        nodes[i] = perft(copy, i, 0, 0, moveList);
        clock_t end = clock();
        times[i] = end - start;
        printf("Depth %d: %llu nodes, %f seconds, %f Mnps\n", i + 1, nodes[i], (double)times[i] / CLOCKS_PER_SEC, (double)nodes[i] / ((double)times[i] / CLOCKS_PER_SEC) / 1000000);
        freeGameState(copy);
    }
}

