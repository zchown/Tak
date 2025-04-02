#include "perft.h"

DenseNeuralNet net;

u64 perft(GameState* state, int depth, int currentDepth, u64 nodes, u32 prevMoves) {

    GeneratedMoves* moves;
    if (currentDepth == depth) {
        feedForwardDense(&net, (7 * 36), state->gameVector, 0);
        /* evaluate(state); */
        return 1;
        /* GeneratedMoves* moves = generateAllMoves(state, prevMoves); */
        /* u64 numMoves = moves->numMoves; */
        /* freeGeneratedMoves(moves); */
        /* return numMoves; */
    } 
    else if (checkGameResult(state) != CONTINUE) {
        return 0;
    }
    moves = generateAllMoves(state, prevMoves);

    for (int i = 0; i < moves->numMoves; i++) {
        makeMoveNoChecks(state, &moves->moves[i], false);
        nodes += perft(state, depth, currentDepth + 1, 0, moves->numMoves);
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

    int layerSizes[] = {(7 * TOTAL_SQUARES), (7 * TOTAL_SQUARES), (7 * TOTAL_SQUARES), 252, 252, 252, 64, 64, 32, 32, 16, 16, 8, 4, 1};
    int numLayers = 15;
    net = createDenseNeuralNet(layerSizes, numLayers, Relu);

    loadDenseNeuralNet(&net, "n_models/tak_model.weights_large");


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

