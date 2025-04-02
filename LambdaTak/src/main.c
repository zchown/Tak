#include "aiPlayer.h"
#include "../lib/perft.h"
#include "../lib/tps.h"
#include "../lib/board.h"
#include "../ai/monteCarlo.h"

int main() {
    /* GameState* state = parseTPS("[TPS x6/x6/x6/x6/x6/x6 0 1]"); */
    /*  */
    /* int layerSizes[] = {(7 * TOTAL_SQUARES), (7 * TOTAL_SQUARES), (7 * TOTAL_SQUARES), 252, 252, 252, 64, 64, 32, 32, 16, 16, 8, 4, 1}; */
    /* int numLayers = 15; */
    /*  */
    /* DenseNeuralNet net = createDenseNeuralNet(layerSizes, numLayers, Relu); */
    /* loadDenseNeuralNet(&net, "n_models/tak_model.weights_large"); */
    /*  */
    /* while(true) { */
    /*     monteCarloTreeSearch(state, 1000, &net); */
    /* } */
    /* runPerft(state, 5); */
    /* return 0; */

    initZobristTable();
    return runAI();
}

