#include "../lib/board.h"
#include "../ai/neuralNetTrainer.h"
#include "../ai/neuralNetworks.h"

int main() {
    srand(time(NULL));
    initZobristTable();

    int layerSizes[] = {(7 * TOTAL_SQUARES), (7 * TOTAL_SQUARES), (7 * TOTAL_SQUARES), 252, 252, 252, 64, 64, 32, 32, 16, 16, 8, 4, 1};
    int numLayers = 15;

    /* int layerSizes[] = {(7 * TOTAL_SQUARES), 64, 32, 8, 1}; */
    /* int numLayers = 5; */

    // calculate the number of weights including biases
    int numWeights = 7 * TOTAL_SQUARES;
    for (int i = 1; i < numLayers; i++) {
        numWeights += layerSizes[i] * (layerSizes[i - 1] + 1);
    }
    printf("Number of weights: %d\n", numWeights);

    printf("Creating neural net\n");
    // last layer is sigmoid hardcoded
    DenseNeuralNet net = createDenseNeuralNet(layerSizes, numLayers, Relu);

    loadDenseNeuralNet(&net, "n_models/tak_model.weights_large");
    Trainer* trainer = createTrainer(&net, 0.9, 0.005, 0.1, 100);

    printf("Training\n");
    /* trainHybrid(trainer, 5000000, 100); */
    trainAlphaBeta(trainer, 5000000, 100);
    /* train(trainer, 5000000); */

    // fun to look at
    // and make sure no nan's or anything weird
    printDenseNeuralNet(&net);

    saveDenseNeuralNet(&net, "n_models/tak_model.weights_large");

    return 0;
}

