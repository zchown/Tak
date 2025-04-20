#include "../lib/board.h"
#include "../ai/neuralNetTrainer.h"
#include "../ai/neuralNetworks.h"

int main() {
    srand(time(NULL));
    initZobristTable();

    int layerSizes[] = {(7 * TOTAL_SQUARES * 3),
                        (7 * TOTAL_SQUARES), 
                        144, 144, 36, 36, 1};
    int numLayers = 7;

    /* int layerSizes[] = {(7 * TOTAL_SQUARES), 64, 32, 8, 1}; */
    /* int numLayers = 5; */

    // calculate the number of weights including biases
    int numWeights = 7 * TOTAL_SQUARES;
    for (int i = 1; i < numLayers; i++) {
        numWeights += layerSizes[i] * (layerSizes[i - 1] + 1);
    }
    printf("Number of weights: %d\n", numWeights);

    int numNodes = 0;
    for (int i = 0; i < numLayers; i++) {
        numNodes += layerSizes[i];
    }
    printf("Number of nodes: %d\n", numNodes);

    printf("Creating neural net\n");
    // last layer is sigmoid hardcoded
    DenseNeuralNet net = createDenseNeuralNet(layerSizes, numLayers, Relu);

    loadDenseNeuralNet(&net, "n_models/tak_model.weights_verysmall");
    Trainer* trainer = createTrainer(&net, 0.999, 0.005, 0.1, 50, 0.9);

    printf("Training\n");
    /* trainHybrid(trainer, 10000, 50); */
    trainAlphaBeta(trainer, 5000, 100);
    /* train(trainer, 5000000); */

    // fun to look at
    // and make sure no nan's or anything weird
    printDenseNeuralNet(&net);

    saveDenseNeuralNet(&net, "n_models/tak_model.weights_verysmall");

    return 0;
}

