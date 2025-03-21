#include "../lib/board.h"
#include "../ai/neuralNetTrainer.h"
#include "../ai/neuralNetworks.h"

int main() {
    srand(time(NULL));
    initZobristTable();

    int layerSizes[] = {(7 * TOTAL_SQUARES), (7 * TOTAL_SQUARES), 
        (7 * TOTAL_SQUARES), (7 * TOTAL_SQUARES), (7 * TOTAL_SQUARES), 
        (7 * TOTAL_SQUARES), (4 * TOTAL_SQUARES), (4 * TOTAL_SQUARES), 
        (4 * TOTAL_SQUARES), (4 * TOTAL_SQUARES), (4 * TOTAL_SQUARES), 
        (4 * TOTAL_SQUARES), (4 * TOTAL_SQUARES), 36, 36, 36, 36, 6, 6, 6, 1};
    int numLayers = 21;

    // calculate the number of weights including biases
    int numWeights = 7 * TOTAL_SQUARES;
    for (int i = 1; i < numLayers; i++) {
        numWeights += layerSizes[i] * (layerSizes[i - 1] + 1);
    }
    printf("Number of weights: %d\n", numWeights);

    printf("Creating neural net\n");
    // last layer is sigmoid hardcoded
    DenseNeuralNet net = createDenseNeuralNet(layerSizes, numLayers, Relu);

    loadDenseNeuralNet(&net, "n_models/tak_model.weights_3");
    Trainer* trainer = createTrainer(&net, 0.9, 0.005, 0.1, 100);

    printf("Training\n");
    trainHybrid(trainer, 500, 100);

    // fun to look at
    // and make sure no nan's or anything weird
    printDenseNeuralNet(&net);

    saveDenseNeuralNet(&net, "n_models/tak_model.weights_3");

    return 0;
}

