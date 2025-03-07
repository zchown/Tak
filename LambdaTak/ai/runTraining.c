#include "../lib/board.h"
#include "../ai/neuralNetTrainer.h"
#include "../ai/neuralNetworks.h"

int main() {
    srand(time(NULL));
    initZobristTable();

    int layerSizes[] = {(7 * TOTAL_SQUARES), (14 * TOTAL_SQUARES), (7 * TOTAL_SQUARES), 144, 144, 72, 72, 36, 36, 6, 1};
    int numLayers = 11;
    printf("Creating neural net\n");
    DenseNeuralNet net = createDenseNeuralNet(layerSizes, numLayers, Sigmoid);

    loadDenseNeuralNet(&net, "n_models/tak_model.weights_3");
    /* printf("Creating trainer\n"); */
    Trainer* trainer = createTrainer(&net, 0.9, 0.1, 100);
    /*  */
    printf("Training\n");
    trainHybrid(trainer, 1000000, 100);
    /* trainAlphaBeta(trainer, 1000000, 50); */
    /*  */
    saveDenseNeuralNet(&net, "n_models/tak_model.weights_3");

    return 0;
}

