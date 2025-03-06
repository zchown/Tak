#include "neuralNetworks.h"

Neuron createNeuron(int numInputs, Activation act) {
    Neuron neuron;
    neuron.bias = rand() / (double)RAND_MAX;
    neuron.numInputs = numInputs;

    switch (act) {
        case Sigmoid:
            neuron.activationFunction = sigmoid;
            neuron.derivativeFunction = sigmoidDerivative;
            break;
        case Relu:
            neuron.activationFunction = relu;
            neuron.derivativeFunction = reluDerivative;
            break;
        case Tanh:
            neuron.activationFunction = tanh;
            neuron.derivativeFunction = tanhDerivative;
            break;
    };

    neuron.weights = (double*)malloc(numInputs * sizeof(double));
    for (int i = 0; i < numInputs; i++) {
        neuron.weights[i] = rand() / (double)RAND_MAX;
    }
    return neuron;
}

DenseLayer createDenseLayer(int numNeurons, int numInputs, Activation act) {
    DenseLayer layer;
    layer.numNeurons = numNeurons;
    layer.inputSize = numInputs;
    layer.neurons = (Neuron*)malloc(numNeurons * sizeof(Neuron));
    layer.outputs = (double*)malloc(numNeurons * sizeof(double));
    for (int i = 0; i < numNeurons; i++) {
        layer.neurons[i] = createNeuron(numInputs, act);
    }
    return layer;
}

DenseNeuralNet createDenseNeuralNet(int* layerSizes, int numLayers, Activation act) {
    DenseNeuralNet net;
    net.numLayers = numLayers;
    net.layerSizes = (int*)malloc(numLayers * sizeof(int));
    for (int i = 0; i < numLayers; i++) {
        net.layerSizes[i] = layerSizes[i];
    }
    net.layers = (DenseLayer*)malloc(numLayers * sizeof(DenseLayer));
    for (int i = 0; i < numLayers; i++) {
        if (i == 0) {
            net.layers[i] = createDenseLayer(layerSizes[i], layerSizes[i], act);
        } else {
            net.layers[i] = createDenseLayer(layerSizes[i], layerSizes[i - 1], act);
        }
        if (i == numLayers - 1) {
            net.layers[i].neurons[0].activationFunction = sigmoid;
            net.layers[i].neurons[0].derivativeFunction = sigmoidDerivative;
        }
    }
    return net;
}

double* feedForwardDense(DenseNeuralNet* net, int inputSize, double* inputs, double dropout) {
    if (!net || !inputs || inputSize <= 0) {
        return NULL;
    }
    double* curInputs = (double*)malloc(inputSize * sizeof(double));
    if (!curInputs) {
        return NULL;
    }
    memcpy(curInputs, inputs, inputSize * sizeof(double));
    int curInputSize = inputSize;
    for (int i = 0; i < net->numLayers; i++) {
        DenseLayer* curLayer = &net->layers[i];
        if (curLayer->inputSize != curInputSize) {
            fprintf(stderr, "Error: Mismatch in input size at layer %d\n", i);
            free(curInputs);
            return NULL;
        }
        double* newInputs = (double*)malloc(curLayer->numNeurons * sizeof(double));
        if (!newInputs) {
            free(curInputs);
            return NULL;
        }
        for (int j = 0; j < curLayer->numNeurons; j++) {
            Neuron* curNeuron = &curLayer->neurons[j];
            double sum = curNeuron->bias;
            for (int k = 0; k < curInputSize; k++) {
                sum += curNeuron->weights[k] * curInputs[k];
            }
            newInputs[j] = curNeuron->activationFunction(sum);
        }
        free(curInputs);
        curInputs = newInputs;
        if (!curLayer->outputs) {
            curLayer->outputs = (double*)malloc(curLayer->numNeurons * sizeof(double));
        }
        memcpy(curLayer->outputs, curInputs, curLayer->numNeurons * sizeof(double));
        curInputSize = curLayer->numNeurons;
    }

    return curInputs;
}
void backpropagateDense(DenseNeuralNet* net, double* inputs, double* outputs, double* expectedOutputs, double learningRate) {
    double* deltas[net->numLayers];
    for (int i = net->numLayers - 1; i >= 0; i--) {
        DenseLayer* layer = &net->layers[i];
        deltas[i] = (double*)malloc(layer->numNeurons * sizeof(double));
        for (int j = 0; j < layer->numNeurons; j++) {
            Neuron* neuron = &layer->neurons[j];
            double output = layer->outputs[j];
            double error = (i == net->numLayers - 1) ? (output - expectedOutputs[j]) : 0;
            deltas[i][j] = error * neuron->derivativeFunction(output);
        }
    }
    for (int i = 0; i < net->numLayers; i++) {
        DenseLayer* layer = &net->layers[i];
        double* prevInputs = (i == 0) ? inputs : net->layers[i - 1].outputs;
        for (int j = 0; j < layer->numNeurons; j++) {
            Neuron* neuron = &layer->neurons[j];
            for (int k = 0; k < layer->inputSize; k++) {
                neuron->weights[k] -= learningRate * deltas[i][j] * prevInputs[k];
            }
            neuron->bias -= learningRate * deltas[i][j];
        }
    }
    for (int i = 0; i < net->numLayers; i++) {
        free(deltas[i]);
    }
}


double sigmoid(double x) {
    return 1.0 / (1.0 + exp(-x));
}

double relu(double x) {
    return x > 0 ? x : 0;
}

double tanh(double x) {
    return (exp(x) - exp(-x)) / (exp(x) + exp(-x));
}

double sigmoidDerivative(double x) {
    return sigmoid(x) * (1 - sigmoid(x));
}

double reluDerivative(double x) {
    return x > 0 ? 1 : 0;
}

double tanhDerivative(double x) {
    return 1 - pow(tanh(x), 2);
}

double meanSquaredError(double x, double y) {
    return 0.5 * pow(x - y, 2);
}

double crossEntropy(double x, double y) {
    return -y * log(x) - (1 - y) * log(1 - x);
}

double softmax(double x, double y) {
    return exp(x) / y;
}

void saveDenseNeuralNet(DenseNeuralNet* net, const char* filename) {
    FILE* file = fopen(filename, "wb");
    fwrite(&net->numLayers, sizeof(int), 1, file);
    fwrite(net->layerSizes, sizeof(int), net->numLayers, file);
    for (int i = 0; i < net->numLayers; i++) {
        DenseLayer* layer = &net->layers[i];
        fwrite(&layer->numNeurons, sizeof(int), 1, file);
        fwrite(&layer->inputSize, sizeof(int), 1, file);
        for (int j = 0; j < layer->numNeurons; j++) {
            Neuron* neuron = &layer->neurons[j];
            fwrite(&neuron->bias, sizeof(double), 1, file);
            fwrite(neuron->weights, sizeof(double), layer->inputSize, file);
        }
    }
    fclose(file);
}

void loadDenseNeuralNet(DenseNeuralNet* net, const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        fprintf(stderr, "Error: Could not open file %s\n", filename);
        return;
    }
    fread(&net->numLayers, sizeof(int), 1, file);
    net->layerSizes = (int*)malloc(net->numLayers * sizeof(int));
    fread(net->layerSizes, sizeof(int), net->numLayers, file);
    net->layers = (DenseLayer*)malloc(net->numLayers * sizeof(DenseLayer));
    for (int i = 0; i < net->numLayers; i++) {
        DenseLayer* layer = &net->layers[i];
        fread(&layer->numNeurons, sizeof(int), 1, file);
        fread(&layer->inputSize, sizeof(int), 1, file);
        layer->neurons = (Neuron*)malloc(layer->numNeurons * sizeof(Neuron));
        layer->outputs = (double*)malloc(layer->numNeurons * sizeof(double));
        Activation defaultAct = Sigmoid;
        for (int j = 0; j < layer->numNeurons; j++) {
            Neuron* neuron = &layer->neurons[j];
            fread(&neuron->bias, sizeof(double), 1, file);
            if (i == net->numLayers - 1) {
                neuron->activationFunction = sigmoid;
                neuron->derivativeFunction = sigmoidDerivative;
            } else {
                switch (defaultAct) {
                    case Sigmoid:
                        neuron->activationFunction = sigmoid;
                        neuron->derivativeFunction = sigmoidDerivative;
                        break;
                    case Relu:
                        neuron->activationFunction = relu;
                        neuron->derivativeFunction = reluDerivative;
                        break;
                    case Tanh:
                        neuron->activationFunction = tanh;
                        neuron->derivativeFunction = tanhDerivative;
                        break;
                }
            }
            neuron->numInputs = layer->inputSize; // Set the number of inputs
            neuron->weights = (double*)malloc(layer->inputSize * sizeof(double));
            fread(neuron->weights, sizeof(double), layer->inputSize, file);
        }
    }
    fclose(file);
}

void freeNeuron(Neuron* neuron) {
    free(neuron->weights);
    free(neuron);
}

void freeDenseLayer(DenseLayer* layer) {
    for (int i = 0; i < layer->numNeurons; i++) {
        freeNeuron(&layer->neurons[i]);
    }
    free(layer->neurons);
    free(layer);
}

void freeDenseNeuralNet(DenseNeuralNet* net) {
    for (int i = 0; i < net->numLayers; i++) {
        freeDenseLayer(&net->layers[i]);
    }
    free(net->layerSizes);
    free(net->layers);
    free(net);
}


