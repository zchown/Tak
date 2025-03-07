#include <Accelerate/Accelerate.h>
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
    if (!net || !inputs || inputSize <= 0) return NULL;

    // Calculate maximum layer size to allocate workspace
    int maxLayerSize = 0;
    for (int i = 0; i < net->numLayers; i++) {
        if (net->layers[i].numNeurons > maxLayerSize) {
            maxLayerSize = net->layers[i].numNeurons;
        }
    }
    
    // Allocate two workspaces to alternate between
    double* workspace1 = malloc(maxLayerSize * sizeof(double));
    double* workspace2 = malloc(maxLayerSize * sizeof(double));
    if (!workspace1 || !workspace2) {
        free(workspace1);
        free(workspace2);
        return NULL; // Memory allocation failed
    }
    
    double* curOutput = workspace1;
    double* nextOutput = workspace2;
    double* curInput = inputs;

    // Process all layers
    for (int i = 0; i < net->numLayers; i++) {
        DenseLayer* layer = &net->layers[i];
        const int M = layer->numNeurons;
        const int N = 1;  // Single input vector
        const int K = layer->inputSize;

        // Flatten weights into contiguous array [M x K]
        double* weightsFlat = malloc(M * K * sizeof(double));
        if (!weightsFlat) {
            free(workspace1);
            free(workspace2);
            return NULL;
        }
        
        for (int n = 0; n < M; n++) {
            memcpy(weightsFlat + n*K, layer->neurons[n].weights, K*sizeof(double));
        }

        // Matrix multiplication: output = weights * input
        vDSP_mmulD(weightsFlat, 1, 
                  curInput, 1,
                  curOutput, 1,
                  M, N, K);

        // Add biases
        double* biases = malloc(M * sizeof(double));
        if (!biases) {
            free(weightsFlat);
            free(workspace1);
            free(workspace2);
            return NULL;
        }
        
        for (int n = 0; n < M; n++) {
            biases[n] = layer->neurons[n].bias;
        }
        vDSP_vaddD(curOutput, 1, biases, 1, curOutput, 1, M);

        // Apply activation function (vectorized)
        if (layer->neurons[0].activationFunction == sigmoid) {
            for (int j = 0; j < M; j++) {
                curOutput[j] = sigmoid(curOutput[j]);
            }
        } else if (layer->neurons[0].activationFunction == relu) {
            vDSP_vthrD(curOutput, 1, &(double){0.0}, curOutput, 1, M);  // ReLU
        }

        // Store outputs in layer for backpropagation use
        memcpy(layer->outputs, curOutput, M * sizeof(double));
        
        if (i != net->numLayers - 1) {
            curInput = curOutput;
            double* temp = curOutput;
            curOutput = nextOutput;
            nextOutput = temp;
        }

        free(weightsFlat);
        free(biases);
    }

    double* result = malloc(net->layers[net->numLayers-1].numNeurons * sizeof(double));
    if (result) {
        memcpy(result, curOutput, net->layers[net->numLayers-1].numNeurons * sizeof(double));
    }
    
    free(workspace1);
    free(workspace2);
    
    return result;
}

void backpropagateDense(DenseNeuralNet* net, double* inputs, double* outputs, 
                       double* expectedOutputs, double learningRate) {
    if (!net || !inputs || !expectedOutputs) return;

    const int numLayers = net->numLayers;
    double** deltas = malloc(numLayers * sizeof(double*));
    
    // Output layer delta
    deltas[numLayers-1] = malloc(net->layers[numLayers-1].numNeurons * sizeof(double));
    vDSP_vsubD(expectedOutputs, 1, outputs, 1, deltas[numLayers-1], 1, 
              net->layers[numLayers-1].numNeurons);
    
    // Hidden layer deltas
    for(int i=numLayers-2; i>=0; i--) {
        const int currentSize = net->layers[i].numNeurons;
        const int nextSize = net->layers[i+1].numNeurons;
        
        // Transpose weights matrix [nextSize x currentSize]
        double* weightsT = malloc(currentSize * nextSize * sizeof(double));
        for(int n=0; n<nextSize; n++) {
            for(int k=0; k<currentSize; k++) {
                weightsT[k*nextSize + n] = net->layers[i+1].neurons[n].weights[k];
            }
        }
        
        // delta = (weights^T * nextDelta) ⊙ derivative
        deltas[i] = malloc(currentSize * sizeof(double));
        vDSP_mmulD(weightsT, 1, deltas[i+1], 1, deltas[i], 1,
                  currentSize, 1, nextSize);
        
        // Apply derivative
        if(net->layers[i].neurons[0].derivativeFunction == sigmoidDerivative) {
            #pragma omp parallel for simd
            for(int j=0; j<currentSize; j++) {
                deltas[i][j] *= outputs[j] * (1 - outputs[j]);
            }
        }
        
        free(weightsT);
    }

    // Update weights and biases
    #pragma omp parallel for
    for(int i=0; i<numLayers; i++) {
        DenseLayer* layer = &net->layers[i];
        const int M = layer->numNeurons;
        const int K = layer->inputSize;
        
        // Flatten weights and deltas
        double* weightUpdates = calloc(M*K, sizeof(double));
        double* biasUpdates = calloc(M, sizeof(double));
        
        // Compute weight updates: ΔW = learningRate * delta * input^T
        vDSP_mmulD(deltas[i], 1, 
                  (i == 0) ? inputs : net->layers[i-1].outputs, 1,
                  weightUpdates, 1,
                  M, K, 1);
        
        // Apply learning rate
        double negLearningRate = -learningRate;
        vDSP_vsmulD(weightUpdates, 1, &negLearningRate, weightUpdates, 1, M*K);
        vDSP_vsmulD(deltas[i], 1, &negLearningRate, biasUpdates, 1, M);

        // Update weights
        double* weightsFlat = malloc(M*K * sizeof(double));
        for(int n=0; n<M; n++) {
            memcpy(weightsFlat + n*K, layer->neurons[n].weights, K*sizeof(double));
        }
        vDSP_vaddD(weightsFlat, 1, weightUpdates, 1, weightsFlat, 1, M*K);
        
        // Distribute back to individual neurons
        for(int n=0; n<M; n++) {
            memcpy(layer->neurons[n].weights, weightsFlat + n*K, K*sizeof(double));
            layer->neurons[n].bias += biasUpdates[n];
        }

        free(weightUpdates);
        free(biasUpdates);
        free(weightsFlat);
    }

    // Cleanup
    for(int i=0; i<numLayers; i++) free(deltas[i]);
    free(deltas);
}

inline double sigmoid(double x) {
    return 1.0 / (1.0 + exp(-x));
}

inline double relu(double x) {
    return x > 0 ? x : 0;
}

inline double tanh(double x) {
    return (exp(x) - exp(-x)) / (exp(x) + exp(-x));
}

inline double sigmoidDerivative(double activated_output) {
    return activated_output * (1 - activated_output);
}


inline double reluDerivative(double x) {
    return x > 0 ? 1 : 0;
}

inline double tanhDerivative(double x) {
    return 1 - pow(tanh(x), 2);
}

inline double meanSquaredError(double x, double y) {
    return 0.5 * pow(x - y, 2);
}

inline double crossEntropy(double x, double y) {
    return -y * log(x) - (1 - y) * log(1 - x);
}

inline double softmax(double x, double y) {
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


void printDenseNeuralNet(DenseNeuralNet* net) {
    printf("Neural Net:\n");
    printf("Number of layers: %d\n", net->numLayers);
    for (int i = 0; i < net->numLayers; i++) {
        DenseLayer* layer = &net->layers[i];
        printf("Layer %d: %d neurons, %d inputs\n", i, layer->numNeurons, layer->inputSize);
        for (int j = 0; j < layer->numNeurons; j++) {
            Neuron* neuron = &layer->neurons[j];
            printf("Neuron %d: Bias: %f\n", j, neuron->bias);
            for (int k = 0; k < layer->inputSize; k++) {
                printf("Weight %d: %f\n", k, neuron->weights[k]);
            }
        }
    }
}
