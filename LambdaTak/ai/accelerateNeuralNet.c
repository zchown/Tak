#include "accelerateNeuralNet.h"
#include <vecLib/BNNS/bnns_graph.h>

int createGraph(const char* modelPath, bnns_graph_t* outGraph, bnns_graph_context_t* outContext) {
    if (!modelPath || !outGraph || !outContext) return 1;
    *outGraph = BNNSGraphCompileFromFile(modelPath, NULL, (bnns_graph_compile_options_t){0});
    *outContext = BNNSGraphContextMake(*outGraph);

    return 0;
}

GraphNN* loadGraphNN(const char* modelPath, size_t inputSize, size_t outputSize) {
    GraphNN* nn = (GraphNN*)malloc(sizeof(GraphNN));
    if (!nn) return NULL;

    if (createGraph(modelPath, &nn->graph, &nn->context) != 0) {
        free(nn);
        return NULL;
    }

    nn->inputSize = inputSize;
    nn->outputSize = outputSize;
    nn->inputBuffer = (float*)malloc(inputSize * sizeof(float));
    nn->outputBuffer = (float*)malloc(outputSize * sizeof(float));

    if (!nn->inputBuffer || !nn->outputBuffer) {
        free(nn->inputBuffer);
        free(nn->outputBuffer);
        free(nn);
        return NULL;
    }

    return nn;
}

int predictGraphNN(GraphNN* nn, const double* input, double* output) {
    if (!nn || !input || !output) return 1;

    // Convert input to float
    for (size_t i = 0; i < nn->inputSize; i++) {
        nn->inputBuffer[i] = (float)input[i];
    }

    bnns_graph_argument_t arguments[2];

    arguments[0].data_ptr = nn->inputBuffer;
    arguments[0].data_ptr_size = nn->inputSize * sizeof(float);

    arguments[1].data_ptr = nn->outputBuffer;
    arguments[1].data_ptr_size = nn->outputSize * sizeof(float);

    // Execute the graph
    BNNSGraphContextExecute(
            nn->context,     // The graph context
            NULL,            // Default function (main graph function)
            2,               // Number of arguments (input and output)
            arguments,       // Array of arguments
            0,               // No workspace needed
            NULL             // No workspace provided
            );

    // Convert output to double
    for (size_t i = 0; i < nn->outputSize; i++) {
        output[i] = (double)nn->outputBuffer[i];
    }

    return 0;
}

