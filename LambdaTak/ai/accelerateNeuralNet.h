#ifndef ACCELERATENEURALNET_H
#define ACCELERATENEURALNET_H

#include <Accelerate/Accelerate.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
    bnns_graph_t graph;
    bnns_graph_context_t context;
    size_t inputSize;
    size_t outputSize;
    float* inputBuffer;
    float* outputBuffer;
} GraphNN;

int createGraph(const char* modelPath, bnns_graph_t* outGraph, bnns_graph_context_t* outContext);

GraphNN* loadGraphNN(const char* modelPath, size_t inputSize, size_t outputSize);

int predictGraphNN(GraphNN* nn, const double* input, double* output);

void freeGraphNN(GraphNN* nn);

#endif // ACCELERATENEURALNET_H
