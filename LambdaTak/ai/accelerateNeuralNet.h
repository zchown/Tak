#ifndef ACCELERATE_NEURAL_NET_H
#define ACCELERATE_NEURAL_NET_H

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <string.h>
#include <Accelerate/Accelerate.h>

typedef struct {
    bnns_graph_t graph;
    bnns_graph_context_t context;
    float* inputBuffer;
    size_t inputSize;
    float* combinedOutputBuffer;
    size_t combinedOutputSize;
    float* policyOutputBuffer;
    size_t policyOutputSize;
    float* valueOutputBuffer;
    size_t valueOutputSize;
    size_t totalOutputSize;
} GraphNN;

int createGraph(const char* modelPath, bnns_graph_t* outGraph, bnns_graph_context_t* outContext);

GraphNN* loadGraphNN(const char* modelPath, size_t inputSize, 
                    size_t combinedOutputSize, size_t policyOutputSize, size_t valueOutputSize);

int predictGraphNN(GraphNN* nn, const double* input, double* output);

void extractValueAndPolicy(const double* combinedOutput, size_t combinedSize, 
                          double* valueOut, double* policyOut);

void freeGraphNN(GraphNN* nn);

#endif /* ACCELERATE_NEURAL_NET_H */
