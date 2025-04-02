#ifndef NEURAL_NETWORKS_H
#define NEURAL_NETWORKS_H

#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#include <Accelerate/Accelerate.h>

typedef struct {
    double bias;
    double* weights;
    int numInputs;
    double (*activationFunction)(double);
    double (*derivativeFunction)(double);
} Neuron;

typedef struct {
    Neuron* neurons;
    double* outputs;
    int numNeurons;
    int inputSize;
} DenseLayer;

typedef struct {
    DenseLayer* layers;
    int* layerSizes;
    int numLayers;

} DenseNeuralNet;

typedef enum {
    Sigmoid,
    Relu,
    Tanh
} Activation;

typedef enum {
    MeanSquaredError,
    CrossEntropy,
    Softmax,
} LossFunction;

Neuron createNeuron(int numInputs, Activation act);

DenseLayer createDenseLayer(int numNeurons, int numInputs, Activation act);

DenseNeuralNet createDenseNeuralNet(int* layerSizes, int numLayers, Activation act);

double* feedForwardDense(DenseNeuralNet* net, int inputSize, double* inputs, double dropout);

void backpropagateDense(DenseNeuralNet* net, double* inputs, double* outputs, double* expectedOutputs, double learningRate);

//activation functions
double sigmoid(double x);
double relu(double x);
double tanh(double x);

// derivatives
double sigmoidDerivative(double x);
double reluDerivative(double x);
double tanhDerivative(double x);

//error functions
double meanSquaredError(double x, double y);
double crossEntropy(double x, double y);
double softmax(double x, double y);

void saveDenseNeuralNet(DenseNeuralNet* net, const char* filename);
void loadDenseNeuralNet(DenseNeuralNet* net, const char* filename);

void freeNeuron(Neuron* neuron);
void freeDenseLayer(DenseLayer* layer);
void freeDenseNeuralNet(DenseNeuralNet* net);

void printDenseNeuralNet(DenseNeuralNet* net);

#endif // NEURAL_NETWORKS_H
