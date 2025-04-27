#include "accelerateNeuralNet.h"

void compile_logger(BNNSGraphMessageLevel level, 
        const char* msg, 
        const char* location, 
        bnns_user_message_data_t* data) {
    printf("[BNNS] %s\n", msg);
}

int createGraph(const char* modelPath, bnns_graph_t* outGraph, bnns_graph_context_t* outContext) {
    struct stat buffer;
    if (stat(modelPath, &buffer) != 0) {
        fprintf(stderr, "Model missing: %s\n", modelPath);
        return 1;
    }

    bnns_graph_compile_options_t options = BNNSGraphCompileOptionsMakeDefault();
    BNNSGraphCompileOptionsSetMessageLogMask(options, 0xF);
    BNNSGraphCompileOptionsSetMessageLogCallback(options, compile_logger, NULL);

    *outGraph = BNNSGraphCompileFromFile(modelPath, NULL, options);

    if (outGraph->data == NULL) {
        fprintf(stderr, "Compilation failed\n");
        BNNSGraphCompileOptionsDestroy(options);
        return 1;
    }

    // Validate I/O count
    size_t num_inputs = BNNSGraphGetInputCount(*outGraph, NULL);
    size_t num_outputs = BNNSGraphGetOutputCount(*outGraph, NULL);
    /* printf("Model requires: %zu inputs, %zu outputs\n", num_inputs, num_outputs); */

    if (num_inputs != 1 || num_outputs != 1) {
        fprintf(stderr, "I/O count mismatch\n");
        BNNSGraphCompileOptionsDestroy(options);
        return 1;
    }

    *outContext = BNNSGraphContextMake(*outGraph);
    BNNSGraphCompileOptionsDestroy(options);

    if (outContext->data == NULL) {
        fprintf(stderr, "Context creation failed\n");
        return 1;
    }

    return 0;
}

GraphNN* loadGraphNN(const char* modelPath, size_t inputSize, size_t outputSize) {
    GraphNN* nn = malloc(sizeof(GraphNN));
    if (!nn) return NULL;

    char command[256];
    // hardcoded path to the model
    sprintf(command, "xcrun coremlcompiler compile neurelnet.mlpackage .");
    int ret = system(command);
    if (ret != 0) {
        fprintf(stderr, "Model compilation failed\n");
        free(nn);
        return NULL;
    }

    const size_t input_bytes = inputSize * sizeof(float);
    const size_t output_bytes = outputSize * sizeof(float);

    nn->inputBuffer = aligned_alloc(64, aligned_size(input_bytes));
    if (!nn->inputBuffer) {
        fprintf(stderr, "Input buffer alloc failed (%zu bytes)\n", input_bytes);
        free(nn);
        return NULL;
    }

    nn->outputBuffer = aligned_alloc(64, aligned_size(output_bytes));
    if (!nn->outputBuffer) {
        fprintf(stderr, "Output buffer alloc failed (%zu bytes)\n", output_bytes);
        free(nn->inputBuffer);
        free(nn);
        return NULL;
    }

    if (createGraph("neurelnet.mlmodelc", &nn->graph, &nn->context) != 0) {
        free(nn->inputBuffer);
        free(nn->outputBuffer);
        free(nn);
        return NULL;
    }

    nn->inputSize = inputSize;
    nn->outputSize = outputSize;
    return nn;
}

int predictGraphNN(GraphNN* nn, const double* input, double* output) {
    bnns_graph_argument_t args[2];

    bnns_graph_shape_t in_shape = {.rank = 1, .shape = &(uint64_t){nn->inputSize}};
    args[1].data_ptr = nn->inputBuffer;
    args[1].data_ptr_size = nn->inputSize * sizeof(float);

    bnns_graph_shape_t out_shape = {.rank = 1, .shape = &(uint64_t){nn->outputSize}};
    args[0].data_ptr = nn->outputBuffer;
    args[0].data_ptr_size = nn->outputSize * sizeof(float);

    // Convert input
    for (size_t i = 0; i < nn->inputSize; i++) {
        nn->inputBuffer[i] = (float)input[i];
    }

    int ret = BNNSGraphContextExecute(nn->context, NULL, 2, args, 0, NULL);
    if (ret != 0) return ret;

    // Convert output
    for (size_t i = 0; i < nn->outputSize; i++) {
        output[i] = nn->outputBuffer[i];
    }

    return 0;
}

void freeGraphNN(GraphNN* nn) {
    if (nn) {
        free(nn->inputBuffer);
        free(nn->outputBuffer);
        BNNSGraphContextDestroy(nn->context);
        free(nn);
    }
}
