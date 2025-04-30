#include "accelerateNeuralNet.h"

static size_t aligned_size(size_t size) {
    return (size + 63) & ~63;  // 64-byte alignment
}

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

    size_t num_inputs = BNNSGraphGetInputCount(*outGraph, NULL);
    size_t num_outputs = BNNSGraphGetOutputCount(*outGraph, NULL);
    printf("Model requires: %zu inputs, %zu outputs\n", num_inputs, num_outputs);

    if (num_inputs != 1 || num_outputs != 3) {
        fprintf(stderr, "I/O count mismatch - expected 1 input, 3 outputs\n");
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

GraphNN* loadGraphNN(const char* modelPath, size_t inputSize, 
                    size_t combinedOutputSize, size_t policyOutputSize, size_t valueOutputSize) {
    GraphNN* nn = malloc(sizeof(GraphNN));
    if (!nn) return NULL;

    char command[256];
    sprintf(command, "xcrun coremlcompiler compile %s .", modelPath);
    int ret = system(command);
    if (ret != 0) {
        fprintf(stderr, "Model compilation failed\n");
        free(nn);
        return NULL;
    }

    const size_t input_bytes = inputSize * sizeof(float);
    nn->inputBuffer = aligned_alloc(64, aligned_size(input_bytes));
    if (!nn->inputBuffer) {
        fprintf(stderr, "Input buffer alloc failed (%zu bytes)\n", input_bytes);
        free(nn);
        return NULL;
    }

    const size_t combined_bytes = combinedOutputSize * sizeof(float);
    nn->combinedOutputBuffer = aligned_alloc(64, aligned_size(combined_bytes));
    if (!nn->combinedOutputBuffer) {
        fprintf(stderr, "Combined output buffer alloc failed (%zu bytes)\n", combined_bytes);
        free(nn->inputBuffer);
        free(nn);
        return NULL;
    }

    const size_t policy_bytes = policyOutputSize * sizeof(float);
    nn->policyOutputBuffer = aligned_alloc(64, aligned_size(policy_bytes));
    if (!nn->policyOutputBuffer) {
        fprintf(stderr, "Policy output buffer alloc failed (%zu bytes)\n", policy_bytes);
        free(nn->inputBuffer);
        free(nn->combinedOutputBuffer);
        free(nn);
        return NULL;
    }

    const size_t value_bytes = valueOutputSize * sizeof(float);
    nn->valueOutputBuffer = aligned_alloc(64, aligned_size(value_bytes));
    if (!nn->valueOutputBuffer) {
        fprintf(stderr, "Value output buffer alloc failed (%zu bytes)\n", value_bytes);
        free(nn->inputBuffer);
        free(nn->combinedOutputBuffer);
        free(nn->policyOutputBuffer);
        free(nn);
        return NULL;
    }

    if (createGraph("neurelnet.mlmodelc", &nn->graph, &nn->context) != 0) {
        free(nn->inputBuffer);
        free(nn->combinedOutputBuffer);
        free(nn->policyOutputBuffer);
        free(nn->valueOutputBuffer);
        free(nn);
        return NULL;
    }

    nn->inputSize = inputSize;
    nn->combinedOutputSize = combinedOutputSize;
    nn->policyOutputSize = policyOutputSize;
    nn->valueOutputSize = valueOutputSize;
    nn->totalOutputSize = combinedOutputSize + policyOutputSize + valueOutputSize;

    return nn;
}

int predictGraphNN(GraphNN* nn, const double* input, double* output) {
    bnns_graph_argument_t args[4];

    bnns_graph_shape_t in_shape = {.rank = 1, .shape = &(uint64_t){nn->inputSize}};
    args[3].data_ptr = nn->inputBuffer;
    args[3].data_ptr_size = nn->inputSize * sizeof(float);

    bnns_graph_shape_t combined_shape = {.rank = 1, .shape = &(uint64_t){nn->combinedOutputSize}};
    args[0].data_ptr = nn->combinedOutputBuffer;
    args[0].data_ptr_size = nn->combinedOutputSize * sizeof(float);

    bnns_graph_shape_t policy_shape = {.rank = 1, .shape = &(uint64_t){nn->policyOutputSize}};
    args[1].data_ptr = nn->policyOutputBuffer;
    args[1].data_ptr_size = nn->policyOutputSize * sizeof(float);

    bnns_graph_shape_t value_shape = {.rank = 1, .shape = &(uint64_t){nn->valueOutputSize}};
    args[2].data_ptr = nn->valueOutputBuffer;
    args[2].data_ptr_size = nn->valueOutputSize * sizeof(float);

    for (size_t i = 0; i < nn->inputSize; i++) {
        nn->inputBuffer[i] = (float)input[i];
    }

    int ret = BNNSGraphContextExecute(nn->context, NULL, 4, args, 0, NULL);
    if (ret != 0) {
        fprintf(stderr, "Graph execution failed with error code: %d\n", ret);
        return ret;
    }

    size_t outputIndex = 0;

    output[outputIndex++] = tanh(nn->combinedOutputBuffer[0]);

    for (size_t i = 1; i < nn->combinedOutputSize; i++) {
        output[outputIndex++] = tanh(nn->combinedOutputBuffer[i]);
    }

    return 0;
}

void freeGraphNN(GraphNN* nn) {
    if (nn) {
        free(nn->inputBuffer);
        free(nn->combinedOutputBuffer);
        free(nn->policyOutputBuffer);
        free(nn->valueOutputBuffer);
        BNNSGraphContextDestroy(nn->context);
        free(nn);
    }
}
