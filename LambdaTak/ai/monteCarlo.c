#include "monteCarlo.h"

Move monteCarloTreeSearch(GameState* state, int timeLimit, DenseNeuralNet* net) {
    printf("Starting MCTS with Neural Net\n");

    double startTime = getTimeMs();
    double endTime = startTime + timeLimit;

    Color rootColor = state->turn;
    MCTSNode* root = createMCTSNode(rootColor, NULL, 1.0, (Move){0});

    // Expand root node
    expand(root, state, 1.0, net);

    int curIteration = 0;
    Move bestMove = {0};
    while (curIteration < MAX_MCTS_ITERATIONS && getTimeMs() < endTime) {
        // Use the same state throughout - we'll make/unmake moves as we go
        MCTSNode* selected = selectNodeWithMakeUnmake(root, state, net);

        if (selected->numVisits < MIN_PLAYOUTS_PER_NODE) {
            expand(selected, state, 1.0, net);
        } 

        // Simulate from current position
        double value = simulateWithMakeUnmake(state, net);

        // Restore state to root position by unmaking moves
        restoreToRoot(selected, state);

        // Update statistics
        backup(selected, value);

        curIteration++;
    }

    // Find best move based on visit count
    MCTSNode* bestChild = NULL;
    int maxVisits = -1;
    for (u32 i = 0; i < root->numChildren; i++) {
        MCTSNode* child = root->children[i];
        printf("Move: %s, Visits: %d, Value: %f\n", 
                moveToString(&child->move), 
                child->numVisits, 
                child->numVisits > 0 ? child->valueSum / child->numVisits : 0.0);

        if (child->numVisits > maxVisits) {
            maxVisits = child->numVisits;
            bestChild = child;
        }
    }
    bestMove = bestChild ? bestChild->move : (Move){0};

    printf("Best move: %s\n", moveToString(&bestMove));
    printf("MCTS iterations: %d\n", curIteration);
    printf("Score: %f\n", bestChild ? (bestChild->valueSum / bestChild->numVisits) : 0.0);

    freeMCTSNode(root);
    return bestMove;
}

MCTSNode* selectNodeWithMakeUnmake(MCTSNode* node, GameState* state, DenseNeuralNet* net) {
    MCTSNode* cur = node;

    while (cur && cur->numChildren > 0) {
        double maxScore = -INFINITY;
        MCTSNode* bestChild = NULL;

        for (u32 i = 0; i < cur->numChildren; i++) {
            MCTSNode* child = cur->children[i];
            double score = ucbScore(cur, child);
            if (score > maxScore) {
                maxScore = score;
                bestChild = child;
            }
        }

        cur = bestChild;
        // Make the move to update the state
        makeMoveNoChecks(state, &cur->move, false);
    }

    return cur;
}

void restoreToRoot(MCTSNode* node, GameState* state) {
    // Create a stack of moves to undo
    Move moveStack[5];  // Adjust size based on your maximum tree depth
    int stackSize = 0;

    // Traverse up the tree to collect moves
    MCTSNode* current = node;
    while (current->parent) {
        moveStack[stackSize++] = current->move;
        current = current->parent;
    }

    // Undo all moves in reverse order
    for (int i = 0; i < stackSize; i++) {
        undoMoveNoChecks(state, &moveStack[i], false);
    }
}

MCTSNode* expand(MCTSNode* node, GameState* state, double prior, DenseNeuralNet* net) {
    GeneratedMoves* gm = generateAllMoves(state, 512);

    node->children = (MCTSNode**)malloc(sizeof(MCTSNode*) * gm->numMoves);
    node->numChildren = gm->numMoves;
    Color childColor = oppositeColor(node->toPlay);

    // Batch evaluation approach - prepare to evaluate all child positions at once
    double** inputs = (double**)malloc(sizeof(double*) * gm->numMoves);

    for (u32 i = 0; i < gm->numMoves; i++) {
        Move move = gm->moves[i];
        makeMoveNoChecks(state, &move, false);

        // Store game state representation for batch evaluation
        inputs[i] = gameStateToVector(state);

        // Undo the move to restore state
        undoMoveNoChecks(state, &move, false);

        // Create node - we'll update the evaluation later
        node->children[i] = createMCTSNode(childColor, node, prior, move);
    }

    // Efficiently batch evaluate all positions
    double* evaluations = batchEvaluateWithNN(inputs, gm->numMoves, net);

    // Update nodes with evaluations and free memory
    for (u32 i = 0; i < gm->numMoves; i++) {
        node->children[i]->prior = evaluations[i];
        free(inputs[i]);  // Free the input vector
    }

    free(inputs);
    free(evaluations);
    freeGeneratedMoves(gm);
    return node;
}

double simulateWithMakeUnmake(GameState* state, DenseNeuralNet* net) {
    // Get neural network evaluation of current position
    double* input = gameStateToVector(state);
    double* output = feedForwardDense(net, 7 * 36, input, 0.0);
    double nnEval = output[0];

    free(input);
    free(output);

    // Store moves made during simulation to undo them later
    Move moveStack[MAX_TURNS];
    int numMoves = 0;

    // Fast random rollout
    int i = 0;
    Result gameResult = CONTINUE;
    while ((gameResult = checkGameResult(state)) == CONTINUE && i < MAX_TURNS) {
        GeneratedMoves* moves = generateAllMoves(state, 512);
        if (moves->numMoves == 0) break;

        Move randomMove = moves->moves[rand() % moves->numMoves];
        moveStack[numMoves++] = randomMove;

        makeMoveNoChecks(state, &randomMove, false);
        freeGeneratedMoves(moves);
        i++;
    }

    // Determine result value
    double resultValue = 0.0;
    if (gameResult != CONTINUE) {
        switch (gameResult) {
            case ROAD_WHITE:
            case FLAT_WHITE:
                resultValue = 1.0;
                break;
            case ROAD_BLACK:
            case FLAT_BLACK:
                resultValue = -1.0;
                break;
            default:
                resultValue = 0.0;
                break;
        }
    }

    // Undo all moves made during simulation
    for (int j = numMoves - 1; j >= 0; j--) {
        undoMoveNoChecks(state, &moveStack[j], false);
    }

    // Blend neural net evaluation with rollout result
    return (gameResult != CONTINUE) ? resultValue : nnEval;
}

// Helper function for batch evaluations
double* batchEvaluateWithNN(double** inputs, int numInputs, DenseNeuralNet* net) {
    double* evaluations = (double*)malloc(sizeof(double) * numInputs);

    // In a real implementation, you'd want to actually batch the evaluations
    // through the neural network, but for now we'll just evaluate them individually
    for (int i = 0; i < numInputs; i++) {
        double* output = feedForwardDense(net, 7 * 36, inputs[i], 0.0);
        evaluations[i] = output[0];
        free(output);
    }

    return evaluations;
}

double ucbScore(MCTSNode* parent, MCTSNode* child) {
    if (child->numVisits < MIN_PLAYOUTS_PER_NODE) {
        return INFINITY;
    }

    double exploitation = child->valueSum / (double)child->numVisits;

    double exploration = DEFAULT_UCT_CONSTANT * 
        child->prior * sqrt(log(parent->numVisits + 1) / (1 + child->numVisits));

    return exploitation + exploration;
}

void backup(MCTSNode* node, double value) {
    MCTSNode* cur = node;
    while (cur) {
        cur->numVisits++;
        cur->valueSum += (cur->toPlay == WHITE) ? value : -value;
        value = -value;
        cur = cur->parent;
    }
}

MCTSNode* createMCTSNode(Color toPlay, MCTSNode* parent, double prior, Move move) {
    MCTSNode* node = (MCTSNode*)malloc(sizeof(MCTSNode));
    node->numVisits = 0;
    node->toPlay = toPlay;
    node->prior = prior;
    node->parent = parent;
    node->children = NULL;
    node->numChildren = 0;
    node->valueSum = 0;
    node->move = move;
    return node;
}

void freeMCTSNode(MCTSNode* node) {
    if (node->children) {
        for (u32 i = 0; i < node->numChildren; i++) {
            freeMCTSNode(node->children[i]);
        }
        free(node->children);
    }
    free(node);
}

#pragma inline
static double getTimeMs() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000.0 + ts.tv_nsec / 1.0e6;
}
