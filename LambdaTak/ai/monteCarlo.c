#include "monteCarlo.h"

Move monteCarloTreeSearch(GameState* state, int timeLimit, DenseNeuralNet* net) {
    printf("Starting MCTS with Neural Net\n");

    double startTime = getTimeMs();
    double endTime = startTime + timeLimit;

    Color rootColor = state->turn;
    MCTSNode* root = createMCTSNode(rootColor, NULL, 1.0, (Move){0});

    GeneratedMoves* gm = generateAllMoves(state, 512);
    Move winningMove = {0};
    bool foundWinningMove = false;
    for (u32 i = 0; i < gm->numMoves && !foundWinningMove; i++) {
        makeMoveNoChecks(state, &gm->moves[i], false);
        Result result = checkGameResult(state);
        if ((rootColor == WHITE && (result == ROAD_WHITE || result == FLAT_WHITE)) ||
            (rootColor == BLACK && (result == ROAD_BLACK || result == FLAT_BLACK))) {
            winningMove = gm->moves[i];
            foundWinningMove = true;
        }
        undoMoveNoChecks(state, &gm->moves[i], false);
    }
    if (foundWinningMove) {
        freeGeneratedMoves(gm);
        printf("Found winning move: %s\n", moveToString(&winningMove));
        return winningMove;
    }
    freeGeneratedMoves(gm);

    expand(root, state, 1.0, net);

    int curIteration = 0;
    Move bestMove = {0};
    while (curIteration < MAX_MCTS_ITERATIONS && getTimeMs() < endTime) {
        GameState* simState = copyGameState(state);

        MCTSNode* selected = selectNode(root, simState, net);
        if (selected->numVisits < MIN_PLAYOUTS_PER_NODE) {
            expand(selected, simState, 1.0, net);
        } 

        double value = simulate(simState, net);
        backup(selected, value);

        freeGameState(simState);
        curIteration++;
    }
    
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

MCTSNode* selectNode(MCTSNode* node, GameState* state, DenseNeuralNet* net) {
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
        makeMoveNoChecks(state, &cur->move, false);
    }

    return cur;
}

MCTSNode* expand(MCTSNode* node, GameState* state, double prior, DenseNeuralNet* net) {
    GeneratedMoves* gm = generateAllMoves(state, 512);

    node->children = (MCTSNode**)malloc(sizeof(MCTSNode*) * gm->numMoves);
    node->numChildren = gm->numMoves;
    Color childColor = oppositeColor(node->toPlay);

    for (u32 i = 0; i < gm->numMoves; i++) {
        Move move = gm->moves[i];
        makeMoveNoChecks(state, &move, false);
        double evaluation = evaluateStateWithNN(state, net);
        undoMoveNoChecks(state, &move, false);

        node->children[i] = createMCTSNode(childColor, node, evaluation, move);
    }

    freeGeneratedMoves(gm);
    return node;
}

double simulate(GameState* state, DenseNeuralNet* net) {
    double* input = gameStateToVector(state);
    double* output = feedForwardDense(net, 7 * 36, input, 0.0);

    int i = 0;
    while (checkGameResult(state) == CONTINUE && i < MAX_TURNS) {
        GeneratedMoves* moves = generateAllMoves(state, 512);
        makeMoveNoChecks(state, &moves->moves[rand() % moves->numMoves], false);
        freeGeneratedMoves(moves);
    }

    // 1.0 for WHITE win, -1.0 for BLACK win
    Result result = checkGameResult(state);

    double resultValue = 0;
    switch (result) {
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
    return (resultValue + output[0]) / 2.0;
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

double evaluateStateWithNN(GameState* state, DenseNeuralNet* net) {
    double* input = gameStateToVector(state);
    double* output = feedForwardDense(net, 7 * 36, input, 0.0);
    double result = output[0];
    free(output);
    return result;
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
