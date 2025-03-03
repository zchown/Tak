# include "monteCarlo.h"
# include "monteCarlo.h"
# include "qlearner.h"

Move monteCarloTreeSearch(GameState* state, int timeLimit, QLearningAgent* agent) {
    printf("Starting MCTS with Q-learning\n");

    double startTime = getTimeMs();
    double endTime = startTime + timeLimit;

    Color rootColor = state->turn;
    MCTSNode* root = createMCTSNode(rootColor, NULL, 1.0, (Move){0});

    expand(root, state, 1.0, agent);

    int curIteration = 0;
    while (curIteration < MAX_MCTS_ITERATIONS && getTimeMs() < endTime) {
        agent->epsilon = 0.0;

        GameState* simState = copyGameState(state);

        MCTSNode* selected = selectNode(root, simState, agent);

        if (selected->numVisits < MIN_PLAYOUTS_PER_NODE) {
            expand(selected, simState, 1.0, agent);
        } 

        double value = simulate(simState, agent);

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

    Move bestMove = bestChild ? bestChild->move : (Move){0};

    printf("Best move: %s\n", moveToString(&bestMove));
    printf("MCTS iterations: %d\n", curIteration);
    printf("Score: %f\n", bestChild ? (bestChild->valueSum / bestChild->numVisits) : 0.0);

    freeMCTSNode(root);
    return bestMove;
}

MCTSNode* selectNode(MCTSNode* node, GameState* state, QLearningAgent* agent) {
    MCTSNode* cur = node;

    while (cur && cur->numChildren > 0 && cur->numVisits > MIN_PLAYOUTS_PER_NODE) {
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

MCTSNode* expand(MCTSNode* node, GameState* state, double prior, QLearningAgent* agent) {
    GeneratedMoves* gm = generateAllMoves(state, 512);

    Features stateFeatures = malloc(sizeof(double) * FEATURES_SIZE);
    getFeatures(state, stateFeatures);

    node->children = (MCTSNode**)malloc(sizeof(MCTSNode*) * gm->numMoves);
    node->numChildren = gm->numMoves;

    Color childColor = oppositeColor(node->toPlay);

    for (u32 i = 0; i < gm->numMoves; i++) {
        Move move = gm->moves[i];

        double actionFeatures[ACTION_FEATURES_SIZE];
        getActionFeatures(&move, actionFeatures);

        double qValue = computeQValue(agent, stateFeatures, actionFeatures);

        if (isnan(qValue)) {
            qValue = 0.0;
        }

        node->children[i] = createMCTSNode(childColor, node, qValue, move);
    }

    freeGeneratedMoves(gm);
    free(stateFeatures);
    return node;
}

double simulate(GameState* state, QLearningAgent* agent) {
    agent->epsilon = 0.15;

    Features stateFeatures = malloc(sizeof(double) * FEATURES_SIZE);
    getFeatures(state, stateFeatures);

    while (checkGameResult(state) == CONTINUE) {
        GeneratedMoves* moves = generateAllMoves(state, 512);
        Move move = selectAction(agent, state, moves, stateFeatures);
        makeMoveNoChecks(state, &move, false);

        free(stateFeatures);
        stateFeatures = malloc(sizeof(double) * FEATURES_SIZE);
        getFeatures(state, stateFeatures);

        freeGeneratedMoves(moves);
    }

    // 1.0 for WHITE win, -1.0 for BLACK win
    Result result = checkGameResult(state);

    if (result == DRAW) {

        free(stateFeatures);
        return 0;
    } else if (result == CONTINUE) {
        agent->epsilon = 0.0;
        Move move = selectAction(agent, state, generateAllMoves(state, 512), stateFeatures);
        double actionFeatures[ACTION_FEATURES_SIZE];
        getActionFeatures(&move, actionFeatures);
        double qValue = computeQValue(agent, stateFeatures, actionFeatures);
        free(stateFeatures);
        if (isnan(qValue) || qValue == -INFINITY || qValue < -1.0) {
            qValue = 0.0;
        } else if (qValue > 1.0) {
            qValue = 1.0;
        }
        free(stateFeatures);
        return qValue;
    }

    free(stateFeatures);
    return (result == ROAD_WHITE || result == FLAT_WHITE) ? 1.0 : -1.0;
}

void backup(MCTSNode* node, double value) {
    MCTSNode* cur = node;

    while (cur) {
        cur->numVisits++;

        if (cur->toPlay == WHITE) {
            cur->valueSum += value;
        } else {
            cur->valueSum += -value;
        }

        value = -value;
        cur = cur->parent;
    }
}

double ucbScore(MCTSNode* parent, MCTSNode* child) {
    if (child->numVisits == 0) {
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
