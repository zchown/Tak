#include "monteCarlo.h"


Move monteCarloTreeSearch(GameState* state, int timeLimit, DenseNeuralNet* net) {
    double startTime = getTimeMs();
    double endTime = startTime + timeLimit;

    Color rootColor = state->turn;
    MCTSNode* root = createMCTSNode(rootColor, NULL, 1.0, (Move){0});

    expand(root, state, 1.0, net);

    int curIteration = 0;
    Move bestMove = {0};

    while (curIteration < MAX_MCTS_ITERATIONS && getTimeMs() < endTime) {
        Move moveStack[MAX_MCTS_DEPTH];
        int moveCount = 0;

        MCTSNode* selected = selectNode(root, state, net, moveStack, &moveCount);

        if (selected->numVisits < MIN_PLAYOUTS_PER_NODE) {
            expand(selected, state, 1.0, net);
        } 

        double value = simulate(state, net);
        if (rootColor == BLACK) {
            value = -value;
        }
        backup(selected, value);

        for (int i = moveCount - 1; i >= 0; i--) {
            undoMoveNoChecks(state, &moveStack[i], false);
        }
        curIteration++;
    }

    MCTSNode* bestChild = NULL;
    int maxVisits = -1;
    for (u32 i = 0; i < root->numChildren; i++) {
        MCTSNode* child = root->children[i];
        if (child->numVisits > maxVisits) {
            maxVisits = child->numVisits;
            bestChild = child;
        }
    }
    bestMove = bestChild ? bestChild->move : (Move){0};

    /* printf("Best move: %s\n", moveToString(&bestMove)); */
    /* printf("MCTS iterations: %d\n", curIteration); */
    /* printf("Score: %f\n", bestChild ? (bestChild->valueSum / bestChild->numVisits) : 0.0); */

    freeMCTSNode(root);
    /* printf("MCTS time: %f ms\n", getTimeMs() - startTime); */
    return bestMove;
}

// Modified selection function that applies moves to the state and records them in a move stack.
MCTSNode* selectNode(MCTSNode* node, GameState* state, DenseNeuralNet* net, Move* moveStack, int* moveCount) {
    MCTSNode* cur = node;
    *moveCount = 0;
    while (cur->numChildren > 0) {
        MCTSNode* bestChild = NULL;
        // Prioritize any child that hasn't reached the minimum number of playouts.
        for (u32 i = 0; i < cur->numChildren; i++) {
            if (cur->children[i]->numVisits < MIN_PLAYOUTS_PER_NODE) {
                bestChild = cur->children[i];
                break;
            }
        }
        // If all children have sufficient playouts, use UCB to choose.
        if (!bestChild) {
            double maxScore = -INFINITY;
            for (u32 i = 0; i < cur->numChildren; i++) {
                MCTSNode* child = cur->children[i];
                double score = ucbScore(cur, child);
                if (score > maxScore) {
                    maxScore = score;
                    bestChild = child;
                }
            }
        }
        if (!bestChild)
            break;

        // Apply the chosen move to the state and record it.
        makeMoveNoChecks(state, &bestChild->move, false);
        moveStack[(*moveCount)++] = bestChild->move;
        cur = bestChild;
    }
    return cur;
}

MCTSNode* expand(MCTSNode* node, GameState* state, double prior, DenseNeuralNet* net) {
    GeneratedMoves* gm = generateAllMoves(state, 512);

    node->children = (MCTSNode**)malloc(sizeof(MCTSNode*) * gm->numMoves);
    node->numChildren = gm->numMoves;
    Color childColor = oppositeColor(node->toPlay);

    int moveScores[gm->numMoves];
    int moveTotal = 0;

    for (u32 i = 0; i < gm->numMoves; i++) {
        Move move = gm->moves[i];
        moveScores[i] = moveHeuristic(state, &move);
        moveTotal += moveScores[i];
        // Temporarily assign a prior probability of 0.5.
        node->children[i] = createMCTSNode(childColor, node, 0.5, move);
    }

    // Normalize priors based on move heuristics.
    for (u32 i = 0; i < gm->numMoves; i++) {
        node->children[i]->prior = (double)moveScores[i] / (double)moveTotal;
    }

    freeGeneratedMoves(gm);
    return node;
}

double simulate(GameState* state, DenseNeuralNet* net) {
    Result result = checkGameResult(state);
    double* gv = gameStateToVector(state);
    double resultValue = 0;
    switch (result) {
        case ROAD_WHITE:
        case FLAT_WHITE:
            resultValue = 100000000.0;
            break;
        case ROAD_BLACK:
        case FLAT_BLACK:
            resultValue = -100000000.0;
            break;
        default:
            resultValue = feedForwardDense(net, 7 * 36, gv, 0.0, true)[0];
            resultValue -= 0.5;
            resultValue *= 2;
            break;
    }
    free(gv);
    return resultValue;
}

void backup(MCTSNode* node, double value) {
    MCTSNode* cur = node;
    while (cur) {
        cur->numVisits++;
        cur->valueSum += value;
        value = -value;
        cur = cur->parent;
    }
}

double ucbScore(MCTSNode* parent, MCTSNode* child) {
    double exploitation = (child->numVisits > 0) ? (child->valueSum / (double)child->numVisits) : 0.0;
    double exploration = DEFAULT_UCT_CONSTANT * child->prior * sqrt(log(parent->numVisits + 1) / (1 + child->numVisits));
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
    if (node == NULL) {
        return;
    }
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

int moveHeuristic(const GameState* state, const Move* move) {
    int score = 0;

    Bitboard whiteControlled = state->whiteControlled;
    Bitboard blackControlled = state->blackControlled;
    Bitboard whiteInterest = (whiteControlled >> 6) | (whiteControlled << 6) |
                             (whiteControlled >> 1) | (whiteControlled << 1);
    Bitboard blackInterest = (blackControlled >> 6) | (blackControlled << 6) |
                             (blackControlled >> 1) | (blackControlled << 1);
    whiteInterest = whiteInterest & state->emptySquares;
    blackInterest = blackInterest & state->emptySquares;

    Bitboard ourInterest = (state->turn == WHITE) ? whiteInterest : blackInterest;
    Bitboard theirInterest = (state->turn == WHITE) ? blackInterest : whiteInterest;
    Bitboard ofInterest = blackInterest | whiteInterest;

    if (move->type == PLACE) {
        if (move->move.place.stone == CAP) {
            score += 1000;
            if (ourInterest & move->move.place.pos) {
                score += 1000;
            }
        } else if (move->move.place.stone == FLAT) {
            score += 600;
            if (ourInterest & move->move.place.pos) {
                score += 1000;
            }
        } else {
            score += 500;
            if (theirInterest & move->move.place.pos) {
                score += 1000;
            }
        }

        int minStonesRemaining = (state->player1.stones < state->player2.stones) ? state->player1.stones : state->player2.stones;
        if (minStonesRemaining < 10) {
            score += 500 * (11 - minStonesRemaining);
        }

        // Favor central placements.
        score += 75 - (GET_X(abs(move->move.place.pos) - BOARD_SIZE / 2) +
                       GET_Y(abs(move->move.place.pos) - BOARD_SIZE / 2));
        score += historyHeuristic[state->turn][move->move.place.pos][move->move.place.pos];
    } else if (move->type == SLIDE) {
        SlideMove mv = move->move.slide;
        score += 400;

        Bitboard mvBitboard = 0;
        for (int i = 0; i < mv.count; i++) {
            if (slidePosition(mv.startPos, mv.direction, i) <= TOTAL_SQUARES) {
                mvBitboard |= 1ULL << slidePosition(mv.startPos, mv.direction, i);
            }
        }
        if (ofInterest & mvBitboard) {
            score += 1000;
        }

        Bitboard enemyControlled = (state->turn == WHITE) ? blackControlled : whiteControlled;
        if (enemyControlled & mvBitboard) {
            score += 1000;
        }
        score += mv.count * mv.count * 10;
    }

    if (state->turnNumber < 3) {
        score = 0 - score;
    }

    return score;
}

